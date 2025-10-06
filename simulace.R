#Simulace, zatím nebudeme brát v potaz opakování domácností

#Knihovny
library(dplyr)
library(tidyr)
library(tibble)
library(openxlsx)
library(purrr)
library(nnet)
library(survey)
library(car)

#Data
setwd("C:/Users/tomik/OneDrive/Plocha/SFG_2025léto/Výpočet vah")
load("populace_rts.RData")
load("data_orig_weighted.RData")

N = nrow(populace_rts) #Velikost populace
n = nrow(data_orig_weighted) #Velikost samplu

# Velikost strat v samplu
stratum_counts = table(data_orig_weighted$ID_strata)

# ID strat
strata = names(stratum_counts)

#Populacni frekvence
pop_margins = list(
  ID_strata = setNames(as.data.frame(table(populace_rts$ID_strata)), c("ID_strata", "Freq")),
  vek_kat6  = setNames(as.data.frame(table(populace_rts$vek_kat6)),  c("vek_kat6", "Freq")),
  IDE_8     = setNames(as.data.frame(table(populace_rts$IDE_8)),     c("IDE_8", "Freq")),
  VZD       = setNames(as.data.frame(table(populace_rts$VZD)),       c("VZD", "Freq"))
)
# Očekávané počty pozorování v samplu (příprava na raking)
pop_margins$ID_strata$Freq = pop_margins$ID_strata$Freq / sum(pop_margins$ID_strata$Freq) * n
pop_margins$vek_kat6$Freq  = pop_margins$vek_kat6$Freq / sum(pop_margins$vek_kat6$Freq) * n
pop_margins$IDE_8$Freq     = pop_margins$IDE_8$Freq / sum(pop_margins$IDE_8$Freq) * n
pop_margins$VZD$Freq       = pop_margins$VZD$Freq / sum(pop_margins$VZD$Freq) * n

#Populacni frekvence
pop_margins2 = list(
  ID_strata = setNames(as.data.frame(table(populace_rts$ID_strata)), c("ID_strata", "Freq")),
  IDE_8     = setNames(as.data.frame(table(populace_rts$IDE_8)),     c("IDE_8", "Freq")),
  VZD       = setNames(as.data.frame(table(populace_rts$VZD)),       c("VZD", "Freq"))
)
# Očekávané počty pozorování v samplu (příprava na raking)
pop_margins2$ID_strata$Freq = pop_margins$ID_strata$Freq / sum(pop_margins$ID_strata$Freq) * n
pop_margins2$IDE_8$Freq     = pop_margins$IDE_8$Freq / sum(pop_margins$IDE_8$Freq) * n
pop_margins2$VZD$Freq       = pop_margins$VZD$Freq / sum(pop_margins$VZD$Freq) * n

# Jednotlive kategorie jako binarni promenne
populace_rts$cat1=ifelse(populace_rts$OZ_2 == 1,1,0)
populace_rts$cat2=ifelse(populace_rts$OZ_2 == 2,1,0)
populace_rts$cat3=ifelse(populace_rts$OZ_2 == 3,1,0)
populace_rts$cat4=ifelse(populace_rts$OZ_2 == 4,1,0)
populace_rts$cat5=ifelse(populace_rts$OZ_2 == 5,1,0)
populace_rts$cat9=ifelse(populace_rts$OZ_2 == 9,1,0)

## Simulace
B = 10  # pocet iteraci
categories = c("cat1","cat2","cat3","cat4","cat5","cat9")
targets = sapply(categories, function(cat) mean(populace_rts[[cat]])) # skutecne populacni proporce

results = list()

# Inicializace ukladani vysledku, nutno spustit pred kazdou simulaci
coverage_store <- list()
joint_store <- list()
length_store <- list()
mse_store <- list()

# Naformatovani
for (response in c("model","random")) {
  for (weight_type in c("srs","poststrat_weight","poststrat_weight_trim", "poststrat_weight_r", "poststrat_weight_r_trim")) {
    if (weight_type %in% c("srs")) {
      for (case in c("srs")) {
        nm <- paste(response,weight_type, case, sep="_")
        coverage_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        joint_store[[nm]] <- 0
        length_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        mse_store[[nm]] <- setNames(rep(0,length(categories)), categories)
      }
    }
    if (weight_type %in% c("poststrat_weight","poststrat_weight_trim")){
      for (case in c("full", "srs", "srs_ess")) {
        nm <- paste(response,weight_type, case, sep="_")
        coverage_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        joint_store[[nm]] <- 0
        length_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        mse_store[[nm]] <- setNames(rep(0,length(categories)), categories)
      }
    }
    if (weight_type %in% c("poststrat_weight_r","poststrat_weight_r_trim")){
      for (case in c("full", "srs", "srs_ess")) {
        nm <- paste(response,weight_type, case, sep="_")
        coverage_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        joint_store[[nm]] <- 0
        length_store[[nm]] <- setNames(rep(0,length(categories)), categories)
        mse_store[[nm]] <- setNames(rep(0,length(categories)), categories)
      }
    }
  }
}

#Začátek měření času
start_time <- Sys.time()
set.seed(2025)
# Simalcni loop
for (b in 1:B) {
  if (b %% 10 == 0) cat("Replication", b, "of", B, "\n")
  
  # Sample podle strat
  sampled_indices = c()
  for (s in strata) {
    stratum_idx = which(populace_rts$ID_strata == s)
    probs = populace_rts$prob[stratum_idx]
    n_s = stratum_counts[s]
    sampled = sample(stratum_idx, n_s, replace=FALSE, prob=probs)
    sampled_indices = c(sampled_indices, sampled)
  }
  # Designové váhy (na úrovni jednotek)
  sampled = populace_rts[sampled_indices, ]
  sampled$design_weight = sampled$Q4_sim / mean(sampled$Q4_sim)
  
  for (response in c("model","random")) {
    for (weight_type in c("srs")) {
      for (case in c("srs")) {
        nm = paste(response, weight_type, case, sep="_")
        sampled$w = 1
        all_covered = TRUE
        
        for (cat in categories) {
          # Selekce správné odezvy
          y = if (response == "random") {
            as.integer(sampled$OZ_2_rand == as.integer(gsub("cat","",cat)))
          } else {
            sampled[[cat]]
          }
          
          # Thompson Horwitz estimator
          odhad = sum(y * sampled$w) / n
          se = sqrt(var(y) * N*(N-n)/n) / N
          
          # Intervaly spolehlivosti a pokrytí
          u = qnorm(0.975)
          ci_low = odhad - u*se
          ci_high = odhad + u*se
          length_store[[nm]][cat] = length_store[[nm]][cat] + (ci_high-ci_low)
          
          if (targets[cat] >= ci_low & targets[cat] <= ci_high) {
            coverage_store[[nm]][cat] = coverage_store[[nm]][cat] + 1
          } else {
            all_covered = FALSE
          }
          
          mse_store[[nm]][cat] = mse_store[[nm]][cat] + (odhad - targets[cat])^2
        }
        
        if (all_covered) joint_store[[nm]] = joint_store[[nm]] + 1
      }
    }
  }
  
  # Raking
  design = svydesign(ids=~1, data=sampled, weights=~design_weight)
  design_raked = rake(design=design,
                       sample.margins=list(~ID_strata, ~vek_kat6, ~IDE_8, ~VZD),
                       population.margins=pop_margins,
                       control=list(epsilon=1e-8, maxit=1000))
  
  sampled$poststrat_weight = weights(design_raked)
  design2 = svydesign(id=~1, weights=~poststrat_weight, data=sampled)
  design2_trim = trimWeights(design2, upper=3, lower=1/3)
  sampled$poststrat_weight_trim = weights(design2_trim)
  
  # Raking bez jedné pozorované proměnné
  design = svydesign(ids=~1, data=sampled, weights=~design_weight)
  design_raked = rake(design=design,
                      sample.margins=list(~ID_strata, ~IDE_8, ~VZD),
                      population.margins=pop_margins2,
                      control=list(epsilon=1e-8, maxit=1000))
  
  sampled$poststrat_weight_r = weights(design_raked)
  design2 = svydesign(id=~1, weights=~poststrat_weight, data=sampled)
  design2_trim = trimWeights(design2, upper=3, lower=1/3)
  sampled$poststrat_weight_r_trim = weights(design2_trim)
  
  # Kalkulace všech scénářů s vahami - typ vah x pomocný model/odezva/typ odhadu
  for (response in c("model","random")) {
    for (weight_type in c("poststrat_weight","poststrat_weight_trim", "poststrat_weight_r","poststrat_weight_r_trim")) {
      for (case in c("full", "srs","srs_ess")) {
        nm = paste(response, weight_type, case, sep="_")
        sampled$w = sampled[[weight_type]]
        all_covered = TRUE
        
        for (cat in categories) {
          # Selekce správné odezvy
          y = if (response == "random") {
            as.integer(sampled$OZ_2_rand == as.integer(gsub("cat","",cat)))
          } else {
            sampled[[cat]]
          }
          
          # Thompson Horwitz estimator
          odhad = sum(y * sampled$w) / n
          
          # Standard errors
          if (case %in% c("full")) {
            if (weight_type %in% c("poststrat_weight","poststrat_weight_trim")) {
              m1 = lm(y ~ as.factor(ID_strata) + as.factor(vek_kat6) +
                      as.factor(IDE_8) + as.factor(VZD), data=sampled)
              se = sqrt(sum(m1$residuals^2 * (sampled$w*(N/n)-1)^2) +
                        (N-n)/n*sum(m1$residuals^2)) / N
            }
            if (weight_type %in% c("poststrat_weight_r","poststrat_weight_r_trim")) {
              m1 = lm(y ~ as.factor(ID_strata) + as.factor(IDE_8) + as.factor(VZD), data=sampled)
              se = sqrt(sum(m1$residuals^2 * (sampled$w*(N/n)-1)^2) +
                          (N-n)/n*sum(m1$residuals^2)) / N
            }
          } else if (case == "srs") {
            se = sqrt(var(y) * N*(N-n)/n) / N
          } else if (case == "srs_ess") {
            ess = sum(sampled$w)^2/sum(sampled$w^2)
            se = sqrt(var(y) * N*(N-ess)/ess) / N
          }
          
          # Intervaly spolehlivosti a pokrytí
          u = qnorm(0.975)
          ci_low = odhad - u*se
          ci_high = odhad + u*se
          length_store[[nm]][cat] = length_store[[nm]][cat] + (ci_high-ci_low)
          
          if (targets[cat] >= ci_low & targets[cat] <= ci_high) {
            coverage_store[[nm]][cat] = coverage_store[[nm]][cat] + 1
          } else {
            all_covered = FALSE
          }
  
          mse_store[[nm]][cat] = mse_store[[nm]][cat] + (odhad - targets[cat])^2
        }
        
        if (all_covered) joint_store[[nm]] = joint_store[[nm]] + 1
      }
    }
  }
}
  
#Konec simulačního loopu







# Převod absolutních součtů na proporce
for (nm in names(coverage_store)) {
  results[[nm]] = list(
    coverage = coverage_store[[nm]] / B,
    joint_coverage = joint_store[[nm]] / B,
    avg_interval_length = length_store[[nm]] / B,
    mse = mse_store[[nm]] / B
  )
}

end_time = Sys.time()
elapsed = end_time - start_time
cat("\nSimulation finished in", elapsed, "seconds\n\n")

# Output pokrytí, délky intervalů, MSE a MAE
for (name in names(results)) {
  cat("=============================================\n")
  cat(" Case:", name, "\n")
  cat("=============================================\n")
  res = results[[name]]
  
  df = data.frame(
    Category = categories,
    Coverage = round(res$coverage,3),
    AvgIntLength = round(res$avg_interval_length,4),
    MSE = round(res$mse,6)
  )
  print(df, row.names=FALSE)
  
  cat("\nJoint coverage (all categories simultaneously):",
      round(res$joint_coverage,3), "\n\n")
}