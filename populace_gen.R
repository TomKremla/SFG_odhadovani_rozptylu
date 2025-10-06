#Skript na vytvoření umělé populace a simulace

#Knihovny
library(dplyr)
library(tidyr)
library(tibble)
library(openxlsx)
library(purrr)

#Data
setwd("C:/Users/tomik/OneDrive/Plocha/SFG_2025léto/Výpočet vah")
load("data_orig_weighted.RData")
age = read.xlsx("./Data/Population_sociodemo_2021.xlsx", 2)
sex = read.xlsx("./Data/Population_sociodemo_2021.xlsx", 1)
edu = read.xlsx("./Data/Population_sociodemo_2021.xlsx", 3)
test = read.csv("sldb2021_vzdelani_vek2_pohlavi.csv")
vazba = read.csv("vazba_obec_kraj.csv")

#Bereme pouze obce a odstraňujeme řádky se smíchaným pohlavím
populace = test[test$uzemi_typ == "obec",]
populace = na.omit(populace)
length(unique(populace$uzemi_kod))

#Filtrování relavantních sloupců pro populaci
populace_filt = populace[c("hodnota", "vzdelani_txt", "vek_txt", "pohlavi_txt", "uzemi_kod", "uzemi_txt")]

#Agregace vzdělání
unique(populace$vzdelani_txt)
populace_filt = populace_filt %>%
  mutate(
    vzdelani_grouped = case_when(
      vzdelani_txt %in% c("Bez vzdělání","ákladní vzdělání", "Neúplné základní vzdělání", "Nižší střední a střední vzdělání") ~ "ZŠ",
      vzdelani_txt %in% c("Úplné střední všeobecné vzdělání", "Úplné střední odborné vzdělání", "Pomaturitní studium", "Nástavbové vzdělání") ~ "SŠ",
      vzdelani_txt %in% c("Vyšší odborné vzdělání", "Vysokoškolské bakalářské vzdělání", "Vysokoškolské magisterské vzdělání", "Vysokoškolské doktorské vzdělání") ~ "VŠ",
      TRUE ~ "Other"
    )
  )
populace_agr = populace_filt %>%
  group_by(uzemi_kod, uzemi_txt, pohlavi_txt, vek_txt, vzdelani_grouped) %>%
  summarise(hodnota = sum(hodnota, na.rm = TRUE), .groups = "drop")

#Přerozdělení NA vzdělání do ostaních kategorií
probs = c(ZŠ = 0.4651998, SŠ = 0.3469696, VŠ = 0.1878307)
probs = probs / sum(probs)

#Rozdělení na pozorované a chybějící hodnoty u vzdělání
other_df = populace_agr %>%
  filter(vzdelani_grouped == "Other") %>%
  select(uzemi_kod, uzemi_txt, pohlavi_txt, vek_txt, other = hodnota)

non_other = populace_agr %>%
  filter(vzdelani_grouped != "Other") %>%
  select(uzemi_kod, uzemi_txt, pohlavi_txt, vek_txt, vzdelani_grouped, hodnota)

#Redistribuce
set.seed(123)
redistributed = other_df %>%
  rowwise() %>%
  mutate(
    sampled_counts = list(as.numeric(rmultinom(1, size = other, prob = probs)))
  ) %>%
  unnest_wider(sampled_counts, names_sep = "_") %>%
  pivot_longer(
    starts_with("sampled_counts_"),
    names_to = "vzdelani_grouped",
    values_to = "hodnota"
  ) %>%
  mutate(
    vzdelani_grouped = names(probs)[as.integer(gsub("sampled_counts_", "", vzdelani_grouped))]
  ) %>%
  ungroup() %>%
  select(-other)

# 4) Combine and sum (Other disappears)
populace_split = bind_rows(non_other, redistributed) %>%
  group_by(uzemi_kod, uzemi_txt, pohlavi_txt, vek_txt, vzdelani_grouped) %>%
  summarise(hodnota = sum(hodnota, na.rm = TRUE), .groups = "drop")

#Sanity check
sum(populace_split[populace_split$uzemi_txt == "Želechovice nad Dřevnicí",]$hodnota)
sum(populace_split[populace_split$vzdelani_grouped == "ZŠ",]$hodnota)/sum(populace_split$hodnota)

#Redistribuce do věkových kategorií
vek5 = read.csv("sldb2021_vek5_pohlavi.csv")
vek5 = na.omit(vek5)
vek5 = vek5[c("hodnota", "uzemi_kod", "uzemi_txt", "pohlavi_txt", "vek_txt")]

vek5 = vek5 %>%
  filter(!vek_txt %in% c("0 - 4 roky", "5 - 9 let", "10 - 14 let")) #pod 15 let nás vůbec nezajímá
vek5 = vek5 %>%
  filter(!vek_txt %in% c("65 - 69 let", "70 - 74 let", "75 - 79 let", "80 - 84 let", "85 - 89 let", 
                         "90 - 94 let", "95 - 99 let", "100 a více let")) #pod 15 let nás vůbec nezajímá

vek5 =  vek5 %>%
  mutate(vek_group = recode(vek_txt,
                            "15 - 19 let" = "15 - 19 let",
                            "20 - 24 let" = "20 - 29 let",
                            "25 - 29 let" = "20 - 29 let",
                            "30 - 34 let" = "30 - 39 let",
                            "35 - 39 let" = "30 - 39 let",
                            "40 - 44 let" = "40 - 54 let",
                            "45 - 49 let" = "40 - 54 let",
                            "50 - 54 let" = "40 - 54 let",
                            "55 - 59 let" = "55 - 64 let",
                            "60 - 64 let" = "55 - 64 let"))

vek5 = vek5 %>%
  group_by(uzemi_kod, pohlavi_txt, vek_group) %>%
  summarise(hodnota = sum(hodnota, na.rm = TRUE), .groups = "drop")

set.seed(123)  # reproducibility

# Step 1: probabilities in vek5
vek5_probs <- vek5 %>%
  group_by(uzemi_kod, pohlavi_txt) %>%
  mutate(prob = hodnota / sum(hodnota)) %>%
  ungroup()

# Step 2: redistribute rows with vek_txt == "15 - 64 let"
redistributed <- populace_split %>%
  filter(vek_txt == "15 - 64 let") %>%
  left_join(
    vek5_probs %>% select(uzemi_kod, pohlavi_txt, vek_group, prob),
    by = c("uzemi_kod", "pohlavi_txt"),
    relationship = "many-to-many"   # silence warning, keep all matches
  ) %>%
  group_by(uzemi_kod, pohlavi_txt, vzdelani_grouped) %>%
  nest() %>%
  mutate(
    data = map(data, ~ {
      size <- unique(.x$hodnota)      # total to redistribute
      probs <- .x$prob
      vek_groups <- .x$vek_group
      alloc <- as.numeric(rmultinom(1, size = size, prob = probs))
      tibble(
        uzemi_kod = unique(.x$uzemi_kod),
        pohlavi_txt = unique(.x$pohlavi_txt),
        vzdelani_grouped = unique(.x$vzdelani_grouped),
        vek_group = vek_groups,
        hodnota = alloc
      )
    })
  ) %>%
  unnest(cols = c(data)) %>%   # <— now unnest correctly
  ungroup()

# Step 3: keep other rows unchanged
populace_split_final <- populace_split %>%
  filter(vek_txt != "15 - 64 let") %>%
  bind_rows(redistributed)

populace_split_final[is.na(populace_split_final$vek_group),]$vek_group = populace_split_final[is.na(populace_split_final$vek_group),]$vek_txt
populace_split_final = populace_split_final[c("hodnota", "uzemi_kod", "pohlavi_txt", "vek_group", "vzdelani_grouped")]

#Namapování kraje na obec
colnames(vazba)[colnames(vazba) == "chodnota1"] = "uzemi_kod"
colnames(vazba)[colnames(vazba) == "text2"] = "kraj"
populace_split_final = merge(populace_split_final, vazba[, c("uzemi_kod", "kraj")],
                       by = "uzemi_kod", all.x = TRUE)

#Numerické ID kraje
kraje = c("Hlavní město Praha", "Středočeský kraj", "Jihočeský kraj", "Plzeňský kraj", 
          "Karlovarský kraj", "Ústecký kraj", "Liberecký kraj", "Královéhradecký kraj",
          "Pardubický kraj", "Kraj Vysočina", "Jihomoravský kraj", "Olomoucký kraj",
          "Zlínský kraj", "Moravskoslezský kraj")
unique(populace_split_final$kraj)
populace_split_final = populace_split_final %>%
  mutate(NUTS3 = match(kraj, kraje))

#Namapování NUTS3 na NUTS2
populace_split_final = populace_split_final %>%
  mutate(NUTS2 = dplyr::recode(NUTS3 %>% unclass,
                               "1"	= "1",
                               "2"	= "2",
                               "3"	= "3",
                               "4"	= "3",
                               "5"	= "4",
                               "6"	= "4",
                               "7"	= "5",
                               "8"	= "5",
                               "9"	= "5",
                               "10"	= "6",
                               "11"	= "6",
                               "12"	= "7",
                               "13"	= "7",
                               "14"	= "8",
  )) 

#Skupiny podle počtu obyvatel obce
populace_split_final = populace_split_final %>%
  group_by(uzemi_kod) %>%
  mutate(obyvatelstvo = sum(hodnota, na.rm = TRUE)) %>%
  ungroup()

populace_split_final = populace_split_final %>%
  mutate(VSO_R = cut(obyvatelstvo,
                     breaks = c(0, 1999, 4999, 19999, 49999, Inf),
                     labels = c("1", "2", "3", "4", "5"),
                     right = TRUE))

summary(populace_split_final)
sum(populace_split_final$hodnota)
sum(populace$hodnota)

#Velikost domácnosti

#Nějaký pokusy
byty = read.csv("sldb2021_byty_osoby.csv")
unique(byty$ukaz_txt)
byty = byty[byty$ukaz_txt == "Počet obydlených bytů",]

nrow(byty[!byty$osob_txt == "",])
byty2 = byty[!byty$osob_txt == "",]
byty2 = byty2[byty2$uzemi_kod == 19,]
sum(byty2$hodnota*byty2$pocet)

byty2 = byty2 %>%
  mutate(pocet = dplyr::recode(osob_txt %>% unclass,
                               "1"	= 1,
                               "2"	= 2,
                               "3"	= 3,
                               "4"	= 4,
                               "5"	= 5,
                               "6 a více"	= 6,
  )) 

prop.table(table(data_orig_weighted$Q4))

byty2$hodnota*byty2$pocet/sum(byty2$hodnota*byty2$pocet)

populace_split_expanded = populace_split_final %>%
  uncount(weights = hodnota)

set.seed(123)
probs = prop.table(table(data_orig_weighted$Q4))
populace_split_expanded = populace_split_expanded %>%
  mutate(Q4_sim = sample(
    x = as.integer(names(probs)),
    size = n(),
    replace = TRUE,
    prob = as.numeric(probs)
  ))

populace_split_expanded$prob = 1/populace_split_expanded$Q4_sim
populace_split_expanded$prob = populace_split_expanded$prob/sum(populace_split_expanded$prob)
table(populace_split_expanded$prob)

populace_rts = populace_split_expanded
populace_rts = populace_rst
unique(populace_rts$vek_group)
populace_rts = populace_rts %>%
  mutate(vek_kat6 = dplyr::recode(vek_group %>% unclass,
                               "15 - 19 let"	= 1,
                               "20 - 29 let"	= 2,
                               "30 - 39 let"	= 3,
                               "40 - 54 let"	= 4,
                               "55 - 64 let"	= 5,
                               "65 a více let"	= 6,
  ))
populace_rts = populace_rts %>%
  mutate(IDE_8 = dplyr::recode(pohlavi_txt %>% unclass,
                                  "muž"	= 1,
                                  "žena"	= 2,
  )) 
populace_rts = populace_rts %>%
  mutate(VZD = dplyr::recode(vzdelani_grouped %>% unclass,
                               "ZŠ"	= 1,
                               "SŠ"	= 2,
                               "VŠ" = 3,
  )) 

#Přidání ID strata
populace_rts$ID_strata = paste0(populace_rts$NUTS2, populace_rts$VSO_R)

#Multinomický model pro OZ_2 a přidání odezvy do populace
mult_model = multinom(OZ_2 ~ as.factor(IDE_8) + as.factor(vek_kat6) + as.factor(VZD) + as.factor(ID_strata), data = data_orig_weighted)
Anova(mult_model, type = "II", test = "LR")

probs = predict(mult_model, newdata = populace_rts, type = "probs")

#Přidání odezvy
set.seed(123)
sample_outcome = function(prob_row, categories) {
  sample(categories, size = 1, prob = prob_row)
}
categories = colnames(probs)
pred_random = apply(probs, 1, sample_outcome, categories = categories)
populace_rts$OZ_2 = pred_random

#Check distribuce
prop.table(table(populace_rts$OZ_2))
prop.table(table(data_orig_weighted$OZ_2))

# Vytvoreni odezvy nezavisle na pomocnych promennych se stejnou distribucí jako OZ_2
set.seed(123)
populace_rts$OZ_2_rand = sample(populace_rts$OZ_2)

save(populace_rts, file = "populace_rts.RData")
