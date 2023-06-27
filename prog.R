# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}

library(aws.s3)
library(dplyr)
library(readr)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)


summary(habitudes_indiv)

ponderations <- description_indiv %>%
  select(NOIND,pond_indiv_adu_pop2,pond_indiv_enf_pop2)
infos_indiv <- description_indiv %>%
  select(NOIND,sex_PS,tage_PS,tage_PS_mois,region_adm_12cl,agglo_5cl)

habitudes <- habitudes_indiv %>%
  left_join(ponderations) %>%
  left_join(infos_indiv)

habitudes_adultes <- habitudes %>%
  filter(!is.na(pond_indiv_adu_pop2))

habitudes_enfants <- habitudes %>%
  filter(!is.na(pond_indiv_enf_pop2))

soda <- habitudes_enfants %>%
  group_by(aime_soda) %>%
  summarise(eff=sum(pond_indiv_enf_pop2))

part_soda <- soda %>%
  filter(!is.na(aime_soda)) %>%
  mutate(part=eff/sum(eff))


# Recodage de la variable de type d'agglomération

description_indiv <- description_indiv %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "Rural",
                                                                              agglo_5cl==2 ~ "2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "Paris"))

habitudes <- habitudes %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                                                          region_adm_12cl==2 ~ "NORMANDIE",
                                                                          region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                                                          region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                                                          region_adm_12cl==5 ~ "BRETAGNE",
                                                                          region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                                                          region_adm_12cl==7 ~ "GRAND EST",
                                                                          region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                                                          region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                                                          region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                                                          region_adm_12cl==11 ~ "OCCITANIE",
                                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",
                                                                          region_adm_12cl==13 ~ "CORSE"))


soda_reg <- habitudes %>%
  filter(is.na(pond_indiv_adu_pop2)) %>%
  group_by(region_recode,aime_soda) %>%
  summarise(eff=sum(pond_indiv_enf_pop2))

part_soda_reg <- soda_reg %>%
  filter(!is.na(aime_soda)) %>%
  group_by(region_recode) %>%
  mutate(part=eff/sum(eff))


# Boxplot pondérées

install.packages("survey")
library(survey)

dw <- svydesign(ids = ~1, data = habitudes, weights = ~habitudes$pond_indiv_pop2)
svyboxplot(conso_viande_age ~ 1, dw)
boxplot(habitudes$conso_viande_age)


test <- as.data.frame(colSums(is.na(habitudes)))
colnames(test) <- c("nb_na")

col_val_manq <- test %>%
  filter(nb_na < 200) %>%
  rownames()
habitudes2 <- habitudes %>%
  select(col_val_manq)

  
