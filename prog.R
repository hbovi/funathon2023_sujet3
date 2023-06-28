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



ponderations <- description_indiv %>%
  select(NOIND,pond_indiv_adu_pop2,pond_indiv_enf_pop2)
infos_indiv <- description_indiv %>%
  select(NOIND,sex_PS,tage_PS,tage_PS_mois,region_adm_12cl,agglo_5cl,imc)

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

habitudes_adultes <- habitudes_adultes %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "Rural",
                                                                              agglo_5cl==2 ~ "2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "Paris"))
habitudes_enfants <- habitudes_enfants %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "Rural",
                                                                              agglo_5cl==2 ~ "2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "Paris"))


habitudes_adultes <- habitudes_adultes %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
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
habitudes_enfants <- habitudes_enfants %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
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


soda_reg <- habitudes_enfants %>%
  group_by(region_recode,aime_soda) %>%
  summarise(eff=sum(pond_indiv_enf_pop2))

part_soda_reg <- soda_reg %>%
  filter(!is.na(aime_soda)) %>%
  group_by(region_recode) %>%
  mutate(part=eff/sum(eff))


# Boxplot pondérées

install.packages("survey")
library(survey)


test <- as.data.frame(colSums(is.na(habitudes_enfants)))
colnames(test) <- c("nb_na")

col_val_manq <- test %>%
  filter(nb_na < 200) %>%
  rownames()
habitudes_enfants_2 <- habitudes_enfants %>%
  select(col_val_manq)

test <- as.data.frame(colSums(is.na(habitudes_adultes)))
colnames(test) <- c("nb_na")

col_val_manq <- test %>%
  filter(nb_na < 200) %>%
  rownames()
habitudes_adultes_2 <- habitudes_adultes %>%
  select(col_val_manq) 

dw <- svydesign(ids = ~1, data = habitudes_enfants_2, weights = ~habitudes_enfants_2$pond_indiv_enf_pop2)
svyboxplot(conso_viande_age ~ 1, dw)
dw_ad <- svydesign(ids = ~1, data = habitudes_adultes_2, weights = ~habitudes_adultes_2$pond_indiv_adu_pop2)

svytable(~lieu_repas_midi,dw_ad)

test <- as.data.frame(rowSums(is.na(habitudes_adultes_2)))
colnames(test) <- c("nb_na")
test2 <- test %>%
  filter(nb_na>20)

svychisq(~table_ketchup+table_mayonnaise,dw_ad)
svytable(~table_ketchup+table_mayonnaise,dw_ad)

svychisq(~restaurationrapide_freq+table_ketchup,dw_ad)
test <- as.data.frame(svytable(~restaurationrapide_freq+table_ketchup,dw_ad))
test2 <- test %>%
  group_by(restaurationrapide_freq) %>%
  summarise(tot=sum(Freq))
test <- test %>%
  left_join(test2) %>%
  filter(table_ketchup == 1) %>%
  mutate(part=Freq/tot)

svychisq(~table_beurre+table_creme_fraiche,dw_ad)
svytable(~table_beurre+table_creme_fraiche,dw_ad)

svytable(~lieu_repas_midi+cantine_freq,dw_ad)

svychisq(~collation_freq+cantine_freq,dw_ad)
svytable(~collation_freq+cantine_freq,dw_ad)

svychisq(~collation_freq+distributeur_freq,dw_ad)
svytable(~collation_freq+distributeur_freq,dw_ad)

svychisq(~table_beurre+table_huile_olive,dw_ad)
svytable(~table_beurre+table_huile_olive,dw_ad)

svychisq(~table_huile_olive+table_vinaigrette,dw_ad)
svytable(~table_huile_olive+table_vinaigrette,dw_ad)

svychisq(~table_ketchup+table_vinaigrette,dw_ad)
svytable(~table_ketchup+table_vinaigrette,dw_ad)

install.packages("FactoMineR")
library(FactoMineR)

table_acm <- habitudes_adultes_2[,c(4:16)]
table_acm <- as.data.frame(lapply(table_acm,as.factor))
table_acm <- as.data.frame(lapply(table_acm,addNA))
acm_conso_gras <- MCA(table_acm,quali.sup=c(2,4,7,8,10))

install.packages("missMDA")
library(missMDA)
table_acm_ss_na <- habitudes_adultes_2[,c(4:16)]
table_acm_ss_na <- as.data.frame(lapply(table_acm_ss_na,as.factor))[,-c(2,4,7,8,10)]
res_acm_ss_na <- imputeMCA(table_acm_ss_na,ncp = 4)
acm_conso_gras_2 <- MCA(table_acm_ss_na,tab.disj=res_acm_ss_na$tab.disj)

classif <- HCPC(acm_conso_gras_2,nb.clust=7)

classe1 <- classif$data.clust %>%
  filter(clust==1)
summary(classe1)
classe2 <- classif$data.clust %>%
  filter(clust==2)
summary(classe2)
classe3 <- classif$data.clust %>%
  filter(clust==3)
summary(classe3)
classe4 <- classif$data.clust %>%
  filter(clust==4)
summary(classe4)
classe5 <- classif$data.clust %>%
  filter(clust==5)
summary(classe5)
classe6 <- classif$data.clust %>%
  filter(clust==6)
summary(classe6)
classe7 <- classif$data.clust %>%
  filter(clust==7)
summary(classe7)

install.packages("tidyverse")
library(tidyverse)

resum1 <- as.data.frame(classif[["desc.var"]][["category"]][["1"]][,2])
colnames(resum1) <- c("Clust1")
rownames(resum1) <- ifelse(as.list(as.data.frame(str_split(rownames(resum1),"="))[2,])=="NA",rownames(resum1),as.list(as.data.frame(str_split(rownames(resum1),"="))[2,]))
resum1$modalite <- rownames(resum1)
resum2 <- as.data.frame(classif[["desc.var"]][["category"]][["2"]][,2])
colnames(resum2) <- c("Clust2")
rownames(resum2) <- ifelse(as.list(as.data.frame(str_split(rownames(resum2),"="))[2,])=="NA",rownames(resum2),as.list(as.data.frame(str_split(rownames(resum2),"="))[2,]))
resum2$modalite <- rownames(resum2)
resum3 <- as.data.frame(classif[["desc.var"]][["category"]][["3"]][,2])
colnames(resum3) <- c("Clust3")
rownames(resum3) <- ifelse(as.list(as.data.frame(str_split(rownames(resum3),"="))[2,])=="NA",rownames(resum3),as.list(as.data.frame(str_split(rownames(resum3),"="))[2,]))
resum3$modalite <- rownames(resum3)
resum4 <- as.data.frame(classif[["desc.var"]][["category"]][["4"]][,2])
colnames(resum4) <- c("Clust4")
rownames(resum4) <- ifelse(as.list(as.data.frame(str_split(rownames(resum4),"="))[2,])=="NA",rownames(resum4),as.list(as.data.frame(str_split(rownames(resum4),"="))[2,]))
resum4$modalite <- rownames(resum4)
resum5 <- as.data.frame(classif[["desc.var"]][["category"]][["5"]][,2])
colnames(resum5) <- c("Clust5")
rownames(resum5) <- ifelse(as.list(as.data.frame(str_split(rownames(resum5),"="))[2,])=="NA",rownames(resum5),as.list(as.data.frame(str_split(rownames(resum5),"="))[2,]))
resum5$modalite <- rownames(resum5)
resum6 <- as.data.frame(classif[["desc.var"]][["category"]][["6"]][,2])
colnames(resum6) <- c("Clust6")
rownames(resum6) <- ifelse(as.list(as.data.frame(str_split(rownames(resum6),"="))[2,])=="NA",rownames(resum6),as.list(as.data.frame(str_split(rownames(resum6),"="))[2,]))
resum6$modalite <- rownames(resum6)
resum7 <- as.data.frame(classif[["desc.var"]][["category"]][["7"]][,2])
colnames(resum7) <- c("Clust7")
rownames(resum7) <- ifelse(as.list(as.data.frame(str_split(rownames(resum7),"="))[2,])=="NA",rownames(resum7),as.list(as.data.frame(str_split(rownames(resum7),"="))[2,]))
resum7$modalite <- rownames(resum7)

resum <- resum1 %>%
  select("modalite","Clust1") %>%
  full_join(resum2) %>%
  full_join(resum3) %>%
  full_join(resum4) %>%
  full_join(resum5) %>%
  full_join(resum6) %>%
  full_join(resum7) %>%
  arrange(modalite)


habitudes_adultes_3 <- habitudes_adultes_2 %>%
  cbind(classif$data.clust$clust)

habitudes_adultes_3 %>%
  group_by(`classif$data.clust$clust`) %>%
  summarise(freq_obese=mean(imc>30,na.rm=TRUE))
