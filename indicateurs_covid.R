library(tidyverse)
library(slider)
library(lubridate)

# INCIDENCE, POSITIVITE

# Incidence : https://www.data.gouv.fr/fr/datasets/taux-dincidence-de-lepidemie-de-covid-19/
# Positivité : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/

## Incidence natio OK

read_delim("https://www.data.gouv.fr/fr/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c", ";")%>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000)%>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_incid_glissant) %>%
  write_csv("data/incidence_france.csv")

## Incidence départements OK

read_delim("https://www.data.gouv.fr/fr/datasets/r/19a91d64-3cd3-42fc-9943-d635491a4d76", ";")%>%
  arrange(jour)%>%
  filter(cl_age90 == "0")%>%
  group_by(dep)%>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000)%>%
  filter(jour>= "2020-09-01")%>%
  select(dep, jour, taux_incid_glissant)%>%
  write_csv("incidence_dep.csv")

##  Positivité natio OK

read_delim("https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c",";") %>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_posiv_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/slider::slide_dbl(T,sum,.before=6,.after=0,.complete=TRUE)*100) %>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_posiv_glissant) %>%
  write_csv("positivite_france.csv")

## Positivité départements OK

read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",";")%>%
  arrange(jour)%>%
  filter(cl_age90 == "0")%>%
  group_by(dep)%>%
  mutate(taux_posiv_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/slider::slide_dbl(T,sum,.before=6,.after=0,.complete=TRUE)*100) %>%
  select(dep, jour, taux_posiv_glissant)%>%
  write_csv("positivite_dep.csv")



# CAS + HOSPIT + REAS + OCCUP REAS + DECES 
# Donnees : https://www.data.gouv.fr/fr/datasets/synthese-des-indicateurs-de-suivi-de-lepidemie-covid-19/

## Cas positifs natio OK

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double())) %>%
  select(date, conf_j1)%>%
  arrange(date)%>%
  mutate(cas_glissant = round(slider::slide_dbl(conf_j1, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("cas_natio.csv")

## Cas positifs departements

read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", ";",
            escape_double = FALSE, col_types = cols(jour = col_date(format = "%Y-%m-%d"),cl_age90 = col_number()), 
            trim_ws = TRUE)%>%
  filter(cl_age90 == 0)%>%
  arrange(jour)%>%
  group_by(dep)%>%
  mutate(cas_glissant = round(slider::slide_dbl(P, sum, .before=6, .complete = TRUE)/7))%>%
  select(jour, dep, P, cas_glissant)%>%
  write_csv("cas_dep.csv")


## Hospitalisations natio (nouveaux patients hospitalisés au cours des dernières 24h) OK

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  select(date, incid_hosp)%>%
  arrange(date)%>%
  mutate(hospit_glissant = round(slider::slide_dbl(incid_hosp, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("hospit_natio.csv")

## Réanimations natio  (nouveaux patients admis en réa au cours des dernières 24h) OK

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  select(date, incid_rea)%>%
  arrange(date)%>%
  mutate(rea_glissant = round(slider::slide_dbl(incid_rea, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("rea_natio.csv")

## Taux occupation en réanimation natio (en %) OK

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  arrange(date)%>%
  select(date, TO)%>%
  write_csv("rea_occup_natio.csv")

## Taux occupation en réanimation dep (en %)

read_csv("https://www.data.gouv.fr/fr/datasets/r/5c4e1452-3850-4b59-b11c-3dd51d7fb8b5", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  arrange(date)%>%
  select(date, dep, lib_dep, reg, lib_reg, TO)%>%
  write_csv("rea_occup_dep.csv")


## Deces à l'hopital natio (au cours des dernieres 24h) OK

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  select(date, incid_dchosp)%>%
  arrange(date)%>%
  mutate(deces_glissant = round(slider::slide_dbl(incid_dchosp, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("deces_hosp_natio.csv")


  
# VACCINS
# Donnees : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#_
  
## Vaccinations natio (au moins 1 dose) par type vaccin OK

# nom_vaccins <- tibble("vaccin" = c(0, 1, 2, 3, 4), "nom_vaccin" = c("Tous vaccins","Pfizer/BioNTech","Moderna","AstraZeneca","Janssen"))
# 
# pop_age <- read_delim("C:/Users/user/Documents/0_PRO/Data et scripts/demographie/demo-pop-sexe-age-effectif.csv", ",")

read_delim("https://www.data.gouv.fr/fr/datasets/r/b273cf3b-e9de-437c-af55-eda5979e92fc", ";", col_types = cols(jour = col_date(format = "%Y-%m-%d")))%>%
  arrange(jour)%>%
  left_join(tibble("vaccin" = c(0, 1, 2, 3, 4), "nom_vaccin" = c("Tous vaccins","Pfizer/BioNTech","Moderna","AstraZeneca","Janssen")))%>%
  group_by(vaccin)%>%
  mutate(dose1_glissant = round(slider::slide_dbl(n_dose1, sum, .before=6, .complete=TRUE)/7),
         dose2_glissant = round(slider::slide_dbl(n_dose2, sum, .before=6, .complete=TRUE)/7))%>%
  ungroup()%>%
  group_by(jour)%>%
  mutate(doses_1_2_glissant = (dose1_glissant + dose2_glissant),
         doses_1_2 = (n_dose1 + n_dose2))%>%
  select(-fra)%>%
  write_csv("vaccins_france.csv")


#Test nouveau fichier avec nouveaux indicateurs et novueaux noms variables : 
# n_dose2 => n_complet
# n_cum_dose2 => n_cum_complet
# couv_dose2 => couv_complet

vaccins_france__2 <- read_delim("https://www.data.gouv.fr/fr/datasets/r/b273cf3b-e9de-437c-af55-eda5979e92fc", ";", col_types = cols(jour = col_date(format = "%Y-%m-%d")))%>%
  arrange(jour)%>%
  left_join(tibble("vaccin" = c(0, 1, 2, 3, 4), "nom_vaccin" = c("Tous vaccins","Pfizer/BioNTech","Moderna","AstraZeneca","Janssen")))%>%
  group_by(vaccin)%>%
  mutate(dose1_glissant = round(slider::slide_dbl(n_dose1, sum, .before=6, .complete=TRUE)/7),
         dosecomplet_glissant = round(slider::slide_dbl(n_complet, sum, .before=6, .complete=TRUE)/7))%>%
  ungroup()%>%
  group_by(jour)%>%
  mutate(doses_1_complet_glissant = (dose1_glissant + dosecomplet_glissant),
         doses_1_complet = (n_dose1 + n_complet))%>%
  select(-fra)
  # write_csv("vaccins_france.csv")


## Vaccinations natio (au moins 1 dose) par age

# classes_age <- tibble("clage_vacsi" = c(0, 9, 17, 24, 29, 39, 49, 59, 69, 74, 79, 80), "tranche_age" = c("Tous âges", "0-9", "10-17", "18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70-74", "75-79", "80 et +"))

read_delim("https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd",";")%>%
  arrange(jour)%>%
  left_join(tibble("clage_vacsi" = c(0, 9, 17, 24, 29, 39, 49, 59, 64, 69, 74, 79, 80), "tranche_age" = c("Tous âges", "0-9", "10-17", "18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70-74", "75-79", "80 et +")))%>%
  group_by(tranche_age)%>%
  mutate(vaccins_glissant = round(slider::slide_dbl(n_dose1, sum, .before=6, .complete = TRUE)/7))%>%
  select(-fra)%>%
  write_csv("vaccins_age_france.csv")

