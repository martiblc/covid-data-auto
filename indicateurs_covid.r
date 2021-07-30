library(tidyverse)
library(slider)
library(lubridate)
library(beepr)



# INCIDENCE, POSITIVITE

# Incidence : https://www.data.gouv.fr/fr/datasets/taux-dincidence-de-lepidemie-de-covid-19/
# Positivité : https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/


## INCIDENCE NATIO

read_delim("https://www.data.gouv.fr/fr/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c", ";")%>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000)%>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_incid_glissant) %>%
  write_csv("incidence_france.csv")

## INCIDENCE DEPARTEMENTS

read_delim("https://www.data.gouv.fr/fr/datasets/r/19a91d64-3cd3-42fc-9943-d635491a4d76", ";")%>%
  arrange(jour)%>%
  filter(cl_age90 == "0")%>%
  group_by(dep)%>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000)%>%
  filter(jour>= "2020-09-01")%>%
  select(dep, jour, taux_incid_glissant)%>%
  write_csv("incidence_dep.csv")

### INCIDENCE RHÔNE

incidence_rhone <- read_csv("incidence_dep.csv") %>%
  filter(dep %in% 69)%>%
  mutate(dep=recode(dep,`69`="Rhône"))%>%
  write_csv("incidence_rhone.csv")


## INCIDENCE DEPARTEMENTS AGE

left_join(tibble("clage_vacsi" = c(0, 9, 17, 24, 29, 39, 49, 59, 64, 69, 74, 79, 80), "tranche_age" = c("Tous âges", "0-9", "10-17", "18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70-74", "75-79", "80 et +")))

incidence_dep_age <- read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", ";")%>%
  arrange(jour)%>%
  left_join(tibble("cl_age90" = c("0", "09", "19", "29", "39", "49", "59", "69", "79", "89", "90"), "tranche_age" = c("Tous âges", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "+90")))%>%
  group_by(dep, cl_age90)%>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000)%>%
  ungroup()%>%
  filter(jour>= "2020-09-01")%>%
  select(dep, jour, taux_incid_glissant, tranche_age)%>%
  write_csv("incidence_dep_age.csv")

### INCIDENCE RHONE AGE

incidence_dep_age %>%
  filter(dep == 69)%>%
  write_csv("incidence_rhone_age.csv")

incidence_dep_age %>%
  filter(dep == 69, tranche_age == "20-29")%>%
  write_csv("incidence_rhone_20-29.csv")


##  POSITIVITÉ NATIO

read_delim("https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c",";") %>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_posiv_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/slider::slide_dbl(T,sum,.before=6,.after=0,.complete=TRUE)*100) %>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_posiv_glissant) %>%
  write_csv("positivite_france.csv")


## POSITIVITÉ DÉPARTEMENTS

read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",";")%>%
  arrange(jour)%>%
  filter(cl_age90 == "0")%>%
  group_by(dep)%>%
  mutate(taux_posiv_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/slider::slide_dbl(T,sum,.before=6,.after=0,.complete=TRUE)*100) %>%
  select(dep, jour, taux_posiv_glissant)%>%
  write_csv("positivite_dep.csv")




# CAS + HOSPIT + REAS + OCCUP REAS + DECES 

# Donnees : https://www.data.gouv.fr/fr/datasets/synthese-des-indicateurs-de-suivi-de-lepidemie-covid-19/


## CAS POSITIFS NATIO

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double())) %>%
  select(date, conf_j1)%>%
  arrange(date)%>%
  mutate(cas_glissant = round(slider::slide_dbl(conf_j1, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("cas_natio.csv")


## CAS POSITIFS DEPARTEMENTS

read_delim("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", ";",
           escape_double = FALSE, col_types = cols(jour = col_date(format = "%Y-%m-%d"),cl_age90 = col_number()), 
           trim_ws = TRUE)%>%
  filter(cl_age90 == 0)%>%
  arrange(jour)%>%
  group_by(dep)%>%
  mutate(cas_glissant = round(slider::slide_dbl(P, sum, .before=6, .complete = TRUE)/7))%>%
  select(jour, dep, P, cas_glissant)%>%
  write_csv("cas_dep.csv")


## HOSPITALISATIONS NATIO (nouveaux patients hospitalisés au cours des dernières 24h)

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  select(date, incid_hosp)%>%
  arrange(date)%>%
  mutate(hospit_glissant = round(slider::slide_dbl(incid_hosp, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("hospit_natio.csv")


## RÉAS NATIO  (nouveaux patients admis en réa au cours des dernières 24h)

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  select(date, incid_rea)%>%
  arrange(date)%>%
  mutate(rea_glissant = round(slider::slide_dbl(incid_rea, sum, .before=6, .complete = TRUE)/7))%>%
  write_csv("rea_natio.csv")


## TAUX OCCUPATION REA NATIO (en %)

read_csv("https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  arrange(date)%>%
  select(date, TO)%>%
  write_csv("rea_occup_natio.csv")

## TAUX OCCUPATION REA DEP (en %)

read_csv("https://www.data.gouv.fr/fr/datasets/r/5c4e1452-3850-4b59-b11c-3dd51d7fb8b5", 
         col_types = cols(date = col_date(format = "%Y-%m-%d"),
                          R = col_number(), pos = col_double(),
                          pos_7j = col_double()))%>%
  arrange(date)%>%
  select(date, dep, lib_dep, reg, lib_reg, TO)%>%
  write_csv("rea_occup_dep.csv")


## DÉCÈS HOPITAL NATIO (dernieres 24h)

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


## VACCINATIONS NATIO PAR TYPE VACCIN

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


## VACCINATION NATIO ÂGE

# classes_age <- tibble("clage_vacsi" = c(0, 9, 17, 24, 29, 39, 49, 59, 69, 74, 79, 80), "tranche_age" = c("Tous âges", "0-9", "10-17", "18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70-74", "75-79", "80 et +"))

read_delim("https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd",";")%>%
  arrange(jour)%>%
  left_join(tibble("clage_vacsi" = c(0, 9, 17, 24, 29, 39, 49, 59, 64, 69, 74, 79, 80), "tranche_age" = c("Tous âges", "0-9", "10-17", "18-24", "25-29", "30-39", "40-49", "50-59", "60-64", "64-69", "70-74", "75-79", "80 et +")))%>%
  group_by(tranche_age)%>%
  mutate(vaccins_glissant = round(slider::slide_dbl(n_dose1, sum, .before=6, .complete = TRUE)/7))%>%
  select(-fra)%>%
  write_csv("vaccins_age_france.csv")

## VACCINATION DEP

vaccins_dep <- read_delim("https://www.data.gouv.fr/fr/datasets/r/4f39ec91-80d7-4602-befb-4b522804c0af", ";")%>%
  arrange(jour)%>%
  group_by(dep)%>%
  mutate(dose1_glissant = round(slider::slide_dbl(n_dose1, sum, .before=6, .complete=TRUE)/7),
       dosecomplet_glissant = round(slider::slide_dbl(n_complet, sum, .before=6, .complete=TRUE)/7))%>%
  write_csv("vaccins_dep.csv")

vaccins_dep_couv <- vaccins_dep %>%
  filter(jour == "2021-07-18")%>%
  select(couv_dose1, couv_complet, jour)

summary(vaccins_dep_couv)

vaccins_dep_couv_metro <- vaccins_dep_couv %>%
  filter(!dep %in% c(971, 972, 973, 974, 975, 976, 977, 978))

summary(vaccins_dep_couv_metro)



## VACCINATION RHONE

read_csv("vaccins_dep.csv")%>%
  filter(dep == 69)%>%
  arrange(jour)%>%
  select(jour,dose1_glissant, dosecomplet_glissant, n_dose1, n_complet)%>%
  write_csv("vaccins_rhone.csv")



beep()



#TODO : pourcentage pop vaccinée par tranche age et tt confondu







# VISUALISATIONS

library(hrbrthemes)
library(wesanderson)
library(Cairo)
library(sf)
library(viridis)
library(grid)
library(tmap)
library(cowplot)
library(ggThemeAssist)
library(ggrepel)

## POSITIFS NATIO

cas_natio <- read_csv("cas_natio.csv")
cas_natio_dernier <- filter(cas_natio, date == max(date))

ggplot()+
  geom_line(data=cas_natio, aes(date, cas_glissant), colour="red", size=1)+
  geom_point(data = cas_natio_dernier,
             aes(date, conf_j1))+
  geom_label_repel(data = cas_natio_dernier, aes(date, conf_j1, label=conf_j1),  min.segment.length = 0,  nudge_y = -0.5, nudge_x = -10, size=4, force=100, direction = "y", segment.curvature=0.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL, expand = c(0, 10)) +
  ylim(0, 50000)+
  labs(x ="", y="", title = "Nouveaux cas positifs", subtitle = "En rouge, la moyenne glissante sur 7 jours des nouveaux cas confirmés de Covid-19. 
Le point noir représente le dernier chiffre brut.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"),
        plot.title = element_text(vjust = 2),
        plot.caption = element_text(colour = "gray40"),
        panel.grid.minor = element_line(linetype = "blank"))


## POSITIFS DEPS ILE-DE-FRANCE

cas_idf <- read_csv("cas_dep.csv") %>%
  filter(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95))%>%
  mutate(dep=recode(dep,
                    `75`="Paris",
                    `77`="Seine-et-Marne",
                    `78`="Yvelines",
                    `91`="Essonne",
                    `92`="Hauts-de-Seine",
                    `93`="Seine-Saint-Denis",
                    `94`="Val-de-Marne",
                    `95`="Val-d'Oise"))

cas_idf_dernier <- filter(cas_idf, jour == max(jour))

ggplot()+
  geom_line(data=cas_idf, aes(jour, cas_glissant), colour="red", size=1)+
  geom_point(data = cas_idf_dernier,
             aes(jour, P), colour="red")+
  geom_text_repel(data = cas_idf_dernier, aes(jour, P, label=P),  nudge_y = 10, force = 20, size=4, colour="red")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  # ylim(0, 50000)+
  labs(x ="", y="", title = "Nouveaux cas positifs en Ile-de-France", subtitle = "En rouge, les moyennes glissantes sur 7 jours des nouveaux cas confirmés de Covid-19. 
Le point rouge représente le dernier chiffre brut.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  facet_wrap(~dep, ncol = 2, scales = "free")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))


## HSOPITS NATIO

hospit_natio <- read_csv("hospit_natio.csv")
hospit_natio_dernier <- filter(hospit_natio, date == max(date))

ggplot()+
  geom_line(data=hospit_natio, aes(date, hospit_glissant), colour="red", size=1)+
  geom_point(data = hospit_natio_dernier,
             aes(date, incid_hosp))+
  geom_label_repel(data = hospit_natio_dernier, aes(date, incid_hosp, label=incid_hosp),  min.segment.length = 0,  nudge_y = -0.5, size=4, force=200)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(x ="", y="", title = "Nouvelles hospitalisations", subtitle = "En rouge, la moyenne glissante sur 7 jours des nouveaux patients hospitalisés
pour Covid-19. Le point noir représente le dernier chiffre brut.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))



## RÉA NATIO

rea_natio <- read_csv("rea_natio.csv")
rea_natio_dernier <- filter(rea_natio, date == max(date))

ggplot()+
  geom_line(data=rea_natio, aes(date, rea_glissant), colour="red", size=1)+
  geom_point(data = rea_natio_dernier,
             aes(date, incid_rea))+
  geom_label_repel(data = rea_natio_dernier, aes(date, incid_rea, label=incid_rea),  min.segment.length = 0,  nudge_y = -0.5, size=4, force=200)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(x ="", y="", title = "Nouveaux patients en réanimation", subtitle = "En rouge, la moyenne glissante sur 7 jours des nouveaux malades de Covid-19 admis en réanimation. 
Le point noir représente le dernier chiffre brut.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))


## OCCUP REA NATIO

rea_occup_natio <- read_csv("rea_occup_natio.csv")
rea_occup_natio_dernier <- filter(rea_occup_natio, date == max(date))

ggplot(data=rea_occup_natio, aes(date, TO))+
  geom_line(colour="red", size=1)+
  geom_point(data = rea_occup_natio_dernier, aes(date, TO), colour="red")+
  geom_hline(yintercept=1, linetype = "dashed", color="red")+
  geom_text_repel(data = rea_occup_natio_dernier, aes(label=scales::percent(TO)), size=5, colour = "red", nudge_y = 0.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1.6,by=0.2), minor_breaks = NULL)+
  labs(x ="", y="", title = "Taux d'occupation en réanimation", subtitle = "En rouge, le taux d'occupation journalier des services de réanimation en France. 
Le point noir représente le dernier chiffre disponible.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))


## OCCUP REA ILE-DE-FRANCE 

rea_occup_idf <- read_csv("rea_occup_dep.csv")%>%
  filter(dep == 75)%>%
  select(date, TO)

dernier_chiffre <- rea_occup_idf %>%
  filter(date==max(date))

ggplot(data=rea_occup_idf, aes(date, TO))+
  geom_line(colour="red", size=1)+
  geom_point(data = dernier_chiffre,aes(date, TO), colour="red")+
  geom_hline(yintercept=1, linetype = "dashed")+
  geom_text_repel(data=dernier_chiffre, aes(label=scales::percent(TO)), size=5, colour = "red", nudge_y = 0.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,20,by=0.5), minor_breaks = NULL)+
  labs(x ="", y="", title = "Taux d'occupation en réanimation en Ile-de-France", subtitle = "Evolution quotidienne de la part des patients atteints de Covid-19 dans les services
de réanimation ou en soins intensifs.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))

ggsave(filename = "rea_occup_idf.png", width=18, height = 13, units = "cm",type="cairo-png")


## DÉCÈS NATIO

deces_natio <- read_csv("deces_hosp_natio.csv")
deces_natio_dernier <- filter(deces_natio, date == max(date))

ggplot()+
  geom_line(data=deces_natio, aes(date, deces_glissant), colour="red", size=1)+
  geom_point(data = deces_natio_dernier,
             aes(date, incid_dchosp))+
  geom_label_repel(data = deces_natio_dernier, aes(date, incid_dchosp, label=incid_dchosp),  min.segment.length = 0,  nudge_y = -0.5, size=4, force=200)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(x ="", y="", title = "Décès hospitaliers quotidiens", subtitle = "En rouge, la moyenne glissante sur 7 jours des décès hospitaliers pour cause de Covid-19. 
Le point noir représente le dernier chiffre brut.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum()+
  theme(panel.grid.minor = element_line(linetype = "blank"))


## INCIDENCE DEPS ILE-DE-FRANCE

incidence_idf <- read_csv("incidence_dep.csv") %>%
  filter(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95))%>%
  mutate(dep=recode(dep,
                    `75`="Paris",
                    `77`="Seine-et-Marne",
                    `78`="Yvelines",
                    `91`="Essonne",
                    `92`="Hauts-de-Seine",
                    `93`="Seine-Saint-Denis",
                    `94`="Val-de-Marne",
                    `95`="Val-d'Oise"))
incidence_idf_dernier <- filter(incidence_idf, jour == max(jour))

ggplot()+
  geom_line(data=incidence_idf, aes(jour, taux_incid_glissant), colour="red", size=1)+
  geom_point(data = incidence_idf_dernier,
             aes(jour, taux_incid_glissant), colour="red")+
  geom_text_repel(data = incidence_idf_dernier, aes(jour, taux_incid_glissant, label=round(taux_incid_glissant)),  nudge_y = 10, force = 20, size=4, colour="red")+
  scale_x_date(limits = c(ymd("2021-01-01"), NA), date_breaks = "1 week", date_labels = "%d/%m", minor_breaks = NULL) +
  labs(x ="", y="", title = "Evolution du taux d'incidence en Ile-de-France", subtitle = "Nombre de cas positifs au Covid-19 sur sept jours et pour 100 000 habitants.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  facet_wrap(~dep, ncol = 2, scales = "free")+
  theme_ipsum(strip_text_face = "bold",
              axis_text_size = 9)+
  theme(panel.grid.minor = element_line(linetype = "blank"))


## INCIDENCE DEPS AUVERGNE-RHÔNE-ALPES

incidence_aura <- read_csv("incidence_dep.csv") %>%
  filter(dep %in% c(01, 03, 07, 15, 26, 38, 42, 43, 63, 69, 73, 74))%>%
  mutate(dep=recode(dep,
                    `01`="Ain",
                    `03`="Allier",
                    `07`="Ardèche",
                    `15`="Cantal",
                    `26`="Drôme",
                    `38`="Isère",
                    `42`="Loire",
                    `43`="Haute-Loire",
                    `63`="Puy-de-Dôme",
                    `69`="Rhône",
                    `73`="Savoie",
                    `74`="Haute-Savoie"))
incidence_aura_dernier <- filter(incidence_aura, jour == max(jour))

ggplot()+
  geom_line(data=incidence_aura, aes(jour, taux_incid_glissant), colour="red", size=1)+
  geom_point(data = incidence_aura_dernier,
             aes(jour, taux_incid_glissant), colour="red")+
  geom_text_repel(data = incidence_aura_dernier, aes(jour, taux_incid_glissant, label=round(taux_incid_glissant)),  nudge_y = 10, force = 20, size=4, colour="black")+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y", minor_breaks = NULL) +
  labs(x ="", y="", title = "Evolution du taux d'incidence en Ile-de-France", subtitle = "Nombre de cas positifs au Covid-19 sur sept jours et pour 100 000 habitants.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  facet_wrap(~dep, ncol = 3, scales = "free")+
  theme_ipsum(strip_text_face = "bold",
              axis_text_size = 9)+
  theme(panel.grid.minor = element_line(linetype = "blank"))

incidence_aura %>%
  spread(dep, taux_incid_glissant)%>%
  write_csv("incidence_aura.csv")


## INCIDENCE RHÔNE + NATIO

incidence_rhone <- read_csv("incidence_rhone.csv")
incidence_rhone_dernier <- filter(incidence_rhone, jour == max(jour))

incidence_natio <- read_csv("incidence_france.csv")
incidence_natio_dernier <- filter(incidence_natio, jour == max(jour))

# ggplot()+
#   geom_line(data=incidence_rhone, aes(jour, taux_incid_glissant), colour="red", size=1)+
#   geom_line(data=incidence_natio, aes(jour, taux_incid_glissant))+
#   geom_point(data = incidence_rhone_dernier,
#              aes(jour, taux_incid_glissant), colour="red")+
#   geom_text_repel(data = incidence_rhone_dernier, aes(jour, taux_incid_glissant, label=round(taux_incid_glissant)),  nudge_y = 10, force = 20, size=4, colour="red")+
#   scale_x_date(limits = c(ymd("2021-01-01"), NA), date_breaks = "1 week", date_labels = "%d/%m", minor_breaks = NULL) +
#   labs(x ="", y="", title = "Evolution du taux d'incidence dans le Rhône", subtitle = "Nombre de cas positifs au Covid-19 sur sept jours et pour 100 000 habitants.
# En rouge, la courbe du Rhône, en noir celle de la France.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
#   theme_ipsum(strip_text_face = "bold",
#               axis_text_size = 9)+
#   theme(panel.grid.minor = element_line(linetype = "blank"))

ggplot()+
  geom_line(data=incidence_rhone, aes(jour, taux_incid_glissant), colour="red", size=1)+
  geom_line(data=incidence_natio, aes(jour, taux_incid_glissant))+
  geom_point(data = incidence_rhone_dernier,
             aes(jour, taux_incid_glissant), colour="red")+
  geom_text_repel(data = incidence_rhone_dernier, aes(jour, taux_incid_glissant, label=round(taux_incid_glissant)),  nudge_y = 100, force = 30, size=5, colour="red")+
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m", minor_breaks = NULL) +
  labs(x ="", y="", title = "Evolution du taux d'incidence dans le Rhône", subtitle = "Nombre de cas positifs au Covid-19 sur sept jours et pour 100 000 habitants. En rouge, la courbe du Rhône, en noir celle de la France.", caption = "Réalisation : Marti Blancho | Source : Santé Publique France")+
  theme_ipsum(strip_text_face = "bold",
              axis_text_size = 9, subtitle_size = 12)+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"), 
      plot.title = element_text(vjust = 2),
      plot.caption = element_text(colour = "gray40"),
      legend.title=element_blank(),
      legend.position = c(0, 1), 
      legend.justification = c(-0.1, 1.1))


ggsave(filename = "incidence_rhone.png", width=18, height = 13, units = "cm",type="cairo-png")



## CARTE INCIDENCE DEP 

incidence_departements <- read_csv("incidence_dep.csv")%>%
  filter(jour == max(jour))

carte_departements <-st_read("france_departements.geojson")

incid_dep_data <- carte_departements %>%
  left_join(incidence_departements, by =c("code" = "dep"))

incid_idf_data <- incid_dep_data %>%
  filter(code %in% c(75, 77, 78, 91, 92, 93, 94, 95))

incid_dep_carte <- ggplot()+
  geom_sf(data = incid_dep_data, aes(fill= taux_incid_glissant))+
  scale_fill_viridis(option = "magma", limits = c(0, 500), direction = -1)+
  theme_void()

incid_idf_carte <- ggplot()+
  geom_sf(data = incid_idf_data, aes(fill= taux_incid_glissant))+
  scale_fill_viridis(option = "magma", direction = -1)+
  theme_void()+
  theme(legend.position="none")

ggdraw() +
  draw_plot(incid_dep_carte, x = -0.01, y = -0.05)+
  draw_plot(incid_idf_carte, x = 0.6, y = 0.7, width = 0.3, height = 0.3)


## VACCINATIONS NATIO PAR JOUR ET TYPE VACCIN

vaccins_natio <- read_csv("vaccins_france.csv")

ggplot()+
  geom_bar(data = subset(vaccins_natio, vaccin != 0), aes(x = jour, y = doses_1_2, fill = nom_vaccin), stat="identity", position = "stack")+
  geom_line(data = subset(vaccins_natio, vaccin == 0), aes(jour, doses_1_2_glissant), size=0.5, color = "black")+
  scale_x_date(expand = c(0,2),date_breaks = "7 days", date_labels = "%d/%m", minor_breaks = NULL)+
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  scale_fill_manual(values=wes_palette(name="Moonrise3"))+
  labs(x ="", y="", 
       title = "Vaccinations contre le Covid-19 en France", 
       subtitle = "Vaccinations journalières en France, premières et secondes doses confondues.
La courbe noire représente la moyenne glissante sur 7 jours.",
       fill = "Vaccin", caption = "Réalisation : @MartiBlancho | Source : Santé Publique France")+
  theme_bw()+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"), 
        plot.title = element_text(vjust = 2),
        plot.caption = element_text(colour = "gray40"),
        legend.title=element_blank(),
        legend.position = c(0, 1), 
        legend.justification = c(-0.1, 1.1))

ggsave("vaccins_natio.svg")
ggsave(filename = "vaccins_natio.png", width=18, height = 13, units = "cm",type="cairo-png")


# Total de personnes vaccinées par jour et par nombre de doses reçues

vaccins_total <- read_csv("vaccins_age_france.csv")%>%
  filter(clage_vacsi == 0)%>%
  select(jour, n_cum_dose1, n_cum_dose2)%>%
  gather(n_cum_dose1, n_cum_dose2, key="dose", value="cumul")

ggplot(vaccins_total, aes(x=jour, y=cumul, fill=dose))+
  geom_area()+
  scale_fill_manual(values=wes_palette(name="Zissou1"), labels = c("1 dose", "2 doses"))+
  scale_y_continuous(limits = c(0, 70000000), expand = c(0,0), breaks = seq(10000000,60000000,by=10000000), minor_breaks = NULL, labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  scale_x_date(expand = c(0,0),date_breaks = "1 month", date_labels = "%B", minor_breaks = NULL)+
  labs(x ="", y="", 
       title = "Total des personnes vaccinées contre la Covid-19", 
       subtitle = "Nombre cumulé de personnes ayant reçu une ou deux doses de vaccin.",
       fill = "Vaccin", caption = "Réalisation : @MartiBlancho | Source : Santé Publique France")+
  theme_bw()+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"),
        plot.title = element_text(vjust = 2),
        plot.caption = element_text(colour = "gray40"),
        legend.title=element_blank(),
        legend.position = c(0.8, 1),
        legend.justification = c(-0.1, 1.1))

# https://www.r-graph-gallery.com/136-stacked-area-chart.html

ggplot()+
  geom_area(data = vaccins_total, aes(jour, n_cum_dose1), size=0.5)+
  geom_area(data = vaccins_total, aes(jour, n_cum_dose2), size=0.5, fill="red")+
  scale_x_date(expand = c(0,2),date_breaks = "7 days", date_labels = "%d/%m", minor_breaks = NULL)+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,67000000,by=2000000), labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  scale_fill_manual(values=wes_palette(name="Darjeeling1"))+
  labs(x ="", y="", 
       title = "Vaccinations contre le Covid-19 en France", 
       subtitle = "Nombre cumulé de personnes ayant reçu une ou deux doses de vaccin.",
       fill = "Vaccin", caption = "Réalisation : @MartiBlancho | Source : Santé Publique France")+
  theme_bw()+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"), 
        plot.title = element_text(vjust = 2),
        plot.caption = element_text(colour = "gray40"),
        legend.title=element_blank(),
        legend.position = c(0, 1), 
        legend.justification = c(-0.1, 1.1))


# VACCINATIONS PAR JOUR RHÔNE

vaccins_rhone <- read_csv("vaccins_dep.csv")%>%
  filter(dep == 69)%>%
  arrange(jour)%>%
  rename("Vaccination complète"="n_complet", "Première dose"="n_dose1")%>%
  select(jour,  dose1_glissant, dosecomplet_glissant, `Première dose`, `Vaccination complète`)%>%
  gather("type", "nombre", -jour, -dose1_glissant, -dosecomplet_glissant)


ggplot(data = vaccins_rhone, aes(x = jour))+
  geom_bar(aes(y = nombre, fill=type), stat="identity")+
  geom_line(aes(y = dosecomplet_glissant), size=0.5, color = "red")+
  geom_line(aes(y = dose1_glissant), size=0.5, color = "blue")+
  scale_x_date(expand = c(0,2),date_breaks = "14 days", date_labels = "%d/%m", minor_breaks = NULL)+
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  scale_fill_manual(values=wes_palette(name="Moonrise3"))+
  labs(x ="", y="", 
       title = "Vaccinations contre la Covid-19 dans le Rhône", 
       subtitle = "Premières doses et vaccinations complètes quotidiennes dans le Rhône. La courbe rouge représente
la moyenne glissante sur 7 jours des vaccinations complètes, la bleue celle des premières doses.",
       fill = "Vaccin", caption = "Réalisation : @MartiBlancho | Source : Santé Publique France")+
  theme_bw()+
  theme(plot.margin = margin(5, 5, 5, 5, "mm"), 
        plot.title = element_text(vjust = 2),
        plot.caption = element_text(colour = "gray40"),
        legend.title=element_blank(),
        legend.position = c(0, 1), 
        legend.justification = c(-0.1, 1.1))
