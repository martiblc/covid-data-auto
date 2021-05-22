library(tidyverse)
library(slider)
library(lubridate)
library(beepr)



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
