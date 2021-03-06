library(dplyr)
library(COVID19data)

hubei = covid19_sorted %>% filter(Country.Region == "China", Province.State == "Hubei") %>% select(-hospitalized_symptoms, -tested, -close_observation) %>%
  mutate(
    susc_not_ill = 59.02e6 - confirmed
  )

readr::write_csv(hubei, "data-raw/hubei_all.csv")
