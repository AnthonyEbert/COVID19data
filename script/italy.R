library(dplyr)
library(COVID19data)

italy <- covid19_sorted %>%
  filter(Country.Region == "Italy", Province.State == "total") %>%
  mutate(
    susc_not_ill = 60317000 - confirmed
  )

mobility_data = readr::read_csv("https://raw.githubusercontent.com/pastelsky/covid-19-mobility-tracker/master/output/IT/mobility-transit-stations.csv") %>%
  mutate(Country.Region = "Italy", Province.State = "total", alpha3 = "ITA") %>%
  rename(transit_stations = value) %>%
  arrange(date)

italy <- left_join(italy, mobility_data)

readr::write_csv(italy, "data-raw/italy_all_full.csv")

italy <- italy %>% filter(date >= "2020-02-24")

readr::write_csv(italy, "data-raw/italy_all.csv")

lombardy <- covid19_sorted %>%
  filter(Country.Region == "Italy", Province.State == "Lombardia") %>%
  mutate(
    susc_not_ill = 10060574 - confirmed
  )

readr::write_csv(lombardy, "data-raw/lombardy_all.csv")
