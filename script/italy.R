
italy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "total") %>%
  mutate(
    susc_not_ill = 60317000 - confirmed
  )

readr::write_csv(italy, "data-raw/italy_all.csv")

lombardy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "Lombardia") %>%
  mutate(
    susc_not_ill = 10060574 - confirmed
  )

readr::write_csv(lombardy, "data-raw/lombardy_all.csv")
