
italy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "total")

readr::write_csv(italy, "data-raw/italy_all.csv")

lombardy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "Lombardia")

readr::write_csv(lombardy, "data-raw/lombardy_all.csv")
