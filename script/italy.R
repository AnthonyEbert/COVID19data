
italy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "total")

write.csv(italy, "data-raw/italy.csv")

lombardy <- all_countries %>%
  filter(Country.Region == "Italy", Province.State == "Lombardia")

write.csv(lombardy, "data-raw/lombardy.csv")
