library(dplyr)
library(COVID19data)
library(dplyr)
library(plotly)

Europe = c(
  "ALB", "AND", "AUT", "BEL", "BIH", "BGR", "DNK", "FIN", "FRA",
  "DEU", "HUN", "ISL", "IRL", "ITA", "LVA", "MNE", "NLD",
  "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "ESP", "SWE",
  "CHE", "UKR", "GBR")

x_week = all_countries() %>%
  left_join(read.csv("https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/JohnsHopkins-to-A3.csv"), by = "Country.Region") %>%
  filter(alpha3 %in% Europe) %>%
  filter(date <= "2020-03-28") %>%
  mutate(week = lubridate::epiweek(date)) %>%
  # filter(Province.State != "total") %>%
  group_by(Country.Region, week) %>%
  summarise(confirmed = max(confirmed)) %>%
  # filter(confirmed >= 100) %>%
  group_by(Country.Region) %>%
  arrange(week) %>%
  mutate(new_confirmed = c(first(confirmed),diff(confirmed)))
# filter(new_confirmed >= 1)

z = (ggplot(x_week) + aes(x = confirmed, y = new_confirmed, col = Country.Region) + geom_line() + geom_point() + scale_x_log10() + scale_y_log10()) %>% plotly::ggplotly()

htmlwidgets::saveWidget(z, "europe_2020-03-29.html")
