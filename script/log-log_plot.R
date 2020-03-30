library(dplyr)
library(COVID19data)

x_day = covid19_cleaned %>%
  filter(Country.Region %in% c("Italy", "Switzerland")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(new_confirmed = c(first(confirmed), diff(confirmed))) %>%
  mutate(label = paste(Country.Region, Province.State, "-"))

x_week = covid19_cleaned %>%
  filter(Country.Region %in% c("Italy", "Switzerland", "Spain", "US")) %>%
  mutate(week = lubridate::isoweek(date)) %>%
  # filter(Province.State != "total") %>%
  group_by(Country.Region, Province.State, week) %>%
  summarise(confirmed = max(confirmed)) %>%
  # filter(confirmed >= 100) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(week) %>%
  mutate(new_confirmed = c(first(confirmed),diff(confirmed))) %>%
  mutate(label = paste(Country.Region, Province.State, "-"))
  # filter(new_confirmed >= 1)

library(ggplot2)
z = ggplot(x_week) + aes(x = confirmed, y = new_confirmed, col = label) + geom_line() + geom_point() + scale_x_log10() + scale_y_log10()

htmlwidgets::saveWidget(z, "italy_regions_2020-03-29.html")
