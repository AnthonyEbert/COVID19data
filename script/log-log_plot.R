library(dplyr)
library(COVID19data)

x_day = covid19_sorted %>%
  filter(Country.Region %in% c("Italy", "Switzerland")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(new_confirmed = c(first(confirmed), diff(confirmed))) %>%
  mutate(label = paste(Country.Region, Province.State, "-"))

x_week = covid19_sorted %>%
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
  filter(day %% 2 == 0)
  # filter(new_confirmed >= 1)

x_week_daily = covid19_sorted %>%
  filter(alpha3 %in% c("ITA", "CHE", "ESP", "USA", "KOR", "IRN", "IRL", "SWE", "CHN", "AUS", "TAI")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    new_confirmed = zoo::rollsum(diff_confirmed, 7, fill = NA, align = "right")
  ) %>%
  mutate(label = paste(Country.Region, Province.State, "-")) %>%
  arrange(date)%>%
  mutate(day = lubridate::day(date)) %>%
  filter(Province.State == "total" | Country.Region != "Switzerland") %>%
  filter(Province.State == "Hubei" | Country.Region != "China") %>%
  filter(Province.State == "total" | Country.Region != "Australia") %>%
  filter(Province.State == "total" | alpha3 != "GBR") %>%
  filter(confirmed >= 150) %>%
  left_join(
    read.csv(
      "https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/full_list.csv"
    ) %>% select(alpha3, Province.State, population)
  ) %>%
  mutate(confirmed_pop = confirmed/population, new_confirmed_pop = new_confirmed/population) %>%
  filter(new_confirmed/confirmed >= 0.05)



library(ggplot2)
z = (ggplot(x_week_daily) +
       aes(x = confirmed/population, y = new_confirmed / confirmed, col = label) +
       geom_line() +
       scale_x_log10() +
       scale_y_log10()) %>%
  plotly::ggplotly() %>%
  plotly::layout(legend = list(
    orientation = "h", y = -0.1
  ))

htmlwidgets::saveWidget(z, "italy_regions_2020-03-31_test.html")
