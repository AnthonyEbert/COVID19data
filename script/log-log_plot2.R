library(dplyr)
library(COVID19data)

x_week_daily = covid19_sorted %>%
  filter(alpha3 %in% c("USA", "KOR", "IRN", "IRL", "SWE", "AUS", "ITA", "CHN")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    confirmed_past7days = zoo::rollsum(diff_confirmed, 7, fill = .$confirmed[1:6], align = "right")
  ) %>%
  mutate(label = paste(Country.Region, ifelse(Province.State == "total", "", Province.State), sep = " ")) %>%
  arrange(date)%>%
  filter(Province.State %in% c("Hubei", "total") | Country.Region != "China") %>%
  filter(Province.State == "total" | Country.Region == "China") %>%
  filter(confirmed >= 100) %>%
  left_join(
    read.csv(
      "https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/full_list.csv"
    ) %>% select(alpha3, Province.State, population)
  ) %>%
  filter(alpha3 != "cruise") %>%
  filter(population >= 0.75e6) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  select(label, confirmed, confirmed_past7days, population, days_since, date) %>%
  filter(confirmed_past7days/confirmed >= 1e-3) %>%
  group_by(label) %>%
  mutate(max_confirmed = max(confirmed)) %>%
  filter(max_confirmed >= 200) %>%
  select(-max_confirmed)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

x0 = x_week_daily %>%
  padr::pad(group = "label", by = "date", start_val = lubridate::as_date("2020-01-28")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date))

x = x0 %>%
  accumulate_by(~days_since) %>%
  mutate(date2 = lubridate::as_date("2020-01-28") + frame)

z = ggplot(x) +
  aes(
    confirmed/population,
    confirmed_past7days/confirmed,
    frame = frame,
    col = label,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line() +
  ggthemes::theme_few()

z = z %>% plotly::ggplotly() %>% plotly::animation_slider(hide = TRUE)
htmlwidgets::saveWidget(z, "covid19_test.html", selfcontained = FALSE)
# %>% plotly::layout(xaxis = list(animation_slider = list(type = "date")))

