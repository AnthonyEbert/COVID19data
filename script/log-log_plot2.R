library(dplyr)
library(COVID19data)

x_week_daily = covid19_sorted %>%
  filter(alpha3 %in% c("USA", "KOR", "IRN", "IRL", "SWE", "AUS", "TAI")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    new_confirmed = zoo::rollsum(diff_confirmed, 7, fill = NA, align = "right")
  ) %>%
  mutate(label = paste(Country.Region, Province.State, "-")) %>%
  arrange(date)%>%
  filter(Province.State == "total") %>%
  filter(confirmed >= 100) %>%
  left_join(
    read.csv(
      "https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/full_list.csv"
    ) %>% select(alpha3, Province.State, population)
  ) %>%
  mutate(confirmed_pop = confirmed/population, new_confirmed_confirmed = new_confirmed/confirmed) %>%
  filter(new_confirmed_confirmed >= 0.05) %>%
  filter(alpha3 != "cruise") %>%
  filter(population >= 0.75e6) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  select(alpha3, confirmed_pop, new_confirmed_confirmed, days_since, date)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

x = x_week_daily %>%
  padr::pad(group = "alpha3", by = "date", start_val = lubridate::as_date("2020-01-28")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  accumulate_by(~days_since) %>%

z = ggplot(x) +
  aes(confirmed_pop, new_confirmed_confirmed, frame = frame, col = alpha3) +
  geom_line()

z %>% plotly::ggplotly()

