library(dplyr)
library(COVID19data)
library(ggplot2)

x_week_daily = covid19_sorted %>%
  #filter(alpha3 %in% c("USA", "KOR", "IRN", "IRL", "SWE", "AUS", "ITA", "CHN")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    confirmed_past7days = zoo::rollsum(diff_confirmed, 7, fill = NA, align = "right")
  ) %>%
  mutate(confirmed_past7days = ifelse(is.na(confirmed_past7days), confirmed, confirmed_past7days)) %>%
  arrange(date)%>%
  filter(Province.State == "total") %>%
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
  select(alpha3, Country.Region, confirmed, confirmed_past7days, population, days_since, date) %>%
  group_by(Country.Region) %>%
  mutate(max_confirmed = max(confirmed)) %>%
  filter(max_confirmed >= 200) %>%
  select(-max_confirmed) %>%
  filter(confirmed > 100)

x0 = x_week_daily %>%
  padr::pad(group = c("Country.Region"), by = "date", start_val = lubridate::as_date("2020-01-28")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  select(-alpha3) %>%
  left_join(read.csv("https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/JohnsHopkins-to-A3.csv"))

x = x0 %>%
  accumulate_by(~days_since) %>%
  mutate(date2 = as.character(lubridate::as_date("2020-01-28") + frame)) %>%
  mutate(frame = date2)

z = ggplot(x) +
  aes(
    confirmed/population,
    confirmed_past7days/confirmed,
    frame = frame,
    col = Country.Region,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line() +
  ggthemes::theme_few()


z = z %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z, "covid19.html", selfcontained = FALSE, title = 'Press the "Play" button')

z2 = ggplot(x) +
  aes(
    confirmed,
    confirmed_past7days,
    frame = frame,
    col = Country.Region,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_line() +
  ggthemes::theme_few() +
  xlab("Total Confirmed Cases") +
  ylab("New Confirmed Cases (in the Past Week)")

z2 = z2 %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z2, "covid19_original.html", selfcontained = FALSE, title = 'Press the "Play" button. Double click a country to see it by itself.')


Europe = c(
  "ALB", "AND", "AUT", "BEL", "BIH", "BGR", "DNK", "FIN", "FRA",
  "DEU", "HUN", "ISL", "IRL", "ITA", "LVA", "MNE", "NLD",
  "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "ESP", "SWE",
  "CHE", "UKR", "GBR")

x0_europe = x0 %>% filter(alpha3 %in% Europe) %>%
  filter(date >= "2020-02-20")

x_europe = x0_europe %>%
  accumulate_by(~days_since) %>%
  mutate(date2 = as.character(lubridate::as_date("2020-01-28") + frame)) %>%
  mutate(frame = date2)

z = ggplot(x_europe) +
  aes(
    confirmed/population,
    confirmed_past7days/confirmed,
    frame = frame,
    col = Country.Region,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line() +
  ggthemes::theme_few() +
  guides(fill=guide_legend(title="Double click a country to see it by itself"))


z = z %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z, "covid19_Europe.html", selfcontained = FALSE, title = 'Press the "Play" button. Double click a country to see it by itself.')


z2 = ggplot(x_europe) +
  aes(
    confirmed,
    confirmed_past7days,
    frame = frame,
    col = Country.Region,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_line() +
  ggthemes::theme_few() +
  xlab("Total Confirmed Cases") +
  ylab("New Confirmed Cases (in the Past Week)")

z2 = z2 %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z2, "covid19_original_Europe.html", selfcontained = FALSE, title = 'Press the "Play" button. Double click a country to see it by itself.')


## Italy ------------------

x_week_daily_italy = covid19_sorted %>%
  filter(alpha3 %in% c("ITA")) %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    confirmed_past7days = zoo::rollsum(diff_confirmed, 7, fill = NA, align = "right")
  ) %>%
  mutate(confirmed_past7days = ifelse(is.na(confirmed_past7days), confirmed, confirmed_past7days)) %>%
  arrange(date)%>%
  filter(Province.State != "total") %>%
  left_join(
    read.csv(
      "https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/full_list.csv"
    ) %>% select(alpha3, Province.State, population)
  ) %>%
  filter(alpha3 != "cruise") %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  select(Province.State, confirmed, confirmed_past7days, population, days_since, date) %>%
  filter(confirmed_past7days/confirmed >= 1e-3) %>%
  group_by(Province.State) %>%
  filter(confirmed > 50)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

x0 = x_week_daily_italy %>%
  padr::pad(group = c("Province.State"), by = "date", start_val = lubridate::as_date("2020-01-28")) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  filter(date >= "2020-02-20")

x = x0 %>%
  accumulate_by(~days_since) %>%
  mutate(date2 = as.character(lubridate::as_date("2020-01-28") + frame)) %>%
  mutate(frame = date2) %>%
  mutate(
    totale_casi = confirmed,
    pre7giorni_casi = confirmed_past7days,
    data = date,
    regione = Province.State,
    popolazione = population
  )

z = ggplot(x) +
  aes(
    totale_casi/popolazione,
    pre7giorni_casi/totale_casi,
    frame = frame,
    col = regione,
    date = data,
    totale_casi = totale_casi,
    pre7giorni_casi = pre7giorni_casi) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line() +
  ggthemes::theme_few() +
  guides(col = guide_legend("Fare doppio clic vederla da sola. Double click a region"))

z = z %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z, "covid19_Italia.html", selfcontained = FALSE, title = 'Premere il pulsante "Play, Double click a country to see it by itself.')

z2 = ggplot(x) +
  aes(
    totale_casi,
    pre7giorni_casi,
    frame = frame,
    col = regione,
    date = data,
    totale_casi = totale_casi,
    pre7giorni_casi = pre7giorni_casi) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_line() +
  ggthemes::theme_few() +
  xlab("Total Confirmed Cases") +
  ylab("New Confirmed Cases (in the Past Week)")

z2 = z2 %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z2, "covid19_original_Italia.html", selfcontained = FALSE, title = 'Premere il pulsante "Play, Double click a country to see it by itself.')




