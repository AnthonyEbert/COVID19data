library(dplyr)
library(COVID19data)
library(ggplot2)


x_week_daily_switzerland = covid19_sorted %>%
  filter(alpha3 %in% c("CHE")) %>%
  filter(Province.State != "CH") %>%
  group_by(Country.Region, Province.State) %>%
  arrange(date) %>%
  mutate(
    diff_confirmed = c(0, diff(confirmed)),
    confirmed_past7days = zoo::rollsum(diff_confirmed, 7, fill = NA, align = "right")
  ) %>%
  mutate(confirmed_past7days = ifelse(is.na(confirmed_past7days), confirmed, confirmed_past7days, diff_confirmed)) %>%
  arrange(date)%>%
  filter(Province.State != "total") %>%
  left_join(
    read.csv(
      "https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/full_list.csv"
    ) %>% select(alpha3, Province.State, population)
  ) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(days_since = -as.numeric(first(date) - date)) %>%
  select(Province.State, confirmed, confirmed_past7days, population, days_since, date) %>%
  # filter(confirmed_past7days/confirmed >= 1e-3) %>%
  group_by(Province.State)

x0 = x_week_daily_switzerland %>%
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
    Canton = Province.State
  )

z = ggplot(x) +
  aes(
    confirmed/population,
    confirmed_past7days/confirmed,
    frame = frame,
    col = Canton,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days,
    diff_confirmed = diff_confirmed) +
  scale_x_log10() +
  scale_y_log10() +
  geom_line() +
  ggthemes::theme_few() +
  guides(col = guide_legend("Double click a canton"))

z = z %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z, "covid19_Switzerland.html", selfcontained = FALSE, title = 'Double click a country to see it by itself.')

z2 = ggplot(x) +
  aes(
    confirmed,
    confirmed_past7days,
    frame = frame,
    col = Canton,
    date = date,
    confirmed = confirmed,
    confirmed_past7days = confirmed_past7days,
    diff_confirmed = diff_confirmed) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_line() +
  ggthemes::theme_few() +
  xlab("Total Confirmed Cases") +
  ylab("New Confirmed Cases (in the Past Week)")

z2 = z2 %>% plotly::ggplotly() %>% plotly::animation_opts(redraw = FALSE)
htmlwidgets::saveWidget(z2, "covid19_original_Switzerland.html", selfcontained = FALSE, title = 'Premere il pulsante "Play, Double click a country to see it by itself.')


