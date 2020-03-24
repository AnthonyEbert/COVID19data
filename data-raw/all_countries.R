
library(dplyr)
library(tidyr)
library(COVID19data)

# Download JHU data

confirmed = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>% jh_process(term = "confirmed")

recovered = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>% jh_process(term = "recovered")

deaths = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>% jh_process(term = "deaths")

john_hopkins <- left_join(confirmed, recovered) %>% left_join(deaths) %>%
  mutate(Province.State = forcats::fct_recode(Province.State, total = "")) %>%
  filter(!(Country.Region %in% c("Italy", "Switzerland")))

# Add Italian data --------------------

italy = readr::read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv") %>%
  transmute(
    date = lubridate::as_date(data),
    Province.State = denominazione_regione,
    hospitalized_intensive = terapia_intensiva,
    hospitalized_symptoms = ricoverati_con_sintomi,
    tested = tamponi,
    confirmed = totale_casi,
    recovered = dimessi_guariti,
    deaths = deceduti
  ) %>%
  mutate(Country.Region = "Italy")

jh_italy <- dplyr::bind_rows(john_hopkins, italy)

# Add Chinese data -------------------

## DXY ------------

country_totals <- jh_italy %>% filter(Province.State != "total") %>%
  group_by(Country.Region, date) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Province.State = "total")

all_countries <- dplyr::bind_rows(jh_italy, country_totals)

china <- readr::read_csv("https://raw.githubusercontent.com/BlankerL/DXY-COVID-19-Data/master/csv/DXYOverall.csv") %>%
  mutate(
    date = lubridate::as_date(updateTime)
  ) %>%
  group_by(date) %>%
  summarise(hospitalized_intensive = max(seriousCount, na.rm = TRUE)) %>%
  mutate(
    hospitalized_intensive = replace(.$hospitalized_intensive, !is.finite(.$hospitalized_intensive) | .$hospitalized_intensive == 0, NA),
    Country.Region = "China",
    Province.State = "total"
  )

all_countries <- left_join_fill(all_countries , china, by = c("Country.Region", "Province.State", "date"))

## NHC -------------

nhc_china <- getURL("https://docs.google.com/spreadsheets/d/1qniOeebfqKTMrLT8hCyR37IJVSfefZH5lhdZwkpTN-c/export?format=csv",.opts=list(ssl.verifypeer=FALSE)) %>%
  textConnection() %>%
  read.csv() %>%
  mutate(
    Country.Region = "China",
    Province.State = "total",
    date = lubridate::as_date(date)
  )

all_countries <- left_join_fill(all_countries, nhc_china, by = c("Country.Region", "Province.State", "date"))

nhc_hubei <- getURL("https://docs.google.com/spreadsheets/d/1l56Y78OszeS3X05t_yUA9nLLQpVuAGPRLqWSrZF-tz8/export?format=csv",.opts=list(ssl.verifypeer=FALSE)) %>%
  textConnection() %>%
  read.csv() %>%
  mutate(
    Country.Region = "China",
    Province.State = "Hubei",
    date = lubridate::as_date(date),
    hospitalized_intensive = intensive_care
  ) %>%
  select(
    Country.Region,
    Province.State,
    date,
    hospitalized_intensive,
    suspected
  )

all_countries <- left_join_fill(all_countries, nhc_hubei, by = c("Country.Region", "Province.State", "date"))

write.csv(all_countries, "all_countries.csv")

usethis::use_data(all_countries)
