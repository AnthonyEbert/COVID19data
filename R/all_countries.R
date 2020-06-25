#' @import dplyr
#' @export
all_countries <- function(){
  # Download JHU data
  john_hopkins <- johns_hopkins_data() %>%
    mutate(Province.State = forcats::fct_recode(Province.State, total = ""))
  # filter(!(Country.Region %in% c("Italy")))

  # Add Italian data --------------------

  italy = readr::read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
                          col_types = "?cccddiiiiiiiiiiicc") %>%
    transmute(
      date = lubridate::as_date(data),
      Province.State = denominazione_regione,
      hospitalized_intensive = terapia_intensiva,
      hospitalized_symptoms = ricoverati_con_sintomi,
      isolated_at_home = isolamento_domiciliare,
      tested = tamponi,
      confirmed = totale_casi,
      recovered = dimessi_guariti,
      deaths = deceduti
    ) %>%
    mutate(Country.Region = "Italy")

  Trentino <- italy %>%
    filter(Province.State %in% c("P.A. Trento", "P.A. Bolzano")) %>%
    group_by(date) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(Country.Region = "Italy", Province.State = "Trentino-Alto Adige")

  italy <- italy %>%
    filter(!(Province.State %in% c("P.A. Trento", "P.A. Bolzano")))

  jh_italy <- dplyr::bind_rows(john_hopkins, italy) %>%
    dplyr::bind_rows(Trentino)


  # Add Chinese data -------------------

  ## DXY ------------

  country_totals <- jh_italy %>%
    filter(!((Country.Region == "Italy") & (Province.State == "total"))) %>%
    group_by(Country.Region, date) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(Province.State = "total") %>%
    select(Country.Region, Province.State, date, confirmed, recovered, deaths, hospitalized_intensive, hospitalized_symptoms, isolated_at_home, tested)

  all_countries <- dplyr::bind_rows(country_totals, jh_italy) %>%
    dplyr::distinct(Country.Region, Province.State, date, .keep_all = TRUE)

  # china <- readr::read_csv("https://raw.githubusercontent.com/BlankerL/DXY-COVID-19-Data/master/csv/DXYOverall.csv",
  #                          col_types = "ccccccciiiiiiiiiiiiccccccccccccccccccccc?") %>%
  #   mutate(
  #     date = lubridate::as_date(updateTime)
  #   ) %>%
  #   group_by(date) %>%
  #   summarise(serious_cases = max(seriousCount, na.rm = TRUE)) %>%
  #   mutate(
  #     serious_cases = replace(.$serious_cases, !is.finite(.$serious_cases) | .$serious_cases == 0, NA),
  #     Country.Region = "China",
  #     Province.State = "total"
  #   )
  #
  # all_countries <- left_join(all_countries , china, by = c("Country.Region", "Province.State", "date"))

  ## NHC -------------

  nhc_china <- RCurl::getURL("https://docs.google.com/spreadsheets/d/1qniOeebfqKTMrLT8hCyR37IJVSfefZH5lhdZwkpTN-c/export?format=csv",.opts=list(ssl.verifypeer=FALSE)) %>%
    textConnection() %>%
    read.csv() %>%
    mutate(
      Country.Region = "China",
      Province.State = "total",
      date = lubridate::as_date(date),
      serious_cases = hospitalized_intensive
    ) %>%
    select(-hospitalized_intensive)

  all_countries <- left_join_fill(all_countries, nhc_china, by = c("Country.Region", "Province.State", "date"))

  nhc_hubei <- RCurl::getURL("https://docs.google.com/spreadsheets/d/1l56Y78OszeS3X05t_yUA9nLLQpVuAGPRLqWSrZF-tz8/export?format=csv",.opts=list(ssl.verifypeer=FALSE)) %>%
    textConnection() %>%
    read.csv() %>%
    mutate(
      Country.Region = "China",
      Province.State = "Hubei",
      date = lubridate::as_date(date),
      serious_cases = intensive_care
    ) %>%
    select(
      Country.Region,
      Province.State,
      date,
      serious_cases,
      suspected
    )

  covid19_complete <- left_join_fill(all_countries, nhc_hubei, by = c("Country.Region", "Province.State", "date")) %>%
    mutate(active = confirmed - recovered - deaths)

  ## Switzerland -------------------

  switzerland_confirmed <- read.csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv") %>%
    mutate(date = lubridate::as_date(Date)) %>%
    select(-Date) %>%
    tidyr::gather(Province.State,confirmed,-date)

  switzerland_recoveries <- read.csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_released_switzerland_openzh.csv") %>%
    mutate(date = lubridate::as_date(Date)) %>%
    select(-Date) %>%
    tidyr::gather(Province.State,recovered,-date)

  switzerland_deaths <- read.csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_fatalities_switzerland_openzh.csv") %>%
    mutate(date = lubridate::as_date(Date)) %>%
    select(-Date) %>%
    tidyr::gather(Province.State,deaths,-date)

  switzerland_hospitalized_symptoms <- read.csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_hospitalized_switzerland_openzh.csv")%>%
    mutate(date = lubridate::as_date(Date)) %>%
    select(-Date) %>%
    tidyr::gather(Province.State,hospitalized_symptoms,-date)

  switzerland_hospitalized_intensive <- read.csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_icu_switzerland_openzh.csv")%>%
    mutate(date = lubridate::as_date(Date)) %>%
    select(-Date) %>%
    tidyr::gather(Province.State,hospitalized_intensive,-date)

  switzerland <- left_join(switzerland_confirmed, switzerland_recoveries) %>%
    left_join(switzerland_deaths) %>%
    left_join(switzerland_hospitalized_symptoms) %>%
    left_join(switzerland_hospitalized_intensive) %>%
    mutate(hospitalized_symptoms = hospitalized_symptoms - hospitalized_intensive) %>%
    mutate(Country.Region = "Switzerland") %>%
    mutate(Province.State = factor(Province.State) %>%
             forcats::fct_recode(
               `Aargau` = "AG",
               `Appenzell Ausserrhoden` = "AR",
               `Appenzell Innerrhoden` = "AI",
               `Basel City` = "BS",
               `Basel-Landschaft` = "BL",
               `Canton of Bern` = "BE",
               `Canton of Zug` = "ZG",
               `Fribourg` = "FR",
               `Geneva` = "GE",
               `Glarus` = "GL",
               `Grisons` = "GR",
               `Jura` = "JU",
               `Lucerne` = "LU",
               `Neuchâtel` = "NE",
               `Nidwalden` = "NW",
               `Obwalden` = "OW",
               `Schaffhausen` = "SH",
               `Schwyz` = "SZ",
               `Solothurn` = "SO",
               `St. Gallen` = "SG",
               `Thurgau` = "TG",
               `Ticino` = "TI",
               `Uri` = "UR",
               `Valais` = "VS",
               `Vaud` = "VD",
               `Zurich` = "ZH"
             )
    )

  covid19_complete <- dplyr::bind_rows(covid19_complete, switzerland)

  iso_info <- read.csv("https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/JohnsHopkins-to-A3.csv")
  country_codes <- read.csv("https://raw.githubusercontent.com/AnthonyEbert/COVID-19_ISO-3166/master/country_codes.csv")

  covid19_complete <- covid19_complete %>%
    left_join(iso_info) %>%
    left_join(country_codes %>% select(-Country.Region))

  # readr::write_csv(covid19_complete, "data-raw/covid19_complete.csv")

  # Check data -------------------

  # Check that confirmed, recovered and deaths are all non-decreasing

  covid19_issorted <- covid19_complete %>%
    group_by(Country.Region, Province.State) %>%
    arrange(date) %>%
    summarise(confirmed_sorted = !is.unsorted(confirmed), recovered_sorted = !is.unsorted(recovered), deaths_sorted = !is.unsorted(deaths))

  # readr::write_csv(covid19_issorted, "data-raw/covid19_is-sorted.csv")

  covid19_sorted <- covid19_complete %>%
    group_by(Country.Region, Province.State) %>%
    arrange(date) %>%
    mutate(confirmed = cummax(NA0(confirmed)), recovered = cummax(NA0(recovered)), deaths = cummax(NA0(deaths))) %>%
    mutate(active = confirmed - recovered - deaths) %>%
    ungroup()

  # readr::write_csv(covid19_sorted, "data-raw/covid19_sorted.csv")

  output <- covid19_sorted %>% left_join(mobility_data(), by = c("alpha2", "Province.State", "date"))

  return(output)
}

