#' @export
jh_process <- function(x, term){
  output <- x %>%
    group_by(Country.Region, Province.State) %>%
    summarise_at(names(.)[purrr::partial(startsWith, prefix = "X")(names(.))], sum) %>%
    tidyr::gather(date, !!term, -Country.Region, -Province.State) %>%
    mutate(date = stringr::str_sub(date, start = 2) %>%
             lubridate::as_date(format = "%m.%d.%y", tz = "Europe/London"))
  return(output)
}

#' @export
johns_hopkins_data <- function(){
  base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

  confirmed = read.csv(
    paste0(base_url, "time_series_covid19_confirmed_global.csv"),
    stringsAsFactors = FALSE
  ) %>% jh_process(term = "confirmed")

  recovered = read.csv(
    paste0(base_url, "time_series_covid19_recovered_global.csv"),
    stringsAsFactors = FALSE
  ) %>% jh_process(term = "recovered")

  deaths = read.csv(
    paste0(base_url, "time_series_covid19_deaths_global.csv"),
    stringsAsFactors = FALSE
  ) %>% jh_process(term = "deaths")

  johns_hopkins <- left_join(confirmed, recovered, by = c("Country.Region", "Province.State", "date")) %>%
    left_join(deaths, by = c("Country.Region", "Province.State", "date"))

  return(johns_hopkins)
}
