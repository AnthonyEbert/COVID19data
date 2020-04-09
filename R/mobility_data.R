#' @export
mobility_data <- function(){

  url_link <- "https://raw.githubusercontent.com/nacnudus/google-location-coronavirus/master/2020-03-29-country.tsv"
  col_types <- c("cdccDcddccdddD")

  mobility_dat <- readr::read_tsv(url_link, col_types = col_types) %>%
    select("country_code", "region_name", "category", "trend", "date") %>%
    rename(alpha2 = country_code, Province.State = region_name) %>%
    filter(alpha2 %in% c("IT", "CH")) %>%
    mutate(Province.State = factor(Province.State) %>%
             forcats::fct_recode(
               `Valle d'Aosta` = "Aosta",
               `Puglia` = "Apulia",
               `Fruili Venezia Giulia` = "Friuli-Venezia Giulia",
               `Lombardia` = "Lombardy",
               `Piemonte` = "Piedmont",
               `Sardegna` = "Sardinia",
               `Sicilia` = "Sicily",
               `Trentino-Alto Adige` = "Trentino-South Tyrol",
               `Toscana` = "Tuscany"
             )
    ) %>%
    mutate(Province.State = as.character(Province.State)) %>%
    padr::fill_by_value(Province.State, value = "total") %>%
    dplyr::distinct(alpha2, Province.State, date, category, .keep_all = TRUE) %>%
    group_by(alpha2, Province.State, date) %>%
    mutate(category = factor(category) %>%
             forcats::fct_recode(
               `Grocery_Pharmacy` = "Grocery & pharmacy",
               `Retail_recreation` = "Retail & recreation",
               `Transit_stations` = "Transit stations"
             )
    ) %>%
    tidyr::spread(category, trend) %>%
    mutate(Workplace = coalesce(Workplace, Workplaces)) %>%
    select(-Workplaces)

  return(mobility_dat)
}



