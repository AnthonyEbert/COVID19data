
library(dplyr)
library(tidyr)
library(COVID19data)

covid19_sorted <- all_countries()

readr::write_csv(covid19_sorted, "data-raw/covid19_sorted.csv")

usethis::use_data(covid19_sorted, overwrite = TRUE)

