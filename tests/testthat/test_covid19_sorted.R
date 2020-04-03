library(dplyr)
library(COVID19data)

test_that("covid19_sorted", {
  output <- covid19_sorted %>%
    group_by(Country.Region, Province.State, date) %>%
    summarise(number = n())

  testthat::expect(all(output$number == 1), "duplicates found")
})

test_that("covid19_equal", {
  all_countries <- readr::read_csv("../../data-raw/covid19_sorted.csv", col_types = "ccDdddddddiiddc")
  #print(sapply(all_countries, class))

  #testthat::expect_equal(sapply(all_countries, class)[7],sapply(COVID19data::covid19_sorted, class)[7])
  testthat::expect_equivalent(all_countries, COVID19data::covid19_sorted  %>% mutate(alpha3 = as.character(alpha3) ))
})
