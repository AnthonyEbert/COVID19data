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

test_that("covid19_france", {
  testdata <- covid19_sorted %>%
    group_by(Country.Region, Province.State) %>%
    summarise_if(is.numeric, max)

  testdata <- testdata %>%
    filter(Country.Region %in% c("France", "Germany", "Italy", "Switzerland", "China"), Province.State == "total")

  testnumber <- testdata %>%
    pull(confirmed) %>%
    as.numeric()

  testthat::expect(all(testnumber > 20000), "France cases too small")

  Germany_max = johns_hopkins_data() %>%
    filter(Country.Region == "Germany", Province.State == "") %>%
    summarise(confirmed = max(confirmed)) %>%
    pull(confirmed)

  testthat::expect(testdata %>% filter(Country.Region == "Germany") %>% pull(confirmed) == Germany_max, "Germany max")
})

test_that("covid19_properties", {
  numeric_data <- covid19_sorted %>% ungroup() %>% select_if(is.numeric) %>% as.matrix() %>% as.numeric()

  testthat::expect(all(numeric_data %% 1 == 0, na.rm = TRUE), "Non-integer data")
  testthat::expect(all(numeric_data >= 0, na.rm = TRUE), "Negative data")
})

test_that("covid19_levels", {
  levels <- covid19_sorted %>% pull(alpha3) %>% levels() %>% length()

  testthat::expect(levels >= 240, "Countries missing or removed")
})


