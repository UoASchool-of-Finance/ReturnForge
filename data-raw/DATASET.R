## code to prepare `DATASET` dataset goes here
# usethis::use_package('dplyr')
# usethis::use_package('tidyr')
# usethis::use_package('tidyquant')
# usethis::use_package('zoo')
# usethis::use_package('stats')
# usethis::use_pipe()
# usethis::use_package('lubridate')
library(tidyverse)

stocks <- tidyquant::tq_get(
  x = c('SPY', 'QQQ'),
  from = '2015-01-01',
  get = 'stock.prices') %>%
  dplyr::select(date, series = symbol, value = adjusted)

usethis::use_data(stocks, overwrite = T)
