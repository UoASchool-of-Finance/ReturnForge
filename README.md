
# ReturnForge

<!-- badges: start -->
<!-- badges: end -->

The goal of ReturnForge is to streamline return data generation and exploratory data analysis workflow generally seen throughout finance courses 
provided by the [University of Alberta](https://www.ualberta.ca/en/finance-department/index.html)

## Installation

You can install the development version of ReturnForge from [GitHub](https://github.com/ReturnForge) with:

``` r
# install.packages("pak")
pak::pak("UoASchool-of-Finance/ReturnForge")
```

## Example

This is a basic example which shows you how to generate a tibble object using yahoo finance tickers:

``` r
library(ReturnForge)

tickers = c('CCOM.TO', '^SPGSCI')

long_tibble = genrets(tickers, '2020-01-01', 'monthly', ret_type = 'rel')
```

