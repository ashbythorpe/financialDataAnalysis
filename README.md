
<!-- README.md is generated from README.Rmd. Please edit that file -->

# financialDataAnalysis

<!-- badges: start -->

[![R-CMD-check](https://github.com/ashbythorpe/financialDataAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbythorpe/financialDataAnalysis/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ashbythorpe/financialDataAnalysis/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ashbythorpe/financialDataAnalysis?branch=master)
<!-- badges: end -->

financialDataAnalysis is a website which allows users to upload, score
and plot data (financial or otherwise). It provides a rich default
dataset describing nearly 500 stocks and over 100 features. It’s
forecasting section uses an advanced machine learning algorithm trained
on daily and monthly data to forecast the price of a stock over time.

## Installation

You can install the development version of financialDataAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ashbythorpe/financialDataAnalysis")
```

The website was built using [shiny](https://shiny.rstudio.com/). Run it
by executing the following:

``` r
financialDataAnalysis()
```
