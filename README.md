activerse
================

`activerse` is a lightweight helper package that attaches
[`actibase`](https://github.com/jhuwit/actibase),
[`actiread`](https://github.com/jhuwit/actiread), and
[`actimetrics`](https://github.com/jhuwit/actimetrics) together.

It is designed to behave a bit like `tidyverse`: load one package, then
use the functions from the actigraphy packages directly.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("jhuwit/activerse")
```

## Packages

| Package | Repository | R CMD check |
|----|----|----|
| `actibase` | [jhuwit/actibase](https://github.com/jhuwit/actibase) | [![R CMD check](https://github.com/jhuwit/actibase/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/actibase/actions/workflows/R-CMD-check.yaml) |
| `actiread` | [jhuwit/actiread](https://github.com/jhuwit/actiread) | [![R CMD check](https://github.com/jhuwit/actiread/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/actiread/actions/workflows/R-CMD-check.yaml) |
| `actimetrics` | [jhuwit/actimetrics](https://github.com/jhuwit/actimetrics) | [![R CMD check](https://github.com/jhuwit/actimetrics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/actimetrics/actions/workflows/R-CMD-check.yaml) |

## Load the packages

``` r
library(activerse)

search()[grepl("^package:(actibase|actiread|actimetrics)$", search())]
#> [1] "package:actimetrics" "package:actiread"    "package:actibase"
```

## Example data

The packages ship with small example datasets that are useful for quick
tests.

``` r
data(acti_raw_data, package = "actibase")
data(acti_count_data, package = "actimetrics")

head(acti_raw_data)
#>                  time     X      Y     Z
#> 1 2019-09-17 18:40:00 0.000  0.008 0.996
#> 2 2019-09-17 18:40:00 0.016  0.000 1.008
#> 3 2019-09-17 18:40:00 0.020 -0.008 1.004
#> 4 2019-09-17 18:40:00 0.016 -0.012 1.012
#> 5 2019-09-17 18:40:00 0.016 -0.008 1.008
#> 6 2019-09-17 18:40:00 0.008 -0.008 1.008
head(acti_count_data)
#>                  time axis1 axis2 axis3 counts
#> 1 2019-09-17 18:40:00  5435  9659  8253  13818
#> 2 2019-09-17 18:41:00  9125  9197  4131  13598
#> 3 2019-09-17 18:42:00  4404  4367  3494   7119
#> 4 2019-09-17 18:43:00  3267  3170  2543   5214
#> 5 2019-09-17 18:44:00  1405   896   894   1891
#> 6 2019-09-17 18:45:00     0     0     0      0
```

## Use `actimetrics`

Once `activerse` is attached, `actimetrics` functions are available
directly. You can also call them with the `actimetrics::` prefix if you
prefer being explicit.

### Calculate ENMO

``` r
enmo <- acti_calculate_enmo(data = acti_raw_data)
head(enmo)
#> # A tibble: 6 × 2
#>   HEADER_TIME_STAMP   ENMO_t
#>   <dttm>               <dbl>
#> 1 2019-09-17 18:40:00 0.688 
#> 2 2019-09-17 18:41:00 0.708 
#> 3 2019-09-17 18:42:00 0.183 
#> 4 2019-09-17 18:43:00 0.150 
#> 5 2019-09-17 18:44:00 0.0278
#> 6 2019-09-17 18:45:00 0.0139
```

### Calculate activity index

``` r
activity_index <- acti_calculate_activity_index(
  acti_raw_data,
  unit = "1 min",
  ensure_all_time = TRUE,
  verbose = FALSE
)

head(activity_index)
#> # A tibble: 6 × 2
#>   HEADER_TIME_STAMP      AI
#>   <dttm>              <dbl>
#> 1 2019-09-17 18:40:00 23.2 
#> 2 2019-09-17 18:41:00 22.9 
#> 3 2019-09-17 18:42:00 11.9 
#> 4 2019-09-17 18:43:00 10.5 
#> 5 2019-09-17 18:44:00  2.35
#> 6 2019-09-17 18:45:00  0
```

### Calculate MAD

``` r
mad <- acti_calculate_mad(data = acti_raw_data)
head(mad)
#> # A tibble: 6 × 8
#>   HEADER_TIME_STAMP       SD   SD_t AI_DEFINED    MAD   MEDAD mean_r ENMO_t
#>   <dttm>               <dbl>  <dbl>      <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 2019-09-17 18:40:00 1.87   1.85        1.33  1.09   0.630     1.65 0.688 
#> 2 2019-09-17 18:41:00 1.54   1.53        1.08  0.853  0.552     1.69 0.708 
#> 3 2019-09-17 18:42:00 0.253  0.204       0.278 0.207  0.188     1.14 0.183 
#> 4 2019-09-17 18:43:00 0.228  0.175       0.228 0.191  0.178     1.10 0.150 
#> 5 2019-09-17 18:44:00 0.0815 0.0637      0.309 0.0300 0.00330   1.02 0.0278
#> 6 2019-09-17 18:45:00 0      0           0     0      0         1.01 0.0139
```

## More examples

`actimetrics` also includes functions for wear/non-wear detection, step
count estimation, and other processing helpers:

``` r
acti_process(acti_raw_data)
acti_calculate_wear(acti_raw_data)
acti_calculate_nonwear(acti_raw_data)
acti_calculate_stepcount(acti_raw_data)
```
