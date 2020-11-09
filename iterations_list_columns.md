Iterations and list columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

## all plots i make will have the viridis color palette
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

can put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.65850 -0.86872 -0.05923 -0.04402  0.62198  3.06158

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = 3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 4.428769 3.155719 3.228899 2.961636 3.040018 1.280438 2.689274 2.629855
    ##  [9] 4.713209 2.999700 5.123575 2.773433 3.216035 2.765429 3.141898 3.184802
    ## [17] 4.367739 2.133411 3.162090 2.875214
    ## 
    ## $b
    ##  [1] -1.4726270 -4.0413307  0.6976195 -8.3152662 -1.2372617  5.6621213
    ##  [7] -3.3467157  5.0393771 -7.1140798  0.6968189  6.2223614 -1.6083993
    ## [13] -0.4829328 -4.0416310  7.6040817  2.2825609  5.8606437 -5.2743504
    ## [19]  0.4455230 -3.9395224 -3.9409939  1.2343666  2.8369500 -1.0267537
    ## [25] -0.2742807  3.5533233 -5.5126919 -5.0261945  5.2785304 -4.8032505
    ## 
    ## $c
    ##  [1] 10.416520  9.667612 10.094863 10.145754  9.831188  9.708311 10.013484
    ##  [8] 10.092273  9.870855  9.700742 10.294298  9.805440  9.992755 10.089711
    ## [15] 10.037340  9.743374 10.106777 10.059647 10.287158  9.868213 10.101198
    ## [22]  9.877775 10.009698 10.148187  9.517549 10.105183 10.361437  9.899520
    ## [29] 10.209890 10.319125 10.197713 10.326217  9.928090  9.851201 10.106494
    ## [36] 10.014886 10.178852 10.080248  9.989020  9.825755
    ## 
    ## $d
    ##  [1] 4.1031016 1.7389233 4.3308376 3.3305752 2.2740501 4.3997434 4.1404007
    ##  [8] 3.7228637 3.8404916 2.3266346 3.3447470 2.7141806 4.0971287 1.1010913
    ## [15] 2.2702674 0.8390875 4.3668520 2.6238448 1.0894909 1.9247403

Pause and get my old function

``` r
mean_sd  = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 observations")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply this function to each list element. We have to copy this 4
times.. we can use a for loop instead

``` r
mean_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.19 0.885

``` r
mean_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.468  4.34

``` r
mean_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.207

``` r
mean_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.93  1.19

Let’s use a for loop

``` r
output = vector("list", length = 4)


for (i in 1:4) {

  output[[i]] = mean_sd(list_norm[[i]])

  }
```

Let’s try map\!

``` r
output = map(list_norm, mean_sd)
```

What if you want a different function?

``` r
output = map(list_norm, IQR)
```
