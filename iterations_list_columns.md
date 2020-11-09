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
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.7135 -0.5287 -0.1429 -0.1183  0.2886  2.2388

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
    ##  [1] 3.097577 3.496639 2.812321 1.838583 2.895260 5.162517 2.532057 3.586887
    ##  [9] 3.407903 5.261517 3.252752 3.856602 3.207468 4.621564 4.517969 3.574021
    ## [17] 1.617454 1.974143 3.001598 3.738766
    ## 
    ## $b
    ##  [1]  5.3681570 -4.9910176 -2.2178314  0.3553764  3.8065150  3.8617556
    ##  [7] -5.6035953 -0.8655908 -5.3788536 -1.7559873 -2.5617104  2.1857005
    ## [13] -0.0144866 -4.5108634 -0.8319983  1.6483988 -0.2376683  6.7493516
    ## [19]  0.1744444  0.7914748 -0.8801171 -2.2200094  1.7372633  0.4482880
    ## [25]  6.2450067 -1.7703900 -2.7715394  3.1328578 -1.0206753  7.0447456
    ## 
    ## $c
    ##  [1]  9.676408 10.209456 10.189562 10.002393 10.118889 10.115346 10.099015
    ##  [8] 10.144180  9.982076  9.574319  9.892347 10.048404 10.123869  9.875316
    ## [15] 10.115332  9.925479  9.702037  9.957668  9.956849  9.992295  9.747084
    ## [22] 10.079867  9.936347  9.739458  9.708744  9.974981  9.775786  9.924254
    ## [29]  9.605981 10.034570 10.166545 10.546688  9.887055  9.959877 10.182371
    ## [36] 10.034402  9.780671  9.950415 10.061837  9.902839
    ## 
    ## $d
    ##  [1] 1.848338 2.557124 2.997182 4.278824 1.576881 2.553722 4.074770 2.951321
    ##  [9] 3.292563 3.027294 2.638914 2.526752 3.054601 1.426748 1.684174 2.995161
    ## [17] 3.903246 3.626641 3.352008 3.392296

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
    ## 1  3.37  1.00

``` r
mean_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.197  3.48

``` r
mean_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.191

``` r
mean_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89 0.809

Let’s use a for loop

``` r
output = vector("list", length = 4)


for (i in 1:4) {

  output[[i]] = mean_sd(list_norm[[i]])

  }
```
