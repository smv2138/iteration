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
    ## -2.64406 -0.66760 -0.03765 -0.06702  0.46081  2.57857

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
    ##  [1] 1.700510 3.359542 2.275139 3.021287 3.557160 3.531333 2.949647 3.558578
    ##  [9] 3.740836 2.223734 2.212533 2.331661 3.106944 2.620886 2.692532 2.865148
    ## [17] 4.248777 3.731544 5.276466 2.426688
    ## 
    ## $b
    ##  [1]  -4.87953111  -1.75582641  -6.02940613  -4.55077617   3.65817368
    ##  [6]  -6.91989513  -5.44326736  -2.51454605   7.21187239   8.20096670
    ## [11]   1.38344490   4.43558092  -1.36660274   0.42880531   6.29660287
    ## [16]   2.77555177  -1.41881545  -6.38083405   5.12411527   2.52766640
    ## [21]  -0.05058082   4.47409536   6.26974788  -4.32680036   2.81409038
    ## [26]   9.39988206   6.09896913   5.39258569   2.29139888 -12.29805375
    ## 
    ## $c
    ##  [1] 10.254284 10.108779 10.033775  9.926909  9.566504 10.021627 10.130674
    ##  [8]  9.836776 10.010858 10.198976  9.989466 10.093131  9.958775  9.972259
    ## [15] 10.393752 10.243744 10.203164 10.024243 10.031962 10.188860 10.103441
    ## [22]  9.658721 10.486362 10.310141 10.067104  9.754377 10.227136 10.131737
    ## [29]  9.890093 10.195138  9.921425 10.050005  9.875546 10.119357  9.897692
    ## [36] 10.102452 10.020888  9.971222  9.984131  9.921881
    ## 
    ## $d
    ##  [1] 0.9909404 3.4764242 3.5130413 2.5097815 3.8131371 4.0776352 2.4730722
    ##  [8] 3.4411029 2.7047855 2.6636942 3.0441653 3.0245107 3.6924265 1.9198349
    ## [15] 1.9859682 3.6864025 3.9225859 3.0751035 3.5294808 3.9717253

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
    ## 1  3.07 0.835

``` r
mean_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.695  5.35

``` r
mean_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.181

``` r
mean_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.08 0.807

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

``` r
output = map_dbl(list_norm, median, .id = "input")
```

Binds the output together to a dataframe

``` r
output = map_df(list_norm, mean_sd, .id = "input")
```
