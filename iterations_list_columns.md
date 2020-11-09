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
    ## -2.47386 -0.91978 -0.02963 -0.09218  0.58738  2.94704

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
    ##  [1]  3.0061078  2.7149837  2.5405297  1.2996312  3.9377089  4.9119402
    ##  [7]  3.6511384  2.2814937  1.6172226  2.2573640  4.9128472  2.2543968
    ## [13] -0.8717012  3.1531259  3.4829077  3.0279163  1.2174642  2.1927456
    ## [19]  1.5003889  2.7756500
    ## 
    ## $b
    ##  [1]   2.1271437   4.0472882  -6.0699922   1.8134086  -1.4135022  -0.6800141
    ##  [7]   3.3637789  -2.9186351   3.4939685  -0.5786488   7.0182687   6.8418759
    ## [13]   3.1293756  -3.4900105  -1.2562490 -14.1933232  -6.8332999   1.7904064
    ## [19] -12.7274313  -2.6691277   0.3709216   2.4366636   1.3108672   5.9583954
    ## [25]   0.1435761   7.3286299  -2.1435945   2.6742809  -0.7197585   3.2492652
    ## 
    ## $c
    ##  [1]  9.792113  9.957911 10.016773 10.227646  9.779966  9.982156 10.246664
    ##  [8]  9.923840 10.106732  9.639504  9.848779 10.167579  9.943154  9.785100
    ## [15] 10.039024 10.030357 10.059642  9.961325  9.696145  9.617196  9.836683
    ## [22] 10.161797 10.091793 10.029378  9.847596  9.982036 10.008686  9.980323
    ## [29] 10.061571  9.780992 10.303851  9.793221  9.599751  9.792942 10.121214
    ## [36]  9.886789 10.007715  9.992479 10.093648 10.079843
    ## 
    ## $d
    ##  [1] 0.963075 3.384811 3.383266 1.191451 3.925544 1.771319 2.802030 3.120219
    ##  [9] 2.649176 3.698264 2.324615 2.589872 2.504926 4.367634 2.319103 1.957757
    ## [17] 1.604083 3.217067 3.686471 2.571055

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
    ## 1  2.59  1.33

``` r
mean_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0468  5.10

``` r
mean_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.171

``` r
mean_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70 0.915

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

## List columns\!\!

keep track of inputs and ouputs at the same time

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1]  3.0061078  2.7149837  2.5405297  1.2996312  3.9377089  4.9119402
    ##  [7]  3.6511384  2.2814937  1.6172226  2.2573640  4.9128472  2.2543968
    ## [13] -0.8717012  3.1531259  3.4829077  3.0279163  1.2174642  2.1927456
    ## [19]  1.5003889  2.7756500
    ## 
    ## $b
    ##  [1]   2.1271437   4.0472882  -6.0699922   1.8134086  -1.4135022  -0.6800141
    ##  [7]   3.3637789  -2.9186351   3.4939685  -0.5786488   7.0182687   6.8418759
    ## [13]   3.1293756  -3.4900105  -1.2562490 -14.1933232  -6.8332999   1.7904064
    ## [19] -12.7274313  -2.6691277   0.3709216   2.4366636   1.3108672   5.9583954
    ## [25]   0.1435761   7.3286299  -2.1435945   2.6742809  -0.7197585   3.2492652
    ## 
    ## $c
    ##  [1]  9.792113  9.957911 10.016773 10.227646  9.779966  9.982156 10.246664
    ##  [8]  9.923840 10.106732  9.639504  9.848779 10.167579  9.943154  9.785100
    ## [15] 10.039024 10.030357 10.059642  9.961325  9.696145  9.617196  9.836683
    ## [22] 10.161797 10.091793 10.029378  9.847596  9.982036 10.008686  9.980323
    ## [29] 10.061571  9.780992 10.303851  9.793221  9.599751  9.792942 10.121214
    ## [36]  9.886789 10.007715  9.992479 10.093648 10.079843
    ## 
    ## $d
    ##  [1] 0.963075 3.384811 3.383266 1.191451 3.925544 1.771319 2.802030 3.120219
    ##  [9] 2.649176 3.698264 2.324615 2.589872 2.504926 4.367634 2.319103 1.957757
    ## [17] 1.604083 3.217067 3.686471 2.571055

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try other operations

``` r
# apply mean and sd function to the list
mean_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.59  1.33

Can I map?

``` r
#samp is the list column
map(listcol_df$samp, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.59  1.33
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0468  5.10
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.96 0.171
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70 0.915

can I add a list column (add output to the list) In 1 dataframe, we have
both input and output information

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_sd),
    medians = map_dbl(samp, median))
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\sushu\AppData\Local\Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-05 10:36:30 (7.537)

    ## file min/max dates: 1869-01-01 / 2020-10-31

    ## using cached file: C:\Users\sushu\AppData\Local\Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-05 10:36:48 (1.703)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: C:\Users\sushu\AppData\Local\Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-05 10:37:01 (0.882)

    ## file min/max dates: 1999-09-01 / 2020-10-31

Get our list columns.. Want separate dataframes for NYC, Waikiki and
Waterhole

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # ... with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # ... with 355 more rows

``` r
#just to get NYC tibble (2 = Waikiki, 3 = Waterhole)
weather_nest$data[[1]]
```

    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows

Suppose I want to regress `tmax` on `tmin` for each station

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Create a function that can spit out these regression results

``` r
weather_lm = function(df) {
 
   lm(tmax ~tmin, data = df)
  
}

    output = vector("list", 3)
    
    for (i in 1:3) {
      
      output[[i]] = weather_lm(weather_nest$data[[i]])
      
  }
```

What about a map?

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column??????

``` r
weather_nest = 
weather_nest %>% 
  mutate(models = map(data, weather_lm))
```
