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
    ## -2.2729 -0.8922 -0.2504 -0.1432  0.6404  2.1171

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
    ##  [1] 2.590786 3.324082 2.713936 2.862392 3.421584 2.456650 2.713811 2.169681
    ##  [9] 2.852694 2.466912 3.046958 3.183763 2.997993 2.914643 2.398380 3.749363
    ## [17] 0.793892 2.770456 2.748334 2.127804
    ## 
    ## $b
    ##  [1]  -9.0996851  -2.1562906  -4.8089709  -3.7614966   6.7964284   2.5905280
    ##  [7]   0.7489083   0.3738398  -2.7079029  -1.4729288  -5.1576334  -4.7999512
    ## [13]   1.7952507   4.6574365   2.4696821  -1.1349208   2.4238422  -2.3049340
    ## [19]  -6.2874636  -1.3487906  -1.2500182 -10.3098789  -3.3871799   1.2607648
    ## [25]  -3.9829155  -4.2343115  -0.4774960  10.5511858  11.1615492  -1.2181096
    ## 
    ## $c
    ##  [1]  9.963688  9.859700 10.267612  9.902223  9.945724  9.719767 10.291288
    ##  [8] 10.050172  9.589182 10.153781  9.968145 10.058632  9.997553 10.377678
    ## [15]  9.866564  9.828628 10.160372  9.838205  9.840484 10.000473  9.701385
    ## [22] 10.079170 10.164560 10.261779  9.855439  9.984845  9.905103 10.155948
    ## [29]  9.685440  9.886226  9.790587  9.761764  9.995340 10.183200 10.309225
    ## [36] 10.011684 10.560908 10.200263  9.913451  9.651357
    ## 
    ## $d
    ##  [1] 3.051515 2.910099 2.748118 2.234104 2.667123 3.898593 2.100712 2.770190
    ##  [9] 5.155636 5.268646 1.813668 3.400298 5.253523 2.268308 3.007917 3.327297
    ## [17] 3.819217 3.294411 2.344613 2.296144

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
    ## 1  2.72 0.607

``` r
mean_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.836  4.89

``` r
mean_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.215

``` r
mean_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.18  1.04

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
    ##  [1] 2.590786 3.324082 2.713936 2.862392 3.421584 2.456650 2.713811 2.169681
    ##  [9] 2.852694 2.466912 3.046958 3.183763 2.997993 2.914643 2.398380 3.749363
    ## [17] 0.793892 2.770456 2.748334 2.127804
    ## 
    ## $b
    ##  [1]  -9.0996851  -2.1562906  -4.8089709  -3.7614966   6.7964284   2.5905280
    ##  [7]   0.7489083   0.3738398  -2.7079029  -1.4729288  -5.1576334  -4.7999512
    ## [13]   1.7952507   4.6574365   2.4696821  -1.1349208   2.4238422  -2.3049340
    ## [19]  -6.2874636  -1.3487906  -1.2500182 -10.3098789  -3.3871799   1.2607648
    ## [25]  -3.9829155  -4.2343115  -0.4774960  10.5511858  11.1615492  -1.2181096
    ## 
    ## $c
    ##  [1]  9.963688  9.859700 10.267612  9.902223  9.945724  9.719767 10.291288
    ##  [8] 10.050172  9.589182 10.153781  9.968145 10.058632  9.997553 10.377678
    ## [15]  9.866564  9.828628 10.160372  9.838205  9.840484 10.000473  9.701385
    ## [22] 10.079170 10.164560 10.261779  9.855439  9.984845  9.905103 10.155948
    ## [29]  9.685440  9.886226  9.790587  9.761764  9.995340 10.183200 10.309225
    ## [36] 10.011684 10.560908 10.200263  9.913451  9.651357
    ## 
    ## $d
    ##  [1] 3.051515 2.910099 2.748118 2.234104 2.667123 3.898593 2.100712 2.770190
    ##  [9] 5.155636 5.268646 1.813668 3.400298 5.253523 2.268308 3.007917 3.327297
    ## [17] 3.819217 3.294411 2.344613 2.296144

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
    ## 1  2.72 0.607

Can I map?

``` r
#samp is the list column
map(listcol_df$samp, mean_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.72 0.607
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.836  4.89
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.215
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.18  1.04

can I add a list column (add output to the list) In 1 dataframe, we have
both input and output information

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_sd),
    medians = map_dbl(samp, median))
```
