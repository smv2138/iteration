Writing Functions
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
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

## Do something simple

``` r
# creating a vector  
x_vec = rnorm(30, mean = 5, sd = 3)

# creating z-score
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.23683710  0.55423123  0.22883633 -0.32088842 -0.13736871  0.38512328
    ##  [7]  1.30581679  2.63106343  0.08887966 -0.04321577 -1.44785500  0.35081716
    ## [13] -0.86319544  0.13200029 -1.72853255  0.22350550  0.76778525 -2.14563045
    ## [19]  0.34526472  0.09915951 -0.12392688  1.10910072 -1.92447782 -0.14235275
    ## [25] -0.66058653  0.05074833  0.51308220 -0.95056960  0.53093057 -0.06458213

I want a function that will compute z-scores We named input “x” and
return “z”

``` r
z_scores  = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

## testing function out

z_scores(x_vec)
```

    ##  [1]  1.23683710  0.55423123  0.22883633 -0.32088842 -0.13736871  0.38512328
    ##  [7]  1.30581679  2.63106343  0.08887966 -0.04321577 -1.44785500  0.35081716
    ## [13] -0.86319544  0.13200029 -1.72853255  0.22350550  0.76778525 -2.14563045
    ## [19]  0.34526472  0.09915951 -0.12392688  1.10910072 -1.92447782 -0.14235275
    ## [25] -0.66058653  0.05074833  0.51308220 -0.95056960  0.53093057 -0.06458213

Testing function on other things

``` r
# can't take mean of character
z_scores("my name is jeff")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
# dataset in r (can't take mean of a dataset)
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
# this works because r converts these into numeric (1 and 0)
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

Update the function so that when people try to use it on something
weird, it doesn’t work (conditional execution) if x is not numeric then
stop with error statement

``` r
z_scores  = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

``` r
# can't take mean of character
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
# Input must be numeric
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
# Input must be numeric
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

Keep updating

``` r
z_scores  = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 observations")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

## Multiple outputs

We want function to spit out mean and SD at the same time

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

Check that function works

``` r
x_vec = rnorm(1000)

mean_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0529 0.981

## Multiple Inputs

GIve me an input that spits out the certain output Create a simulated
dataset

I’d like to do the below with a function instead

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )


sim_data %>% 
  summarize(
    mean  = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  2.97

trying to create a function input is the 3 parameters in the tibble
above

``` r
sim_mean_sd = function(samp_size, mu, sigma) {
  
  sim_data = 
  tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )


sim_data %>% 
  summarize(
    mean  = mean(x),
    sd = sd(x)
  )
}


sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.67  2.87

``` r
# also can do
sim_mean_sd(samp_size  = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.32  2.64
