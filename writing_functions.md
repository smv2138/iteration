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

    ##  [1] -0.8324535 -0.5714376  0.8519792 -0.2374206  0.3912988  1.3174699
    ##  [7] -2.3592318 -0.4835181  0.4442506 -0.1155117  0.5792563 -1.8614709
    ## [13] -0.7313013  0.1443873 -1.1294085  2.2538969  1.0219649  1.0506768
    ## [19]  0.3035235  0.5382738  0.1845623  1.1985581  0.6922977 -0.7515618
    ## [25] -0.5962653  0.8205249 -1.0078049 -0.6806563 -0.9029383  0.4680594

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

    ##  [1] -0.8324535 -0.5714376  0.8519792 -0.2374206  0.3912988  1.3174699
    ##  [7] -2.3592318 -0.4835181  0.4442506 -0.1155117  0.5792563 -1.8614709
    ## [13] -0.7313013  0.1443873 -1.1294085  2.2538969  1.0219649  1.0506768
    ## [19]  0.3035235  0.5382738  0.1845623  1.1985581  0.6922977 -0.7515618
    ## [25] -0.5962653  0.8205249 -1.0078049 -0.6806563 -0.9029383  0.4680594

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
