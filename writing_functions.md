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

    ##  [1] -0.226079656  2.339375808  0.310507665 -1.094940013  0.007191083
    ##  [6] -0.348335146 -0.100312123 -0.322790463 -1.363724119  1.323670708
    ## [11]  0.648121443  0.146120289 -0.406483809  1.360493680 -1.014000858
    ## [16]  0.694000262 -1.245568044  0.927409943  1.393992246 -1.399151787
    ## [21] -0.198831238  0.694223436 -1.262598128  0.555066610 -0.271153388
    ## [26] -0.120118059  0.919051512 -1.767243076  0.793749290 -0.971644066

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

    ##  [1] -0.226079656  2.339375808  0.310507665 -1.094940013  0.007191083
    ##  [6] -0.348335146 -0.100312123 -0.322790463 -1.363724119  1.323670708
    ## [11]  0.648121443  0.146120289 -0.406483809  1.360493680 -1.014000858
    ## [16]  0.694000262 -1.245568044  0.927409943  1.393992246 -1.399151787
    ## [21] -0.198831238  0.694223436 -1.262598128  0.555066610 -0.271153388
    ## [26] -0.120118059  0.919051512 -1.767243076  0.793749290 -0.971644066

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
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 -0.0104  1.01

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
    ## 1  3.59  2.98

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
    ## 1  5.88  2.93

``` r
# also can do
sim_mean_sd(samp_size  = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.14  2.91

## Reviewing Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page1 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews… this is tedious to rerun this code

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews_page2 = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Let’s turn the code into a function… only thing that changes is the page
number

``` r
read_page_reviews = function(url) {


  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)

reviews

}
```

Let’s try it out

Can change the url to whatever page you want

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "Boo"                                   1 "We rented this movie because our ~
    ##  2 "Movie is still silly fun....amazo~     1 "We are getting really frustrated ~
    ##  3 "Brilliant and awkwardly funny."        5 "I've watched this movie repeatedl~
    ##  4 "Great purchase price for great mo~     5 "Great movie and real good digital~
    ##  5 "Movie for memories"                    5 "I've been looking for this movie ~
    ##  6 "Love!"                                 5 "Love this movie. Great quality"   
    ##  7 "Hilarious!"                            5 "Such a funny movie, definitely br~
    ##  8 "napoleon dynamite"                     5 "cool movie"                       
    ##  9 "Top 5"                                 5 "Best MOVIE ever! Funny one liners~
    ## 10 "\U0001f44d"                            5 "Exactly as described and came on ~

Let’s read a few pages of reviews

URL base contains url but no page number

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

# call whatever page you want

all_reiews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
)
```

## Mean scoping example

``` r
f = function(x) {
  
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguements

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 2.516644

``` r
median(x_vec)
```

    ## [1] 1.485152

``` r
my_summary(x_vec, IQR)
```

    ## [1] 9.904804
