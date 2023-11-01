STAT 545B Assignment B1 Deliverable
================

# Exercise 1 & 2: Make and Document a Function

One example of a versatile series of functions is to `group_by()` one or
more variables, then calculate the proportion and count of a specific
observation within these groups.

## 1.0 Examples of Grouping & Counting Conditional Observations

``` r
#Loading the necessary packages
library(tidyverse) #includes dplyr package for data wrangling, used in designed function
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(palmerpenguins) #contains sample data set to test function
library(gapminder) #contains sample data set to test function
library(testthat) #includes multiple formal tests to evaluate function behaviors
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

### 1.0.1 `penguins` Data Set

``` r
#Previewing `penguins` data set 
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

``` r
#Summarizing the variables of `penguins` data set 
summary(penguins)
```

    ##       species          island    bill_length_mm  bill_depth_mm  
    ##  Adelie   :152   Biscoe   :168   Min.   :32.10   Min.   :13.10  
    ##  Chinstrap: 68   Dream    :124   1st Qu.:39.23   1st Qu.:15.60  
    ##  Gentoo   :124   Torgersen: 52   Median :44.45   Median :17.30  
    ##                                  Mean   :43.92   Mean   :17.15  
    ##                                  3rd Qu.:48.50   3rd Qu.:18.70  
    ##                                  Max.   :59.60   Max.   :21.50  
    ##                                  NA's   :2       NA's   :2      
    ##  flipper_length_mm  body_mass_g       sex           year     
    ##  Min.   :172.0     Min.   :2700   female:165   Min.   :2007  
    ##  1st Qu.:190.0     1st Qu.:3550   male  :168   1st Qu.:2007  
    ##  Median :197.0     Median :4050   NA's  : 11   Median :2008  
    ##  Mean   :200.9     Mean   :4202                Mean   :2008  
    ##  3rd Qu.:213.0     3rd Qu.:4750                3rd Qu.:2009  
    ##  Max.   :231.0     Max.   :6300                Max.   :2009  
    ##  NA's   :2         NA's   :2

The following code groups the above data by `species` and `island`, then
counts the number and proportion of observations where
`bill_length_mm > 40`.

``` r
penguins %>% group_by(species, island) %>%
  summarize(n = n(),
            count = length(which(bill_length_mm > 40)),
            prop = (length(which(bill_length_mm > 40))/n())*100)
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   species [3]
    ##   species   island        n count  prop
    ##   <fct>     <fct>     <int> <int> <dbl>
    ## 1 Adelie    Biscoe       44    16  36.4
    ## 2 Adelie    Dream        56    17  30.4
    ## 3 Adelie    Torgersen    52    18  34.6
    ## 4 Chinstrap Dream        68    68 100  
    ## 5 Gentoo    Biscoe      124   123  99.2

### 1.0.2 `gapminder` Data Set

``` r
#Previewing `gapminder` data set 
head(gapminder)
```

    ## # A tibble: 6 × 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

``` r
#Summarizing the variables of `gapminder` data set 
summary(gapminder)
```

    ##         country        continent        year         lifeExp     
    ##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
    ##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
    ##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
    ##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
    ##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
    ##  Australia  :  12                  Max.   :2007   Max.   :82.60  
    ##  (Other)    :1632                                                
    ##       pop              gdpPercap       
    ##  Min.   :6.001e+04   Min.   :   241.2  
    ##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
    ##  Median :7.024e+06   Median :  3531.8  
    ##  Mean   :2.960e+07   Mean   :  7215.3  
    ##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
    ##  Max.   :1.319e+09   Max.   :113523.1  
    ## 

The following code groups the above data by `year`, then counts the
number and proportion of observations where `lifeExp > 60`.

``` r
gapminder %>% group_by(year) %>%
  summarize(n = n(),
            count = length(which(lifeExp > 60)),
            prop = (length(which(lifeExp > 60))/n())*100)
```

    ## # A tibble: 12 × 4
    ##     year     n count  prop
    ##    <int> <int> <int> <dbl>
    ##  1  1952   142    35  24.6
    ##  2  1957   142    45  31.7
    ##  3  1962   142    52  36.6
    ##  4  1967   142    55  38.7
    ##  5  1972   142    62  43.7
    ##  6  1977   142    70  49.3
    ##  7  1982   142    81  57.0
    ##  8  1987   142    92  64.8
    ##  9  1992   142    94  66.2
    ## 10  1997   142    96  67.6
    ## 11  2002   142    96  67.6
    ## 12  2007   142    99  69.7

## 2.0 Designing a Generic Function

The following code creates a function called `group_count_and_prop` that
uses three inputs: `.data`, `...` and `cond` to call (1) a data frame,
(2) variables to group by and (3) the condition to evaluate,
respectively. The output is a new data frame summarizing the number and
proportion of observations in the input data frame that match the input
condition within each given group.

``` r
#' @title Count and calculate proportions in group(s) 
#' 
#' @description Summarizes the number and proportion of observations that meet one or more conditions within given group(s).
#' 
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... Variables to group by.
#' @param cond Short for "condition". Expressions that return a logical value. Expressions are defined in terms of variables in .data and multiple expressions, if included, must be combined with the & operator. Only rows for which all conditions return TRUE are evaluated.  
#' 
#' @return A new data frame summarizing the groups, total number of observations in each group, count of observations that return TRUE for the condition(s) and proportion (%) of observations that return TRUE for the condition(s)
#' @export
#' 
#' @examples
#' group_count_and_prop(starwars, gender, cond = height > 150 & mass > 50)
#' group_count_and_prop(mtcars, cyl, cond = mpg > 20)

group_count_and_prop <- function(.data, ..., cond) {
  .data %>% group_by(...) %>%
    summarize(n = n(),
              count = length(which({{ cond }})),
              prop = (length(which({{ cond }}))/n())*100)
}
```

# Exercise 3: Include Examples

## 3.0 Examples

### 3.0.1 `penguins` Data Set

Recall the `penguins` data set introduced in **Section 1.0**.

``` r
#Summarizing the variables of `penguins` data set
summary(penguins)
```

    ##       species          island    bill_length_mm  bill_depth_mm  
    ##  Adelie   :152   Biscoe   :168   Min.   :32.10   Min.   :13.10  
    ##  Chinstrap: 68   Dream    :124   1st Qu.:39.23   1st Qu.:15.60  
    ##  Gentoo   :124   Torgersen: 52   Median :44.45   Median :17.30  
    ##                                  Mean   :43.92   Mean   :17.15  
    ##                                  3rd Qu.:48.50   3rd Qu.:18.70  
    ##                                  Max.   :59.60   Max.   :21.50  
    ##                                  NA's   :2       NA's   :2      
    ##  flipper_length_mm  body_mass_g       sex           year     
    ##  Min.   :172.0     Min.   :2700   female:165   Min.   :2007  
    ##  1st Qu.:190.0     1st Qu.:3550   male  :168   1st Qu.:2007  
    ##  Median :197.0     Median :4050   NA's  : 11   Median :2008  
    ##  Mean   :200.9     Mean   :4202                Mean   :2008  
    ##  3rd Qu.:213.0     3rd Qu.:4750                3rd Qu.:2009  
    ##  Max.   :231.0     Max.   :6300                Max.   :2009  
    ##  NA's   :2         NA's   :2

Here is an example of employing the `group_count_and_prop` function to
group the `penguins` data set by `species` and `island`, then counting
the number and proportion of observations where `bill_length_mm > 40`.
The resulting tibble matches the output generated in **Section 1.0**.

``` r
group_count_and_prop(penguins,
                     species, island,
                     cond = bill_length_mm > 40)
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   species [3]
    ##   species   island        n count  prop
    ##   <fct>     <fct>     <int> <int> <dbl>
    ## 1 Adelie    Biscoe       44    16  36.4
    ## 2 Adelie    Dream        56    17  30.4
    ## 3 Adelie    Torgersen    52    18  34.6
    ## 4 Chinstrap Dream        68    68 100  
    ## 5 Gentoo    Biscoe      124   123  99.2

Here is an an example of employing the `group_count_and_prop` function
to group the `penguins` data set by `species`, then counting the number
and proportion of observations where `body_mass_g > 4000` and
`sex == "female"`.

``` r
group_count_and_prop(penguins,
                     species,
                     cond = body_mass_g > 4000 & sex == "female")
```

    ## # A tibble: 3 × 4
    ##   species       n count  prop
    ##   <fct>     <int> <int> <dbl>
    ## 1 Adelie      152     0  0   
    ## 2 Chinstrap    68     1  1.47
    ## 3 Gentoo      124    57 46.0

### 3.0.2 `gapminder` Data Set

Recall the `gapminder` data set introduced in **Section 1.0**.

``` r
#Summarizing the variables of `gapminder` data set
summary(gapminder)
```

    ##         country        continent        year         lifeExp     
    ##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
    ##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
    ##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
    ##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
    ##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
    ##  Australia  :  12                  Max.   :2007   Max.   :82.60  
    ##  (Other)    :1632                                                
    ##       pop              gdpPercap       
    ##  Min.   :6.001e+04   Min.   :   241.2  
    ##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
    ##  Median :7.024e+06   Median :  3531.8  
    ##  Mean   :2.960e+07   Mean   :  7215.3  
    ##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
    ##  Max.   :1.319e+09   Max.   :113523.1  
    ## 

Here is an example of employing the `group_count_and_prop` function to
group the `gapminder` data set by `year`, then counting the number and
proportion of observations where `lifeExp > 60`. The resulting tibble
matches the output generated in **Section 1.0**.

``` r
group_count_and_prop(gapminder, 
                     year,
                     cond = lifeExp > 60)
```

    ## # A tibble: 12 × 4
    ##     year     n count  prop
    ##    <int> <int> <int> <dbl>
    ##  1  1952   142    35  24.6
    ##  2  1957   142    45  31.7
    ##  3  1962   142    52  36.6
    ##  4  1967   142    55  38.7
    ##  5  1972   142    62  43.7
    ##  6  1977   142    70  49.3
    ##  7  1982   142    81  57.0
    ##  8  1987   142    92  64.8
    ##  9  1992   142    94  66.2
    ## 10  1997   142    96  67.6
    ## 11  2002   142    96  67.6
    ## 12  2007   142    99  69.7

# Exercise 4: Test the Function

The following section formally tests the `group_count_and_prop` function
given various types of input to test the robustness of its intended use
and its response to inappropriate inputs.

## 4.0 Multiple inputs in single argument

These scenarios test the flexibility of the two variable inputs in
`group_count_and_prop` for the embedded `group_by` and `length(which())`
functions.

### 4.0.1 Vector with no NAs, **two** `group_by` inputs

The following tibble is the output of the `gapminder` data set grouped
by `continent` and `year`, followed by the count and proportion of
observations where `lifeExp > 60`.

``` r
#Expected output when ... = continent, year, cond = lifeExp > 60 in `gapminder`
test401 <- gapminder %>% group_by(continent, year) %>%
  summarize(n = n(),
            count = length(which(lifeExp > 60)),
            prop = (length(which(lifeExp > 60))/n())*100)
```

    ## `summarise()` has grouped output by 'continent'. You can override using the
    ## `.groups` argument.

``` r
print(test401)
```

    ## # A tibble: 60 × 5
    ## # Groups:   continent [5]
    ##    continent  year     n count  prop
    ##    <fct>     <int> <int> <int> <dbl>
    ##  1 Africa     1952    52     0  0   
    ##  2 Africa     1957    52     0  0   
    ##  3 Africa     1962    52     1  1.92
    ##  4 Africa     1967    52     2  3.85
    ##  5 Africa     1972    52     2  3.85
    ##  6 Africa     1977    52     2  3.85
    ##  7 Africa     1982    52     8 15.4 
    ##  8 Africa     1987    52    12 23.1 
    ##  9 Africa     1992    52    13 25   
    ## 10 Africa     1997    52    13 25   
    ## # ℹ 50 more rows

This test evaluates if the outputs of `group_count_and_prop` versus
`test401` are identical.

``` r
#Test function against expected outcome test401
test_that("returns vector grouped by two variables, count and prop by each group", {
  expect_identical(object = group_count_and_prop(gapminder, 
                                                 continent, year,
                                                 cond = lifeExp > 60),
                   expected = test401)
})
```

    ## Test passed 🥇

### 4.0.2 Vector with no NAs, **two** `cond` inputs

The following tibble is the output of the `penguins` data set grouped by
`species`, followed by the count and proportion of observations where
`flipper_length_mm > 200` and `sex == "male"`.

``` r
#Expected output when ... = species, cond = flipper_length_mm > 200 & sex == "male" in `penguins`
test402 <- penguins %>% group_by(species) %>%
  summarize(n = n(),
            count = length(which(flipper_length_mm > 200 & sex == "male")),
            prop = (length(which(flipper_length_mm > 200 & sex == "male"))/n())*100)

print(test402)
```

    ## # A tibble: 3 × 4
    ##   species       n count  prop
    ##   <fct>     <int> <int> <dbl>
    ## 1 Adelie      152     6  3.95
    ## 2 Chinstrap    68    17 25   
    ## 3 Gentoo      124    61 49.2

This test evaluates if the outputs of `group_count_and_prop` versus
`test402` are identical.

``` r
#Test function against expected outcome test402
test_that("returns vector with count and prop of observations matching two conditions", {
  expect_identical(object = group_count_and_prop(penguins, 
                                                 species,
                                                 cond = flipper_length_mm > 200 & sex == "male"),
                   expected = test402)
})
```

    ## Test passed 😀

### 4.1 Invalid `group_by` argument

The following code returns an error due to the `group_by` argument not
being found in the data frame, `penguins`.

``` r
#Expected error given a group_by() input not found in `penguins`
penguins %>% group_by(notfound) %>%
  summarize(n = n(),
            count = length(which(body_mass_g > 4000)),
            prop = (length(which(body_mass_g > 4000))/n())*100)
```

    ## Error in `group_by()`:
    ## ! Must group by variables found in `.data`.
    ## ✖ Column `notfound` is not found.

This test evaluates if inputting the same invalid `group_by` argument in
`group_count_and_prop` returns an error.

``` r
#Test function with invalid group_by() argument
test_that("error given invalid group_by argument", {
  expect_error(object = group_count_and_prop(penguins,
                                             notfound,
                                             cond = body_mass_g > 4000))
})
```

    ## Test passed 🥳

### 4.2 Invalid `cond` argument

The following code returns an error due to a string input to `which()`
when a logical expression is required.

``` r
#Expected error given a string input in which() argument
gapminder %>% group_by(continent) %>%
  summarize(n = n(),
            count = length(which("notlogical")),
            prop = (length(which("notlogical"))/n())*100)
```

    ## Error in `summarize()`:
    ## ℹ In argument: `count = length(which("notlogical"))`.
    ## ℹ In group 1: `continent = Africa`.
    ## Caused by error in `which()`:
    ## ! argument to 'which' is not logical

This test evaluates if inputting the same invalid `cond` argument in
`group_count_and_prop` returns an error.

``` r
#Test function with invalid `cond` argument
test_that("error given invalid cond argument", {
  expect_error(object = group_count_and_prop(gapminder,
                                             continent,
                                             cond = "notlogical"))
})
```

    ## Test passed 😸
