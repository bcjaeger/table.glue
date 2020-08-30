
<!-- README.md is generated from README.Rmd. Please edit that file -->

# table.glue

<!-- badges: start -->

<!-- badges: end -->

The goal of table.glue is to give you more control over the presentation
of your data and also simplify the process of rounding and formatting
data. The main idea is to create rounding specifications (starting with
`round_spec()`) that can be plugged in, directly or through global
options, to the `table_glue()` and `table_value()` functions.

## Installation

<!-- You can install the released version of table.glue from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("table.glue") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bcjaeger/table.glue")
```

## Example

Suppose we want to write a summary statement about the mean and standard
deviation (SD) of a column in the `mtcars` data. We’ll start by loading
the `glue`, `table.glue`, and `magrittr` packages.

``` r

library(glue)       # for comparisons with table_glue
library(table.glue) # similar to glue but with built in rounding
library(magrittr)   # make rounding specifications using %>%
```

### Use base R

The classic approach is to use base R functions `format`, `round`, and
`paste`:

``` r

col_name <- "mpg"
col_mean <- mean(mtcars[[col_name]])
col_sd <- sd(mtcars[[col_name]])

col_mean_pretty <- format(signif(col_mean, digits = 3), nsmall = 1)
col_sd_pretty <- format(signif(col_sd, digits = 3), nsmall = 1)

paste0("The mean (SD) of ", col_name, " is ", 
       col_mean_pretty, " (", col_sd_pretty, ") ")
#> [1] "The mean (SD) of mpg is 20.1 (6.03) "
```

This gets the job done\! Still, the code may be a little hard to read
for a user who isn’t a grizzled `paste()` veteran. This is where the
`glue` package really shines.

### Use `glue()`

Instead of using `paste()`, `glue()` lets us write everything in one
string, surrounding R object with curly brackets ( { } ) tells R that
the `glue()` function should print the value of that R object rather
than the raw string. For instance,

``` r

glue("the mean (SD) of {col_name} is {col_mean_pretty} ({col_sd_pretty})")
#> the mean (SD) of mpg is 20.1 (6.03)
```

This is certainly more readable and clean. The only thing `glue()`
doesn’t do is make the pretty versions of `col_mean` and `col_sd`.
This is where `table.glue` comes in.

### Use `table_glue()`

The `table.glue` package lets you use `glue()` without having to make
numbers pretty beforehand. For example, the code below uses
`table_glue()`, one of the main functions in `table.glue`, to replicate
the results we got from `glue()` but without using the pretty versions
of `col_mean` and `col_sd`.

``` r

# notice that we are not using 'pretty' versions of col_mean and col_sd
table_glue("the mean (SD) of {col_name} is {col_mean} ({col_sd})")
#> [1] "the mean (SD) of mpg is 20 (6.0)"
```

### Summary

  - `table_glue` applies a general rounding convention to all numeric
    data that are supplied in the input string.

  - The goal is to combine the clean syntax of `glue()` with a
    convenient and generally acceptable rounding specification.

Hopefully, most of your rounding needs will be met without going any
further than this. However, the rabbit hole does go deeper. Let’s say
you don’t like the default rounding specification and you want to make
your own. You can do that\!

``` r

rspec <- round_spec() %>% # make your own rounding specification
  round_using_signif(digits = 2) # round to 2 significant digits

# table glue adopts all the rules given by your specification
table_glue("the mean (SD) of {col_name} is {col_mean} ({col_sd})", 
           rspec = rspec)
#> [1] "the mean (SD) of mpg is 20 (6)"
```

More details on the different rounding specification rules are given in
the package vignette: “Rounding specifications” (not written yet; in
progress). One final nice feature of `table.glue` is the ability to make
any rounding specification the default rounding specification used by
all of the `table_` functions, including `table_glue`:

``` r

# save your rounding specification, making it the default for table_glue
make_default_rounder(rspec)

# now you don't have to write rspec = rspec in table_glue()

table_glue("the mean (SD) of {col_name} is {col_mean} ({col_sd})")
#> [1] "the mean (SD) of mpg is 20 (6)"
```
