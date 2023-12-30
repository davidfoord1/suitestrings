
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suitestrings

<!-- badges: start -->
<!-- badges: end -->

This is primarily a learning project, for me to build my understanding
of string manipulation and regular expressions (both using base R and
popular packages) as well as how to create packages.

All `{suitestrings}` functionality is implemented in base R; most
functions are built as wrappers around `{base}` string manipulation
functions. The interface largely mimics what’s seen in
[stringr](https://stringr.tidyverse.org/reference/index.html) and
[stringi](https://stringi.gagolewski.com/rapi/stri_count.html), with the
intent of being expressive and easily understandable in English. As
such, it is set up to be used *instead* of those packages.

So, the goal of suitestrings is to provide comprehensive set of
functions for string manipulation. Some notable features:

- Consistent naming scheme - Including prefixes designed for use with
  autocomplete :

  - `str_` for vectorised operations on strings.

  - `chr_` for vector-wide operations over strings.

- Order of inputs - The string(s) to work on is usually used as the
  first argument instead of the pattern, which makes it easier to use
  with pipes than base R.

- No additional dependencies - this package has no Imports and doesn’t
  do anything you can’t replicate exactly in base R.

## Installation

You can install suitestrings from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidfoord1/suitestrings")
```

## Examples

``` r
# Load the package
library(suitestrings)


# Prepare a vector of strings
strings <- c("flat-hat", "backpack", "roll", "cat-sat-on-a-mat")
# Define a pattern for a three character string with "a" in the middle
regex_pattern <- "\\wa\\w"


# Replace the first match in each string
str_replace_first(strings, regex_pattern, "lea")
#> [1] "flea-hat"         "leakpack"         "roll"             "lea-sat-on-a-mat"

# Get the second match for each string
str_extract_nth(strings, regex_pattern, 2)
#> [1] "hat" "pac" NA    "sat"

# Get a list of every match for each string
str_extract_all(strings, regex_pattern)
#> [[1]]
#> [1] "lat" "hat"
#> 
#> [[2]]
#> [1] "bac" "pac"
#> 
#> [[3]]
#> character(0)
#> 
#> [[4]]
#> [1] "cat" "sat" "mat"

# Get every match from the entire character vector
chr_extract_all(strings, regex_pattern)
#> [1] "lat" "hat" "bac" "pac" "cat" "sat" "mat"
```
