
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suitestrings

<!-- badges: start -->

[![Repo status: work in
progress](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

This is primarily a personal learning project, but `{suitestrings}` aims
to provide comprehensive and convenient set of functions for working
with strings in R. More specifically:

- Combining strings.
- Cleaning and transforming strings.
- Manipulating strings based on regular expression patterns.

A few benefits of the design of this package:

- A consistent naming scheme - Including prefixes designed for use with
  auto-complete:

  - `str_` for vectorised operations on strings.

  - `chr_` for vector-wide operations over strings.

- Order of inputs - The string or strings to work on is usually used as
  the first argument instead of the pattern, which makes it easier to
  use with pipes than base R.

- No additional dependencies - this package has no Imports so you’ll
  only need base R to use it.

## Installation

You can install suitestrings from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidfoord1/suitestrings")
```

## Examples

``` r
library(suitestrings)
```

### Combine strings together

Concatenation across multiple vectors.

``` r
str_concat(c("Mary ", "Joseph "), 
           "had a ", 
           c("little ", "technicolour "), 
           c("lamb ", "dreamcoat "))
#> [1] "Mary had a little lamb "             
#> [2] "Joseph had a technicolour dreamcoat "
```

Evaluate R expression in strings

``` r
x <- 25
str_glue("{x} squared is {x^2} and the square root of {x} are {sqrt(x)}")
#> [1] "25 squared is 625 and the square root of 25 are 5"
```

### Clean and transform strings

``` r
str_to_snake_case("  This IS  a->>GreAt<<-strING!!")
#> [1] "this_is_a_gre_at_str_ing"
```

### Manipulating strings based on regular expression patterns

``` r
# Prepare a vector of strings
strings <- c("flat-hat", "backpack", "roll", "cat-sat-on-a-mat")
# Define a pattern for any three characters with "a" in the middle
regex_pattern <- "\\wa\\w"
```

Detect, extract and replace patterns in strings

``` r
str_detect(strings, regex_pattern)
#> [1]  TRUE  TRUE FALSE  TRUE
str_extract_first(strings, regex_pattern)
#> [1] "lat" "bac" NA    "cat"
str_replace_first(strings, regex_pattern, "lea")
#> [1] "flea-hat"         "leakpack"         "roll"             "lea-sat-on-a-mat"
```

Work with specific occurrences of patterns

``` r
# Get the second match for each string
str_extract_nth(strings, regex_pattern, 2)
#> [1] "hat" "pac" NA    "sat"

# Replace the first match in each string
str_remove_last(strings, regex_pattern)
#> [1] "flat-"         "backk"         "roll"          "cat-sat-on-a-"
```

Work with every occurrence of a pattern

``` r
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

# Get every match from `strings` into one character vector
chr_extract_all(strings, regex_pattern)
#> [1] "lat" "hat" "bac" "pac" "cat" "sat" "mat"

# Get the subset of `strings` that contain a match
chr_subset(strings, regex_pattern)
#> [1] "flat-hat"         "backpack"         "cat-sat-on-a-mat"
```
