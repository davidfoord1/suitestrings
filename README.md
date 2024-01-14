
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suitestrings

<!-- badges: start -->

[![Repo status: work in
progress](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

This is primarily a personal learning project, though nonetheless aims
to provide a comprehensive and convenient set of functions for working
with strings in R. More specifically, for:

- Combining strings.
- Cleaning and transforming strings.
- Manipulating strings based on regular expression patterns.

The interface is built with a consistent naming scheme and argument
structure. The first argument will be the string or strings to work on,
which makes it particularly convenient to work with pipes `|>`.
Functions start with prefixes for easy identification and
auto-completion. Get familiar with the design by reading
`vignette("suitestrings-conventions")`:

    -   `str_` for vectorised operations on strings.
    -   `chr_` for vector-wide operations over strings.

This package is built with base R (no imports).

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

\####Concatenate strings

``` r
str_concat("Mary", "had", "a", "little", "lamb", separator = " ")
#> [1] "Mary had a little lamb"
```

#### Concatenate across across multiple vectors.

``` r
str_concat(c("Mary ", "Joseph "), 
           "had a ", 
           c("little ", "technicolour "), 
           c("lamb.", "dreamcoat."))
#> [1] "Mary had a little lamb."             
#> [2] "Joseph had a technicolour dreamcoat."
```

#### Evaluate R expression in strings

`str_glue()` treats text in braces {} like R code

``` r
statement <- "{x} squared is {x^2} and its square root is {sqrt(x)}."
x <- 25
str_glue(statement)
#> [1] "25 squared is 625 and its square root is 5."

x <- 16
str_glue(statement)
#> [1] "16 squared is 256 and its square root is 4."
```

### Clean and transform strings

``` r
str_to_snake_case("  This /IS/  a ->>!!GREAT!!<<-STRing!!")
#> [1] "this_is_a_great_string"
```

### Manipulate strings based on regular expression patterns

``` r
# Prepare a vector of strings
strings <- c("flat-hat", "backpack", "roll", "cat-sat-on-a-mat")
# Define a pattern for any three characters with "a" in the middle
regex_pattern <- "\\wa\\w"
```

#### Detect, extract and replace patterns in strings

``` r
str_detect(strings, regex_pattern)
#> [1]  TRUE  TRUE FALSE  TRUE
str_extract_first(strings, regex_pattern)
#> [1] "lat" "bac" NA    "cat"
str_replace_first(strings, regex_pattern, "lea")
#> [1] "flea-hat"         "leakpack"         "roll"             "lea-sat-on-a-mat"
```

#### Work with specific occurrences of patterns

Using suffixes \_first, \_nth and \_last

``` r
# Get the second match for each string
str_extract_nth(strings, regex_pattern, 2)
#> [1] "hat" "pac" NA    "sat"

# Replace the last match in each string
str_remove_last(strings, regex_pattern)
#> [1] "flat-"         "backk"         "roll"          "cat-sat-on-a-"
```

#### Work with every occurrence of a pattern

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
