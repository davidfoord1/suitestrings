
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suitestrings

<!-- badges: start -->

[![Repo status: work in
progress](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/davidfoord1/suitestrings/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/davidfoord1/suitestrings/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/davidfoord1/suitestrings/graph/badge.svg?token=F41JP7X4YP)](https://codecov.io.gh.davidfoord1/suitestrings)

<!-- badges: end -->

This is primarily a personal learning project. It uses wrappers around
base R string operations to provide a comprehensive and convenient set
of functions for working with strings in R. More specifically, for:

- Combining strings.
- Cleaning and transforming strings.
- Manipulating strings based on regular expression patterns.

The interface is built with a consistent naming scheme and argument
structure. The first argument will be the string or strings to work on,
which makes it particularly convenient to work with pipes. Functions
start with prefixes for easy identification and auto-completion:

- `str_` for vectorised operations on strings.
- `chr_` for vector-wide operations over strings.

You can read more at `vignette("suitestrings-conventions")`.

## Installation

You can install suitestrings from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("davidfoord1/suitestrings", build_vignettes = TRUE)

# Load the package to your library
library(suitestrings)
```

This package is built with base R, so there are no additional
dependencies to install.

## Examples

### Combine strings

``` r
str_concat(c("Hello", "How are"), c("world!", "you?"), separator = " ")
#> [1] "Hello world!" "How are you?"
```

**With R expressions**

`str_glue()` treats text in braces `{}` like R code

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

#### Shorten strings

``` r
# Remove leading/trailing spaces and reduce middle spaces down to one
str_squish("  Too   much     space        ")
#> [1] "Too much space"

# Reduce a string to a specified length
str_truncate("This string is far too long, let's make it shorter", 20)
#> [1] "This string is fa..."
```

#### Extend strings

``` r
# Append a specific number of characters
str_indent(c("Hello", "World"), 3)
#> [1] "   Hello" "   World"

# Repeat the contents of strings
str_repeat("hello", 3, ", ")
#> [1] "hello, hello, hello"
```

#### Reformat strings

``` r
# Convert strings to different cases
str_to_snake_case("  This /IS/  a ->>!!GREAT!!<<-STriNg!!")
#> [1] "this_is_a_great_string"
```

### Manipulate strings based on regular expression patterns

``` r
# Prepare an example character vector.
strings <- c("flat-hat", "backpack", "roll", "cat-sat-on-a-mat")
# Define a pattern for a three letter sequence with "a" in the middle.
pattern <- "\\wa\\w"
```

#### Detect, extract and replace patterns in strings

``` r
# Does a string contain a match?
str_detect(strings, pattern)
#> [1]  TRUE  TRUE FALSE  TRUE
# Get the first match in a string
str_extract_first(strings, pattern)
#> [1] "lat" "bac" NA    "cat"
# Replace the first match in a string
str_replace_first(strings, pattern, "rat")
#> [1] "frat-hat"         "ratkpack"         "roll"             "rat-sat-on-a-mat"
```

#### Work with specific occurrences of patterns

Using suffixes \_first, \_nth and \_last

``` r
# Get the second match for each string
str_extract_nth(strings, pattern, 2)
#> [1] "hat" "pac" NA    "sat"
# Remove the last match in each string
str_remove_last(strings, pattern)
#> [1] "flat-"         "backk"         "roll"          "cat-sat-on-a-"
```

#### Work with every occurrence of a pattern

With suffix \_all

``` r
str_locate_all(strings, pattern)
#> [[1]]
#>      start end
#> [1,]     2   4
#> [2,]     6   8
#> 
#> [[2]]
#>      start end
#> [1,]     1   3
#> [2,]     5   7
#> 
#> [[3]]
#>      start end
#> [1,]    NA  NA
#> 
#> [[4]]
#>      start end
#> [1,]     1   3
#> [2,]     5   7
#> [3,]    14  16

# Get a list of every match for each string
str_extract_all(strings, pattern)
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
```

#### Work on the character vector as a whole

With prefix chr\_

``` r
# Get every match from `strings` into one character vector
chr_extract_all(strings, pattern)
#> [1] "lat" "hat" "bac" "pac" "cat" "sat" "mat"

# Which elements of the vector contain a match?
chr_which(strings, pattern)
#> [1] 1 2 4

# Get the subset of `strings` that contain a match
chr_subset(strings, pattern)
#> [1] "flat-hat"         "backpack"         "cat-sat-on-a-mat"
```
