#' Detect the presence of a pattern in a string
#'
#' @description
#' These functions search for patterns in strings
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return A logical vector indicating the presence of each pattern in a string.
#' @export
#' @rdname str_detect
#'
#' @examples
#' str_detect_match(c("apple", "banana", "cherry"), "a")
#' #> [1]  TRUE  TRUE FALSE
str_detect_match <- function(strings, pattern, fixed = FALSE) {
  grepl(pattern, strings, perl = TRUE, fixed = fixed)
}

# str_locate ----------------------------------------------------

#' Locate pattern matches in strings
#'
#' @description
#' These functions find occurrences of a pattern in strings.
#'
#' `str_locate_first()` finds the first occurrence of a pattern in a string.
#' `str_locate_all()` finds all occurrences of a pattern in each string of the input vector.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return `str_locate_first()` returns a two-column matrix with the start and end positions
#' of the first match. There is a row for each string. `str_locate_all()` returns a list of matrices.
#' There is a matrix for each string and a row for each match.
#' If no match is found, NA values are returned.
#'
#' @examples
#' str_locate_first("Hello world", "world")
#' #>      start end
#' #> [1,]     7  11
#' str_locate_first(c("Hello world", "Goodbye world"), "o")
#' #>      start end
#' #> [1,]     5   5
#' #> [2,]     2   2
#'
#' str_locate_all(c("Hello world", "Goodbye world"), "world")
#' #> [[1]]
#' #>      start end
#' #> [1,]     7  11
#' #>
#' #> [[2]]
#' #>      start end
#' #> [1,]     9  13
#' str_locate_all(c("Hello world", "Goodbye world"), "o")
#' #> [[1]]
#' #> start end
#' #> [1,]     5   5
#' #> [2,]     8   8
#' #>
#' #> [[2]]
#' #> start end
#' #> [1,]     2   2
#' #> [2,]     3   3
#' #> [3,]    10  10
#'
#' @rdname str_locate
#' @export
str_locate_first <- function(strings, pattern, fixed = FALSE) {
  # Get the match info
  match_info <- regexpr(pattern, strings, perl = TRUE, fixed = fixed)
  match_length <- attr(match_info, "match.length")
  match_end <- match_info + match_length - 1L

  no_match <- match_info == -1
  match_info[no_match] <- NA_integer_
  match_end[no_match] <- NA_integer_

  matrix(c(match_info, match_end),
         ncol = 2,
         dimnames = list(NULL, c("start", "end")))
}

#' @rdname str_locate
#' @export
str_locate_all <- function(strings, pattern, fixed = FALSE) {
  find_matches_in_string <- function(strings) {
    match_info <- gregexpr(pattern, strings, perl = TRUE, fixed = fixed)[[1]]
    ends <- match_info + attr(match_info, "match.length") - 1L

    # If there are no matches, store as NA NA
    if (all(match_info == -1L)) {
      return(matrix(NA_integer_,
                    ncol = 2,
                    nrow = 1,
                    dimnames = list(NULL, c("start", "end"))))
    }

    cbind(start = match_info, end = ends)
  }

  matches <- lapply(strings, find_matches_in_string)
  return(matches)
}

# str_extract ---------------------------------------------------

#' Extract complete matches from strings
#'
#' @description
#' These functions extract parts of strings based on a pattern.
#'
#' `str_extract_first()`, `str_extract_nth()` and `str_extract_last()` extract
#'  the first, nth and last occurrence of a pattern in each string,
#'  into a character vector the same length as `strings`.
#'
#' `str_extract_all()` extracts a character vector of all occurrences of a pattern
#' for each string, into a list the same length as `strings`.
#'
#' `chr_extract_all()` extracts all occurrences of a pattern from `strings`
#' into a character vector.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param n (`str_extract_nth` only) Integer, the nth occurrence of the pattern to extract.
#' Negative values count back from the end.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return
#' `str_extract_first()`, `str_extract_nth()` and `str_extract_last()`
#' each return a character vector the same length as the input vector `strings`.
#' It contains the extracted portion of the string, corresponding to
#' the first, nth and last match of the pattern, respectively. Strings
#' with no corresponding match are represented as `NA` values.
#'
#' `str_extract_all()`: returns a list of character vectors, where each list element corresponds
#' to a string in the input vector. Each element is a character vector of all matches in that string.
#' If no matches are found in a string, the corresponding list element is an empty character vector i.e. `character(0)`.
#' The list is the same length as the input vector `strings`.
#'
#' `chr_extract_all()`: returns a character vector containing every single match in the input vector.
#' Non-matches are ignored. This is equivalent to calling `unlist()` on the output of `str_extract_all()`.
#'
#' @details
#' These functions are built using the base R regular expression functions.
#' `{suitestrings}` uses Perl-compatible Regular Expressions (PCRE).
#' This is achieved by setting `perl = TRUE` in the underlying base functions.
#' See R's [regexp] documentation for info on the regex implementation.
#' For complete syntax details see \href{https://www.pcre.org/current/doc/html/}{https://www.pcre.org/current/doc/html/}
#'
#' @seealso
#' [regmatches()] for base R matched substring extraction.
#'
#' @examples
#' str_extract_first(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
#' #> [1] "mat" "bat" NA    "cat"
#'
#' str_extract_all(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
#' #> [[1]]
#' #> [1] "mat"
#' #>
#' #> [[2]]
#' #> [1] "bat"
#' #>
#' #> [[3]]
#' #> character(0)
#' #>
#' #> [[4]]
#' #> [1] "cat" "hat"
#'
#' str_extract_nth(c("mat", "bat", "pig", "cat-in-a-hat"), ".at", 2)
#' #> [1] NA    NA    NA    "hat"
#'
#' str_extract_last(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
#' #> [1] "mat" "bat" NA    "hat"
#'
#' chr_extract_all(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
#' #> [1] "mat" "bat" "cat" "hat"
#'
#' @export
#' @rdname str_extract
str_extract_first <- function(strings, pattern, fixed = FALSE) {
  matches <- regexpr(pattern, strings, perl = TRUE, fixed = fixed)

  extracted <- character(length(strings))

  for (index in seq_along(strings)) {
    if (matches[[index]] == -1) {
      extracted[[index]] <- NA_character_
    } else {
      index_match <- matches[[index]]
      attr(index_match, "match.length") <- attr(matches, "match.length")[[index]]
      extracted[[index]] <- regmatches(strings[[index]], index_match)
    }
  }

  extracted
}

#' @rdname str_extract
#' @export
str_extract_all <- function(strings, pattern, fixed = FALSE) {
  regmatches(strings, gregexpr(pattern, strings, perl = TRUE, fixed = fixed))
}

#' @rdname str_extract
#' @export
str_extract_nth <- function(strings, pattern, n, fixed = FALSE) {
  vapply(str_extract_all(strings, pattern),
         function(matches) {
           len <- length(matches)
           index <- if (n > 0) n else len + n + 1
           if (index >= 1 && index <= len) matches[index] else NA_character_
         },
         FUN.VALUE = character(1))
}

#' @rdname str_extract
#' @export
str_extract_last <- function(strings, pattern, fixed = FALSE) {
  vapply(str_extract_all(strings, pattern),
         function(matches) {
           if (length(matches) > 0) matches[[length(matches)]] else NA_character_
         },
         FUN.VALUE = character(1))
}

# str_replace ---------------------------------------------------

#' Replace parts of a string with new text.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param replacement A single string containing the text to replace the pattern with.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return
#' `str_replace()` Returns an altered character vector of equal length to `strings`,
#' with the first match in each string replaced.
#'
#' `str_replace_all()` Returns an altered character vector of equal length to `strings`,
#' with every match in each string replaced. Regular expression matches are non-overlapping.
#'
#' @examples
#' str_replace_first("Hello world!", "o", "ooo")
#' #> [1] "Hellooo world!"
#'
#' str_replace_all("Hello world!", "o", "ooo")
#' #> [1] "Hellooo wooorld!"
#'
#' @rdname str_replace
#' @export
str_replace_first <- function(strings, pattern, replacement, fixed = FALSE) {
  sub(pattern, replacement, strings, perl = TRUE, fixed = fixed)
}

#' @rdname str_replace
#' @export
str_replace_all <- function(strings, pattern, replacement, fixed = FALSE) {
  gsub(pattern, replacement, strings, perl = TRUE, fixed = fixed)
}

# str_remove ----------------------------------------------------

#' Remove Patterns from Strings
#'
#' @description
#' `str_remove_first()` removes the first occurrence of a pattern in each string.
#' `str_remove_all()` removes all occurrences of a pattern in each string.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return
#' A character vector of the same length as `strings`, with the specified pattern removed.
#' For `str_remove_first()`, only the first occurrence of the pattern in each string is removed.
#' For `str_remove_all()`, all occurrences of the pattern in each string are removed.
#'
#' @examples
#' str_remove_first("Hello world", "o")
#' #> [1] "Hell world"
#'
#' str_remove_all("Hello world", "o")
#' #> [1] "Hell wrld"
#'
#' @seealso
#' \code{\link{str_replace_first}}, \code{\link{str_replace_all}} for replacing patterns with specific text.
#'
#' @rdname str_remove
#' @export
str_remove_first <- function(strings, pattern, fixed = FALSE) {
  sub(pattern, "", strings, perl = TRUE, fixed = fixed)
}

#' @rdname str_remove
#' @export
str_remove_all <- function(strings, pattern, fixed = FALSE) {
  gsub(pattern, "", strings, perl = TRUE, fixed = fixed)
}

# str_split -----------------------------------------------------

#' Split strings by a pattern
#'
#' @description
#' `str_split_all()` splits each string in the input vector into substrings based on a pattern.
#'
#' `chr_split_all()` splits all strings in the input vector and concatenates the results into a single character vector.
#'
#' `str_split_first()` takes the first result of splitting each string in the input vector based on a pattern.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return
#' `str_split_all()`: A list of the same length as `strings`, with each element being a vector of substrings obtained by splitting the corresponding element of `strings`.
#'
#' `chr_split_all()`: A single character vector containing all substrings obtained by splitting each element of `strings` and concatenating them.
#'
#' `str_split_first()`: A character vector the same length as `strings`, with each element being the first substring obtained by splitting the corresponding element of `strings`.
#'
#' @examples
#' str_split_all(c("one,two,three", "abc,def,ghi"), ",")
#' #> [[1]]
#' #> [1] "one"   "two"   "three"
#' #>
#' #> [[2]]
#' #> [1] "abc" "def" "ghi"
#'
#' chr_split_all(c("one,two,three", "abc,def,ghi"), ",")
#' #> [1] "one"   "two"   "three" "abc"   "def"   "ghi"
#'
#' str_split_first(c("", "a b c"), " ")
#' #> [1] NA  "a"
#'
#' @seealso
#' \code{\link{strsplit}} for the base R function.
#'
#' @rdname str_split
#' @export
str_split_all <- function(strings, pattern, fixed = FALSE) {
  strsplit(strings, pattern, perl = TRUE, fixed = fixed)
}

#' @rdname str_split
#' @export
str_split_first <- function(strings, pattern, fixed = FALSE) {
  split_string <- strsplit(strings, pattern, perl = TRUE, fixed = fixed)
  sapply(split_string, \(x) ifelse(length(x) > 1, x[[1]], x))
}

