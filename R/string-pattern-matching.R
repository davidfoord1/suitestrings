#' Detect the presence of a pattern in a string
#'
#' @description
#' These functions search for patterns in strings
#'
#' @param string A character vector of strings to search in.
#' @param pattern The pattern to look for.
#'
#' @return A logical vector indicating the presence of each pattern in a string.
#' @export
#' @rdname str_detect
#'
#' @examples
#' str_detect_match(c("apple", "banana", "cherry"), "a")
#' #> [1]  TRUE  TRUE FALSE
str_detect_match <- function(string, pattern) {
  grepl(pattern, string, perl = TRUE)
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
#' @param string A character vector of strings to search in.
#' @param pattern A character string containing a regular expression.
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
str_locate_first <- function(string, pattern) {
  # Get the match info
  match_info <- regexpr(pattern, string, perl = TRUE)
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
str_locate_all <- function(string, pattern) {
  find_matches_in_string <- function(string) {
    match_info <- gregexpr(pattern, string, perl = TRUE)[[1]]
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

  matches <- lapply(string, find_matches_in_string)
  return(matches)
}



# str_extract ---------------------------------------------------


#' Extract complete matches from strings.
#' @description
#' These functions extract parts of strings based on a pattern.
#'
#' `str_extract_first()` extracts the first occurrence of a pattern in each string.
#' `str_extract_all()` extracts all occurrences of a pattern in each string.
#'
#' @param string A character vector of strings to search in.
#' @param pattern A character string containing a regular expression.
#'
#' @return
#' `str_extract_first()` returns a character vector with the extracted portion of the string
#' corresponding to the first match of the pattern. If no match is found, returns `NA` or an
#' empty string.
#'
#' `str_extract_all()` returns a list of character vectors, where each list element corresponds
#' to a string in the input vector. Each element is a character vector of all matches in that string.
#' If no matches are found in a string, the corresponding list element is an empty character vector.
#'
#' @export
#' @rdname str_extract
#'
#' @examples
#' str_extract_first(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
#' #> [1] "mat" "bat" NA    "cat"
#'
#' str_extract_all(c("mat", "bat", "pig", "cat-in-a-hat"), ".at")
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
str_extract_first <- function(string, pattern) {
  matches <- regexpr(pattern, string, perl = TRUE)

  extracted <- character(length(string))

  for (index in seq_along(string)) {
    if (matches[[index]] == -1) {
      extracted[[index]] <- NA_character_
    } else {
      index_match <- matches[[index]]
      attr(index_match, "match.length") <- attr(matches, "match.length")[[index]]
      extracted[[index]] <- regmatches(string[[index]], index_match)
    }
  }

  extracted
}

#' @rdname str_extract
#' @export
str_extract_all <- function(string, pattern) {
  regmatches(string, gregexpr(pattern, string, perl = TRUE))
}

# str_replace ---------------------------------------------------

#' Replace parts of a string with new text.
#'
#' @param string A character vector of strings to edit.
#' @param pattern A single string containing a regular expression pattern to match.
#' @param replacement A single string containing the text to replace the pattern with.
#'
#' @return
#' `str_replace()` Returns an altered character vector of equal length to `string`,
#' with the first match in each string replaced.
#'
#' `str_replace_all()` Returns an altered character vector of equal length to `string`,
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
str_replace_first <- function(string, pattern, replacement) {
  sub(pattern, replacement, string, perl = TRUE)
}

#' @rdname str_replace
#' @export
str_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string, perl = TRUE)
}

# str_remove ----------------------------------------------------

#' Remove Patterns from Strings
#'
#' @description
#' `str_remove_first()` removes the first occurrence of a pattern in each string.
#' `str_remove_all()` removes all occurrences of a pattern in each string.
#'
#' @param string A character vector or a single string.
#' @param pattern A character string containing a regular expression (or a fixed string to be matched).
#'
#' @return
#' A character vector of the same length as `string`, with the specified pattern removed.
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
str_remove_first <- function(string, pattern) {
  sub(pattern, "", string, perl = TRUE)
}

#' @rdname str_remove
#' @export
str_remove_all <- function(string, pattern) {
  gsub(pattern, "", string, perl = TRUE)
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
#' @param string A character vector of strings to be split.
#' @param pattern A character string containing a regular expression (or a fixed string) to split the input `string` by.
#' See \code{\link{regexp}} for a details on regular expressions in base R.
#' @return
#' `str_split_all()`: A list of the same length as `string`, with each element being a vector of substrings obtained by splitting the corresponding element of `string`.
#'
#' `chr_split_all()`: A single character vector containing all substrings obtained by splitting each element of `string` and concatenating them.
#'
#' `str_split_first()`: A character vector the same length as `string`, with each element being the first substring obtained by splitting the corresponding element of `string`.
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
str_split_all <- function(string, pattern) {
  strsplit(string, pattern, perl = TRUE)
}

str_split_first <- function(string, pattern) {
  split_string <- strsplit(string, pattern, perl = TRUE)
  sapply(split_string, \(x) ifelse(length(x) > 1, x[[1]], x))
}

