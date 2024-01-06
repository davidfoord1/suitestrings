#' Concatenate strings
#'
#' @description
#' `str_concat()` takes multiple string arguments and concatenates them into a single string,
#' inserting a specified separator between each piece.
#'
#' @param ...
#' One or more character vectors or objects coercible to character vectors. These are the strings
#' or objects to be concatenated.
#'
#' @param separator
#' A character string to separate the concatenated elements. Defaults to an empty string,
#' which results in no separation between elements.
#'
#' @return
#' A single character string representing the concatenation of all input elements, separated by
#' the specified `separator`.
#'
#' @examples
#' str_concat("Hello", "world", separator = " ")
#' #> [1] "Hello world"
#' str_concat("2023", "01", "01", separator = "-")
#' #> [1] "2023-01-01"
#' str_concat("One", "Two", "Three")
#' #> [1] "OneTwoThree"
#'
#' @export
#'
#' @seealso
#' [chr_collapse()] for reduce a character vector into a single string.
#'
#' [paste()], which `str_concat()` wraps around.
str_concat <- function(..., separator = "") {
  paste(..., sep = separator)
}

#' Collapse a character vector to a single string
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#'
#' @param separator
#' A character string to separate the concatenated elements. Defaults to an empty string,
#' which results in no separation between elements.
#'
#' @return
#' A single character string
#'
#' @export
#'
#' @seealso
#' [str_concat()] for concatenating multiple character strings/vectors together.
#'
#' [paste0()] which `chr_collapse()` wraps around.
#'
#' @examples
#' chr_collapse(c("Hello", "world"), " ")
#' #> [1] "Hello world"
#' chr_collapse(letters, "-")
#' #> [1] "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x-y-z"
#' chr_collapse(1:9)
#' #> [1] "123456789"
chr_collapse <- function(strings, separator = "") {
  paste0(strings, collapse = separator)
}

str_glue <- function(strings, .envir = parent.frame()) {
  # Regular expression to find {{variable}} patterns

  replace_vars <- function(string, .envir) {
    pattern <- "\\{([^\\}]+)\\}"
    matches <- gregexpr(pattern, string)[[1]]

    if (matches[[1]] == -1) {
      return(string)
    }

    match_lengths <- attr(matches, "match.length")
    var_names <- character(length(matches))
    var_values <- character(length(matches))
    for (index in seq_along(matches)) {
      var_names[[index]] <- substr(string,
                                   matches[[index]] + 1, # +2 to skip {{
                                   matches[[index]] + match_lengths[[index]] - 1 - 1)

      var_values[[index]] <- tryCatch(
        as.character(eval(parse(text = var_names[[index]]), envir = .envir)),
        error = function(e) NA_character_
      )
    }

    for (index in seq_along(matches)) {
      pattern <- paste0("{", var_names[[index]], "}")
      string <- sub(pattern, var_values[[index]], string, fixed = TRUE)
    }

    return(string)
  }

  result <- vapply(strings,
                   \(string) replace_vars(string, .envir),
                   FUN.VALUE = character(1))
  names(result) <- NULL

  return(result)
}
