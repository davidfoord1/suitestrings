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

#' Evaluate R code within strings
#'
#' @description Combine strings, with text inside braces `{}`
#' evaluated as R expressions to be output directly within the string.
#'
#' @param ...
#' Expression strings to format and concatenate.
#'
#' Named arguments are taken to be temporary variables available
#' for substitution.
#' @param separator
#' A character string to separate the concatenated elements. Defaults to an empty string,
#' which results in no separation between elements.
#' @param environment
#' An evironment object to evaluate the expression in. By default this is the environment
#' `str_glue()` was called from.
#'
#' @return
#' A character vector with same length as the longest input.
#'
#' @details
#' Unnamed arguments to `...` are taken as strings containing expressions
#' to evaluate and format. Multiple inputs are concatenated together
#' before being formatted. Named arguments will be added as variables
#' to a temporary environment along with the variables supplied within
#' `environment`.
#'
#' Doubling the braces escapes them, for when you want braces in your
#' output strings.
#'
#' @export
#'
#' @seealso
#' [str_concat()] for simply combining strings and
#' [chr_collapse()] for collapsing strings in character vectors
#' to a single string.
#'
#' [sprintf()] for C-style string formatting and variable interpolation.
#'
#' @examples
#' x <- 5
#'
#' # Evaluate R expressions in braces {} in the middle of a string
#' str_glue("The square of {x} is {x^2}")
#' #> [1] "The square of 5 is 25"
#' tri_num_str <- \(n) chr_collapse(cumsum(seq(n)), ", ")
#' str_glue("The first {x} triangle numbers are {tri_num_str(x)}.")
#' #> [1] "The first 5 triangle numbers are 1, 3, 6, 10, 15."
#'
#' # Named arguments are taken as temporary variables.
#' str_glue(
#'    "My name is {name}, ",
#'    "I am {age} years old. ",
#'    "In {x} years I'll be {age + x}.",
#'    name = "John",
#'    age = 45
#' )
#' #> [1] "My name is John, I am 45 years old. In 5 years I'll be 50."
str_glue <- function(..., separator = "", environment = parent.frame()) {
  args <- list(...)
  arg_names <- names(args)

  if (is.null(arg_names)) {
    # If no named arguments, use the input args and envir as-is
    unnamed_args <- args
    eval_env <- environment
  } else {
    # Separate named and unnamed arguments
    named_args <- args[arg_names != ""]
    unnamed_args <- args[arg_names == ""]

    # So that we can evaluate variables from both the input envir
    # and the named arguments, create a new environment
    # combining named_args and input envir
    eval_env <- list2env(named_args, parent = environment)
  }

  # Combine unnamed arguments as strings
  strings <- do.call(paste, c(lapply(unnamed_args, as.character), sep = separator))

  replace_vars <- function(string) {
    # Braces can be included in strings be escaping them with a second brace
    # So first we replace escaped bracers with placeholders
    # To ensure they don't get caught in the expression evaluation
    string <- gsub("\\{\\{", "__OPENBRACE__", string)
    string <- gsub("\\}\\}", "__CLOSEBRACE__", string)

    # Define a pattern for the interpolation structure {expression}
    # As any group of characters between { and } that aren't }
    pattern <- "\\{([^\\}]+)\\}"

    # Locate all {expression} patterns in the string
    matches <- gregexpr(pattern, string)[[1]]

    # If no matches are found
    if (matches[[1]] == -1) {
      # Revert placeholders back to single braces
      string <- gsub("__OPENBRACE__", "{", string)
      string <- gsub("__CLOSEBRACE__", "}", string)
      # And otherwise return the string as-is
      return(string)
    }

    # If matches are found, get their lengths so we can locate the substring
    match_lengths <- attr(matches, "match.length")

    # Pre-allocate character vectors to populate
    expr_strings <- character(length(matches))
    expr_values <- character(length(matches))

    # For each match
    for (index in seq_along(matches)) {
      # Get the expression between the braces {expression}
      expr_strings[[index]] <- substr(string,
                                   matches[[index]] + 1, # +1 to skip {
                                   matches[[index]] + match_lengths[[index]] - 2)

      # parse() from text to expression
      # eval() from expression to result, within the eval_env
      # as.character() to convert back to string
      expr_values[[index]] <- tryCatch(
        as.character(eval(parse(text = expr_strings[[index]]), envir = eval_env)),
        error = function(e) NA_character_
      )
    }

    # Loop through the matches again, replacing the pattern occurrences
    # with a string of the expression's value
    for (index in seq_along(matches)) {
      pattern <- paste0("{", expr_strings[[index]], "}")
      string <- sub(pattern, expr_values[[index]], string, fixed = TRUE)
    }

    # Revert placeholders back to single braces after processing
    string <- gsub("__OPENBRACE__", "{", string)
    string <- gsub("__CLOSEBRACE__", "}", string)

    return(string)
  }

  # Apply the function to all strings (unnamed args) passed to
  result <- vapply(strings,
                   \(string) replace_vars(string),
                   FUN.VALUE = character(1))

  names(result) <- NULL
  return(result)
}
