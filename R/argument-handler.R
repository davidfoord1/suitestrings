check_strings_arg <- function(replacement, function_name) {
  if (!is.character(strings)) {
    warning(paste("In", function_name, "(): \n",
                  "`strings` is not a character vector.",
                  "Coercing with `as.character()`"))

    strings <- as.character(patterns)
  }

  strings
}

check_pattern_arg <- function(pattern, function_name) {
  if (!is.character(pattern)) {
    warning(paste("In", function_name, "(): \n",
                   "`pattern` is not a character vector.",
                   "Coercing with `as.character()`"))

    pattern <- as.character(pattern)
  }

  if (length(pattern) > 1) {
    warning(paste("In ", function_name, "():\n",
                   "Argument `pattern` is of `length()` > 1;",
                   "only the first element will be used. ",
                   "To match multiple patterns,",
                   "consider `pattern_any_string()` or `mapply()`."))

    pattern <- pattern[[1]]
  }

  pattern
}

get_function_name <- function() {
  deparse(sys.call(-1)[[1]])
}
