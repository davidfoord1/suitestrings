str_concat <- function(...) {
  paste0(...)
}

chr_collapse <- function(strings, separator = "") {
  paste0(strings, collapse = separator)
}

str_glue <- function(strings, .envir = parent.frame()) {
  # Regular expression to find {{variable}} patterns

  replace_vars <- function(string, .envir) {
    pattern <- "\\{\\{([^\\}\\}]+)\\}\\}"
    matches <- gregexpr(pattern, string)[[1]]

    if (matches[[1]] == -1) {
      return(string)
    }

    match_lengths <- attr(matches, "match.length")
    var_names <- character(length(matches))
    var_values <- character(length(matches))
    for (index in seq_along(matches)) {
      var_names[[index]] <- substr(string,
                                   matches[[index]] + 2, # +2 to skip {{
                                   matches[[index]] + match_lengths[[index]] - 2 - 1)

      var_values[[index]] <- tryCatch(
        as.character(eval(parse(text = var_names[[index]]), envir = .envir)),
        error = function(e) NA_character_
      )
    }

    for (index in seq_along(matches)) {
      pattern <- paste0("{{", var_names[[index]], "}}")
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
