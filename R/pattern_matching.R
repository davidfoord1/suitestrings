str_detect_match <- function(string, pattern) {
  grepl(pattern, string, perl = TRUE)
}
