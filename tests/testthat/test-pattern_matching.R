# str_detect ----------------------------------------------------

test_that("str_detect_match() detects a pattern in a string", {
  string <- c("apple", "banana", "cherry")
  pattern <- "a"

  expect_equal(str_detect_match(string, pattern), c(TRUE, TRUE, FALSE))
})

# str_which -----------------------------------------------------

test_that("str_which() identifies the matching indices in a character vector", {
  expect_equal(str_which(letters, "[aeiou]"), c(1, 5, 9, 15, 21))
})

