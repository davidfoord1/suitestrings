test_that("str_detect_match() detects a pattern in a string", {
  string <- c("apple", "banana", "cherry")
  pattern <- "a"

  expect_equal(str_detect_match(string, pattern), c(TRUE, TRUE, FALSE))
})
