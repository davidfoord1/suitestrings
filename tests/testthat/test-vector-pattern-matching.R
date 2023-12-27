# str_which -----------------------------------------------------

test_that("chr_which() identifies the matching indices in a character vector", {
  expect_equal(chr_which(letters, "[aeiou]"), c(1, 5, 9, 15, 21))
})


# chr_count_matches ------------------------------------

test_that("chr_count_matches() counts the number of matching indices in a vector", {
  expect_equal(chr_count_matches(letters, "[aeiou]"), 5L)
})
