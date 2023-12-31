# chr_sort ------------------------------------------------------

test_that("chr_sort correctly sorts a character vector", {
  expect_equal(chr_sort(c("banana", "apple", "cherry")), c("apple", "banana", "cherry"))
  expect_equal(chr_sort(c("banana", "apple", "cherry"), decreasing = TRUE), c("cherry", "banana", "apple"))
})

# chr_order -----------------------------------------------------

test_that("chr_order correctly orders a character vector", {
  expect_equal(chr_order(c("banana", "apple", "cherry")), c(2, 1, 3))
  expect_equal(chr_order(c("banana", "apple", "cherry"), decreasing = TRUE), c(3, 1, 2))
})

# chr_rank ------------------------------------------------------

test_that("chr_rank correctly ranks a character vector", {
  expect_equal(chr_rank(c("banana", "apple", "cherry")), c(2, 1, 3))
  expect_equal(chr_rank(c("banana", "apple", "cherry"), decreasing = TRUE), c(2, 3, 1))
})

# chr_unique ----------------------------------------------------

test_that("chr_unique correctly remove duplicates from a character vector", {
  expect_equal(chr_unique(c("aaa", "bbb", "aaa")), c("aaa", "bbb"))
})

test_that("chr_unique leaves vectors without duplicates the same", {
  expect_equal(chr_unique(c("aaa", "bbb", "ccc")), c("aaa", "bbb", "ccc"))
})
