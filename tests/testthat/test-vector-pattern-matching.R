
# chr_detect_any ----------------------------------------------------

test_that("chr_detect_any() identifies if any strings contain the pattern", {
  strings <- c("apple", "banana", "cherry")

  expect_equal(chr_detect_any(strings, "a"), TRUE)
  expect_equal(chr_detect_any(strings, "z"), FALSE)
})

# chr_detect_all ------------------------------------------------

test_that("chr_detect_all() identifies if any strings contain the pattern", {
  strings <- c("apple", "banana", "cherry")

  expect_equal(chr_detect_all(strings, "a"), FALSE)
  expect_equal(chr_detect_all(strings, "[aeiou]"), TRUE)
})

# str_which -----------------------------------------------------

test_that("chr_which() identifies the matching indices in a character vector", {
  expect_equal(chr_which(letters, "[aeiou]"), c(1, 5, 9, 15, 21))
})


# chr_count_matches ------------------------------------

test_that("chr_count_matches() counts the number of matching indices in a vector", {
  expect_equal(chr_count_matches(letters, "[aeiou]"), 5L)
})


# chr_extract_all ---------------------------------------------------------

test_that("chr_extract_all gets all matches from a vector", {
  expect_equal(chr_extract_all(letters, "[aeiou]"), c("a", "e", "i", "o", "u"))
  expect_equal(chr_extract_all(1:9, "[12789]"), c("1", "2", "7", "8", "9"))
  expect_equal(chr_extract_all(c("cat-sat-on-a-mat", "rat"), "\\wa\\w"),
               c("cat", "sat", "mat", "rat"))
})

# chr_split -----------------------------------------------------

test_that("chr_split_all splits and concatenates all strings correctly", {
  expect_equal(chr_split_all("a,b,c", ","), c("a", "b", "c"))
  expect_equal(chr_split_all("hello world", " "), c("hello", "world"))
})

test_that("chr_split_all handles no matches", {
  expect_equal(chr_split_all("hello", ","), "hello")
})

test_that("chr_split_all handles empty pattern", {
  expect_equal(chr_split_all(c("abc", "def"), ""), c("a", "b", "c", "d", "e", "f"))
})

test_that("chr_split_all handles empty string", {
  expect_equal(chr_split_all(c("a,b", ""), ","), c("a", "b"))
})

test_that("chr_split_all handles multiple strings", {
  expect_equal(chr_split_all(c("a,b,c", "d,e"), ","), c("a", "b", "c", "d", "e"))
})


# chr_subset ----------------------------------------------------

test_that("chr_subset returns elements matching the pattern", {
  expect_equal(chr_subset(c("apple", "banana", "cherry", "date"), "a"), c("apple", "banana", "date"))
  expect_equal(chr_subset(c("apple", "banana", "cherry", "date"), "^a"), c("apple"))
})

test_that("chr_subset handles no matches", {
  expect_equal(chr_subset(c("apple", "banana", "cherry", "date"), "z"), character(0))
})

test_that("chr_subset handles empty pattern", {
  expect_equal(chr_subset(c("apple", "banana", "cherry", "date"), ""), c("apple", "banana", "cherry", "date"))
})

test_that("chr_subset handles empty string vector", {
  expect_equal(chr_subset(character(0), "a"), character(0))
})
