# str_detect ----------------------------------------------------

test_that("str_detect_match() detects a pattern in a string", {
  strings <- c("apple", "banana", "cherry")
  pattern <- "a"

  expect_equal(str_detect_match(strings, pattern), c(TRUE, TRUE, FALSE))
})

# str_locate ----------------------------------------------------
## str_locate_first----------------------------------------------

test_that("str_locate_first finds the first match correctly", {
  expect_equal(str_locate_first("Hello world", "world"),
               matrix(c(7, 11), ncol = 2, dimnames = list(NULL, c("start", "end"))))
  expect_equal(str_locate_first("abcdef", "cd"),
               matrix(c(3, 4), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles no match", {
  expect_equal(str_locate_first("Hello world", "xyz"),
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles empty pattern", {
  expect_equal(str_locate_first("Hello world", ""),
               matrix(c(1, 0), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles empty string", {
  expect_equal(str_locate_first("", "pattern"),
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

## str_locate_all() ----------------------------------------------

test_that("str_locate_all finds all matches correctly", {
  result <- str_locate_all(c("abc def abc", "ghi jkl"), "abc")
  expect_equal(result[[1]],
               matrix(c(1, 3, 9, 11), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
  expect_equal(result[[2]],
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_all handles no matches", {
  result <- str_locate_all(c("abc", "def"), "xyz")
  expect_equal(result,
               list(matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))),
                    matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end")))))
})

test_that("str_locate_all handles empty pattern", {
  result <- str_locate_all(c("abc", "def"), "")
  expect_equal(result[[1]],
               matrix(c(1, 0, 2, 1, 3, 2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
  expect_equal(result[[2]],
               matrix(c(1, 0, 2, 1, 3, 2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_all handles empty string", {
  result <- str_locate_all(c("", ""), "pattern")
  expect_equal(result,
               list(matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))),
                    matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end")))))
})

## str_locate_nth -----------------------------------------------

test_that("str_locate_nth finds the correct nth match", {
  strings <- c("banana", "apple apple", "no match here")
  pattern <- "a"

  # Test with a valid nth occurrence
  expect_equal(str_locate_nth(strings, pattern, 2),
               matrix(c(4, 4, 7, 7, NA_integer_, NA_integer_), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))

  # Test with nth occurrence that doesn't exist
  expect_equal(str_locate_nth(strings, pattern, 4),
               matrix(c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_nth handles edge cases correctly", {
  # Test with empty string
  expect_equal(str_locate_nth("", "a", 1), matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))

  # Test with no pattern match
  expect_equal(str_locate_nth("banana", "x", 1), matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

# str_locate_last -----------------------------------------------

test_that("str_locate_last finds the last match correctly", {
  strings <- c("banana", "apple apple")
  pattern <- "a"

  # Test with strings having valid matches
  expect_equal(str_locate_last(strings, pattern),
               matrix(c(6, 6, 7, 7), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_last handles edge cases correctly", {
  # Test with empty string
  expect_equal(str_locate_last("", "a"), matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))

  # Test with no pattern match
  expect_equal(str_locate_last("banana", "x"), matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

# str_extract ---------------------------------------------------
## str_extract_first -------------------------------------------

test_that("str_extract_first extracts the first match correctly", {
  expect_equal(str_extract_first("abcdef", "[aeiou]"), "a")
  expect_equal(str_extract_first("Hello world", ".o"), "lo")
})

test_that("str_extract_first handles no match", {
  expect_equal(str_extract_first("Hello world", "xyz"), NA_character_)
})

test_that("str_extract_first handles empty pattern", {
  expect_equal(str_extract_first("Hello world", ""), "")
})

test_that("str_extract_first handles empty string", {
  expect_equal(str_extract_first("", "pattern"), NA_character_)
})


test_that("str_extract_first extracts the first match from a vector", {
  expect_equal(str_extract_first(c("abcde", "defghi"), "[aeiou]"), c("a", "e"))
})

## str_extract_all ---------------------------------------------

test_that("str_extract_first extracts the first match from a vector", {
  expect_equal(str_extract_all(c("abcde", "defghi"), "[aeiou]"),
               list(c("a", "e"), c("e", "i")))
})

test_that("str_extract_all handles handles non-matches", {
  expect_equal(str_extract_all(c("abcde", "xyz", "defghi"), "[aeiou]"),
               list(c("a", "e"), character(0), c("e", "i")))
})

test_that("str_extract_all handles empty pattern", {
  expect_equal(str_extract_all("abc", ""), list(rep("", nchar("abc"))))
})

test_that("str_extract_all handles empty string", {
  expect_equal(str_extract_all("", "pattern"), list(character(0)))
})

## str_extract_nth ---------------------------------------------

test_that("str_extract_nth extracts the nth match correctly", {
  expect_equal(str_extract_nth("one two three two four", "\\b\\w+\\b", 2), "two")
  expect_equal(str_extract_nth("repeat repeat repeat", "repeat", 3), "repeat")
})

test_that("str_extract_nth handles no matches", {
  expect_equal(str_extract_nth("hello world", "xyz", 1), NA_character_)
})

test_that("str_extract_nth handles insufficient matches", {
  expect_equal(str_extract_nth("one two three", "\\b\\w+\\b", 5), NA_character_)
})

test_that("str_extract_nth handles negative indexing", {
  expect_equal(str_extract_nth("one two three four", "\\b\\w+\\b", -2), "three")
})

test_that("str_extract_nth handles multiple strings", {
  input <- c("one two three", "hello world", "repeat repeat repeat")
  expect_equal(str_extract_nth(input, "\\b\\w+\\b", 2), c("two", "world", "repeat"))
})

## str_extract_last --------------------------------------------

test_that("str_extract_last extracts the last match correctly", {
  expect_equal(str_extract_last("one two three two four", "\\b\\w+\\b"), "four")
  expect_equal(str_extract_last("repeat repeat repeat", "repeat"), "repeat")
})

test_that("str_extract_last handles no matches", {
  expect_equal(str_extract_last("hello world", "xyz"), NA_character_)
})

test_that("str_extract_last handles single match", {
  expect_equal(str_extract_last("hello world", "world"), "world")
})

test_that("str_extract_last handles multiple strings", {
  input <- c("one two three", "hello world", "repeat repeat repeat")
  expect_equal(str_extract_last(input, "\\b\\w+\\b"), c("three", "world", "repeat"))
})

# str_replace ---------------------------------------------------
## str_replace_first -------------------------------------------

test_that("str_replace_first replaces the first match correctly", {
  expect_equal(str_replace_first("Hello world", "o", "O"), "HellO world")
  expect_equal(str_replace_first("abcdef", "cd", "CD"), "abCDef")
})

test_that("str_replace_first handles no match", {
  expect_equal(str_replace_first("Hello world", "xyz", "XYZ"), "Hello world")
})

test_that("str_replace_first handles empty pattern", {
  expect_equal(str_replace_first("Hello world", "", "X"), "XHello world")
})

test_that("str_replace_first handles empty string", {
  expect_equal(str_replace_first("", "pattern", "replacement"), "")
})

test_that("str_replace_first handles empty replacement", {
  expect_equal(str_replace_first("Hello world", "o", ""), "Hell world")
})


## str_replace_all ---------------------------------------------

test_that("str_replace_all replaces all matches correctly", {
  expect_equal(str_replace_all("Hello world", "o", "O"), "HellO wOrld")
  expect_equal(str_replace_all("abc abc abc", "abc", "XYZ"), "XYZ XYZ XYZ")
})

test_that("str_replace_all handles no matches", {
  expect_equal(str_replace_all("Hello world", "xyz", "XYZ"), "Hello world")
})

test_that("str_replace_all handles empty pattern", {
  expect_equal(str_replace_all("abc", "", "X"), "XaXbXcX")
})

test_that("str_replace_all handles empty string", {
  expect_equal(str_replace_all("", "pattern", "replacement"), "")
})

test_that("str_replace_all handles empty replacement", {
  expect_equal(str_replace_all("Hello world", "o", ""), "Hell wrld")
})


# str_remove ----------------------------------------------------
## str_remove_first ----------------------------------------------

test_that("str_remove_first removes the first match correctly", {
  expect_equal(str_remove_first("Hello world", "o"), "Hell world")
  expect_equal(str_remove_first("Hello world", "l"), "Helo world")
})

test_that("str_remove_first handles no match", {
  expect_equal(str_remove_first("Hello world", "xyz"), "Hello world")
})

test_that("str_remove_first handles empty pattern", {
  expect_equal(str_remove_first("Hello world", ""), "Hello world")
})

test_that("str_remove_first handles empty string", {
  expect_equal(str_remove_first("", "pattern"), "")
})

test_that("str_remove_first handles pattern at start", {
  expect_equal(str_remove_first("Hello world", "He"), "llo world")
})

test_that("str_remove_first handles pattern at end", {
  expect_equal(str_remove_first("Hello world", "ld"), "Hello wor")
})

## str_remove_all ----------------------------------------------

test_that("str_remove_all removes all matches correctly", {
  expect_equal(str_remove_all("Hello world", "o"), "Hell wrld")
  expect_equal(str_remove_all("ababab", "ab"), "")
})

test_that("str_remove_all handles no matches", {
  expect_equal(str_remove_all("Hello world", "xyz"), "Hello world")
})

test_that("str_remove_all handles empty pattern", {
  expect_equal(str_remove_all("abc", ""), "abc")
})

test_that("str_remove_all handles empty string", {
  expect_equal(str_remove_all("", "pattern"), "")
})

test_that("str_remove_all handles repeated patterns", {
  expect_equal(str_remove_all("abcabcabc", "abc"), "")
})

# str_split -----------------------------------------------------
## str_split_all -----------------------------------------------

test_that("str_split_all splits each string correctly", {
  expect_equal(str_split_all("a,b,c", ","), list(c("a", "b", "c")))
  expect_equal(str_split_all("hello world", " "), list(c("hello", "world")))
})

test_that("str_split_all handles no matches", {
  expect_equal(str_split_all("hello", ","), list("hello"))
})

test_that("str_split_all handles empty pattern", {
  expect_equal(str_split_all("abc", ""), list(c("a", "b", "c")))
})

test_that("str_split_all handles empty string", {
  expect_equal(str_split_all("", ","), list(character(0)))
})

test_that("str_split_all handles multiple strings", {
  expect_equal(str_split_all(c("a,b,c", "d,e"), ","), list(c("a", "b", "c"), c("d", "e")))
})

## str_split_first ---------------------------------------------

test_that("str_split_first splits at the first occurrence correctly", {
  expect_equal(str_split_first("a,b,c", ","), "a")
  expect_equal(str_split_first("hello world", " "), "hello")
})

test_that("str_split_first handles no matches", {
  expect_equal(str_split_first("hello", ","), "hello")
})

test_that("str_split_first handles empty pattern", {
  expect_equal(str_split_first("abc", ""), "a")
})

test_that("str_split_first handles empty string", {
  expect_equal(str_split_first("", ","), NA_character_)
})

test_that("str_split_first handles multiple strings", {
  expect_equal(str_split_first(c("a,b,c", "d,e,f", "g,h"), ","), c("a", "d", "g"))
})

test_that("str_split_first handles strings with no delimiter", {
  input <- c("apple", "banana", "cherry")
  expect_equal(str_split_first(input, ","), input)
})

## str_split_last ---------------------------------------------

test_that("str_split_last splits to the last occurrence correctly", {
  expect_equal(str_split_last("a,b,c", ","), "c")
  expect_equal(str_split_last("hello world", " "), "world")
})

test_that("str_split_last handles no matches", {
  expect_equal(str_split_last("hello", ","), "hello")
})

test_that("str_split_last handles empty pattern", {
  expect_equal(str_split_last("abc", ""), "c")
})

test_that("str_split_last handles empty string", {
  expect_equal(str_split_last("", ","), NA_character_)
})

test_that("str_split_last handles multiple strings", {
  expect_equal(str_split_last(c("a,b,c", "d,e,f", "g,h"), ","), c("c", "f", "h"))
})

test_that("str_split_last handles strings with no delimiter", {
  input <- c("apple", "banana", "cherry")
  expect_equal(str_split_last(input, ","), input)
})
