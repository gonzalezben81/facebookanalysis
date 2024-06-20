library(testthat)

test_that("version_replace works with simple inputs", {
  result <- version_replace("1", "2.3")
  expect_equal(result, as.name("1.2"))
  
  result <- version_replace("2", "3.4")
  expect_equal(result, as.name("2.3"))
  
  result <- version_replace("10", "11.22")
  expect_equal(result, as.name("10.11"))
})

test_that("version_replace handles single digit minor versions", {
  result <- version_replace("3", "4")
  expect_equal(result, as.name("3.4"))
  
  result <- version_replace("5", "6.0")
  expect_equal(result, as.name("5.6"))
})

test_that("version_replace handles complex minor versions", {
  result <- version_replace("7", "8.9.10")
  expect_equal(result, as.name("7.8"))
  
  result <- version_replace("11", "12.34.56")
  expect_equal(result, as.name("11.12"))
})

test_that("version_replace handles major versions with multiple digits", {
  result <- version_replace("100", "200.300")
  expect_equal(result, as.name("100.200"))
  
  result <- version_replace("999", "888.777")
  expect_equal(result, as.name("999.888"))
})

test_that("version_replace handles edge cases", {
  result <- version_replace("0", "0.0")
  expect_equal(result, as.name("0.0"))
  
  result <- version_replace("", "1.2")
  expect_equal(result, as.name(".1"))
  
  result <- version_replace("1", "")
  expect_equal(result, as.name("1."))
  
  result <- version_replace("", "")
  expect_equal(result, as.name("."))
})
