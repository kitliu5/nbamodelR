library(nbamodelR)
context("Website Query Preparation")

test_that("getBBRefUrls returns the correct number of URLs", {
  expect_equal(length(nbamodelR:::getBBRefUrls(2000, 2009)), 10)
})

test_that("getQueryParamsMatrix returns the correct number of rows", {
  expect_equal(nrow(nbamodelR:::getQueryParamMatrix(2015, 2017, "Regular Season")), 90)
  expect_equal(nrow(nbamodelR:::getQueryParamMatrix(1950, 1953, "Regular Season")), 180)
  expect_equal(nrow(nbamodelR:::getQueryParamMatrix(1953, 1956, "Regular Season")), 150)
})