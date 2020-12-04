library(nbamodelR)
context("Collecting Data from Basketball Reference")

testHTML = xml2::read_html("https://www.basketball-reference.com/leagues/NBA_2017.html")
test_that("getYear gives the correct year", {
  expect_equal(nbamodelR:::getYear(testHTML), "2016-17")
})

test_that("getTeamWL gives the correct dimensions", {
  expect_equal(nrow(nbamodelR:::getTeamWL(testHTML)), 30)
  expect_equal(ncol(nbamodelR:::getTeamWL(testHTML)), 4)
})

test_that("getTeamWL returns accurate records", {
  expect_equal(sum(nbamodelR:::getTeamWL(testHTML)$W),
               sum(nbamodelR:::getTeamWL(testHTML)$L))
})