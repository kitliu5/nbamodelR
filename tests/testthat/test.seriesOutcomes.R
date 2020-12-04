library(nbamodelR)
context("Series Outcomes")

test_that("seriesOutcomes gives statistically possible outcomes", {
  expect_true(all(nbamodelR:::seriesOutcomes("home", "away", 0.6, 0.4) <= 1 &
                    nbamodelR:::seriesOutcomes("home", "away", 0.6, 0.4) >= 0))
  expect_equal(sum(nbamodelR:::seriesOutcomes("home", "away", 0.7, 0.4)), 1)
  expect_equal(sum(nbamodelR:::seriesOutcomes("home", "away", 0.63, 0.35)), 1)
})
