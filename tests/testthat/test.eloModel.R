library(nbamodelR)
context("Elo Model Structure")

testEloMod = nbamodelR:::createEloModel(eloLogs2017, 2017)
test_that("createEloModel has correctly formatted output", {
  expect_equal(length(testEloMod), 3)
  expect_equal(names(testEloMod), c("wpctCoefs", "ptdiffCoefs", "teamElos"))
  expect_equal(length(testEloMod[[1]]), 2)
  expect_equal(length(testEloMod[[2]]), 2)
})

testDiffs = nbamodelR:::buildEloDiffsMatrix(extractTeamElos(eloLogs2017, 2017))
test_that("buildEloDiffsMatrix is shaped correctly", {
  expect_equal(nrow(testDiffs), ncol(testDiffs) - 1)
  expect_equal(as.character(testDiffs$Team), colnames(testDiffs)[-1])
})
