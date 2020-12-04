#' Check whether the columns of a Game Log are valid
#'
#' Checks whether the column names match up with the column names expected
#' by the functions within this package that fit Bradley-Terry models.
#'
#' @param gameLog The game log to be supplied to the \code{gameLog} argument
#' of either \code{\link{buildWinProbMatrixBT}}, \code{\link{buildPtDiffMatrixBT}}, or
#' \code{\link{buildModelsBT}}.
#' @return Either returns nothing, or errors out.
#'
#' @keywords internal
#'
#' @author astroud
checkGamelogColumnsBT = function(gameLog) {
  if(!all(c("H.TEAM", "A.TEAM", "H.PTS", "A.PTS") %in% colnames(gameLog))) {
    stop("Game Log does not contain the correct columns, must include at least the following:
         H.TEAM, A.TEAM, H.PTS, A.PTS.")
  }
}
