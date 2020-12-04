# need to make sure getEloGameLogs from getEloGameLogs.R is loaded (or move that to this file)

#' Create Elo models for a certain season using FiveThirtyEight gamelogs
#'
#' @details Uses logistic regression to predict a home team's chances of winning
#' given the difference between their Elo rating and their opponent's. Uses linear
#' regression to predict a home team's expected point advantage given the difference
#' between their Elo rating and their opponent's.
#'
#' @param eloLogs A dataframe with certain columns, for a list see the parameter
#' \code{eloLogs} of the function \code{\link{buildModelsElo}}. The output of a call
#' to \code{\link{getEloGameLogs}} will work here.
#' @param season An integer indicating the NBA season on which to create these models.
#' @return A list with coefficients for the win-percentage logistic regression, the point
#' differential linear regression, and each team's final Elo rating for the season (see Details).
#'
#' @importFrom stats glm lm coef binomial
#'
#' @keywords internal
#'
#' @author astroud
createEloModel = function(eloLogs, season) {
  if(!all(c("elo1_pre", "elo2_pre", "score1", "score2", "team1", "team2") %in% colnames(eloLogs))) {
    stop("eloLogs does not contain the correct columns, must include at least the following:
         elo1_pre, elo2_pre, score1, score2, team1, team2")
  }
  eloLogs$elodiff = with(eloLogs, elo1_pre - elo2_pre)
  eloLogs$scorediff = with(eloLogs, score1 - score2)
  eloLogs$t1win = with(eloLogs, as.integer(score1 > score2))

  elo_glm_wpct = stats::glm(t1win ~ elodiff, family = stats::binomial, data = eloLogs[eloLogs$neutral == 0,])
  elo_lm_ptdiff = stats::lm(scorediff ~ elodiff, data = eloLogs[eloLogs$neutral == 0 & eloLogs$season == season,])
  #add elo_glm_pts

  teamElos = extractTeamElos(eloLogs, season)

  list(wpctCoefs = stats::coef(elo_glm_wpct), ptdiffCoefs = stats::coef(elo_lm_ptdiff), teamElos = teamElos)
}

#' Extract team Elo ratings for a given NBA season
#'
#' @param eloLogs A dataframe with certain columns, for a list see the parameter
#' \code{eloLogs} of the function \code{\link{buildModelsElo}}. The output of a call
#' to \code{\link{getEloGameLogs}} will work here.
#' @param season An integer indicating the NBA season from which to extract Elo ratings.
#' @return A data frame with each team and their final Elo rating.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eloLogs = getEloGameLogs(2015, 2017)
#' extractTeamElos(eloLogs, 2017)
#' }
#'
#' @author astroud
extractTeamElos = function(eloLogs, season) {
  seasonlogs = eloLogs[eloLogs$season == season,] #put in check if this returns zero records
  teams = unique(c(unique(seasonlogs$team1), unique(seasonlogs$team2)))
  elos = lapply(teams, function(x) {
    idx1 = max(which(seasonlogs$team1 == x))
    idx2 = max(which(seasonlogs$team2 == x))
    ifelse(idx1 > idx2, seasonlogs[idx1, "elo1_post"], seasonlogs[idx2, "elo2_post"])
  })
  elo_df = data.frame(Team = teams, as.numeric(elos), stringsAsFactors = FALSE)
  colnames(elo_df)[2] = "Elo"
  elo_df[order(elo_df$Team),]
}

#' Build a matrix of elo differences between two teams
#'
#' @param teamElos A dataframe in the same format as a result from
#' \code{\link{extractTeamElos}}: team names and elo ratings.
#' @return A dataframe where each row and each column correspond to a team,
#' and individual cells correspond to the Elo rating of the row-team minus
#' the Elo rating of the column-team.
#'
#' @keywords internal
#'
#' @author astroud
buildEloDiffsMatrix = function(teamElos) {
  teams = teamElos$Team

  diffsList = lapply(teams, function(x) {
    oppElos = vapply(teams, function(X) {
      with(teamElos, Elo[Team == X])
    }, 0.1)
    oppElos[x] - oppElos
  })
  data.frame(Team = teams, do.call("rbind", diffsList))
}

#' Predict Win Probabilities between every pair of NBA teams with an Elo model
#'
#' @details Note that diagonal entries of the return matrix are
#' not exactly 50% because home-court advantage is taken into account.
#'
#' @param eloLogs A dataframe of Elo gamelogs, as from the output of
#' \code{\link{getEloGameLogs}}.
#' @param season An integer corresponding to the NBA season for which outputs are desired.
#' @return A matrix with each entry representing the expected win probability
#' of the home team in a matchup of the team in that row at home against the
#' team in that column on the road.
#'
#' @export
#'
#' @seealso \code{\link{buildPtDiffMatrixElo}} for obtaining a similar matrix
#' with predicted point differentials, and \code{\link{buildModelsElo}} to obtain
#' both types of matrices at once.
#'
#' @examples
#' \dontrun{
#' data(eloLogs2017)
#' buildWinProbMatrixElo(eloLogs2017, 2017)
#' }
#'
#' @author astroud
buildWinProbMatrixElo = function(eloLogs, season) {
  eloModel = createEloModel(eloLogs, season)
  wpctCoefs = eloModel$wpctCoefs
  teams = eloModel$teamElos$Team

  diffsMtrx = buildEloDiffsMatrix(eloModel$teamElos)

  sigm = function(x) {exp(x) / (1 + exp(x))}
  winProbs = apply(diffsMtrx[,-1], c(1,2), FUN = function(x) {
    sigm(wpctCoefs[1] + wpctCoefs[2]*x)
  })

  data.frame(Team = teams, winProbs)
}

#' Predict Point Differentials between every pair of NBA teams with an Elo model
#'
#' @param eloLogs A dataframe of Elo gamelogs, as from the output of
#' \code{\link{getEloGameLogs}}.
#' @param season An integer corresponding to the NBA season for which outputs are desired.
#' @return A matrix with each entry representing the point differential in a
#' matchup of the team in that row at home against the team in that column on
#' the road. Note that the diagonal terms are estimates of the home court advantage
#' coefficient.
#'
#' @export
#'
#' @seealso \code{\link{buildWinProbMatrixElo}} for obtaining a similar matrix
#' with predicted win probabilities, and \code{\link{buildModelsElo}} to obtain
#' both types of matrices at once.
#'
#' @examples
#' \dontrun{
#' data(eloLogs2017)
#' buildPtDiffMatrixElo(eloLogs2017, 2017)
#' }
#'
#' @author astroud
buildPtDiffMatrixElo = function(eloLogs, season) {
  eloModel = createEloModel(eloLogs, season)
  ptCoefs = eloModel$ptdiffCoefs
  teams = eloModel$teamElos$Team

  diffsMtrx = buildEloDiffsMatrix(eloModel$teamElos)

  ptDiffs = apply(diffsMtrx[,-1], c(1,2), FUN = function(x) {
    ptCoefs[1] + ptCoefs[2]*x
  })

  data.frame(Team = teams, ptDiffs)
}


#' Obtain Point Differential and Win Probability Matrices using an Elo model
#'
#' @param eloLogs A data frame of game logs including the columns \code{season},
#' \code{team1}, \code{team2}, \code{elo1_pre}, \code{elo2_pre}, \code{score1}, and
#' \code{score2}. The output of a call to \code{\link{getEloGameLogs}} will work here.
#' @param season An integer indicating the NBA season for which outputs are desired.
#' @return A list of two matrices, where the first matrix is the output of
#' \code{\link{buildWinProbMatrixElo}} and the second matrix is the output of
#' \code{\link{buildPtDiffMatrixElo}}.
#'
#' @export
#'
#' @seealso \code{\link{buildModelsBT}} to obtain the same list of matrices using a
#' Bradley-Terry model instead of Elo ratings.
#'
#' @examples
#' \dontrun{
#' data(eloLogs2017)
#' nbaMatrices = buildModelsElo(eloLogs2017)
#' }
#'
#' @author astroud
buildModelsElo = function(eloLogs, season) {
  wp = buildWinProbMatrixElo(eloLogs, season)
  pd = buildPtDiffMatrixElo(eloLogs, season)

  list(winProb = wp, pointDiff = pd)
}
