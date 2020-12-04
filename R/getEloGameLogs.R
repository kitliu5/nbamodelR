#' Obtain NBA game logs from FiveThirtyEight's repository
#'
#' Gather game logs from a specified range of seasons. Game logs will include
#' FiveThirtyEight's pre- and post-game elo ratings, as well as elo-projected
#' win probabilities.
#'
#' @details FiveThirtyEight makes their data available at
#' \url{https://github.com/fivethirtyeight/data/tree/master/nba-carmelo}. Note that
#' the entire FiveThirtyEight-provided csv is downloaded each time this function
#' is called, so unlike for \code{\link{getGameLogs}}, choosing a smaller
#' range of seasons will not result in a shorter runtime. Also, FiveThirtyEight's
#' dataset contains ABA games as well as NBA games.
#'
#' @param from An integer denoting the first season from which to search.
#' As NBA seasons span two calendar years, use the later of the two years
#' as the passed-in parameter (e.g. for the 2017-18 season, use 2018).
#' @param to An integer denoting the last season to include in the search.
#'
#' @return A data frame with game logs for every NBA season between \code{from}
#' and \code{to}, inclusive.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @seealso \code{\link{getGameLogs}} for gathering game logs with more
#' comprehensive statistics, and \code{\link{getSeasonStats}} for gathering
#' team statistics aggregated by seasons, or \code{\link{buildModelsElo}} for
#' evaluating team strength using these gamelogs.
#'
#' @examples
#' \dontrun{
#' getEloGameLogs()
#' getEloGameLogs(2017)
#' getEloGameLogs(2000, 2017)
#' }
#'
#' @author astroud
getEloGameLogs = function(from = NULL, to = from) {
  nba_elo = utils::read.csv("https://projects.fivethirtyeight.com/nba-model/nba_elo.csv", stringsAsFactors = FALSE)
  seasons = unique(nba_elo$season)

  if(!is.null(from)) {
    if(is.na(as.integer(from))) {
      warning("'from' not passed as valid integer, substituting earliest available season")
      from = min(seasons)
    }
    else if(from > max(seasons)) {
      stop(paste("Dataset contains no data after", from, "-- Latest available season is", max(seasons)))
    }
    else if(from < min(seasons)) {
      warning(paste("Dataset does not go back to", from, "-- collecting from", min(seasons), "instead"))
    }
  }

  if(!is.null(to)) {
    if(is.na(as.integer(to))) {
      warning("'to' not passed as valid integer, substituting latest available season")
      to = max(seasons)
    }
    else if(to < min(seasons)) {
      stop(paste("Dataset contains no data before", to, "-- Earliest available season is", min(seasons)))
    }
    else if(to > max(seasons)) {
      warning(paste("Dataset does not go beyond", to, "-- collecting up to", max(seasons), "instead"))
    }
  }

  #need this down here instead of as an 'else' to avoid issues with lazy evaluation
  if(is.null(from)) {
    from = min(seasons)
  }
  if(is.null(to)) {
    to = max(seasons)
  }

  if(to < from) {
    temp = to
    to = from
    from = temp
  }

  nba_elo = nba_elo[nba_elo$season >= from & nba_elo$season <= to,]
  nba_elo[!is.na(nba_elo$score1),] # remove games that have not yet been played
}
