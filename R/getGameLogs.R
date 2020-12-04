#' Create a matrix of parameters for nba.stats.com queries
#'
#' @param from A year, integer or string -- the first year
#' from which data is desired.
#' @param to A year, integer or string -- the last year from
#' which data is desired.
#' @param type Either "Playoffs" or "Regular Season".
#' @return A data frame where each row contains query parameters
#' to send to \code{stats.nba.com/teamgamelog}, and where each
#' column contains the parameter names.
#'
#' @keywords internal
#'
#' @author astroud
getQueryParamMatrix = function(from, to, type) {
  if(is.na(as.integer(from))) {
    stop("Invalid input, 'from' must be an integer value")
  }
  if(is.na(as.integer(to))) {
    stop("Invalid input, 'to' must be an integer value")
  }

  teamIDs = 1610612737:1610612766

  seasons = paste(from:to - 1, substr(from:to, 3, 4), sep="-") # given, say, 2015, this gives back 2014-15
  stype = ifelse(type == "Playoffs", "Playoffs", "Regular%20Season")

  teamIDs = paste("TeamID", teamIDs, sep="=")
  seasons = paste("Season", seasons, sep="=")
  stype = paste("SeasonType", stype, sep="=")

  qparams = expand.grid(TeamID = teamIDs, Season = seasons, SeasonType = stype, stringsAsFactors = FALSE)
  if(from > 1954) { #for 1954 back to 1947, add in 161060023:161060037 -- very early NBA contains teams with differing IDs
    return(qparams)
  } else {
    oldIDs = paste("TeamID", 1610610023:1610610037, sep="=")
    if(to > 1954) {
      oldseasons = paste("Season", paste(from:1954 - 1, substr(from:1954, 3, 4), sep="-"), sep="=")
    } else {
      oldseasons = paste("Season", paste(from:to - 1, substr(from:to, 3, 4), sep="-"), sep="=")
    }
    oldparams = expand.grid(TeamID = oldIDs, Season = oldseasons, SeasonType = stype, stringsAsFactors = FALSE)
    return(rbind(oldparams, qparams))
  }
}

#' Query the stats.nba.com API to obtain game logs
#'
#' @details This function uses the "teamgamelog" API endpoint.
#' An example URL that will return a response with regular season
#' game logs for the Atlanta Hawks with all games from 2014-15 is
#' \url{http://stats.nba.com/stats/teamgamelog/?TeamID=1610612737&Season=2014-15&SeasonType=Regular\%20Season}.
#'
#' @param TeamID An integer corresponding to the stats.nba.com team ID.
#' @param Season A character string of the form YYYY-YY.
#' @param SeasonType Either "Regular Season" or "Playoffs".
#' @return An html nodeset containing a stats.nba.com API response.
#'
#' @importFrom xml2 read_html
#'
#' @keywords internal
#'
#' @author astroud
queryNBAStatsAPI = function(TeamID, Season, SeasonType) {
  #create URL
  baseURL = "http://stats.nba.com/stats/teamgamelog/"
  qParams = paste(TeamID, Season, SeasonType, sep="&")
  queryURL = paste(baseURL, qParams, sep="?")

  Sys.sleep(0.2) #avoid overloading the NBA site
  xml2::read_html(queryURL)
}

#' Extract game logs from a stats.nba.com API response
#'
#' @param htmlGamelog An HTML nodeset corresponding to a response from the
#' stats.nba.com teamgamelog API endpoint.
#' @return A data frame containing all the gamelogs from a single
#' stats.nba.com API response.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_text
#'
#' @keywords internal
#'
#' @author astroud
extractGameLog = function(htmlGamelog) {
  jGamelog = jsonlite::fromJSON(rvest::html_text(htmlGamelog))
  headers = jGamelog$resultSets$headers[[1]]
  data = as.data.frame(jGamelog$resultSets$rowSet[[1]], stringsAsFactors = FALSE)

  if(all(dim(data) != c(0,0))) { #only apply changes if resulting data not empty
    colnames(data) = headers
    data = cbind(data[,1:5], apply(data[,6:27], 2, as.numeric))

    matchups = t(as.data.frame(strsplit(data$MATCHUP, " ")))
    data$TEAM = matchups[,1]
    data$OPPONENT = matchups[,3]
    data$SITE = ifelse(matchups[,2] == "@", "Away", "Home")
    data$GAME_DATE = as.Date(data$GAME_DATE, format = "%b %d, %Y")

    #debug print(paste("Parsing", data$TEAM[1], "in season", data$GAME_DATE[1]))

    #remove unwanted columns
    data = data[,-which(colnames(data) %in% c("Team_ID", "MATCHUP", "W_PCT", "FG_PCT", "FG3_PCT", "FT_PCT"))]
  }
  colTitles = colnames(data)
  if(all(dim(data) == c(0,0))) { #if API response empty, make sure empty dataframe is mergeable
    data = data.frame(matrix(ncol=length(colTitles), nrow=0))
    colnames(data) = colTitles
  }
  data
}

#' Obtain NBA game logs
#'
#' Gather gamelogs with either scores-only or comprehensive data for a
#' specified range of NBA seasons.
#'
#' @param from An integer denoting the first season from which to search.
#' As NBA seasons span two calendar years, use the later of the two years
#' as the passed-in parameter (e.g. for the 2017-18 season, use 2018).
#' @param to An integer denoting the last season to include in the search.
#' @param type A character string specifying whether regular season game logs
#' or playoff game logs are desired.
#' @param simple A logical value indicating whether the user would like simple
#' game logs (only gameID, date, teams, and score) or full game logs (all recorded
#' statistics for both teams).
#'
#' @return A data frame with game logs of the given \code{type} for every NBA
#' season between \code{from} and \code{to}, inclusive.
#'
#' @export
#'
#' @seealso \code{\link{getEloGameLogs}} for collecting game logs with Elo ratings
#' for each team, and \code{\link{getSeasonStats}} for gathering team statistics
#' aggregated over the course of a season. \code{\link{buildModelsBT}} can be useful
#' for modeling team strength using these game logs.
#'
#' @details Note that a small delay is included between each API request. Because
#' stats.nba.com only makes gamelogs available for a specific team/season/type
#' combination, each season of gamelogs gathered will require 30 API requests, and
#' thus scraping gamelogs for the entire history of the NBA will take some time. As
#' far as I am aware, the stats.nba.com API has no volume limits, but in tests
#' without a fraction-of-a-second delay, the site refused to respond after an
#' excessive rate/quantity of requests, so the delay was included.
#'
#' @examples
#' \dontrun{
#' getGameLogs()
#' getGameLogs(2018, type = "Regular Season")
#' getGameLogs(2000, 2017, type = "Playoffs")
#' getGameLogs(1947, 2017, simple = TRUE)
#' }
#'
#' @author astroud
getGameLogs = function(from = 2017, to = from, type = c("Regular Season", "Playoffs"), simple = FALSE) {
  # need TODO some checking on from and to params
  type = match.arg(type)
  qpm = getQueryParamMatrix(from, to, type)
  gameHTMLs = apply(qpm, 1, function(x) {
    queryNBAStatsAPI(x[1], x[2], x[3])
  })

  gamelist = lapply(gameHTMLs, extractGameLog)
  gamelogs = do.call("rbind", gamelist)

  if(nrow(gamelogs) == 0) {
    stop("No data available for the given range of years")
  }

  hlogs = (gamelogs[gamelogs$SITE == "Home",])[,-ncol(gamelogs)+1:-ncol(gamelogs)]
  alogs = (gamelogs[gamelogs$SITE == "Away",])[,-ncol(gamelogs)+1:-ncol(gamelogs)]

  colnames(hlogs)[-1:-2] = paste("H", colnames(hlogs)[-1:-2], sep=".")
  colnames(alogs)[-1:-2] = paste("A", colnames(alogs)[-1:-2], sep=".")

  gamelogs = merge(hlogs, alogs)
  if(simple) {
    return(gamelogs[,which(colnames(gamelogs) %in% c("gamelogs", "Game_ID", "GAME_DATE", "H.TEAM", "A.TEAM", "H.PTS", "A.PTS"))])
  } else {
    return(gamelogs)
  }
}
