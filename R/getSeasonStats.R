#' Create appropriate Basketball-Reference URLs for a range of seasons
#'
#' @param from An integer indicating the first season in the desired range.
#' @param to An integer indicating the last season in the desired range.
#' @return A character vector containing the URLs of the Basketball-Reference
#' season summary webpages for every year from \code{from} to \code{to}, inclusive.
#'
#' @keywords internal
#'
#' @author astroud
getBBRefUrls = function(from, to) {
  seasons = seq(from, to)
  paste("https://www.basketball-reference.com/leagues/NBA_", seasons, ".html", sep="")
}

#' Extract the year from a Basketball-Reference season-summary webpage
#'
#' @param html An HTML nodeset corresponding to a Basketball-Reference.com
#' season summary webpage.
#' @return A character string containing the year corresponding to the
#' data contained in \code{html}.
#'
#' @importFrom rvest html_node html_text
#' @keywords internal
#'
#' @author astroud
getYear = function(html) {
  yearnode = rvest::html_node(html, css="#info h1 span")
  rvest::html_text(yearnode)
}

#' Extract win-loss records from a Basketball-Reference season-summary webpage
#'
#' @param seasonSummaryHTML An HTML nodeset corresponding to a Basketball-Reference.com
#' season summary webpage.
#' @return A data frame containing win-loss records for every NBA team during the
#' season of information contained in \code{seasonSummaryHTML}.
#'
#' @importFrom rvest html_nodes html_text
#' @keywords internal
#'
#' @author astroud
getTeamWL = function(seasonSummaryHTML) { #consider exposing this function in the future?
  year = getYear(seasonSummaryHTML)

  standings = rvest::html_nodes(seasonSummaryHTML, css="#all_standings .standings_divs")

  teamNames = rvest::html_text(rvest::html_nodes(standings, css="th[data-stat='team_name'] a"))
  teamWins = rvest::html_text(rvest::html_nodes(standings, css="tbody td[data-stat='wins']"))
  teamLosses = rvest::html_text(rvest::html_nodes(standings, css="tbody td[data-stat='losses']"))

  data.frame(Year = year, Team = teamNames, W = as.integer(teamWins), L = as.integer(teamLosses), stringsAsFactors = FALSE)
}

#' Extract team statistics from a Basketball-Reference HTML Table
#'
#' @param statsHTML An HTML nodeset corresponding to a data table from Basketball-Reference.
#' @param year The season to which the data corresponds.
#' @param opponent A logical value indicating whether these statistics record a team's own
#' performance or the performance of teams playing against them.
#' @return A data frame containing the statistics in \code{statsHTML}.
#'
#' @importFrom rvest html_text html_nodes
#'
#' @keywords internal
#'
#' @author astroud
getStatsFromHTML = function(statsHTML, year, opponent = FALSE) {
  # TODO: clean this up/try to vectorize instead of using an ungodly list
  teams = rvest::html_text(rvest::html_nodes(statsHTML, css = "tbody td[data-stat='team_name'] a"))
  games = as.integer(rvest::html_text(rvest::html_nodes(statsHTML, css = "tbody td[data-stat='g']")))

  fg = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg']", "tbody td[data-stat='fg']"))))
  fgA = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fga']", "tbody td[data-stat='fga']"))))
  fgPct = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg_pct']", "tbody td[data-stat='fg_pct']"))))

  fg3 = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg3']", "tbody td[data-stat='fg3']"))))
  fg3A = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg3a']", "tbody td[data-stat='fg3a']"))))
  fg3Pct = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg3_pct']", "tbody td[data-stat='fg3_pct']"))))

  fg2 = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg2']", "tbody td[data-stat='fg2']"))))
  fg2A = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg2a']", "tbody td[data-stat='fg2a']"))))
  fg2Pct = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fg2_pct']", "tbody td[data-stat='fg2_pct']"))))

  ft = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_ft']", "tbody td[data-stat='ft']"))))
  ftA = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_fta']", "tbody td[data-stat='fta']"))))
  ftPct = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_ft_pct']", "tbody td[data-stat='ft_pct']"))))

  orebs = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_orb']", "tbody td[data-stat='orb']"))))
  drebs = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_drb']", "tbody td[data-stat='drb']"))))
  trebs = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_trb']", "tbody td[data-stat='trb']"))))
  assists = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_ast']", "tbody td[data-stat='ast']"))))
  steals = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_stl']", "tbody td[data-stat='stl']"))))
  blocks = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_blk']", "tbody td[data-stat='blk']"))))
  turnovers = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_tov']", "tbody td[data-stat='tov']"))))
  fouls = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_pf']", "tbody td[data-stat='pf']"))))
  points = as.numeric(rvest::html_text(rvest::html_nodes(statsHTML, css = ifelse(opponent, "tbody td[data-stat='opp_pts']", "tbody td[data-stat='pts']"))))

  names = c("Year", "Team", "G", "FG", "FGA", "FG.PCT", "FG3", "FG3A", "FG3.PCT", "FG2", "FG2A", "FG2.PCT", "FT", "FTA", "FT.PCT",
            "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")
  stats = data.frame(year, teams, games, fg, fgA, fgPct, fg3, fg3A, fg3Pct, fg2, fg2A, fg2Pct, ft, ftA, ftPct, orebs, drebs, trebs,
                     assists, steals, blocks, turnovers, fouls, points, stringsAsFactors = FALSE)
  if(opponent) {
    names[-1:-3] = paste("opp", names[-1:-3], sep="")
  }
  colnames(stats) = names
  stats
}

#' Extract team statistics from a Basketball-Reference season summary webpage
#'
#' @param seasonSummaryHTML An HTML nodeset corresponding to a Basketball-Reference.com
#' season summary webpage.
#' @param type A character string specifying whether per-game statistics, total
#' statistics, or per-100-possession statistics (when available) are desired.
#' @return A data frame with team statistics and team opponent statistics of the specified
#' \code{type} for the season contained in \code{seasonSummaryHTML}.
#'
#' @importFrom rvest html_nodes
#' @importFrom xml2 read_html
#'
#' @keywords internal
#'
#' @author astroud
getTeamStats = function(seasonSummaryHTML, type = "per 100 poss") {
  year = getYear(seasonSummaryHTML)

  if(type == "per game") {
    div = rvest::html_nodes(seasonSummaryHTML, css="#all_team-stats-per_game")
    op_div = rvest::html_nodes(seasonSummaryHTML, css="#all_opponent-stats-per_game")
  } else if (type == "totals") {
    div = rvest::html_nodes(seasonSummaryHTML, css="#all_team-stats-base")
    op_div = rvest::html_nodes(seasonSummaryHTML, css="#all_opponent-stats-base")
  } else { # per 100 pos
    div = rvest::html_nodes(seasonSummaryHTML, css="#all_team-stats-per_poss")
    op_div = rvest::html_nodes(seasonSummaryHTML, css="#all_opponent-stats-per_poss")
  }

  if(length(div) == 0) {
    # TODO provide error message that such type of data does not exist for that year
  }

  # basketball-reference hides their data inside comment tags - this peels them off
  statsHTML = xml2::read_html(sub("-->", "", sub("<!--", "", div)))
  oppHTML = xml2::read_html(sub("-->", "", sub("<!--", "", op_div)))

  teamstats = getStatsFromHTML(statsHTML, year)
  oppstats = getStatsFromHTML(oppHTML, year, opponent = TRUE)
  merge(teamstats, oppstats)
}

#' Obtain NBA season statistics
#'
#' Returns a data-frame containing various season summary statistics for
#' every NBA team in each specified season. Statistics include metrics
#' for a team itself and for that team's opponents. Throws an error if a
#' season does not exist in the basketball-reference database.
#'
#' @param from An integer denoting the first season from which to search.
#' As NBA seasons span two calendar years, use the later of the two years
#' as the passed-in parameter (e.g. for the 2017-18 season, use 2018).
#' @param to An integer denoting the last season to include in the search.
#' @param type A character string specifying whether per-game statistics, total
#' statistics, or per-100-possession statistics (when available) are desired.
#' @return A data frame with team statistics according to \code{type} for
#' every NBA season between \code{from} and \code{to}, inclusive.
#'
#' @importFrom xml2 read_html
#'
#' @export
#'
#' @seealso \code{\link{getGameLogs}} for gathering individual game data, and
#' \code{\link{getEloGameLogs}} for gathering game logs with elo ratings.
#'
#' @examples
#' getSeasonStats()
#' getSeasonStats(2018, type = "per 100 poss")
#' \dontrun{
#' getSeasonStats(2000, 2017, type = "totals")
#' }
#'
#' @author astroud
getSeasonStats = function(from = 2017, to = from, type = c("per game", "totals", "per 100 poss")) {
  season_urls = getBBRefUrls(from, to)
  seasonHTMLs = lapply(season_urls, function(x) {
      response = tryCatch({
        con = url(x)
        xml2::read_html(con)
      }, error = function(e) {
        warning(paste("Data unavailable for year", substr(x, gregexpr('_', x)[[1]][1] + 1, nchar(x) - 5), "-- skipping"))
        close(con)
        return(NULL)
      })
    })
  seasonHTMLs = seasonHTMLs[!vapply(seasonHTMLs, is.null, TRUE)]
  teamWL = lapply(seasonHTMLs, getTeamWL)

  type = match.arg(type)
  teamStats = lapply(seasonHTMLs, getTeamStats, type = type)

  WL = do.call("rbind", teamWL)
  stats = do.call("rbind", teamStats)

  merge(WL, stats)
}
