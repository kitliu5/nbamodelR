#' Predict wining probability of each team in a matchup by
#' fitting Bradley-Terry model
#'
#' @details Note that diagonal entries of the return matrix are
#' not exactly 50% because home-court advantage is taken into account.
#' Also, for the Bradley-Terry models here we based much of our code
#' off of
#' \url{http://www.stanford.edu/class/stats50/files/5-bradley-terry-complete.r}
#' and \url{http://www.stanford.edu/class/stats50/files/5-bradley-terry.pdf}.
#'
#' @param GameLog A data frame containing individual game records
#' with the following columns: \code{H.TEAM}, \code{A.TEAM},
#' \code{H.PTS}, and \code{A.PTS}.
#' @param regularize logical. If \code{TRUE} regularization term
#' will be added to predict team strength. It is recommended but
#' can be set to \code{FALSE}.
#' @return A probability matrix with each entry representing the
#' probability of the team in that row defeating the team in that
#' column in a game where the team in the row is the home team, and
#' the team in the column is the away team.
#'
#' @importFrom glmnet cv.glmnet predict.cv.glmnet glmnet predict.glmnet
#'
#' @export
#'
#' @seealso \code{\link{buildPtDiffMatrixBT}} for obtaining a similar matrix with predicted
#' point differentials, and \code{\link{buildModelsBT}} to obtain both types of matrices at once.
#'
#' @examples
#' buildWinProbMatrixBT(gamelogs2017_full)
#' buildWinProbMatrixBT(gamelogs2017_simple)
#'
#' @author kitliu5 and astroud
buildWinProbMatrixBT = function(GameLog, regularize = T){
  checkGamelogColumnsBT(GameLog)
  teams = levels(as.factor(GameLog$H.TEAM))
  Y = GameLog$H.PTS - GameLog$A.PTS
  X = matrix(c(rep(1,nrow(GameLog)), rep(0, nrow(GameLog)*length(teams))),
             nrow = nrow(GameLog), ncol = length(teams) + 1)
  for (i in 1:nrow(X)){
    X[i, 1+ which(teams == GameLog$H.TEAM[i])] = 1
    X[i, 1+ which(teams == GameLog$A.TEAM[i])] = -1
  }

  if (regularize){
    BTmodel = glmnet::cv.glmnet(X, Y > 0, alpha = 0, intercept = TRUE, standardize = TRUE, family = 'binomial')
    beta = glmnet::predict.cv.glmnet(BTmodel, diag(ncol(X)), s = 'lambda.min')
  }
  else {
    ###################
    # begin astroud code
    BTmodel = glmnet::glmnet(X, Y > 0, alpha = 0, lambda = 0, intercept = TRUE, standardize = TRUE, family = 'binomial')
    beta = glmnet::predict.glmnet(BTmodel, diag(ncol(X)))
    # end astroud code
    ##################
    # all other code in this file by kitliu5
  }

  plus = matrix(rep(beta[-1], length(beta)-1), ncol=length(beta)-1)
  minus = matrix(rep(beta[-1], each=length(beta)-1), ncol=length(beta)-1)
  differential = beta[1]+plus-minus
  prob.tmp = exp(differential)/(1+exp(differential))
  prob = data.frame(teams, prob.tmp)
  colnames(prob) = c("Team", teams)

  return(prob)
}

