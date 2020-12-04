#' Predict point differential in a matchup between two teams by
#' fitting Bradley-Terry model
#'
#' @details Note that for the Bradley-Terry models here we based much of our code
#' off of \url{http://www.stanford.edu/class/stats50/files/5-bradley-terry-complete.r}
#' and \url{http://www.stanford.edu/class/stats50/files/5-bradley-terry.pdf}.
#'
#' @param GameLog A data frame containing individual game records
#' with the following columns: \code{H.TEAM}, \code{A.TEAM},
#' \code{H.PTS}, and \code{A.PTS}.
#' @param regularize logical. If \code{TRUE} regularization term
#' will be added to predict team strength. It is recommended but
#' can be omitted.
#' @return A matrix with each entry representing the point differential in a
#' matchup of the team in that row at home against the team in that column on the
#' road. Note that the diagonal terms are estimates of the home court advantage
#' coefficient.
#'
#' @importFrom glmnet cv.glmnet predict.cv.glmnet
#'
#' @export
#'
#' @seealso \code{\link{buildWinProbMatrixBT}} for obtaining a similar matrix with predicted
#' win probabilities, and \code{\link{buildModelsBT}} to obtain both types of matrices at once.
#'
#' @examples
#' buildPtDiffMatrixBT(gamelogs2017_full)
#' buildPtDiffMatrixBT(gamelogs2017_simple, regularize = FALSE)
#'
#' @author kitliu5 astroud
buildPtDiffMatrixBT = function(GameLog, regularize = TRUE){
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
    ########################################################################
    # begin astroud contribution -- only added glmnet:: notation to function
    BTmodel = glmnet::cv.glmnet(X, Y, alpha = 0, intercept = TRUE, standardize = TRUE)
    beta = glmnet::predict.cv.glmnet(BTmodel, diag(ncol(X)), s = 'lambda.min')
    # end astroud contribution, all other code in this file by kitliu5
    ##################################################################
  }
  else{
    beta.tmp = solve(t(X[,-2])%*%X[,-2])%*%t(X[,-2])%*%Y
    beta = c(beta.tmp[1], 0, beta.tmp[-1])
  }

  plus = matrix(rep(beta[-1], length(beta)-1), ncol=length(beta)-1)
  minus = matrix(rep(beta[-1], each=length(beta)-1), ncol=length(beta)-1)
  differential.tmp = beta[1]+plus-minus
  differential = data.frame(teams, differential.tmp)
  colnames(differential) = c("Team", teams)

  return(differential)
}
