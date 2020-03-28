#' Probabilistic Composition of Correlated Preference
#'
#' \code{cppcor} function that returns the confusion matrix and parameters of classification analysis
#' @param dataset  Data frame object
#' @param ID Logical argument, \code{TRUE} or \code{FALSE}. The default is \code{FALSE}
#' @param cores The number of cores to use for parallel execution. The default is 1.
#' @return \code{cppcor} return the confusion matriz and parameters of classification analysis
#' @details The \code{dataset} argument must be a data frame object, and the last column must
#' be the classes of the evaluated elements.
#' The \code{ID} argument must be \code{FALSE} if the data are correlated and \code{TRUE} if the data
#' are independents.
#' @examples
#' # Seed
#' set.seed(10)
#' c1 <- matrix(rnorm(30, mean = c(70,80,90), sd = 30), 10, 3, byrow = TRUE)
#' c2 <- matrix(rnorm(45, mean = c(30,40,50), sd = 10), 15, 3, byrow = TRUE)
#' # Data set
#' dataset <- as.data.frame(cbind(rbind(c1,c2), c(rep(1, 10), rep(2, 15))))
#' colnames(dataset) <- c("Var1", "Var2", "Var3", "Class")
#' # Loading package
#' library(cppcor)
# \donttest{
#' cppcor(dataset, ID = FALSE)
#' @import "foreach" "mvtnorm"
#' @importFrom "caret" "confusionMatrix"
#' @importFrom "e1071" "classAgreement"
#' @importFrom "stats" "cor" "var"
#' @importFrom "parallel" "makePSOCKcluster" "stopCluster"
#' @importFrom "doParallel" "registerDoParallel"
#' @export

cppcor <- function(dataset, ID = FALSE, cores = 1) {

  # Parallel on
  cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cluster)


  # Dataset
  rm <- dataset

  # Dimension of rm
  d <- dim(rm)

  # Labels
  r <- rm[,d[2]]

  # Evaluation matrix
  ma <- rm[1:d[1], 1:(d[2] - 1)]

  # Names of cols of rm
  colnames(rm) <- c(1:(d[2] - 1),"rot")


  # Means of labels
  aux <- as.numeric(names(table(r)))
  rotmean <- matrix(0, length(aux), d[2] - 1)
  rmaux <- data.frame(rm)
  for (i in 1:length(aux)) {
    rotmean[i,] <- colMeans(subset(rmaux, rot == aux[i])[,-d[2]])
  }

  #Dimensions of rotmean
  drm <- dim(rotmean)

  # Variance of labels
  aux <- as.numeric(names(table(r)))
  rotvar <- matrix(0, length(aux), d[2] - 1)
  rmaux <- data.frame(rm)
  for (i in 1:length(aux)) {
    rotvar[i,] <- apply(subset(rmaux, rot == aux[i])[,-d[2]], 2, var)
  }


  # Position of central label
  tab <- table(rm$rot)
  posc <- which(tab == max(tab))


  # Central label mean
  clabel <- rotmean[posc,]

  # Adjusted Matrix
  decision <- posc == 1 | posc == drm[1]
  if (decision == TRUE) {
    if (posc == 1) { # Posicao inicial
      for (i in 1:drm[2]) {
        aux <- rotmean[1:(drm[1] - 1),i] > rotmean[2:drm[1],i]
        if (length(aux[aux == TRUE]) >= 1) rotmean[,i] <- -rotmean[,i]
        for (j in 2:drm[1]) {
          if (rotmean[j,i] < rotmean[j - 1,i])  rotmean[j,i] <- rotmean[j - 1,i]
        }
      }
    }else{# Posicao final
      for (i in 1:drm[2]){
        aux <- rotmean[1:(drm[1] - 1),i] > rotmean[2:drm[1],i]
        if (length(aux[aux == TRUE]) >= 1) rotmean[,i] <- -rotmean[,i]
        for (j in (drm[1] - 1):1) {
          if (rotmean[j,i] > rotmean[j + 1,i])  rotmean[j,i] <- rotmean[j + 1,i]
        }
      }
    }
  } else{
    for (i in 1:drm[2]){
      aux <- rotmean[posc:(drm[1] - 1),i] > rotmean[(posc + 1):drm[1],i]
      if (length(aux[aux == TRUE]) >= 1) rotmean[,i] <- -rotmean[,i]
      #Up
      for (j in (posc -1):1) {
        if (rotmean[j,i] > rotmean[j + 1,i])  rotmean[j,i] <- rotmean[j + 1,i]
      }
      #Down
      for (j in (posc + 1):drm[1]) {
        if (rotmean[j,i] < rotmean[j - 1,i])  rotmean[j,i] <- rotmean[j - 1,i]
      }
    }
  }

  # Ajusted data
  aux <- as.numeric(names(table(r)))
  meanrm <- matrix(0, drm[1], drm[2])

  for (i in 1:drm[1]){
    meanrm[i,] <- apply(subset(rmaux, rot == aux[i], -rot), 2, mean)
  }
  # Position of adjusted data
  before <- meanrm[1,] > 0
  after <- rotmean[1,] > 0
  ##
  pos <- which(!(before == after))

  # Adjusted data
  x <- as.matrix(rm)
  ajrmaux <- x[,-d[2]]
  ajrmaux[,pos] <- -ajrmaux[,pos]

  # Correlation of labels
  aux <- as.numeric(names(table(r)))
  rotcor <- array(0, c(d[2] - 1, d[2] - 1, length(aux)))
  rmaux <- data.frame(ajrmaux, rot = rm$rot)
  for (i in 1:length(aux)) {
    rotcor[,,i] <- cor(subset(rmaux, rot == aux[i])[,-d[2]])
  }

  # Lower probability
  plower <- function(i, j, ID = ID, ...){

    # Variables independents or not
    if (ID == TRUE) {
      pm <- mvtnorm::pmvnorm(lower = rep(-Inf, d[2] - 1), upper = rotmean[j,],
                             mean = as.vector(ajrmaux[i,]), corr = NULL, sigma = diag(rotvar[j,]),
                             algorithm = mvtnorm::GenzBretz())[1]
    }
    if (ID == FALSE) {
      pm <- mvtnorm::pmvnorm(lower = rep(-Inf, d[2] - 1), upper = rotmean[j,],
                             mean = as.vector(ajrmaux[i,]), corr = rotcor[,,j], sigma = NULL,
                             algorithm = mvtnorm::GenzBretz())[1]
    }

    return(pm)
  }

  # Up probability
  pup <- function(i, j, ID = ID, ...){

    # Variables independents or not
    if (ID == TRUE) {
      pm <- mvtnorm::pmvnorm(lower = rotmean[j,], upper = rep(+Inf, d[2] - 1),
                             mean = as.vector(ajrmaux[i,]), corr = NULL, sigma = diag(rotvar[j,]),
                             algorithm = mvtnorm::GenzBretz())[1]
    }
    if (ID == FALSE) {
      pm <- mvtnorm::pmvnorm(lower = rotmean[j,], upper = rep(+Inf, d[2] - 1),
                             mean = as.vector(ajrmaux[i,]), corr = rotcor[,,j], sigma = NULL,
                             algorithm = mvtnorm::GenzBretz())[1]
    }

    return(pm)
  }



  plow <- foreach::foreach(i = 1:d[1], .combine = rbind) %:% foreach::foreach(j = 1:drm[1]) %dopar% plower(i, j, ID = ID)
  pu <- foreach(i = 1:d[1], .combine = rbind) %:% foreach(j = 1:drm[1]) %dopar% pup(i, j, ID = ID)

  # Transform in vector = > matrix
  x <- as.numeric(plow)
  y <- as.numeric(pu)
  pabs <- abs(matrix(x, d[1], drm[1]) - matrix(y, d[1], drm[1]))
  ##
  res <- apply(pabs, 1, which.min)
  resaux <- factor(aux[res], levels = aux)
  Prediction <- resaux

  # Parallel off
  parallel::stopCluster(cluster)

  # Table of comparison
  tabaux <- table(Prediction, rm$rot)

  confmat <- caret::confusionMatrix(tabaux)


  return(confmat)
}


