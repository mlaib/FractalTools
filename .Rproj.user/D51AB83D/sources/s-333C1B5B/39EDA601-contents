#' Simulated uniform matrix
#'
#' Gives a matrix of uniform variables
#' @usage unidat(n=1000, d=20)
#' @param n Number of points.
#' @param d Number of variables.
#'
#' @return A matrix of n*d
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' raw_data<-unidat(n=1000, d=10)
#' dim(raw_data)
#' }
#'
#' @importFrom stats runif
#' @export
#'

unidat <- function(n=1000, d=20){
  dat<-matrix(0, ncol=d, nrow=n)
  for (i in 1:d){
    dat[,i]<-runif(n)
  }
  return(dat)
}
