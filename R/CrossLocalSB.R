#' Cross Local fractality (Sand-Box method) by fixing radial
#'
#' Calculate the cross local fractal dimension between two
#'  datasets by using the Sand-Box method.
#' @usage CrossLocalSB(data1, data2, rad)
#' @param data1 First dataset. Data of class: \code{matrix} or 
#'    \code{data.frame}.
#' @param data2 Second dataset. Data of class: \code{matrix} or 
#'    \code{data.frame}.
#' @param rad Vector containning values of the radial.
#'
#' @return A data.frame contains: data, columns at each radials. The last
#'      presents the Fractal dimension of each points.
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' # OK :: put it here
#' }
#'
#' @import Rcpp RcppArmadillo
#' @importFrom stats dist lm quantile var
#' @useDynLib FractalTools
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @export
#'
CrossLocalSB<-function(data1, data2, rad){
  ## Working on it
}