#' Global fractality by fixing a radial (Sand-Box method)
#'
#' Calculate the fractal dimension of a dataset by using the Sand-Box method
#' @usage GlobalSB(data, rad)
#' @param data Data of class: \code{matrix} or \code{data.frame}.
#' @param rad Vector containning values of the radial.
#'
#' @return A list containing
#'  \itemize{
#'   \item \code{Fdim} Computed fractal dimension.
#'   \item \code{SB} Data frame of log (radial) and the log (number of points).
#'   }
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' data <- unidat(n=10000, d=20)
#' data<-apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.7,0.9,0.01)
#' A<-GlobalSB(data, rad)
#' A$Fdim
#' plot(A[[2]][,1],A[[2]][,2],type="b",pch=16,lwd=2,xlab="log(Rad)",ylab="log(N)",
#'       main="Sandbox plot",  cex.main=1.5, las=1, axes=F)
#' axis(1)
#' axis(2)
#' grid()
#' Regline <- lm(A[[2]][,2]~A[[2]][,1])
#' abline(Regline, col="red", lwd=2)
#' legend("bottomright",paste("Fractal dimension:", round(A$Fdim,2)), bty="n")
#'
#'
#' #### IDmining examples #####
#' library(IDmining)
#'
#' #### SwissRoll dataset ####
#'
#' data<-SwissRoll(N=1000)
#' data<-apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.1,0.31,0.01)
#' A<-GlobalSB(data, rad)
#' A$Fdim
#' plot(A[[2]][,1],A[[2]][,2],type="b",pch=16,lwd=2,xlab="log(Rad)",ylab="log(N)",
#'       main="Sandbox plot",  cex.main=1.5, las=1, axes=F)
#' axis(1)
#' axis(2)
#' grid()
#' Regline <- lm(A[[2]][,2]~A[[2]][,1])
#' abline(Regline, col="red", lwd=2)
#' legend("bottomright",paste("Fractal dimension:", round(A$Fdim,2)), bty="n")
#'
#' #### Butterfly dataset ####
#' data <- Butterfly(1000)[,-9]
#' data<-apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.1,0.31,0.01)
#' A<-GlobalSB(data, rad)
#' A$Fdim
#' plot(A[[2]][,1],A[[2]][,2],type="b",pch=16,lwd=2,xlab="log(Rad)",ylab="log(N)",
#'       main="Sandbox plot",  cex.main=1.5, las=1, axes=F)
#' axis(1)
#' axis(2)
#' grid()
#' Regline <- lm(A[[2]][,2]~A[[2]][,1])
#' abline(Regline, col="red", lwd=2)
#' legend("bottomright",paste("Fractal dimension:",
#'          round(Regline$coefficient[2],2)), bty="n")
#'
#' }
#'
#' @import Rcpp RcppArmadillo bigmemory biganalytics 
#' @importFrom stats dist lm quantile
#' @useDynLib FractalTools, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
#' @export
#'
#'
GlobalSB<-function (data, rad){
  n<-nrow(data)
  data <- as.big.matrix(data)
  bm_out <- bigDist(data, n)
  #Nbpts <- big.matrix(1, length(rad), type="double", init=0)
  Nbpts <- c()
  for (i in 1:length(rad)){
    Nbpts[i] <- mean(biganalytics::apply(bm_out, 2, function(x) sum(abs(x) <= rad[i])))
  }
  rm(bm_out)
  gc()
  Nbpts[which(Nbpts==0)]<-1
  Sbox <- data.frame(logR=log(rad),logN=log(Nbpts))
  Slope <- as.numeric(lm(Sbox[,2]~Sbox[,1])$coefficients[2])
  
  return(list("Fdim" = Slope, "SB"= Sbox))
}

#sourceCpp("src/FractalTools.cpp")
bigDist <- function(bigMat, n){
  zeros <- big.matrix(nrow = n,
                      ncol = n,
                      init = 0,
                      type = "double")
  BigArmaEuc(bigMat@address, zeros@address)
  return(zeros)
}