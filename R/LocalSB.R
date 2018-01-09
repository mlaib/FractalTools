#' Local fractality (Sand-Box method) by fixing radial
#'
#' Calculate the fractal dimension of a dataset by using the Sand-Box method
#' @usage LocalSB(data, Rad)
#' @param data Data of class: \code{matrix} or \code{data.frame}.
#' @param Rad Vector containning values of the radial.
#'
#' @return A data.frame contains: data, columns at each radials. The last
#'      presents the Fractal dimension of each points.
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' #### Uniform variables ####
#'
#' data <- unidat(n=10000, d=20)
#' data<-apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.1,1,0.01)
#' A<-LocalSB(data, rad)
#' A$Results$Fdim
#'
#'
#' #### IDmining examples #####
#' library(IDmining)
#'
#' #### SwissRoll dataset ####
#'
#' data<-SwissRoll(N=1000)
#' data<-apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.1,1,0.01)
#' A<-LocalSB(data, rad)
#' A$Results$Fdim
#' #### Butterfly dataset ####
#' data <- Butterfly(1000)[,-9]
#' data <- apply(data,MARGIN=2,FUN = function(X) (X - min(X))/diff(range(X)))
#' rad<-seq(0.1,0.31,0.01)
#' A<-LocalSB(data, rad)
#' A$Results$Fdim
#'
#' }
#'
#' @import Rcpp RcppArmadillo bigmemory biganalytics 
#' @importFrom stats dist lm quantile var
#' @useDynLib FractalTools, .registration = TRUE
#' @export
#'

LocalSB <- function (data, Rad){
  
  n<-nrow(data)
  data <- as.big.matrix(data)
  bm_out <- bigDist(data, n)
  
  Nbpts <- matrix(0, ncol=length(Rad), nrow=n)
  for (i in 1:length(Rad)){
    Nbpts[,i] <- biganalytics::apply(bm_out, 2, function(x) sum(abs(x) <= Rad[i]))
  }
  rm(bm_out)
  gc()
  SboxL <- list() # list qui contient les data frame pour chaque point
  SlopeL <- c()
  Nbpts[which(Nbpts==0)]<-1
  vrc <- c()
  Err <- c()
  for (i in 1:n){
    SboxL[[i]] <- data.frame(logR=log(Rad),logN=log(Nbpts[i,]))
    A <- summary(lm(SboxL[[i]][,2]~SboxL[[i]][,1]))
    SlopeL[i] <- as.numeric(A$coefficients[2,1])
    vrc[i] <- var(Nbpts[i,])
    Err[i] <- as.numeric(A$coefficients[2,2])
  }
  
  Nbpts<-as.data.frame(Nbpts)
  colnames(Nbpts)<-paste('R',round(Rad,2) , sep='_' )
  result <- data.frame(data[], Nbpts, Fdim=SlopeL, variance=vrc, 
                       lmError=Err)
  
  return(result)
}

