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
#' D1<-matrix(runif(10*2), ncol=2)
#' D2<-matrix(runif(100*2), ncol=2)
#' 
#' D1<-as.data.frame(D1)
#' D2<-as.data.frame(D2)
#' rad<-seq(0.1,0.9,0.01)
#' tst<-CrossLocalSB(D1,D2, rad = rad)
#' tst
#' 
#' }
#'
#' @import Rcpp RcppArmadillo
#' @importFrom stats dist lm quantile var
#' @export
#'
CrossLocalSB<-function(data1, data2, rad){
  N1<-nrow(data1)  # centres ...
  N2<-nrow(data2)  # points ...
  in_out1<- matrix(0, nrow=N1, ncol=max(N2,N1))
  for (i in 1:N1){
    cent<-data1[i,]
    for (j in 1:N2){
      poin<-data2[j,]
      in_out1[i,j]<-sqrt(sum((cent-poin)^2))
    }
  } # ça donne la matrice in_out1 qui contient la 
  #distance des centres par rapport au point
  Nbpts <- matrix(0, ncol=length(rad), nrow=N1)
  for (i in 1:length(rad)){
    Nbpts[,i] <- apply(in_out1, 1, function(x) sum(abs(x) <= rad[i]))
  }
  SboxL <- list() # list qui contient les data frame pour chaque point n
  SlopeL <- c()
  Err <- c()
  Nbpts[which(Nbpts==0)]<-1
  vrc <- c()
  for (i in 1:N1){
    SboxL[[i]] <- data.frame(logR=log(rad),logN=log(Nbpts[i,]))
    A <- summary(lm(SboxL[[i]][,2]~SboxL[[i]][,1]))
    SlopeL[i] <- as.numeric(A$coefficients[2,1])
    vrc[i] <- var(Nbpts[i,])
    Err[i] <- as.numeric(A$coefficients[2,2])
  }
  
  Nbpts<-as.data.frame(Nbpts)
  colnames(Nbpts)<-paste('R',round(rad,2) , sep='_' )
  result <- data.frame(data1, Nbpts, Fdim=SlopeL, variance=vrc, lmError=Err)
}