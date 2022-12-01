#' @title A illustration dataset
#' @name data
#' @description A dataset used to illustrate the performance of \code{shortestR} and \code{shortestC}.
#' @examples
#' \dontrun{
#' data(data)
#' tm <- microbenchmark::microbenchmark(
#'   shortestR = shortestR(M),
#'   shortestC = shortestC(M)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @examples
#' \dontrun{
#' data(data)
#' tm1 <- microbenchmark::microbenchmark(shortestR = shortestR(M), shortestC = shortestC(M))
#' summary(tm1)[,c(1,3,5,6)]
#' 
#' tm2 <- microbenchmark::microbenchmark(gibbR = gibbsR(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9), gibbC = gibbsC(100, 0, 0, 1, 1, 0.9))
#' summary(tm2)[,c(1,3,5,6)]
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib StatComp22064
NULL

#' @title Calculate the shortest path problem between cities using R.
#' @description The prediction model is described in https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/examples.html#examples-scicomp-citylink.
#' @param M the number of the city connected. It's a two-column matrix. The first number of each row is less than the second one.
#' @return a matrix of size \code{max(M)} whose elements are the minimum distances between cities.
#' @examples
#' \dontrun{
#' data(data)
#' distanceR <- shortestR(M)
#' }
#' @export
shortestR <- function(M){
  n<-max(M)
  A<-matrix(Inf,n,n)
  for(i in 1:nrow(M)){A[M[i,1],M[i,2]]<-A[M[i,2],M[i,1]]<-1}
  diag(A)<-0
  
  while(TRUE){
    B<-A
    for(i in 1:n){
      for(j in 1:n){
        for(k in 1:n){
          if(A[i,j]>A[i,k]+A[k,j]){
            A[i,j]<-A[i,k]+A[k,j]
          }
        }
      }
    }
    if(identical(B,A)){break}else{B<-A}
  }
  return(A)
}

#' @title A Gibbs sampler for two-dimensional normal random numbers using R
#' @description A Gibbs sampler for two-dimensional normal random numbers using R
#' @param N the number of samples
#' @param mu1 the mean of the first dimension
#' @param mu2 the mean of the second dimension
#' @param sigma1 the standard deviation of the first dimension
#' @param sigma2 the standard deviation of the second dimension
#' @param pho the correlation of two dimensions
#' @return a random sample of size \code{N}
#' @examples
#' \dontrun{
#' rnR <- gibbsR(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9)
#' par(mfrow=c(1,3));
#' plot(rnR[,1],type='l')
#' plot(rnR[,2],type='l')
#' plot(rnR)
#' }
#' @export
gibbsR <- function(N, mu1, mu2, sigma1, sigma2, pho) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- 0
  y <- 0
  s1 = sqrt(1-pho * pho) * sigma1
  s2 = sqrt(1-pho * pho) * sigma2
  for (i in 1:N) {
    m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
    x = rnorm(1, m1, s1);
    mat[i, 1] = x;
    
    m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
    y = rnorm(1, m2, s2);
    mat[i, 2] = y;
  }
  mat
}
