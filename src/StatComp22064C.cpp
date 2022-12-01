#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler for two-dimensional normal random numbers using R
//' @description A Gibbs sampler for two-dimensional normal random numbers using R
//' @param N the number of samples
//' @param mu1 the mean of the first dimension
//' @param mu2 the mean of the second dimension
//' @param sigma1 the standard deviation of the first dimension
//' @param sigma2 the standard deviation of the second dimension
//' @param pho the correlation of two dimensions
//' @return a random sample of size \code{N}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9)
//' par(mfrow=c(1,3));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' plot(rnC)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, double mu1, double mu2, double sigma1, double sigma2, double pho) {
  NumericMatrix mat(N, 2);
  double s1 = sqrt(1-pho * pho) * sigma1, s2 = sqrt(1-pho * pho) * sigma2;
  double x = 0, y = 0;
  for(int i = 0; i < N; i++) {
    double m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
    x = rnorm(1, m1, s1)[0];
    mat(i, 0) = x;

    double m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
    y = rnorm(1, m2, s2)[0];
    mat(i, 1) = y;
  }
  return(mat);
}

//' @title Calculate the shortest path problem between cities using Rcpp.
//' @description The prediction model is described in https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/examples.html#examples-scicomp-citylink.
//' @param M the number of the city connected. It's a two-column matrix. The first number of each row is less than the second one.
//' @return a matrix of size \code{max(M)} whose elements are the minimum distances between cities.
//' @examples
//' \dontrun{
//' data(data)
//' distanceC <- shortestC(M)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix shortestC(NumericMatrix M){
  M=M-1; 
  int n=max(M(_,1))+1; 
  NumericMatrix A(n,n);
  A.fill(R_PosInf);
  A.fill_diag(0);
  for(int i=0;i<M.nrow();i++){
    A(M(i,0),M(i,1))=1;
    A(M(i,1),M(i,0))=1;
  }
  
  while(true){
    NumericMatrix B=clone(A);
    for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
          for(int k=0;k<n;k++){
            if(A(i,j)>A(i,k)+A(k,j)){
              A(i,j)=A(i,k)+A(k,j);
            }
          }
      }
    }
    if(sum(A!=B)==0){break;}else{B=clone(A);}
  }
  return A;
}

