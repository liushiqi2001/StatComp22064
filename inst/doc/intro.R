## ----eval=FALSE---------------------------------------------------------------
#  shortestR <- function(M){
#    n<-max(M)
#    A<-matrix(Inf,n,n)
#    for(i in 1:nrow(M)){A[M[i,1],M[i,2]]<-A[M[i,2],M[i,1]]<-1}
#    diag(A)<-0
#  
#    while(TRUE){
#      B<-A
#      for(i in 1:n){
#        for(j in 1:n){
#          for(k in 1:n){
#            if(A[i,j]>A[i,k]+A[k,j]){
#              A[i,j]<-A[i,k]+A[k,j]
#            }
#          }
#        }
#      }
#      if(identical(B,A)){break}else{B<-A}
#    }
#    return(A)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  NumericMatrix shortestC(NumericMatrix M){
#    M=M-1;
#    int n=max(M(_,1))+1;
#    NumericMatrix A(n,n);
#    A.fill(R_PosInf);
#    A.fill_diag(0);
#    for(int i=0;i<M.nrow();i++){
#      A(M(i,0),M(i,1))=1;
#      A(M(i,1),M(i,0))=1;
#    }
#  
#    while(true){
#      NumericMatrix B=clone(A);
#      for(int i=0;i<n;i++){
#        for(int j=0;j<n;j++){
#            for(int k=0;k<n;k++){
#              if(A(i,j)>A(i,k)+A(k,j)){
#                A(i,j)=A(i,k)+A(k,j);
#              }
#            }
#        }
#      }
#      if(sum(A!=B)==0){break;}else{B=clone(A);}
#    }
#    return A;
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp22064)
library(microbenchmark)
data(data)
tm2 <- microbenchmark(
  shortestR = shortestR(M),
  shortestC = shortestC(M)
)
knitr::kable(summary(tm2)[,c(1,3,5,6)])

## ----eval=FALSE---------------------------------------------------------------
#  gibbsR <- function(N, mu1, mu2, sigma1, sigma2, pho) {
#    mat <- matrix(nrow = N, ncol = 2)
#    x <- 0
#    y <- 0
#    s1 = sqrt(1-pho * pho) * sigma1
#    s2 = sqrt(1-pho * pho) * sigma2
#    for (i in 1:N) {
#      m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
#      x = rnorm(1, m1, s1);
#      mat[i, 1] = x;
#  
#      m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
#      y = rnorm(1, m2, s2);
#      mat[i, 2] = y;
#    }
#    mat
#  }

## ----eval=FALSE---------------------------------------------------------------
#  NumericMatrix gibbsC(int N, double mu1, double mu2, double sigma1, double sigma2, double pho) {
#    NumericMatrix mat(N, 2);
#    double s1 = sqrt(1-pho * pho) * sigma1, s2 = sqrt(1-pho * pho) * sigma2;
#    double x = 0, y = 0;
#    for(int i = 0; i < N; i++) {
#      double m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
#      x = rnorm(1, m1, s1)[0];
#      mat(i, 0) = x;
#  
#      double m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
#      y = rnorm(1, m2, s2)[0];
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }

## ----eval=TRUE----------------------------------------------------------------
tm2 <- microbenchmark(
  vR = gibbsR(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9),
  vC = gibbsC(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9)
)
knitr::kable(summary(tm2)[,c(1,3,5,6)])

