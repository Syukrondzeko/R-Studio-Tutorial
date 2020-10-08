gcv_LocalPolynomial = function(X,Y,band,width,p,u,v){
  n=length(X)
  x0=matrix(seq((min(X)-u),(max(X)+v),by=1),ncol=1)
  z=length(x0)
  q=c(seq(band,width,by=1))
  k=length(q)
  M=matrix(nrow=(z*k),ncol=4)
  for (j in 1:z){
    a=rep(x0[j],n) 
    X_x0=X-a
    h=c(seq(band,width,by=1))
    s=length(h)
    Gcv<-matrix(nrow=s,ncol=1)
    Mse<-matrix(nrow=s,ncol=1)
    X_nol=rep(x0[j],s)
    for (m in 1:s){
      matrix_X=matrix(0,ncol=p+1,nrow=n)
      for (i in 1:(p+1))
        matrix_X[,i]=X_x0^(i-1)
      W=matrix(0,ncol=n,nrow=n)
      for (g in 1:n)
        W[g,g]=ker(X_x0[g]/h[m])
      inverse=MPL(t(matrix_X)%*%W%*%matrix_X)
      beta=inverse%*%t(matrix_X)%*%W%*%Y
      m_hat=matrix_X%*%beta
      H_hat=matrix_X%*%inverse%*%t(matrix_X)%*%W
      MSE=(t(Y-m_hat)%*%(Y-m_hat))/n
      I=matrix(0,ncol=n,nrow=n)
      for (g in 1:n)
        I[g,g]=1
      GCV=(n^2*MSE)/(sum(diag(I-H_hat)))^2
      Gcv[m]=GCV
      Mse[m]=MSE
    }
    R<-matrix(c(X_nol,h,Gcv,Mse),nrow=s)
    if (j==1){
      M[1:s,]=R}
    else {
      M[(((j-1)*s)+1):(j*s),]=R}
  }
  sort.M<-M[order(M[,3]),]
  S<-sort.M[1:10,]
  cat("Running program untuk order =",p+1,"\n")
  cat("=========================================","\n")
  cat ("       Xo     h     GCV      \n")
  cat("=========================================","\n")
  print(S)
  cat("=========================================","\n")
}

