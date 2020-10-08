LocalPolynomial<-function(p,x0,h){
  n=length(X)
  X_x0=X-x0
  matrix_X=matrix(0,ncol=p+1,nrow=n)
  for (i in 1:(p+1))
    matrix_X[,i]=X_x0^(i-1)
  W=matrix(0,ncol=n,nrow=n)
  for (g in 1:n)
    W[g,g]=ker(X_x0[g]/h)
  inverse=MPL(t(matrix_X)%*%W%*%matrix_X)
  beta=inverse%*%t(matrix_X)%*%W%*%Y
  m_hat=matrix_X%*%beta
  H_hat=matrix_X%*%inverse%*%t(matrix_X)%*%W
  MSE=(t(Y-m_hat)%*%(Y-m_hat))/n
  I=matrix(0,ncol=n,nrow=n)
  for (g in 1:n)
    I[g,g]=1
  GCV=(n^2*MSE)/(sum(diag(I-H_hat)))^2
  cat("=========================================","\n")
  cat("       Estimasi Parameter Lokal          ","\n")
  print(beta)
  cat("=========================================","\n")
  cat("dengan nilai X0 =",x0,"\n")
  cat("Polinomial order =",p+1,"\n")
  cat("Bandwidth =",h,"\n")
  cat("GCV =",GCV,"\nMSE =",MSE, "\n")
  cat("=========================================","\n")
  cat("    Estimasi Y")
  cat("\n================\n")
  print(m_hat)
  cat("================\n")
  win.graph()
  plot(X,Y,type="p",xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab=" ",ylab=" ")
  par(new=T)
  plot(X,m_hat,type="l",col="red",xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab="USIA",ylab="KADAR KOLESTEROL")
}
