gcv1<-function(y,x,m,l)
{
  a<-min(x)+1
  b<-max(x)-1
  k<-seq(a,b,l)
  v<-length(k)
  n<-length(y)
  Gcv<-matrix(nrow=v,ncol=1)
  Mse<-matrix(nrow=v,ncol=1)
  for (j in 1:v)
  {
    w<-matrix(0,ncol=m+1,nrow=n)
    for (i in 1:m)
      w[,i]<-x^(i-1)
    for (i in m+1)
      w[,i]<-trun(x,k[j],m-1)
    wtw<- t(w) %*% w
    z<- MPL(wtw) 
    beta<- z %*% (t(w) %*% y)
    h<- w %*% z %*% t(w)
    mu<-w%*%beta
    MSE<- t(y-mu) %*% (y-mu)/n
    I<-matrix(0,ncol=n,nrow=n)
    for(i in 1: n)
      I[i,i]<-1
    GCV<-(n^2*MSE)/(sum(diag(I-h)))^2
    Gcv[j]<-GCV
    Mse[j]<-MSE
  }
  R<-matrix(c(k,Gcv,Mse),ncol=3)
  sort.R<-R[order(R[,2]),]
  S<-sort.R[1:10,]
  cat("Untuk spline order",m,"dengan 1 titik knot, diperoleh knot optimal=",S[1,1]," dengan GCV minimum=",S[1,2],"dan MSE =",S[1,3])
  cat("\nBerikut 10 nilai GCV terkecil, nilai MSE dan letak titik knotnya:\n")
  cat("====================================\n")
  cat("  No  Ttk knot   GCV     MSE   \n")
  cat("====================================\n")
  S
}

