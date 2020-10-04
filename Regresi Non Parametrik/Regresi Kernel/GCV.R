gcv=function(X,Y,a, b, c)
{
  h <- seq(a, b, by = c)
  s <- length(h)
  GCV=matrix(nrow=s,ncol=1)
  Gcv=matrix(nrow=s,ncol=1)
  MSE=matrix(nrow=s,ncol=1)
  n=length(X)
  e2=matrix(nrow=n,ncol=1)
  e1=matrix(nrow=n,ncol=1)
  hi=matrix(ncol=1)
  for(m in 1:s) {
    g <- length(X)
    Ghat <- rep(0, g)
    n <- length(Y)
    gcv.h=0
    mse=0
    gcv=0
    for(i in 1:g) {
      e22=0
      e11=0
      ghat <- 0
      faktor <- 0
      for(j in 1:n) {
        faktor <- faktor + ker((X[i] - X[j])/h[m])
        ghat <- ghat + Y[j] * ker((X[i] - X[j])/h[m])
      }
      if(faktor== 0)
        Ghat [i]<- 0
      #else Ghat[i] <- (ghat-ker(0)*Y[i])
      else Ghat[i] <- (ghat/faktor)
      hi[i]=ker(0)/faktor
      hii=hi[i]^2
      #e22=(Y[i]-Ghat[i])^2/(n*(1-hi[i])^2)
      #e2[i]=e22
      e11=(Y[i]-Ghat[i])^2
      e1[i]=e11
      gcv.h=gcv.h+e11
    }
    mse=(sum(e1))/n
    GCV[m]=(n*gcv.h)/(n-sum(hi))^2
    MSE[m]=mse
  }
  R<-matrix(c(h,GCV,MSE),ncol=3)
  sort.R1<-R[order(R[,2]),]
  S<-sort.R1[1:10,]
  cat("\nh opt=",S[1,1],"dengan GCV minimal=",S[1,2],"dan MSE=",S[1,3],"\nBerikut 10 nilai GCV terkecil dan MSE beserta nilai bandwidh h:\n")
  cat("\n============================")
  cat("\n  No   h     GCV     MSE  ")
  cat("\n============================\n")
  S
}
