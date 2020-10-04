cv1=function(X,Y,a, b, c)
{ 
  h <- seq(a, b, by = c)
  s <- length(h)
  CV=matrix(nrow=s,ncol=1)
  for(m in 1:s) {
    g <- length(X)
    Ghat <- rep(0, g)
    n <- length(Y)
    for(j in 1:g) {
      ghat <- 0
      faktor <- 0
      for(i in 1:n) {
        faktor <- faktor + ker((X[j] - X[i])/h[m])
        ghat <- ghat + Y[i] * ker((X[j] - X[i])/h[m])
      }
      if((faktor-ker(0))== 0)
        Ghat [j]<- 0
      else Ghat[j] <- (ghat-ker(0)*Y[j])/(faktor-ker(0))
    }
    cv.h <- 1/n * sum((Y - Ghat)^2)
    CV[m]=cv.h
  }
  R<-matrix(c(h,CV),nrow=s)
  sort.R<-R[order(R[,2]),]
  S<-sort.R[1:10,]
  cat("\nh opt=",S[1,1],"dengan cv minimal=",S[1,2],"\nBerikut 10 nilai cv terkecil beserta nilai bandwidh h:\n")
  cat("\n======================")
  cat("\n  No   h        CV     ")
  cat("\n======================\n")
  S
}
