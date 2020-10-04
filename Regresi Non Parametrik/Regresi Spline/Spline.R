model.spline=function(prediktor,respon,m,knots=c(...))
{
  y<-respon
  n<-length(y)
  k<-length(knots)
  w<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w[,i]<-prediktor^(i-1)
  for(i in (m+1):(m+k))
    w[,i]<-trun(prediktor,knots[i-m],m-1)
  wtw<-t(w)%*%w
  Z<-MPL(wtw)
  beta<-Z%*%t(w)%*%y
  yfits<-w%*%beta
  res<-y-yfits
  MSE<-t(y-yfits)%*%(y-yfits)/n
  I<-matrix(0,ncol=n,nrow=n)
  for(i in 1:n)
    I[i,i]<-1
  h<-w%*%MPL(wtw)%*%t(w)
  GCV<-(n^2*MSE)/(sum(diag(I-h)))^2
  q<-seq(min(prediktor),max(prediktor),length=1000)
  u<-matrix(0,ncol=m+k,nrow=1000)
  cat("\n Spline orde",m)
  cat("\n Titik Knots  = c( ",format(knots),")")
  cat("\n Nilai GCV    = ",format(GCV),
      "\n Nilai MSE    = ",format(MSE),"\n")
  cat("\n ******************************************************************")
  cat("\n      Koefisen         Estimasi")
  cat("\n ******************************************************************")
  for(i in 1:(m+k))
    cat("\n     beta[",i-1,"]          ",format(beta[i]))
  cat("\n ******************************************************************")
  par(mfrow=c(1,1))
  z0=cbind(prediktor,respon)
  z1=z0[order(z0[,1]),]
  x1=z1[,1]
  y1=z1[,2]
  w1<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w1[,i]<-x1^(i-1)
  for(i in (m+1):(m+k))
    w1[,i]<-trun(x1,knots[i-m],m-1)
  yfits1<-w1%*%beta
  plot(x1,y1, type="p",xlim=c(min(prediktor),max(prediktor)),ylim=c(180,220),
       xlab="usia pasien(tahun)",ylab="Kadar gula darah")
  par(new=T)
  plot(x1,yfits1, type="l",col="red",
       xlim=c(min(prediktor),max(prediktor)),
       ylim=c(180,220),
       xlab="  ",ylab="  ")
}


gcv1(y,x,2,1)
model.spline(x,y,2,c(54))

