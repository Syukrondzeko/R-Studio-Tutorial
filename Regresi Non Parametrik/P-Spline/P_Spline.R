#Input Data
data=read.table("clipboard",header=T)

#Menampilkan Data
View(data)

#Plot variabel x dan y
plot(data$P2,data$IPM)

#Menghitung invers dengan Singular Value Decomposition
ginverse<-function(x,eps=1e-016)
{
  x<-as.matrix(x)
  xsvd<-svd(x)
  diago<-xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus<-as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  {
    xplus<-
      xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}

#Quantil dari variabel prediksi sebagai calon titik knot
quant<-function(pred,P)
{
  r<-quantile(pred,seq(0,1,by=1/P))
  return(r)
}

#Fungsi truncated
trun<-function(pred,k,m)
{
  pred[pred<k]<-k
  b<-(pred-k)^m
  return(b)
}

#Membentuk matriks X
matrikx<-function(pred,m,k)
{
  predbaru<-(unique(pred))
  n<-length(pred)
  w<-quant(predbaru,k+1)
  z1<-matrix(0,n,m+1)
  for(i in 1:(m+1))
  {
    z1[,i]<-pred^(i-1)
  }
  z2<-matrix(0,n,k)
  for(j in 1:k)
  {
    z2[,j]<-trun(pred,w[j+1],m)
  }
  x<-cbind(z1,z2)
  return(x)
}

#Membentuk matrix D
matrikd<-function(m,k)
{
  d1<-matrix(0,m+1,m+k+1)
  d2<-matrix(0,k,m+1)
  d3<-diag(k)
  d4<-cbind(d2,d3)
  d<-rbind(d1,d4)
  return(d)
}

#Matriks Beta
beta<-function(respon,pred,m,k,lam)
{
  n<-length(respon)
  y<-as.vector(respon)
  x<-matrikx(pred,m,k)
  d<-matrikd(m,k)
  b1<-(t(x)%*%x)+(n*lam*d)
  b2<-ginverse(b1)
  beta<-b2%*%t(x)%*%y
  print(beta)
  return(beta)
}

#Fungsi Y Prediksi
ytopi<-function(respon,pred,m,k,lam)
{
  n<-length(respon)
  y<-as.vector(respon)
  x<-matrikx(pred,m,k)
  d<-matrikd(m,k)
  b1<-(t(x)%*%x)+(n*lam*d)
  b2<-ginverse(b1)
  beta<-b2%*%t(x)%*%y
  Hlam<-x%*%b2%*%t(x)
  ytopi<-x%*%beta
  return(ytopi)
}

#H Lambda
Hlam<-function(respon,pred,m,k,lambda) 
{
  n<-length(respon)
  y<-as.vector(respon)
  h<-lambda
  x<-matrikx(pred,m,k)
  d<-matrikd(m,k)
  f1<-(t(x)%*%x)+(n*h*d)
  f2<-ginverse(f1)
  beta<-f2%*%t(x)%*%y
  Hlambda<-x%*%f2%*%t(x)
  return(Hlambda)
}

#Menghitung GCV
gcv<-function(respon,pred,m,k,lam)
{
  n<-length(respon)
  y<-as.vector(respon)
  x<-matrikx(pred,m,k)
  d<-matrikd(m,k)
  b1<-(t(x)%*%x)+(n*lam*d)
  b2<-ginverse(b1)
  beta<-b2%*%t(x)%*%y
  Hlam<-x%*%b2%*%t(x)
  ytopi<-x%*%beta
  MSE<-(t(y-ytopi)%*%(y-ytopi))/n
  GCV<-MSE/(1-((1/n)*sum(diag(Hlam))))^2
  nilai<-cbind(GCV,MSE)
  return(nilai)
}

#Untuk mencari Lambda optimal
carilambda<-function(respon,pred,m,k,bb,ba,ic)
{
  y<-as.vector(respon)
  n<-length(respon)
  x<-matrikx(pred,m,k)
  d<-matrikd(m,k)
  w<-quant(x,k+1)
  z1<-matrix(0,n,m+1)
  for(a in 1:k)
  {
    cat("titik knot[",a,"]=",w[a+1],"\n")
  }
  lambda<-seq(bb,ba,ic)
  nk<-length(lambda)
  GCV<-rep(0,nk) 
  MSE<-rep(0,nk)
  for(i in 1:nk)
  {
    b1<-(t(x)%*%x)+(n*lambda[i]*d)
    b2<-ginverse(b1)
    betatopi<-b2%*%t(x)%*%y
    aps<-x%*%b2%*%t(x)
    ytopi<-x%*%betatopi
    MSE<-(t(y-ytopi)%*%(y-ytopi))/n
    GCV[i]<-MSE/(1-((1/n)*sum(diag(aps))))^2
  }
  s<-matrix(c(lambda,GCV),length(lambda),2)
  GCVmin<-min(GCV)
  lambdaopt<-s[s[,2]==min(GCV),1]
  plot(lambda,GCV,type="l")
  c<-cbind(lambdaopt,GCVmin)
  return(c)
}

#Untuk mencari Orde, Jumlah Knot dan Lambda Optimal
carioptimal<-function(respon,pred)
{
  bb<-as.numeric(readline("Masukkan batas bawah lambda:"))
  ba<-as.numeric(readline("Masukkan batas atas lambda:"))
  ic<-as.numeric(readline("Masukkan nilai increment lambda:"))
  n<-length(respon)
  y<-as.vector(respon)
  x<-as.vector(pred)
  i<-1
  repeat
  {
    k<-2
    repeat
    {
      hasil1<-carilambda(y,x,i,1,bb,ba,ic)
      lambda1<-hasil1[,1]
      gcv1<-hasil1[,2]
      m1<-matrix(0,1,4)
      m1[1,]<-c(i,1,lambda1,gcv1)
      m2<-matrix(0,(k-1),4)
      for(n in 1:(k-1))
      {
        hasil2<-carilambda(y,x,i,n+1,bb,ba,ic)
        lambda<-hasil2[,1]
        gcv<-hasil2[,2] 
        m2[n,]<-c(i,n+1,lambda,gcv)
      }
      m3<-rbind(m1,m2)
      m4<-matrix(0,1,4)
      if(m3[k,4]<m3[k-1,4])
      {
        m4[1,]<-m3[k,]
      }
      else
      {
        m4[1,]<-m3[k-1,]
      }
      if(m3[k,4]>(m3[k-1,4]))break
      k<-k+1
    }
    if(i==1)
    {
      mgcv<-m4[,4]
      mgcvlama<-mgcv
    }
    else
    {
      mgcv<-m4[,4]
      if(mgcv>mgcvlama)break
      mgcvlama<-mgcv
    }
    i<-i+1
    mopt<-m4
  }
  colnames(mopt)=c("Orde","Jumlah Knot","Lambda","GCV")
  return(mopt)
}

#Estimasi Parameter PSpline
pspline.satu<-function(respon,predictor)
{
  Y<-as.vector(respon)
  X<-as.vector(predictor)
  n<-length(Y)
  optimal<-carioptimal(Y,X)
  m<-optimal[,1]
  k<-optimal[,2]
  h<-optimal[,3]
  
  predictorbaru<-unique(X)
  w<-quant(predictorbaru,k+1)
  cat("orde:",m,"\n")
  cat("lambda:",h,"\n")
  cat("jumlah knot:",k,"\n")
  cat("quantile(",1/(k+1),")=",w,"\n")
  for(i in 1:k)
  {
    cat("titik knots[",i,"]=",w[i+1],"\n")
  }
  hasil<-gcv(Y,X,m,k,h)
  GCV<-hasil[,1]
  MSE<-hasil[,2]
  beta<-beta(Y,X,m,k,h)
  fstar<-ytopi(Y,X,m,k,h)
  error<-Y-fstar
  cat("\nGCV=",(GCV))
  cat("\nMSE=",format(MSE),"\n")
  cat("\n-------------------------------------------------------------")
  cat("\n   X    Y      (Y*topi)       error")
  cat("\n-------------------------------------------------------------")
  for(i in 1:n)
    cat("\n",X[i]," ",Y[i]," ",fstar[i]," ",error[i])
  cat("\n-------------------------------------------------------------")
  for(i in 1:(m+k+1))
    cat("\n nilai beta[",i,"]=",beta[i])
  Xurut<-sort(X) 
  Yurut<-Y[order(X)]
  fstarurut<-fstar[order(X)]
  win.graph()
  plot(Xurut,Yurut,xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab="
predictor",ylab="respon")
  title("Fungsi Penalize untuk Satu Prediktor")
  par(new=T)
  plot(Xurut,fstarurut,type="l",xlim=c(min(X),max(X)),ylim=c(min(Y),max
                                                             (Y)),xlab="prediktor",ylab="respon")
}


carioptimal(data$IPM,data$P2)
pspline.satu(data$IPM,data$P2)

