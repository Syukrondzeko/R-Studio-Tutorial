library(readxl)
dataku <- read_excel("F:/Course/youtube/19. Regresi Kernel/dataku.xlsx")
View(dataku)
dataku <- dataku[order(dataku$kurs),]
X=dataku$kurs
Y=dataku$lq45

gh1=function(hopt,X,Y)
{
  g <- length(X)
  Ghat <- rep(0, g)
  n <- length(Y)
  for(j in 1:g) {
    ghat <- 0
    faktor <- 0
    for(i in 1:n) {
      faktor <- faktor + ker((X[j] - X[i])/hopt)
      ghat <- ghat + Y[i] * ker((X[j] - X[i])/hopt)
    }
    if(faktor==0)Ghat[j]=0
    else Ghat[j] <- ghat/faktor
  }
  Gh=matrix(Ghat,ncol=1)
  plot(X,Y,xlim = c(min(X), max(X)), ylim = c(min(Y),max(Y)),xlab = "Kurs Rupiah", ylab =  "LQ45")
  par(new = T)
  lines(X, Gh, xlim = c(min(X), max(X)),  ylim = c(min(Y),max(Y)),  xlab = "", ylab = "",col=2)
  #plot(X, Ghat, xlim = c(min(X), max(X)), ylim = c(4900,5300),  xlab = "", ylab = "",type="l",col=2)
  Gh
}


mdata=as.matrix(dataku)
hop <- np.gcv(mdata)
plot(hop$h.seq, hop$GCV, xlab="h", ylab="GCV", type="l")
hop2 <- np.gcv(mdata,h.seq=seq(10, 50, 1))
plot(hop2$h.seq, hop2$GCV, xlab="h", ylab="GCV", type="l")
#np.cv(data = data, h.seq = NULL, num.h = 50, w = NULL, num.ln = 1,ln.0 = 0, step.ln = 2, estimator = "NW", kernel = "quadratic")

gh1(hop$h.opt,X,Y)

