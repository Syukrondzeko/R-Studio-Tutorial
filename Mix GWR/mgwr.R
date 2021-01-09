library(readxl)
dataku <- read_excel("F:/Course/youtube/40. mgwr/dataku.xlsx")
View(dataku)
library(readr)
data2 <- read_csv("F:/Course/youtube/40. mgwr/data2.csv")
coord=data2[,4:5]

library(spgwr)
adaptgauss=gwr.sel(Merokok~Pendidikan+Pria+Kemiskinan,data=dataku,adapt=TRUE,coords=cbind(coord$U,coord$V),gweight=gwr.Gauss)
gwr.adaptgauss=gwr(Merokok~Pendidikan+Pria+Kemiskinan,data=dataku,adapt=adaptgauss,coords=cbind(coord$U,coord$V),hatmatrix=TRUE,gweight=gwr.Gauss) 

bw=gwr.adaptgauss$bandwidth
y=as.matrix(dataku$Merokok)
lat=as.matrix(coord$U)
lon=as.matrix(coord$V)
xl=as.matrix(cbind(dataku$Pendidikan,dataku$Kemiskinan))
xg=as.matrix(dataku$Pria)
x=as.matrix(cbind(xl,xg)) 


ng=ncol(xg) 
nl=ncol(xl) 
n=length(y)

I=diag(1,n,n) 
W=matrix(0,n,n) 
d=matrix(0,n,n) 
for (i in 1:n){
  for (j in 1:n)
  {
    d[i,j]=sqrt((lat[i,1]-lat[j,1])^2+(lon[i,1]-lon[j,1])^2) 
    if (d[i,j] > bw[i]){
      W[i,j]=0
    }else
      W[i,j]=1-((d[i,j]/bw[i])^2)^2
  }
}
beta.l=matrix(0,nl,n) 
Sl=matrix(0,n,n)
for (i in 1:n){
  Sl[i,]=((xl[i,]%*%(solve((t(xl)%*%diag(W[,i]))%*%xl)))%*%t(xl))%*%diag(W[,i])
}

beta.g=((((solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg))%*%t(xg))%*%t(I-Sl))%*%(I-Sl))%*%y 

for(i in 1:n){
  beta.l[,i]=((solve((t(xl)%*%diag(W[,i]))%*%xl)%*%t(xl)%*%diag(W[,i]))%*%(y-(xg%*%beta.g)))
}

Sg=(xg%*%solve(t(xg)%*%xg))%*%t(xg)
S=Sl+((((((I-Sl)%*%xg)%*%solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg))%*%t(xg))%*%t(I-Sl))%*%(I-Sl)) 
y.hat=S%*%y
residual=(y-y.hat)
H=(x%*%solve(t(x)%*%x))%*%t(x) 

v=c(0,0)
u=c(0,0)
r=c(0,0)
t=c(0,0)
library(psych)
for(i in 1:2){
  v[i]=tr(((I-H)-(t(I-S)%*%(I-S)))^i) 
  u[i]=tr((t(I-S)%*%(I-S))^i)
  r[i]=tr((t(I-Sl)%*%(I-Sl)-t(I-S)%*%(I-S))^i)
  t[i]=tr((t(I-Sg)%*%(I-Sg)-t(I-S)%*%(I-S))^i)
} 


F1=as.vector((((t(y)%*%((I-H)-(t(I-S)%*%(I- S))))%*%y)/v[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.1=(v[1]^2/v[2])
df2=(u[1]^2)/u[2]
F2=as.vector((((t(y)%*%(((t(I-Sl)%*%(I-Sl))-(t(I-S)%*%(I-S)))))%*%y)/r[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.2=(r[1]^2/r[2])
F3=as.vector((((t(y)%*%(((t(I-Sg)%*%(I-Sg))-(t(I-S)%*%(I-S)))))%*%y)/t[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.3=(t[1]^2/t[2]) 
Fgab=as.vector(rbind(F1,F2,F3)) 
df1=c(df1.1,df1.2,df1.3) 
p.value=as.vector(matrix(0,3,1)) 
for(i in 1:3){
  p.value[i]=1-(pf(Fgab[i], df1=df1[i], df2=df2))
}
Uji.Serentak=cbind(Fgab,df1,df2,p.value) 


G=((solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg)%*%t(xg))%*%t(I-Sl))%*%(I-Sl) 
gkk=diag(G%*%t(G)) 
t.g=as.vector(matrix(0,ng,1)) 
p.val=as.vector(matrix(0,ng,1))
sigma=as.vector(sqrt(((((t(y)%*%t(I-S))%*%(I-S))%*%y)/n)))
for(i in 1:ng){
  t.g[i]=beta.g[i]/(sigma*sqrt(gkk[i])) 
  df=df2
  p.val[i]=pt(t.g[i], df=df)
}
Uji.Parsial.Global=cbind(t.g,df,p.val) 

beta.g
beta.l
Uji.Serentak 
Uji.Parsial.Global 



























#============================================================
RSS.H0.F1=as.vector(((t(y)%*%(I-H))%*%y))
RSS.H0.F2=as.vector((((t(y)%*%(t(I-Sl)))%*%(I-Sl))%*%y))
RSS.H0.F3=as.vector((((t(y)%*%(t(I-Sg)))%*%(I-Sg))%*%y)) 
RSS.H0=cbind(RSS.H0.F1,RSS.H0.F2,RSS.H0.F3) 
RSS.H1=as.vector((((t(y)%*%(t(I-S)))%*%(I-S))%*%y)) 
DSS1=RSS.H0.F1-RSS.H1
DSS2=RSS.H0.F2-RSS.H1 
DSS3=RSS.H0.F3-RSS.H1 
DSS=cbind(DSS1,DSS2,DSS3) 

sigma=as.vector(sqrt(((((t(y)%*%t(I-S))%*%(I-S))%*%y)/n))) 
t.hit.l=matrix(0,nl,n) 
pvalue=matrix(0,nl,n) 
ringkasan=matrix(0,n,2*nl) 
for(i in 1:n){
  M=((((solve(((t(xl)%*%diag(W[,i]))%*%xl)))%*%t(xl))%*%diag(W[,i]))%*%(I-(xg%*%G)))
  m=diag(M%*%t(M))
  m=as.matrix(m)
  for (j in 1:nl)
  {
    t.hit.l[j,i]=beta.l[j,i]/(sigma*(sqrt(m[j,]))) 
    pvalue[j,i]=pt(t.hit.l[j,i],df=df2,lower.tail=TRUE)
  }
  ringkasan[i,]=t(cbind(t.hit.l[,i],pvalue[,i]))
}
#nilai t, nilai pval, dst# 
AICc=(2*n*log(sigma))+(n*log(2*pi))+((n*((n+tr(S)))/(n-2- tr(S))))
AIC=(2*n*log(sigma))+(n*log(2*pi))+n+tr(S) 
resid=y-y.hat
sigu=(t(resid))%*%resid 
ym=y-mean(y) 
rsqrt1=sigu
rsqrt2=t(ym)%*%ym
rsqrt=1-(rsqrt1/rsqrt2) #r-squared# rsqrt1=rsqrt1/(n-ng-nl) rsqrt2=rsqrt2/(n-1)
rbar=1-(rsqrt1/rsqrt2) #rbar-squared# bw

ringkasan
AICc 
AIC
rsqrt 
rbar

lat
lon
xl
xg
x
Sl
Sg
S
W
RSS.H0 
RSS.H1 
DSS

