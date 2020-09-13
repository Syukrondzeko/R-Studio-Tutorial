library(spgwr)
data(columbus,package="spgwr")
head(columbus)
?columbus
str(columbus)

if (requireNamespace("rgdal", quietly = TRUE)) {
  library(rgdal)
  columbus <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])
  plot(columbus)
}

if (requireNamespace("spdep", quietly = TRUE)) {
  library(spdep)
  col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])
}

dataku=read.delim('C:/Users/HP PC/Downloads/7. GWR/columbus.txt')
head(dataku)

a<-lm(formula = dataku$crime ~ dataku$income + dataku$housing,
      data = dataku)
summary(a)

library(spgwr)
h <- gwr.sel(dataku$crime ~ dataku$income + dataku$housing, 
             coords=cbind(dataku$x,dataku$y),
             data=dataku, adapt=FALSE,gweight=gwr.Gauss)

gwr2 <- gwr(dataku$crime ~ dataku$income + dataku$housing, 
            coords=cbind(dataku$x,dataku$y),bandwidth=h,
            data=dataku,hatmatrix=TRUE,gweight=gwr.Gauss)

gwr2
names(gwr2)
names(gwr2$SDF)

# Menampilkan nilai koefisien beta
gwr2$SDF$"(Intercept)"
gwr2$SDF$"dataku$income"
gwr2$SDF$"dataku$housing"

# Uji Kecocokan Model
BFC02.gwr.test(gwr2)

# Uji Pengaruh Geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr2)

# Melihat nilai bandwidth
gwr2$bandwidth

# Menampilkan Nilai koefisien dan nilai prediksi
gwr2$SDF[,2:4]
gwr2$SDF[,c(2:4,9,11)]

