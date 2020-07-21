library(spgwr)
data(columbus,package="spgwr")
head(columbus)
?columbus
str(columbus)

#plot data
if (requireNamespace("rgdal", quietly = TRUE)) {
  library(rgdal)
  columbus <- readOGR(system.file("shapes/columbus.shp", package="spData")[1])
  plot(columbus)
}

if (requireNamespace("spdep", quietly = TRUE)) {
  library(spdep)
  col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])
}

# Input Data
dataku=read.delim('C:/Users/HP PC/Downloads/7. GWR/columbus.txt')
head(dataku)


# Model Regresi OLS
a<-lm(formula = dataku$crime ~ dataku$income + dataku$housing,
      data = dataku)
summary(a)


library(spgwr)
# Mencari bandwidth optimal (fixed bandwidth)
h <- gwr.sel(dataku$crime ~ dataku$income + dataku$housing, 
             coords=cbind(dataku$x,dataku$y),
             data=dataku, adapt=FALSE,gweight=gwr.Gauss)

# Estimasi Parameter fixed bandwidth
gwr2 <- gwr(dataku$crime ~ dataku$income + dataku$housing, 
            coords=cbind(dataku$x,dataku$y),bandwidth=h,
            data=dataku,hatmatrix=TRUE,gweight=gwr.Gauss)

# Membaca Output
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


#bandwith adaptif
library(spgwr) # Mengaktifkan paket spgwr
data(columbus,package="spgwr") # Data columbus diambil dari spgwr

# Menampilkan Data 
head(columbus)

# Model Regresi OLS
a<-lm(formula = CRIME ~ INC + HOVAL, data = columbus)
summary(a)



b <- gwr.sel(CRIME ~ INC + HOVAL, 
             coords=cbind(columbus$x,columbus$y),
             data=columbus, adapt=TRUE,gweight=gwr.Gauss)

# Estimasi Parameter
gwr1 <- gwr(CRIME ~ INC + HOVAL, 
            coords=cbind(columbus$x,columbus$y),
            data=columbus, adapt=b,hatmatrix=TRUE,gweight=gwr.Gauss)

# Membaca Output
gwr1
names(gwr1)
names(gwr1$SDF)

# Menampilkan nilai koefisien beta
gwr1$SDF$"X.Intercept."
gwr1$SDF$INC
gwr1$SDF$HOVAL

# Uji Kecocokan Model
BFC02.gwr.test(gwr1)

# Uji Pengaruh Geografis terhadap setiap prediktor
LMZ.F3GWR.test(gwr1)

# Melihat nilai bandwidth
gwr1$bandwidth

# Menampilkan Nilai koefisien dan nilai prediksi
gwr1$SDF[,2:4]
gwr1$SDF[,c(2:4,9,11)]



