library(spgwr) # Mengaktifkan paket spgwr
data(columbus,package="spgwr") # Data columbus diambil dari spgwr

head(columbus)

a<-lm(formula = CRIME ~ INC + HOVAL, data = columbus)
summary(a)

b <- gwr.sel(CRIME ~ INC + HOVAL, 
             coords=cbind(columbus$x,columbus$y),
             data=columbus, adapt=TRUE,gweight=gwr.Gauss)

# Estimasi Parameter
gwr1 <- gwr(CRIME ~ INC + HOVAL, 
            coords=cbind(columbus$x,columbus$y),
            data=columbus, adapt=b,hatmatrix=TRUE,gweight=gwr.Gauss)

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
