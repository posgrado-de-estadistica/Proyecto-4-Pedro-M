# Analisis

library(rgdal)
library(RColorBrewer)
library(spdep)
library(sp)
library(ggplot2)
library(tmap)
library(rgeos)
library(raster)
library(deldir)
library(spdep)
library(coda)
library(maps)
library(shape)


# Carga de datos

# sp_data <- readOGR("Datos/Cantones", "cantones_final", stringsAsFactors=FALSE, encoding="UTF-8")

nac_final <- readRDS("Datos/nac_final.Rds") %>% mutate(Canton= ifelse(Canton== "Montes De Oro", "Montes de Oro", Canton)) 
shape <- readRDS("Datos/Cantones/CRI_adm22.rds")
shape <- crop(shape, extent(-88, -82, 7, 12))

sp_data  <- merge(shape, nac_final, by.x="NAME_2", by.y="Canton")

rm(nac_final)
rm(shape)

# Tendencia 

spplot(sp_data, "tvim", col.regions = brewer.pal(9, "YlOrRd"), 
       at = seq(-1,0.5, length.out = 9),
       par.settings = list(axis.line = list(col = "transparent")), 
       colorkey = list(axis.line = list(col = "black")))


# Tendencia 

# 2011
spplot(sp_data, "t_11_1", col.regions = brewer.pal(9, "YlOrRd"), 
       at = seq(0,80, length.out = 9),
       par.settings = list(axis.line = list(col = "transparent")), 
       colorkey = list(axis.line = list(col = "black")))

# 2000
spplot(sp_data, "t_0_1", col.regions = brewer.pal(9, "YlOrRd"), 
       at = seq(0,80, length.out = 9),
       par.settings = list(axis.line = list(col = "transparent")), 
       colorkey = list(axis.line = list(col = "black")))


sp_data$t_0_1 <- replace(sp_data$t_0_1, sp_data$t_0_1 >=100, 80)

# Gráficos Tmap

a1 <- tm_shape(sp_data) + 
    tm_polygons("t_0_1", palette= "RdYlBu", n = 8, title = "TBN") +
    tm_layout(main.title = "Tasa Bruta de Natalidad por Cantón \n Costa Rica 2000", main.title.position = "center", main.title.size = 0.7,frame= FALSE) 

a2 <- tm_shape(sp_data) + 
    tm_polygons("t_11_1", palette= "RdYlBu", n = 6, title = "TBN") +
    tm_compass(type = "8star",size = 2, position = c("right", "top")) +
    tm_layout(main.title = "Tasa Bruta de Natalidad por Cantón \n Costa Rica 2011", main.title.position = "center", main.title.size = 0.7,frame= FALSE) 

pdf("Imagenes/analisis_1.pdf")
tmap_arrange(a1,a2)
dev.off()

pdf("Imagenes/analisis_22.pdf")
tm_shape(sp_data) + 
    tm_polygons("tvim", palette= "RdYlBu", n = 10, title = "Tendencia") +
    tm_compass(type = "8star",size = 2, position = c("right", "top")) +
    tm_layout(main.title = "Tendencia de la natalidad \n Costa Rica 2000 -2011", main.title.position = "center", main.title.size = 0.7,frame= FALSE) +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(legend.stack = FALSE,inner.margins = 0.04)
    
dev.off()

# Coordenadas de los cantones para ver las relaciones

coords <- coordinates(sp_data)
IDs <- row.names(sp_data)

# Criterios  Dama, Torre

list.queen <- poly2nb(sp_data)
list.queen

# matriz de pesos estandarizadas

W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
W

class(W);summary(W);str(W)

plot(sp_data,  border='gray', lwd=2)
plot(W,coordinates(sp_data),lwd=2, add=TRUE)

# Revisión de distancias de vecinos

W_dist<-dnearneigh(coords,0,1,longlat = FALSE)
plot(sp_data,  border='gray', lwd=2)
plot(W_dist,coordinates(sp_data),lwd=2, add=TRUE)


# Plots Criterios analizados

# Reina
pdf("Imagenes/vecinos2.pdf")

par(xaxt = "n", yaxt = "n", tck = 0, mfrow = c(1, 2), mai = c(0, 0, 0, 0), 
    bty = "n")

plot(sp_data, axes=FALSE, border="gray", lwd=2, main= "Dama")
plot(list.queen, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")

# Torre

list.torre <- poly2nb(sp_data, queen = F)

plot(sp_data, axes=FALSE, border="gray", lwd=2, main= "Torre") 
plot(list.torre, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")
box(col = "white")

dev.off()

list.torre
summary(list.torre)

# KNN Vecinos orden 1

pdf("Imagenes/vecinos3.pdf")

par(xaxt = "n", yaxt = "n", tck = 0, mfrow = c(1, 3), mai = c(0, 0, 0, 0), 
    bty = "n")

list.neig <- knn2nb(knearneigh(coords, k=1), row.names=IDs)

plot(sp_data, axes=FALSE, border="gray", lwd=2)
title("K = 1", adj = 0.5, line = -1.5)
plot(list.neig, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")
box(col = "white")

# Vecinos orden 2

list.neig2 <- knn2nb(knearneigh(coords, k=2), row.names=IDs)

plot(sp_data, axes=FALSE, border="gray", lwd=2)
title("K = 2", adj = 0.5, line = -1.5)
plot(list.neig2, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")
box(col = "white")

# Vecinos orden 4

list.neig4 <- knn2nb(knearneigh(coords, k=4), row.names=IDs)

plot(sp_data, axes=FALSE, border="gray", lwd=2)
title("K = 4", adj = 0.5, line = -1.5)
plot(list.neig4, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")
box(col = "white")

dev.off()

# kNN con la distancia estandarizada

dsts <- unlist(nbdists(list.queen, coords))
queen_nb <- dnearneigh(coords, d1=0, d2=0.75*max(dsts), row.names=IDs)

plot(sp_data, axes=FALSE, border="gray", lwd=2)
plot(queen_nb, coordinates(sp_data), pch = 19, cex = 0.6, add = T, col = "red")
box(col = "white")

# Estandarizado

torre_S <- nb2listw(list.torre,style = "B")

summary(unlist(torre_S$weights))
summary(sapply(torre_S$weights, sum))


# Calculamos datos teóricos sin autocorrelacion espacial

set.seed(1)
n <- length(list.torre)
uncorr_x <- rnorm(n)
rho <- 0.5
autocorr_x <- invIrW(torre_S, rho) %*% uncorr_x


lm.morantest(lm(sp_data$tvim ~ 1, sp_data), listw = torre_S)
lm.morantest.sad(lm(sp_data$tvim ~ 1, sp_data), listw = torre_S)
lm.morantest.exact(lm(sp_data$tvim ~ 1, sp_data), listw = torre_S)


Im_perm <- moran.mc(sp_data$tvim, listw = torre_S, nsim = 999)
Im_perm


# Gráfico de dispersón de los datos observados y rezagados.

oopar <- par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
msp <- moran.plot(sp_data$tvim, listw=torre_S, quiet=TRUE, xlab = "Intoxicaciones", 
                  ylab = "Tendencia rezagada")


pdf("Imagenes/moran1.pdf")

infl <- apply(msp$is.inf, 1, any)

x <- sp_data$tvim
lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), 
           include.lowest=TRUE)
wx <- stats::lag(nb2listw(list.torre, style="B"), sp_data$tvim)

lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), 
            include.lowest=TRUE)
lhlh <- interaction(lhx, lhwx, infl, drop=TRUE)
cols <- rep(1, length(lhlh))


# Hacemos el gráfico con los cantones de influencia

lm_1 <- localmoran(sp_data$tvim, listw = torre_S)
lm_2 <- as.data.frame(localmoran.sad(lm(tvim ~ 1,sp_data), nb = list.torre, style = "B"))
lm_3 <- as.data.frame(localmoran.exact(lm(tvim ~ 1,sp_data), nb = list.torre, style = "B"))

sp_data$Normal <- lm_2[,3]
sp_data$Aleatorizado <- lm_1[,5]

gry <- c(rev(brewer.pal(6, "Reds")), brewer.pal(6, "Blues"))

# Estimación 

spplot(sp_data, c("Normal", "Aleatorizado"), 
       at=c(0,0.01,0.05,0.1,0.9,0.95,0.99,1), 
       col.regions=colorRampPalette(gry)(7))

tm_shape(sp_data) + 
    tm_polygons("t_0_1", palette= "RdYlBu", n = 8, title = "TBN") +
    tm_layout(main.title = "Tasa Bruta de Natalidad por Cantón \n Costa Rica 2000", main.title.position = "center", main.title.size = 0.7,frame= FALSE) 


# Modelos Autorregresivos

El problema de ignorar la estructura espacial de los datos implica que las estimaciones de OLS en el modelo no espacial pueden ser sesgadas, inconsistentes o ineficientes, dependiendo de cuál sea la verdadera dependencia subyacente (para más información, ver Anselin y Bera (1998) )

chi.ols<-lm(tvim~NAME_1, data=sp_data@data)
summary(chi.ols)

moran.lm<-lm.morantest(chi.ols, W, alternative="two.sided")
print(moran.lm)

# Modelos SAR
Una forma es asumir la normalidad del término de error y utilizar la máxima probabilidad.

sar.chi<-lagsarlm(violent~1, data=sp_data$data, W)
summary(sar.chi)