# Construcción de mapa shapefile

library(RDS)
library(raster)
library(rgdal)

nac_final <- readRDS("Datos/nac_final.Rds")
shape <- readRDS("Datos/Cantones/CRI_adm22.rds")
shape <- crop(shape, extent(-88, -82, 7, 12))
shapefile(shape, 'Datos/Cantones/cantones.shp', overwrite = TRUE)
shp  <- readOGR("Datos/Cantones", "cantones", stringsAsFactors = FALSE,
                encoding = "UTF-8")

temp  <- merge(shp, nac_final, by.x="NAME_2", by.y="Canton") 
shapefile(temp, 'Datos/Cantones/cantones_final.shp', overwrite = TRUE)
