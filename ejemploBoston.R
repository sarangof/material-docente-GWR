library(sp)
library(lattice)
library(maptools)
library(rgeos) # A package that Maptools uses.
library(ipdw)
library(spgwr)
library(spdep)
library(gstat)
library(ggplot2)

setwd("C:/Users/sarangof/Documents/GWR método/Preparación curso")

#bst_data=readShapeSpatial('boston_pts.shp')
#CMEDV <- bst_data$CMEDV
#write.csv2(file="CMEDV.csv", x=CMEDV)
#bst_cmedv=read.csv("CMEDV.csv",sep=";",dec=",")
#bst_data$CMEDV <- bst_cmedv[,2]
#writeSpatialShape(bst_data,'boston_housing.shp')
bst=readShapeSpatial('boston_housing.shp')

# Guardar 

bst_shape=readShapeSpatial('boston_bound.shp')

# Gráfico de la distribución

plot(bst_shape,axes=TRUE,main="Great Boston. Distribución de viviendas")
points(bst,col="blue",pch=19,cex=0.5)
grid()
colours = c("dark blue", "blue",  "dark red","red")
spplot(bst, "CMEDV",cuts=quantile(bst$CMEDV),col.regions=colours,sp.layout=bst_shape)


# Regresión Lineal

linear_model <- lm(data=bst,CMEDV~CRIM+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B)

pred_lm <- linear_model$fitted.values
res_lm <- linear_model$residuals
res_lm_abs <- abs(res_lm)
res_map_lm <- SpatialPointsDataFrame(coords=cbind(bst$LON_COR,bst$LAT_COR), data=cbind(as.data.frame(bst),res_lm,res_lm_abs))


# GWR

map_gwr = SpatialPointsDataFrame(data=as.data.frame(bst), coords=cbind(bst$LON_COR,bst$LAT_COR))
bw <- gwr.sel(CMEDV~CRIM+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B, data=map_gwr, adapt=T)
gwr.model <- gwr(CMEDV~CRIM+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B, data=map_gwr, adapt=bw)
pred_gwr <- gwr.model$SDF$pred
res_gwr <- bst$CMEDV - pred_gwr
res_gwr_abs <- abs(res_gwr) # Debo imprimir coeficientes para todo el mundo también
r2_gwr <- gwr.model$SDF$localR2
res_map_gwr = SpatialPointsDataFrame(data=cbind(as.data.frame(bst),res_gwr_abs), coords=cbind(bst$LON_COR,bst$LAT_COR))



# Graficas continuas

bst_shape <- fortify(bst_shape)
bst_sp=readShapeSpatial('boston_bound.shp')


rago <- raster(extent(bst_sp))
res(rago) <- c(0.001,0.001)
rago[] <- 1
proj4string(rago) <- CRS(proj4string(bst_sp))
grd <- mask(rago, bst_sp)

grd_bst <- as(grd, 'SpatialPointsDataFrame')
grd_bst <- grd_bst[!is.na(grd_bst@data$layer), ]
gridded(grd_bst) <- TRUE

idw_ago <- idw(formula = CMEDV ~ 1, locations=bst ,newdata=grd_bst, idp = 2.5)
idw.output = as.data.frame(idw_ago)
names(idw.output)[1:3] <- c("long", "lat", "CMEDV.Pred") 
spplot(idw_ago,"var1.pred")

#coordinates(grd) <- ~x + y
#gridded(grd)=TRUE

prueba <- dry.grid

idw <- idw(formula = CMEDV ~ 1, locations = bst, newdata = prueba)
idw.output = as.data.frame(idw)
names(idw.output)[1:3] <- c("long", "lat", "CMEDV.Pred") 
BS <- data.frame(bst)
map.idw.cmedv = SpatialPointsDataFrame(data=data.frame(bst$CMEDV), coords=cbind(bst$LON_COR,bst$LAT_COR))
ggplot() + geom_tile(data = idw.output, aes(x = long, y = lat, fill = CMEDV.Pred)) + scale_fill_gradient(low="cyan",high="orange") + geom_path(data = bst_shape, aes(long, lat, group = group), colour = "grey")    + geom_point(data = BS, aes(x = LON_COR, y = LAT_COR), shape = 21, colour = "red") + labs(fill = "Dollars (thousands)", title = "Property prices in Boston, 1930")

