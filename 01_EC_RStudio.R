#Refere ; https://rpubs.com/richkt/269797

#Time Series for Eastern Cape
library(TSA)
library(ggplot2)
library(readx1) #not available for this version
library(ggmap)
library(tseries)

# install.packages("devtools")
devtools::install_github("tidyverse/readxl")

######################################################################################################
#Data Formating and Visualisation
#StationAverage - Precipitation Montlhy Average for each Year

getwd()
setwd("C:/Users/Doctor Shandu/OneDrive - University of Witwatersrand/MODULES/STAT 7006A_Spatial Stats/Project/Data/R_Stations/Work")

#Loading Data
ec_preci <- read.csv("PreciMontlyStation.csv") 
ec_preci
#Assigning Years to row names
row.names(ec_preci) <- ec_preci$Date
#Looking for head of data frame and view in tabular format
head(ec_preci)
View(ec_preci)
#Viewing data dimension rows an columns
dim(ec_preci)
#looking at columns and rows and accessing specific row
head(ec_preci$Date)
row.names(ec_preci)
ec_preci["1/1/1990",]
ec_preci[c("1/1/1990","12/1/2021"),] #Checking Multiple Rows

summary(ec_preci$Precipitation..mm.day.)

class(ec_preci)

###############################################################################################
###________________________START: SPATIAL INTERPOLATION__________________________________

#MAPPING IN R PRECIPITATION
#Spatial Data Processing
library(tidyverse)
library(sf)
library(mapview)
library(sp)
library(raster)

#Spatial Data Processing
library(gstat) #Responsible for variogram and krigin
remove.packages("gstat")
install.packages("gstat") #install.packages("gstat",dependencies=T)

#Error in #Error gstat 
#library(devtools)
#install_github("edzer/gstat")

sessionInfo()
library(automap)
remove.packages("automap")
install.packages("automap")

#Plotting Packages
library(patchwork)
library(viridis)
library(data.table)
library(rgdal)
library(geoR)

#Data Loading
perStationPrecipitation <- read.csv("Precipitation.csv")
perStationPrecipitation
#perStationPrecipitation -> Precipitation.csv -> Station/Lat/long for all years averaged 

#Maiking a Map #->stationPrecipitation
mapview(perStationPrecipitation, xcol = "Longitude", ycol = "Latitude", crs = 4326, grid = FALSE)

#Interpolating 
#https://swilke-geoscience.net/post/2020-09-10-kriging_with_r/kriging/

summary(perStationPrecipitation)

##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(1,1))

#Plot and Mapping
coordinates(perStationPrecipitation) = c("Longitude", "Latitude")
crs.geo1 = CRS("+proj=longlat")
proj4string(perStationPrecipitation) = crs.geo1
plot(perStationPrecipitation, pch=20, col="blue")

#Shapefiles
ec_district = readOGR(dsn="./Shapefile/ec_district.shp")
ec_localm = readOGR(dsn = "./Shapefile/ec_localMun.shp")
#Plot with Overlay
plot(ec_district, size = 2, col = "grey",lwd=4)
plot(ec_localm, add=TRUE, fill=NA, lwd=1, border='red')
points(perStationPrecipitation, pch = 20, col="blue")
crs(ec_district)

#Plot with Voronoi Coverage
library(dismo)
v <- voronoi(perStationPrecipitation)
plot(ec_district, size = 2, col = "grey",lwd=4)
points(perStationPrecipitation, pch = 20, col="blue")
plot(v, add=TRUE)


#___________________________________INTERPOLATING_________________________________________
#SpATIAL INTERPOLATION DAT VIEW
perStationPrecipitation
qqnorm(perStationPrecipitation$X1990)
qqline(perStationPrecipitation$X1990)

#REF https://www.analyticsvidhya.com/blog/2021/05/spatial-interpolation-with-and-without-predictors/
#https://rspatial.org/raster/analysis/4-interpolation.html
#Packages
library(dplyr)
library(xlsx)
library(raster)
library(sf)
library(sp)
library(terra)
library(Metrics)

#Interpolation Packages
#library(gstat)  # Nearest Neighbour, IDW, Kriging
library(fields) # TPS ~ Thin Plate Spline Regression
#library(mgcv)   # GAM ~ General Additive Model
#library(interp) # TIN ~ Triangular Irrelar Network
#library(automap)# Automatic Kriging
library(gstat)
#library(nngeo) #NN Nearest Neighbor


PrecipitationInterpo <- read.csv("Precipitation.csv")
PrecipitationInterpo
#_______________________________TRYING METHODS___________________________________
#Creating Grid Boundary
boundary <- c(
  "xmin" = min(PrecipitationInterpo$Longitude),
  "ymin" = min(PrecipitationInterpo$Latitude),
  "xmax" = max(PrecipitationInterpo$Longitude),
  "ymax" = max(PrecipitationInterpo$Latitude)
)
extent_grid <- expand.grid(
  Longitude = seq(from=boundary["xmin"], to=boundary["xmax"], by=0.002),
  Latitude = seq(from=boundary["ymin"], to=boundary["ymax"], by=0.002)
)
# Output extent to raster
extent_grid_raster <- extent_grid %>%
  dplyr::mutate(Z = 0) %>%
  raster::rasterFromXYZ(
    crs = "+proj=longlat +datum=WGS84")

###____________TRYING THE TPS SPATIAL INTERPOLATION METHOD__________________________
#For 1995
TPS1995 <- Tps(x = as.matrix(PrecipitationInterpo[, c("Longitude", "Latitude")]), Y=PrecipitationInterpo$X1995, miles=FALSE)
TPS1995_intr <- interpolate(extent_grid_raster, model=TPS)
plot(TPS1995_intr, main = "Thin Plate Spline Regression for 1995 Precipitation")
plot(ec_district,add=TRUE, size = 2, fill=NA,lwd=1)
points(perStationPrecipitation, pch = 20, col="blue", legend=TRUE)

#Adding Legends
#legend("topleft", lty=1,  col=c("black","darkgray"), c("District", "Station"))
legend( x="topleft", 
        legend=c("Point","Line"), 
        col=c("blue","black"), pch=c(19,NA), lwd=4, lty=c(NA,1), 
        merge=FALSE )

#North Arror
library(maptools)
north(plocation = "tl", which_north = "true", style = "style_1", fill = c("black","black") )


#Export 
 


#Adding Scale





#For 2006





 

########################################################################################################################

###_____________________SPATIAL INTERPOLATION MODEL CROSS VALIDATION_____________________












