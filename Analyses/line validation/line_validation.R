library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)
library(shape)


# Random line creation -------------------------------------------------------------------------------
cor <- st_read(dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_Moll")

set.seed(124)
cor.list <- list()
path.list <- list()
for (i in 1:dim(cor)[1]) {
  the.path <- cor[i,]
  dist.path <- the.path$Shape_Le_1[1]
  the.path <- as(the.path, "Spatial") # converts to SpatialLinesDataframe
  the.path <- as(the.path, "SpatialLines") # converts to simple spatial object
  
  temp.coords <- as.data.frame(coordinates(the.path))  ## get points of line
  max.shift <- 50000  # using the distance of corridor as range of shift in x and y direction
  
  for (j in 1:5) {
    the.shiftx <- runif(1,-max.shift,max.shift)  ## draw x shift value
    the.shifty <- runif(1,-max.shift,max.shift)  ## draw y shift value
    temp.coords.s <- cbind((temp.coords[,1]+the.shiftx), (temp.coords[,2]+the.shifty))  
    # (a SHIFT of random distance, in any of the 4 possible quadrants, up to max)
    temp.coords.rs <- rotatexy(as.matrix(temp.coords.s), angle=runif(1,0,360))  ## rotate random angle around end point of path; 
    temp.line <- Line(as.data.frame(temp.coords.rs))  ## a Line object
    test.lines <- Lines(list(temp.line),ID="a")  ## a Lines object
    s1 <- SpatialLines(c(test.lines))   ## the final SpatialLines object
    proj4string(s1) <- crs(cor)   ## assigning projection
    
    path.list[[j]] <- s1
    cor.list[[i]] <- do.call(rbind, path.list)
    
  }
  
  
}

random.cors <- do.call(rbind, cor.list)

head(cor)
dat <- data.frame(id = rep(cor$OBJECTID, each = 5), rowname = row.names(random.cors))
rownames(dat) <- row.names(random.cors)
random.cors <- SpatialLinesDataFrame(random.cors, data = dat)

writeOGR(random.cors, dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", 
         layer = "AllDigitization_project_5rep_50km",driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Raster values of real corridors -------------------------------------------------------------------------------
kenya<-raster("C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Procedure/Trial/Exported/globalmodel_Kenyabuffer200km.tif")
crs(kenya)
res(kenya)
class(kenya)

cor.value<-raster::extract(kenya,cor,method='simple')
cor.rmna<-Filter(Negate(anyNA), cor.value)
cor.dat<-data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric())
for(i in 1:length(cor.rmna)){
  mean <- mean(cor.rmna[[i]])
  min <- min(cor.rmna[[i]])
  max <- max(cor.rmna[[i]])
  ninety_pct <- quantile(cor.rmna[[i]], probs = 0.9)
  sd = sd(cor.rmna[[i]])
  cor.dat.add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd)
  cor.dat <- rbind(cor.dat, cor.dat.add)
}

mean(cor.dat$mean)
# 1278.671

# Raster values of sample corridors -------------------------------------------------------------------------------
sample.cor <- st_read(dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", 
                      layer = "AllDigitization_project_5rep_50km")

line.value<-raster::extract(kenya,sample.cor,method='simple')
line.rmna<-Filter(Negate(anyNA), line.value)

line.dat<-data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric())
for(i in 1:length(line.rmna)){
  mean <- mean(line.rmna[[i]])
  min <- min(line.rmna[[i]])
  max <- max(line.rmna[[i]])
  ninety_pct <- quantile(line.rmna[[i]], probs = 0.9)
  sd = sd(line.rmna[[i]])
  line.dat.add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd)
  line.dat <- rbind(line.dat, line.dat.add)
}

mean(line.dat$mean)
# 1310.709