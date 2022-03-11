
library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)
library(shape)


#-------------------------------------------------------------------------------
cor <- st_read(dsn = "C:/Users/abrenn19/Desktop/UBC/Research/Connectivity/Kenya_TNC", layer = "AllDigitization_project")

set.seed(124)
cor.list <- list()
path.list <- list()
for (i in 1:dim(cor)[1]) {
  the.path <- cor[i,]
  dist.path <- the.path$Shape_Le_1[1]
  the.path <- as(the.path, "Spatial") # converts to SpatialLinesDataframe
  the.path <- as(the.path, "SpatialLines") # converts to simple spatial object
  
  temp.coords <- as.data.frame(coordinates(the.path))  ## get points of line
  max.shift <- dist.path  # using the distance of corridor as range of shift in x and y direction
  
  for (j in 1:10) {
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
dat <- data.frame(id = rep(cor$OBJECTID, each = 10), rowname = row.names(random.cors))
rownames(dat) <- row.names(random.cors)
random.cors <- SpatialLinesDataFrame(random.cors, data = dat)

writeOGR(random.cors, dsn = "C:/Users/abrenn19/Desktop/UBC/Research/Connectivity/Kenya_TNC", 
         layer = "AllDigitization_project_Random",driver = "ESRI Shapefile", overwrite_layer = TRUE)


