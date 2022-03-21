library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)
library(shape)

# Check raster values of real corridors -------------------------------------------------------------------------------
cor <- st_read(dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_Moll")
#cor <- st_read(dsn = "C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_Moll")

kenya<-raster("C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Procedure/Trial/Exported/kenya buffer/globalmodel_Kenyabuffer200km.tif")
#kenya<-raster("C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Procedure/Trial/Exported/kenya buffer/globalmodel_Kenyabuffer200km.tif")

crs(kenya)
res(kenya)
class(kenya)

cor.value<-raster::extract(kenya,cor,method='simple')

# Remove corridors with NAs
cor<-cor[which(sapply(cor.value,function(x) sum(is.na(x)==T))==0),]

# Random line creation -------------------------------------------------------------------------------
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

writeOGR(random.cors, dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_project_5rep_50km",driver = "ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(random.cors, dsn = "C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_project_5rep_50km",driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Compile statistics of real corridors -------------------------------------------------------------------------------
cor.value<-raster::extract(kenya,cor,method='simple')
cor.dat<-data.frame(mean = numeric(), max = numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric(),length = numeric())
for(i in 1:length(cor.value)){
  mean <- mean(cor.value[[i]])
  min <- min(cor.value[[i]])
  max <- max(cor.value[[i]])
  ninety_pct <- quantile(cor.value[[i]], probs = 0.9)
  sd = sd(cor.value[[i]])
  length = length(cor.value[[i]])
  cor.dat.add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd, length = length)
  cor.dat <- rbind(cor.dat, cor.dat.add)
}

cor.dat$identifier <- seq(1,nrow(cor.dat),by=1)

mean(cor.dat$mean)
# 1278.671

name1<-paste("C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation/all_corridors.csv",sep="")
#name1<-paste("C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Data/Random line generation/all_corridors.csv",sep="")
write.csv(cor.dat,name1, row.names = FALSE)

# Raster values of sample corridors -------------------------------------------------------------------------------
sample.cor <- st_read(dsn = "C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_project_5rep_50km")
#sample.cor <- st_read(dsn = "C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Data/Random line generation", layer = "AllDigitization_project_5rep_50km")

line.value<-raster::extract(kenya,sample.cor,method='simple')
#line.rmna<-Filter(Negate(anyNA), line.value)

line.dat<-data.frame(mean = numeric(), max= numeric(), min = numeric(), ninety_pct = numeric(), sd = numeric())
for(i in 1:length(line.value)){
  mean <- mean(line.value[[i]])
  min <- min(line.value[[i]])
  max <- max(line.value[[i]])
  ninety_pct <- quantile(line.value[[i]], probs = 0.9,na.rm=T)
  sd = sd(line.value[[i]])
  line.dat.add <- data.frame(mean = mean, max= max, min = min, ninety_pct = ninety_pct, sd = sd)
  line.dat <- rbind(line.dat, line.dat.add)
}

line.dat$identifier = rep(unique(cor.dat$identifier),each=5)

# Remove sample lines with NAs
line.dat<-line.dat[which(sapply(line.value,function(x) sum(is.na(x)==T))==0),]

# Remove sample lines with only 1 point
line.dat<-line.dat[-which(is.na(line.dat$sd) == T),]         
mean(line.dat$mean)
# 1310.709

name2<-paste("C:/Users/Vania/OneDrive - The University Of British Columbia/TNC Kenya GIS/Data/Random line generation/all_random_lines.csv",sep="")
#name2<-paste("C:/Users/vania185.stu/OneDrive - UBC/TNC Kenya GIS/Data/Random line generation/all_random_lines.csv",sep="")
write.csv(line.dat,name2, row.names = FALSE)
