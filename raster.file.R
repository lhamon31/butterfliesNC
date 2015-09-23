###FILE TO LOAD RASTER LAYERS AND CREATE FILES OF MEAN

## READING IN RASTER LAYERS
setwd("C:/Users/lhamon/Documents/clim data/1990")

library(raster)

## Reading in raster data
# long-term mean temperature data downloaded from WorldClim.org

# Read in single raster layer from january
jan90<-raster('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM2_199001_bil.bil')
plot(jan90)

#list all the files (i'm not sure what this is)
filenames <- list.files("C:/Users/lhamon/Documents/clim data/1990",pattern="_bil.bil")
mean<-stack(filenames)

# Read in stack of layers from all 12 months 1990
files1<-paste('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_19900',1:9,'_bil.bil',sep='')
files2<-paste('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_1990',10:12,'_bil.bil',sep='')
files = c(files1, files2)
tmean<-stack(files)
plot(tmean)

#for loop to read in all the files
For (y in 1991:2014) {
  filenames.y<- paste("C:/Users/lhamon/Documents/temp/", y, "/PRISM_tmean_stable_4kmM2_", y, "0", 1:4, "_bil.bil", sep="")
  filenames.prevy<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, "0", 8:9, "_bil.bil", sep="")
  filenames.prevy2=paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, 10:12, "_bil.bil", sep="")
  filenames=c(filenames.y,filenames.prevy, filenames.prevy2)
}  
  3) overlay with counties (reprojection)
  4) extract mean temp by county
And store that info
}
for (i in c(1:numfiles)){  
    filenames[i] <- paste(".\\",filenames[i],sep="")  
assign(gsub("[.]csv$","",filenames[i]),read.csv(filenames[i], header=FALSE))
}




## EXTRACT TEMP MEANS FROM COUNTY DATA
#read in county boundary file
library(rgdal)
counties=readOGR("C:/Users/lhamon/Documents/boundarydata","COUNTYBOUNDARYSHORELINE")

#reading in the raster file
jan90<-raster('C:/Users/lhamon/Documents/temp/PRISM_ppt_stable_4kmM3_199001_bil.bil')

#check map projections
counties_geog = spTransform(counties, crs(jan90))
plot(jan90)
plot(counties_geog, add=T)
proj4string(counties_geog)

#crop data and plot
jan90NC = crop(jan90, counties_geog)
plot(jan90NC)
plot(counties_geog,add=T)

#extract tempmean data
tempmean = extract(jan90NC, counties_geog, fun=mean)
temp[counties_geog@data$NAME == 'ORANGE']
head(tempmean)
print(tempmean)

