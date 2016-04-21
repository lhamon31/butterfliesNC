###TEMPLATE FOR LOADING RASTER LAYERS AND CREATING FILES OF MEAN USING JAN 1990

setwd("C:/Users/lhamon/Documents/temp/1990")

library(raster)

## Reading in raster data
# long-term mean temperature data downloaded from WorldClim.org

# Read in single raster layer from january
files=list.files()
numfiles=length(files)
jan90<-raster(files[8])
plot(jan90)

# Read in stack of layers from all 12 months 1990 (doesn't work for these datasets for some reason)
files1<-paste('C:/Users/lhamon/Documents/temp/1990/PRISM_tmean_stable_4kmM2_19900',1:9,'_bil.bil',sep='')
files2<-paste('C:/Users/lhamon/Documents/temp/1990/PRISM_tmean_stable_4kmM2_1990',10:12,'_bil.bil',sep='')
files= c(files1, files2)
tmean<-stack(files)
plot(tmean)

## EXTRACT TEMP MEANS FROM COUNTY DATA
#read in county boundary file
library(rgdal)
counties=readOGR("C:/Users/lhamon/Documents/boundarydata","COUNTYBOUNDARYSHORELINE")

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

######################################################################################

###USING A FOR LOOP TO INPUT DATA AND EXTRACT MEANS BY COUNTY

## Read in single raster layer from january
#check map projections
counties_geog = spTransform(counties, crs(jan90))
plot(jan90)
plot(counties_geog, add=T)
proj4string(counties_geog)

#crop data and plot
jan90NC = crop(jan90, counties_geog)
plot(jan90NC)
plot(counties_geog,add=T)

years = 1991:1992

#for loop to read in all the files
output = matrix(NA, nrow = nrow(counties_geog@data), ncol = length(years))

for(y in years){
  filenames.y<- paste("C:/Users/lhamon/Documents/temp/", y, "/PRISM_tmean_stable_4kmM2_", y, "0", 1:4, "_bil.bil", sep="")
  filenames.prevy<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, "0", 9, "_bil.bil", sep="")
  filenames.prevy2<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, 10:12, "_bil.bil", sep="")
  filenames=c(filenames.y,filenames.prevy, filenames.prevy2)
  temp_allmonths<-stack(filenames)
  tmean = calc(temp_allmonths, mean)
  tempmean = extract(tmean, counties_geog, fun=mean)
  output[,which(years==y)] = tempmean
}  

output1 = data.frame(output)
names(output1) = years
output2 = cbind(counties_geog@data$NAME[2:nrow(output1)], output1[2:nrow(output1),])
names(output2)[1] = "county"


#plots year as x-axis and average over the 8 months as y axis
plot(years, output2[1,2:ncol(output2)], type = 'l', xlab = "Year", 
     ylab = "Mean temperature", ylim = c(5, 20))
points(years, output2[2, 2:ncol(output2)], type = 'l', col = 'red')

sapply(3:nrow(output2), function(x) 
  points(years, output2[x, 2:ncol(output2)], type = 'l'))
