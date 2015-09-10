## READING IN RASTER LAYERS
setwd("C:/Users/lhamon/Documents/clim data/1990")

library(raster)

## Reading in raster data
# long-term mean temperature data downloaded from WorldClim.org

# Read in single raster layer from january
jan90<-raster('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_199001_bil.bil')
plot(tmean_jan)

#list all the files
filenames <- list.files("C:/Users/lhamon/Documents/clim data/bil files",pattern="_bil.bil")
mean<-stack(filenames)

# Read in stack of layers from all 12 months 1990
files1<-paste('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_19900',1:9,'_bil.bil',sep='')
files2<-paste('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_1990',10:12,'_bil.bil',sep='')
files = c(files1, files2)
tmean<-stack(files)
plot(tmean)

#function to read in a stack of layers from all 12 months for all 25 yrs
stackfiles <- function(id, directory, summarize = FALSE) {
  te1 <- formatC(id, width=3, flag="0")
  filename = paste(directory, te1, sep = "/")
  filename1 = paste(filename, "csv", sep = ".")
  test <- read.table(file = filename1, header=T, sep=",")
  if(summarize) {
    print(summary(test))
    return (test)
  } else {
    return (test)
  }
}




## EXTRACT TEMP MEANS FROM COUNTY DATA
#read in county boundary file
counties=readOGR("C:/Users/lhamon/Documents/boundarydata","COUNTYBOUNDARYSHORELINE")

#reading in the raster file
jan90<-raster('C:/Users/lhamon/Documents/clim data/1990/PRISM_ppt_stable_4kmM3_199001_bil.bil')

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



##TESTING FLIGHT DATE VS. AVG TEMP (USING P. GLAUCUS IN ORANGE)
setwd("C:/Users/lhamon/Dropbox/NC butterfly project")
alldat <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")
colnames(alldat)<-c("Cname", "sciName", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian", "voltinism", "voltinismnotes","diettype", "dietbreadth", "dietnotes","migratory","overwinter")

#subset data for observations made since 1990
alldat$year <-as.numeric(alldat$year) 
alldat <-subset(alldat, year > 1989)

#subset Papilio glaucus
glaucus<-subset(alldat, sciName=="Papilio glaucus")

#sort data by year and julian date and load dplyr
glaucus<-glaucus[order(glaucus$julian),] 
library(dplyr)

#here's a change

# subset glaucus data by year (I know there's a quicker way to do this)
glaucus_2004<-subset(glaucus, year=="2014")
###WILL RETURN TO THIS POINT AS I LEARN##
#pull mean monthly temp data for the 8 months prior to avg month of emergence for all years
setwd("C:/Users/lhamon/Documents/clim data/bil files")
files1<-paste('C:/Users/lhamon/Documents/clim data/bil files/PRISM_ppt_stable_4kmM3_19900',1:9,'_bil.bil',sep='')
files2<-paste('C:/Users/lhamon/Documents/clim data/bil files/PRISM_ppt_stable_4kmM3_1990',10:12,'_bil.bil',sep='')
files = c(files1, files2)
tmean<-stack(files)
plot(tmean)
#make this the x-value
#pull first flight date for all years
#make this the y value
#linear regression