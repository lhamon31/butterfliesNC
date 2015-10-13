###MAKING A REGRESSION PLOT WITH TEMP AS X-AXIS AND FIRST FLIGHT AS Y-AXIS USING A SINGLE SPP.; PAPILIO GLAUCUS
##x-values- temperature
setwd("~/temp")
files=list.files()
library(raster)

# Read in single raster layer from january
files=list.files()
numfiles=length(files)
jan90<-raster(files[8])

#read in county boundary file
library(rgdal)
state=readOGR("C:/Users/lhamon/Documents/boundarydata","cb_2014_us_state_500k")
NC<-state[state$NAME=="North Carolina",]

#check map projections
NC_geog = spTransform(NC, crs(jan90))
plot(jan90)
plot(NC_geog, add=T)
proj4string(NC_geog)

#crop data and plot
jan90NC = crop(jan90, NC_geog)
plot(jan90NC)
plot(NC_geog,add=T)

#############################################################################
#create list of 'species' with 'month of arrival' for subsequent analysis
# Load libraries
library(lubridate)

# Loading full dataset
alldat<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/ncbutterfly.tenpercent.summary.9.29.2015.csv")

#mean early date for each species
mean.earlydate<-aggregate(alldat[,4], list(alldat$species), mean, na.rm=T)
names(mean.earlydate) = c('species', 'julianday')

#converts julian date to numerical month 
mean.month<-month(strptime(paste(mean.earlydate[,2]),format="%j")) 

#adds arrivalMonth to mean.earlydate dataframe
mean.earlydate$arrivalMonth = mean.month

#############################################################################

years = 1993:2014

#for loop to read in all the files
output = matrix(NA, nrow = nrow(NC_geog@data), ncol = length(years))

for(y in years){
  filenames.y<- paste("C:/Users/lhamon/Documents/temp/", y, "/PRISM_tmean_stable_4kmM2_", y, "0", 1:4, "_bil.bil", sep="")
  filenames.prevy<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, "0", 9, "_bil.bil", sep="")
  filenames.prevy2<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, 10:12, "_bil.bil", sep="")
  filenames=c(filenames.y,filenames.prevy, filenames.prevy2)
  temp_allmonths<-stack(filenames)
  tmean = calc(temp_allmonths, mean)
  tempmean = extract(tmean, NC_geog, fun=mean)
  output[,which(years==y)] = tempmean
}  

output1 = data.frame(output)
names(output1) = years
output2 = cbind(NC_geog@data$NAME[2:nrow(output1)], output1[2:nrow(output1),])
names(output2)[1] = "state"

#change 'output 1' to be a vector 
cbind(output1)#cbind the years
NCtempmean<-data.frame(colMeans(output1,na.rm=T)) #mean each column using colmeans, with na.rm=T
#NCmean is a list, with the avg temp listed for each year. I want to now add the first flight date for each year and add it to this list

########################################################################################3333
#ADDING FIRST FLIGHT DATA TO THE LIST

MyData<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/ncbutterfly.tenpercent.summary.9.29.2015.csv")

#subsetting papilio glaucus
earlydate<-MyData[890:911,4]

##########################################################################################
##COMBINE THE VARIABLES AND GRAPH
papilio<-cbind(NCtempmean,earlydate)

#plots temperature das x-axis and first flight as y
fit<-lm(papilio)
summary(fit)
plot(papilio, xlab = " Mean temperature", ylab = "julian")
abline(lm(papilio))



