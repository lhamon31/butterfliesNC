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
species = unique(mean.earlydate$species)
years = 1993:2014

#for loop to read in all the files
output = data.frame(county = character(),
                    species = character(),
                    year = integer(),
                    temp = numeric())

# Specify months of climate data to get
numMonths = 8 # You can change this here if you decide to use a longer or shorter window

for (s in species) { # add a species loop to pull out species-specific arrival month
  arrivMonth = mean.earlydate$arrivalMonth[mean.earlydate$species == s]
  monthsToGet1 = max(1, (arrivMonth - (numMonths - 1))):arrivMonth
  if (arrivMonth < numMonths) {
    monthsToGet2 = (arrivMonth + (12 - numMonths + 1)):12
  }
  monthsText1 = paste("0", monthsToGet1, sep = "")
  monthsText2 = sapply(monthsToGet2, function(x) {
    if (nchar(monthsToGet2)[monthsToGet2 == x] == 1) { paste("0", x, sep = "") 
    } else if (nchar(monthsToGet2)[monthsToGet2 == x] == 2) { paste(x) }})
  
  # Now you should have a character string of months that you want to extract
  # called monthsText1 which are the early months in the year c("01", "02", "03", ...)
  # and monthsText2 which are the later months, e.g., c("09", "10", "11", "12").
  
  # Get temperature data across years for the 8-month period up to and including
  # the arrival month for species s
  for(y in years){
    filenames.y<- paste("C:/Users/lhamon/Documents/temp/", y, "/PRISM_tmean_stable_4kmM2_", y, monthsText1, "_bil.bil", sep="")
    filenames.prevy <-filenames<- paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, monthsText2, "_bil.bil", sep="")
    filenames = c(filenames.y, filenames.prevy)
    temp_allmonths<-stack(filenames)
    tmean = calc(temp_allmonths, mean)
    tempmean = extract(tmean, NC_geog, fun=mean)
    tempoutput = data.frame(county = NC_geog@data$NAME, species = s, year = y, temp = tempmean)
    output = rbind(output, tempmean)
  } # end of year loop
} # end of species loop

# The output dataframe has 4 columns: the county name, species name, year, and mean temperature
# for that 8-month window for that year.


#NCtempmean<-data.frame(colMeans(output1,na.rm=T)) #mean each column using colmeans, with na.rm=T
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



