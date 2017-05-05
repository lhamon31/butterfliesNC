#####LOADING SPATIAL DATA, CHECKING MAP PROJECTIONS

#make some crazy edits

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

#####READING IN ALL APPROPRIATE TEMP FILES AND FINDING NCTEMPMEAN

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
    output = rbind(output, tempoutput)
  } # end of year loop
} # end of species loop

# The output dataframe has 4 columns: the county name, species name, year, and mean temperature
# for that 8-month window for that year.

write.csv(output,file="species.year.tempmean.10.13.2015.csv")

#####ADD JULIAN DATE AND MERGE

MyData<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/ncbutterfly.tenpercent.summary.9.29.2015.csv")

mydat<-subset(MyData[,2:4])

tempjulian = merge(output, mydat, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)

earlydate<-tempjulian$earlydate
temp<-tempjulian$temp #check you haven't overrided some other object here

#plots temperature das x-axis and first flight as y
lm(formula = earlydate ~ temp, data = tempjulian, subset = species)
plot(earlydate ~ temp, groups=species, data=tempjulian)
