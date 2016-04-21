#####LOADING SPATIAL DATA, CHECKING MAP PROJECTIONS

##x-values- temperature
setwd("~/temp/1990")
files=list.files()
library(raster)

# Read in single raster layer from january
files=list.files()
numfiles=length(files)
jan90<-raster(files[8])

#read in county boundary file
library(rgdal)
counties=readOGR("C:/Users/lhamon/Documents/Documents/CountyBoundaryShoreline2","COUNTYBOUNDARYSHORELINE")

#check map projections
counties_geog = spTransform(counties, crs(jan90))
plot(jan90)
plot(counties_geog, add=T)
proj4string(counties_geog)

#crop data and plot
jan90NC = crop(jan90, counties_geog)
plot(jan90NC)
plot(counties_geog,add=T)



#####READING IN ALL APPROPRIATE TEMP FILES AND FINDING NCTEMPMEAN

mean.earlydate<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 395H/mean.arrivalmonth.11.9.2015.csv")

species = unique(mean.earlydate$species)
years = 1990:2014


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
    tempmean = extract(tmean, counties_geog, fun=mean)
    tempoutput = data.frame(county = counties_geog@data$NAME, species = s, year = y, temp = tempmean)#check whether NAME is right here
    output = rbind(output, tempoutput)
  } # end of year loop
} # end of species loop

# The output dataframe has 4 columns: the county name, species name, year, and mean temperature
# for that 8-month window for that year.

write.csv(output, file="C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/tempmean.by.province.2.22.2016.csv")

#merge with province labels
labels <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")
labels<-labels[c("county","province")]


#####ADD JULIAN DATE AND MERGE



tempdat<-read.csv("C:/Users/lhamon/Documents/Biology/BIOL 395H/full.tempmean.11.10.2015.csv")
earlydate<-read.csv("C:/Users/lhamon/Documents/Biology/BIOL 395H/NC.earlydate.11.9.2015.csv")

earlydate<-na.omit(earlydate)

tempjulian = merge(tempdat, earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4,6)]
colnames(tempjulian)<-c("species","year","temp","earlydate")

mydat<-subset(MyData[,2:4])

tempjulian = merge(output, mydat, by.x = c('species','year'), by.y =c('species'), all.x = T, all.y = T)

write.csv(tempjulian,file="C:/Users/lhamon/Documents/Biology/BIOL 395H/fulldat.11.10.2015.csv")

earlydate<-tempjulian$earlydate
temp<-tempjulian$temp #check you haven't overrided some other object here

#plots temperature das x-axis and first flight as y
lm(formula = earlydate ~ temp, data = tempjulian, subset = species)
plot(earlydate ~ temp, groups=species, data=tempjulian)

#temp+species
#temp*species is the same as temp+species+temp:species
#geomabline()
