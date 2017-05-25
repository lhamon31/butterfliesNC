#####LOADING SPATIAL DATA, CHECKING MAP PROJECTIONS

##x-values- temperature
setwd("~/temp data/1990")
files=list.files()
library(raster)

# Read in single raster layer from january
files=list.files()
numfiles=length(files)
jan90<-raster(files[8])

#read in county boundary file
library(rgdal)
counties=readOGR("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/boundarydata","COUNTYBOUNDARYSHORELINE")

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

mean.earlydate<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlymonth.2016.csv") #issue is possibly extra column. 
species = unique(mean.earlydate$species)
years = 1990:2016


#for loop to read in all the files
output = data.frame(county=character(),
                    species = character(),
                    year = integer(),
                    temp = numeric())

# Specify months of climate data to get
numMonths = 8 # You can change this here if you decide to use a longer or shorter window

for (s in species) { # add a species loop to pull out species-specific arrival month. Approx runtime=24hrs
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
    filenames.y<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, monthsText1, "_bil.bil", sep="")
    filenames.prevy <-filenames<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, monthsText2, "_bil.bil", sep="")
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

#save output from for loop
write.csv(output, file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.by.province.8.months.csv")

#merge with province labels
    #an issue- in the temperature script, it returns county names in shapefile fomrat, ie "ORANGE" 
    #in the earlydate script, it returns county names the way they're formatted in the approx. "ie Orange"
    #solution: in theory u just need to tack on othe earlydate values onto the file created here
labels <-read.csv("C:/Users/lhamo/Documents/Biology/BIOL 692H/fixing.province.names.2.24.2016.csv")
dat<-merge(output,labels, by.x=c("county"),by.y=c("county"), all.x = T, all.y = T)

#aggregate data by region
#mean the county temp values FOR each species, FOR each year, FOR each province
aggdat <-aggregate(dat, by=list(dat$species,dat$year,dat$province), FUN=mean, na.rm=TRUE)
mountain.group<-subset(aggdat,aggdat$Group.3=="M")
piedmont.group<-subset(aggdat,aggdat$Group.3=="P")
coast.group<-subset(aggdat,aggdat$Group.3=="C")

#clean the above files
mountains<-mountain.group[c("Group.1","Group.3","year","temp")]
colnames(mountains)<-c("species","province","year","temp")

piedmont<-piedmont.group[c("Group.1","Group.3","year","temp")]
colnames(piedmont)<-c("species","province","year","temp")

coast<-coast.group[c("Group.1","Group.3","year","temp")]
colnames(coast)<-c("species","province","year","temp")


#####ADD JULIAN DATE AND MERGE

#load earlydat data. Note that the province labels here are according to their original subdivisions, so that should be changed
earlydate<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlydate.by.province.csv")
earlydate<-do.call(data.frame,lapply(earlydate, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
earlydate<-na.omit(earlydate) #removes NA values

#subset by province
M.earlydate<-subset(earlydate,province=="M")
P.earlydate<-subset(earlydate,province=="P")
C.earlydate<-subset(earlydate,province=="C")

#merge earlydate and tempdat for each region
M.tempjulian = merge(mountains, M.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
M.tempjulian<-na.omit(M.tempjulian)
colnames(M.tempjulian)<-c("species","year","province","temp","X","province.y","julian")
M.tempjulian<-M.tempjulian[c("species","year","province","temp","julian")]

P.tempjulian = merge(piedmont, P.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
P.tempjulian<-na.omit(P.tempjulian)
colnames(P.tempjulian)<-c("species","year","province","temp","X","province.y","julian")
P.tempjulian<-P.tempjulian[c("species","year","province","temp","julian")]

C.tempjulian = merge(coast, C.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
C.tempjulian<-na.omit(C.tempjulian)
colnames(C.tempjulian)<-c("species","year","province","temp","X","province.y","julian")
C.tempjulian<-C.tempjulian[c("species","year","province","temp","julian")]

#save as csv for each region
write.csv(M.tempjulian,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/mountain.fulldat.csv")
write.csv(P.tempjulian,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/piedmont.fulldat.csv")
write.csv(C.tempjulian,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/coast.fulldat.csv")



