#FINDING THE 10th and 25th FLIGHT DATE FOR EACH SPECIES FOR EACH YEAR
#AND MERGING THIS WITH THE TEMPERATURE DATA
#TO CREATE A NEW DOCUMENT WITH NEW PROXIES WITH WHICH TO RERUN MY ANALYSES
setwd("~/Documents/Biology/Biol 692H")
alldat <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")

#Changing column names to match the 2nd summary dataframe
colnames(alldat)<-c("Cname", "species", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian", "voltinism",
                    "voltinismnotes", "diettype", "dietbreadth", "dietnotes", "migratory", "overwinter")
alldat<-alldat[c("year","species",  "number", "julian")]

alldat <-subset(alldat, year > 1989)

alldat$ID<-paste(alldat$species,alldat$year, sep=".")


library(dplyr)

# First need to create a vector of dates for individuals
species<-unique(alldat$species)
year=1990:2014

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    earlydate=numeric())
#-------------------------------------------------------

# Pulling out date associated with first 10 individuals

#perform this function to return the 10th date
dateXth.ind = function(data, num) { #data is a df with number and julian cols
  data = data[order(data$julian, decreasing = F), ]
  data$cum = cumsum(data$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(data$julian[data$cum >= num])
  return(mindate)
}

#for loop to perform this for each species/year 
for (s in species){
  for (y in year){
    b=subset(alldat,year==y & species==s)
    mindate<-dateXth.ind(b,10)
    datoutput = data.frame(species = s, year = y, julian=mindate)
    output=rbind(output,datoutput)
  }
}

#merge with tempdat 
tempdat<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 395H/full.tempmean.11.10.2015.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4)]
colnames(tempjulian)<-c("species","year","temp","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file = "10th.fulldat.1.24.2016.csv")

#----------------------------------------------------------------------

# Pulling out date associated with first 25 individuals

#perform this function to return the 25th date
dateXth.ind = function(data, num) { #data is a df with number and julian cols
  data = data[order(data$julian, decreasing = F), ]
  data$cum = cumsum(data$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(data$julian[data$cum >= num])
  return(mindate)
}

#for loop to perform this for each species/year 
for (s in species){
  for (y in year){
    b=subset(alldat,year==y & species==s)
    mindate<-dateXth.ind(b,25)
    datoutput = data.frame(species = s, year = y, julian=mindate)
    output=rbind(output,datoutput)
  }
}

#merge with tempdat 
tempdat<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 395H/full.tempmean.11.10.2015.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4)]
colnames(tempjulian)<-c("species","year","temp","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file = "25th.fulldat.1.24.2016.csv")