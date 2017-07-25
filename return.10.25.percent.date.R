#FINDING THE 10 % and 25% FLIGHT DATE FOR EACH SPECIES FOR EACH YEAR
#AND MERGING THIS WITH THE TEMPERATURE DATA
#TO CREATE A NEW DOCUMENT WITH NEW PROXIES WITH WHICH TO RERUN MY ANALYSES
setwd("~/Documents/Biology/Biol 692H")
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")

#Changing column names to match the 2nd summary dataframe
colnames(alldat)<-c("x","Cname", "species", "county", "observer", "number", "comments", "dateCalc", "year", "julian", "voltinism",
                    "voltinismnotes", "diettype", "dietbreadth", "dietnotes", "migratory", "overwinter")
alldat<-alldat[c("year","species","number", "julian")]

alldat <-subset(alldat, year > 1989)

alldat$ID<-paste(alldat$species,alldat$year,sep=".")


library(dplyr)

# First need to create a vector of dates for individuals
species<-unique(alldat$species)
year=1990:2016

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    julian=numeric())
#-------------------------------------------------------

# Pulling out date associated with first 10 % individuals

#perform this function to return the 10 % date
dateXpct.ind = function(data, pct) {
  data = data[order(data$julian, decreasing = F), ]
  data$cumindividuals = cumsum(data$number)
  data$cumpct = data$cumindividuals/sum(data$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(data$julian[data$cumpct >= pct])
  return(mindate)
}

#for loop to perform this for each species/year 
for (s in species){
  for (y in year){
    b=subset(alldat,year==y & species==s)
    mindate<-dateXpct.ind(b,0.10)
    datoutput = data.frame(species = s, year = y, julian=mindate)
    output=rbind(output,datoutput)
  }
}

#save output
write.csv(output, file = "earlydates.thru.2016.csv")

#merge with tempdat 
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.8.months.2016.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3,6)]
colnames(tempjulian)<-c("species","year","temp","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.8.months.2016.csv")

#-------------------------------------------------------------------------

# Pulling out date associated with first 25 % individuals

#perform this function to return the 25 % date
dateXpct.ind = function(data, pct) {
  data = data[order(data$julian, decreasing = F), ]
  data$cumindividuals = cumsum(data$number)
  data$cumpct = data$cumindividuals/sum(data$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(data$julian[data$cumpct >= pct])
  return(mindate)
}

#for loop to perform this for each species/year 
for (s in species){
  for (y in year){
    b=subset(alldat,year==y & species==s)
    mindate<-dateXpct.ind(b,0.25)
    datoutput = data.frame(species = s, year = y, julian=mindate)
    output=rbind(output,datoutput)
  }
}

#merge with tempdat 
output<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlydates.thru.2016.csv")
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.4.months.2016.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4,6)]
colnames(tempjulian)<-c("species","year","temp","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.4months.NC.2016.csv")
