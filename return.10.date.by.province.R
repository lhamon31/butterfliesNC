#FINDING THE 10 % and 25% FLIGHT DATE FOR EACH SPECIES FOR EACH YEAR
#AND MERGING THIS WITH THE TEMPERATURE DATA
#TO CREATE A NEW DOCUMENT WITH NEW PROXIES WITH WHICH TO RERUN MY ANALYSES
setwd("~/Documents/Biology/butterfly paper 2016")
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")

#adding province labels
labels <-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/NCbutterflies.65species.June2015.csv")
labels<-labels[c("county","province")]
labels<-unique(labels)
dat<-merge(alldat,labels, by.x=c("county"),by.y=c("county"), all.x = T, all.y = T)

#Changing column names to match the 2nd summary dataframe
colnames(dat)<-c("county","x","Cname","species","observer","number","comments","dateCalc","year","julian",
                 "voltinism","voltinismnotes","diettype","dietbreadth","dietnotes","migratory","overwinter",
                 "province")
alldat<-dat[c("species","year","number", "julian","province")]
alldat <-subset(alldat, year > 1989)

#create unique ids 
alldat$ID<-paste(alldat$species,alldat$year,alldat$province, sep=".")

library(dplyr)

# First need to create a vector of dates for individuals
species<-unique(alldat$species)
year=1990:2016
province<-unique(alldat$province)

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    province=character(),
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

#for loop to perform this for each species/year/region 
for (s in species){
  for (p in province){
    for (y in year){
      b=subset(alldat, year==y & species==s & province==p)
      mindate<-dateXpct.ind(b,0.10)
      datoutput = data.frame(species = s,province=p,year = y, julian=mindate)
      output=rbind(output,datoutput)
    }
  }
}

output2<-na.omit(output) #note: many nas where there were not enough data
write.csv(output2,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlydate.by.province.csv")

######################################################################################################
#merge with tempdat 
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/full.tempmean.11.10.2015.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4)]
colnames(tempjulian)<-c("species","year","temp","province","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file = "10.percent.by.province.2.17.2016.csv")

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
tempdat<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 395H/full.tempmean.11.10.2015.csv")
tempjulian = merge(tempdat, output, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
tempjulian <- tempjulian[ -c(3:4)]
colnames(tempjulian)<-c("species","year","temp","earlydate")
tempjulian<-na.omit(tempjulian)

#remove those values with "inf"
tempjulian<-do.call(data.frame,lapply(tempjulian, function(x) replace(x, is.infinite(x),NA))) #replaces "inf" with "NA"
tempjulian<-na.omit(tempjulian) #removes NA values

#create a csv that includes temperature and julian dates
write.csv(tempjulian, file = "25.percent.fulldat.1.24.2016.csv")
