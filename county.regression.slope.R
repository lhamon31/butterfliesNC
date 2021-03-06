#calculating the first flight dates for buncombe, wake, pitt counties, generating a dataframe/boxplot of the slopes
## FIND THE EARLYDATE FOR EACH SPECIES FOR EACH YEAR FOR EACH COUNTY ##
# Loading full dataset
alldat2<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")
colnames(alldat2)<-c("Cname", "species", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian")
alldat2<-alldat2[c("species","county", "year", "number", "julian")]
alldat2<-subset(alldat2,year>1989)
alldat<- alldat2[alldat2$county %in% c("Buncombe", "Pitt","Wake"),]

# creating a unique ID vector for each species, year, and province
alldat$ID<-paste(alldat$species, alldat$county, alldat$year, sep=".")

# sorting each dataset by this new vector
alldat<-alldat[order(alldat$ID,alldat$julian),]

# Determining the total number of entries per species per year
library(plyr)
sum3<-table(alldat$county,alldat$year,alldat$species)
sum3<-as.data.frame(sum3)
colnames(sum3)<-c("county", "year","species","total.entries")
# Include rows with greater than "0" in total.entries
sum<-subset(sum3, (total.entries > 0))

#find the number of rows you need to subset by
percent_10<-as.data.frame(sum$total.entries*.1)
sum2<-cbind(sum,percent_10)
colnames(sum2)<-c("county","year","species","total.entries","percent_10")

# rounding the ten percent variable
sum2$percent_10<-round(sum2$percent_10, digits=0)

#merge with alldat
county.dat= merge(sum2, alldat, by.x = c('county','species','year'), by.y =c('county','species','year'), all.x = T, all.y = T)

# reformating dataframe as a datatable to subset the 10%
require(data.table)
county.dat<-data.table(county.dat)
county.dat<-county.dat[,.SD[order(julian,decreasing=FALSE)[1:percent_10]],by=ID]

# Calculating weighted mean of emergence date per year per county by number of sightings
library(plyr)
library(SDMTools)
wm<-ddply(county.dat, ~ species + year + county, function(x) wt.mean(x$julian, x$number))
# Get the same answers between weight.mean and wt.mean so going to use wt.mean
colnames(wm)<-c("species", "year", "county","earlydate")



##MERGE WITH TEMP DATA FOR EACH COUNTY
#load the temperature data
tempdata<-read.csv("C:/Users/lhamon/Documents/Biology/BIOL 395H/temp.data.bycounty.11.10.2015.csv") #This file includes the temperature data for each of the three counties for each year. It has 3 columns ("county", "year", and "temp") and 75 rows

#merge the temperature data with the calculated earlydates
county.fulldat<-merge(tempdata, wm, by.x = c('county','year'), by.y =c('county','year'), all.x = T, all.y = T) #county.fulldat includes "county", "year", "species", "temp" and "earlydate". It has 2583 rows.

#remove rows w/ NA from county.fulldat
row.has.na <- apply(county.fulldat, 1, function(x){any(is.na(x))})
sum(row.has.na)
county.clean <-county.fulldat[!row.has.na,]

##GENERATE AN OUTPUT OF SPECIES SLOPES FOR EACH COUNTY##
#subset county.fulldat by county for individual analysis
buncombe.dat<-subset(county.clean, county=="Buncombe")

wake.dat<-subset(county.clean, county=="Wake")

pitt.dat<-subset(county.clean, county=="Pitt")


# output1:Buncombe
output1 = data.frame(species = character(),
                    slope = numeric(),
                    rsquared = numeric(),
                    pvalue = numeric())

species<-unique(buncombe.dat$species)

for (s in species){
  df<-buncombe.dat[buncombe.dat$species==s,]
  lm.sub<-lm(df$earlydate~df$temp)
  slope<-summary(lm.sub)$coefficients[2, 1]
  rsquared<-summary(lm.sub)$r.squared
  pvalue<-summary(lm.sub)$coefficients[2,4]
  tempoutput<-data.frame(species=s, slope= slope, rsquared= rsquared, pvalue= pvalue)
  output1<-rbind(output1,tempoutput)
}

write.csv(output1, file="C:/Users/lhamon/Documents/Biology/BIOL 395H/buncombe.output.11.10.2015.csv")

#output2: Wake
output2 = data.frame(species = character(),
                     slope = numeric(),
                     rsquared = numeric(),
                     pvalue = numeric())

species<-unique(wake.dat$species) 

for (s in species){
  df<-wake.dat[wake.dat$species==s,]
  lm.sub<-lm(df$earlydate~df$temp)
  slope<-summary(lm.sub)$coefficients[2,1]
  rsquared<-summary(lm.sub)$r.squared
  pvalue<-summary(lm.sub)$coefficients[2,4]
  tempoutput<-data.frame(species=s, slope= slope, rsquared= rsquared, pvalue= pvalue)
  output2<-rbind(output2,tempoutput)
}

write.csv(output2, file="C:/Users/lhamon/Documents/Biology/BIOL 395H/wake.output.11.10.2015.csv")

#output3: Pitt
#version of pitt.dat with species with only one entry (interferes with regression) removed 
pitt.dat<-read.csv("C:/Users/lhamon/Documents/Biology/BIOL 395H/pitt.dat.csv")
output3 = data.frame(species = character(),
                     slope = numeric(),
                     rsquared = numeric(),
                     pvalue = numeric())

species<-unique(pitt.dat$species)
