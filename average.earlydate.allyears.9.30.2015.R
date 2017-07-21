##SCRIPT TO FIND THE AVERAGE FIRST FLIGHT DATE (ACROSS ALL YEARS) FOR ALL SPECIES
#will be used to 'walk back' the months for analysis
setwd("~/Biology/butterfly paper 2016")

# Loading full dataset
# Loading full dataset
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlydate.by.province.csv")
alldat<- alldat[ -c(1)]

#change infs to nas
is.na(alldat) <- sapply(alldat, is.infinite)

#mean early date for each species in NC
mean.earlydate<-aggregate(alldat[,4], list(alldat$species), mean, na.rm=T)
names(mean.earlydate) = c('species', 'julian')
#mean early date for species for each province
mean.earlydate<-aggregate(alldat[,4], list(alldat$species,alldat$province), mean, na.rm=T)
names(mean.earlydate) = c('species', 'province','earlydate')

#converts julian date to numerical month 
mean.month<-month(strptime(paste(mean.earlydate[,3]),format="%j")) 

#adds arrivalMonth to mean.earlydate dataframe
mean.earlydate$arrivalMonth = mean.month

#by state
write.csv(mean.earlydate, file = "C:/Users/lhamo/Documents/Biology/butterfly paper 2016/earlymonth.2016.csv")
#by province
write.csv(mean.earlydate, file = "C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlymonth.province.2016.csv")


 
