##SCRIPT TO FIND THE AVERAGE FIRST FLIGHT DATE (ACROSS ALL YEARS) FOR ALL SPECIES
#will be used to 'walk back' the months for analysis
setwd("C:/Users/lhamon/Dropbox/NC butterfly project")

# Loading full dataset
alldat<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/ncbutterfly.tenpercent.summary.9.29.2015.csv")

#mean early date for each species
mean.earlydate<-aggregate(alldat[,4], list(alldat$species), mean)

#translating earlydate (julian)
mean.month<-month(strptime(paste(mean.earlydate[,2]),format="%j")) #converts julian date to numberical month 
mean.month<-as.data.frame(mean.month) #converts to data frame
month.list<-cbind(mean.earlydate,mean.month) 
 
