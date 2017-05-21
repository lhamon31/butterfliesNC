# Read in file provided by Tom Howard.
ncb = read.table('county.csv', sep = '\t', header=T, quote = '\"', comment.char="")
ncb=read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/2016_bnc_records_csv.csv")

# View the unique species names and the number of times each occurs
library(dplyr)
library(lubridate)
data.frame(count(ncb, comName))

# Fix duplicate names due to typos, using a convention of capitalization after a '-'
ncb$comName = gsub("Black Swallowtail ", "Black Swallowtail", ncb$comName)
ncb$comName = gsub("Byssus Skipper ", "Byssus Skipper", ncb$comName)
ncb$comName = gsub("Carolina Roadside-skipper", "Carolina Roadside-Skipper", ncb$comName)
ncb$comName = gsub("Common Checkered-skipper", "Common Checkered-Skipper", ncb$comName)
ncb$comName = gsub("Common Roadside-skipper", "Common Roadside-Skipper", ncb$comName)
ncb$comName = gsub("Common Wood-nymph", "Common Wood-Nymph", ncb$comName)
ncb$comName = gsub("Eastern Tailed-blue", "Eastern Tailed-Blue", ncb$comName)
ncb$comName = gsub("Lace-winged Roadside-skipper", "Lace-winged Roadside-Skipper", ncb$comName)
ncb$comName = gsub("Little Wood-satyr", "Little Wood-Satyr", ncb$comName)
ncb$comName = gsub("Northern Broken-dash", "Northern Broken-Dash", ncb$comName)
ncb$comName = gsub("Northern Pearly-eye", "Northern Pearly-Eye", ncb$comName)
ncb$comName = gsub("Palamedes Swallowtail ", "Palamedes Swallowtail", ncb$comName)
ncb$comName = gsub("Pepper And Salt Skipper", "Pepper and Salt Skipper", ncb$comName)
ncb$comName = gsub("Question Mark ", "Question Mark", ncb$comName)
ncb$comName = gsub("Reversed Roadside-skipper", "Reversed Roadside-Skipper", ncb$comName)
ncb$comName = gsub("Silver-spotted Skipper", "Silver-Spotted Skipper", ncb$comName)
ncb$comName = gsub("Southern Broken-dash", "Southern Broken-Dash", ncb$comName)
ncb$comName = gsub("Southern Pearly-eye", "Southern Pearly-Eye", ncb$comName)
ncb$comName = gsub("Yucca Giant-skipper", "Yucca Giant-Skipper", ncb$comName)

# Fix instances of "Macon " county
ncb$county = gsub("Macon ", "Macon", ncb$county)

ncb$dateCalc = as.Date(paste(ncb$month,ncb$day,ncb$year, sep="/"), format = "%m/%d/%Y")
ncb$year = format(ncb$dateCalc, "%Y")
ncb$jd = yday(ncb$dateCalc)
ncb$dateCalc<- format(as.Date(ncb$dateCalc), "%m/%d/%Y")

#rename columns
colnames(ncb)<-c("Cname","county","observer","number","comments","year","month","day","province","coid","dateCalc","jd")
ncb<-as.data.frame(ncb)

#rearrange dateCalc
ncb<-ncb[,c("Cname","county","observer","number","comments","dateCalc","year","jd")]

#load full dataset
ncb2015=read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/approximation through 2015.csv")

#combine 2016 and full datasets
fulldat<-rbind(ncb2015,ncb)

#add species names
speciesnames<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/species list 2017.csv")

#merge fulldat and species names 
dat<-merge(fulldat,speciesnames, by=c("Cname"))

#reorder columns
colnames(dat)<-c("Cname","county","observer","number","comments","dateCalc","year","jd","species","voltinism","voltinismnotes","diettype","dietbreadth","dietnotes","migratory","overwinter")
dat2<-dat[,c("Cname", "species", "county","observer","number","comments","dateCalc","year","jd","voltinism","voltinismnotes","diettype","dietbreadth","dietnotes","migratory","overwinter")]

#write dataset
write.csv(dat2,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/approximation_thru_2016.csv")




