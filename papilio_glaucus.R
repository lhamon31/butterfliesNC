###MAKING A REGRESSION PLOT WITH TEMP AS X-AXIS AND FIRST FLIGHT AS Y-AXIS USING A SINGLE SPP.; PAPILIO GLAUCUS
##x-values- temperature
setwd("~/temp")
files=list.files()
library(raster)

years = 1993:2014

#for loop to read in all the files
output = matrix(NA, nrow = nrow(counties_geog@data), ncol = length(years))

for(y in years){
  filenames.y<- paste("C:/Users/lhamon/Documents/temp/", y, "/PRISM_tmean_stable_4kmM2_", y, "0", 1:4, "_bil.bil", sep="")
  filenames.prevy<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, "0", 9, "_bil.bil", sep="")
  filenames.prevy2<-paste("C:/Users/lhamon/Documents/temp/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, 10:12, "_bil.bil", sep="")
  filenames=c(filenames.y,filenames.prevy, filenames.prevy2)
  temp_allmonths<-stack(filenames)
  tmean = calc(temp_allmonths, mean) #calcultates the mean temp for all the 8 months in a particular year
  tempmean = extract(tmean, y, fun=mean) #originaly this was extracting the tmean values for each county- we want the t mean values for each year
  output[,which(years==y)] = tempmean
} 

output1 = data.frame(output)
names(output1) = years
output2 = cbind(counties_geog@data$NAME[2:nrow(output1)], output1[2:nrow(output1),])
names(output2)[1] = "county"

#change 'output 1' to be a vector 
cbind(output1)#cbind the years
NCmean<-data.frame(colMeans(output1,na.rm=T)) #mean each column using colmeans, with na.rm=T
#NCmean is a list, with the avg temp listed for each year. I want to now add the first flight date for each year and add it to this list

########################################################################################3333
#ADDING FIRST FLIGHT DATA TO THE LIST
##ESTIMATING FIRST DATE USING 10% MEAN 
setwd("C:/Users/lhamon/Dropbox/NC butterfly project")

# Loading full dataset
alldat<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")

# Changing column names to match the 2nd summary dataframe
colnames(alldat)<-c("Cname", "species", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian")
alldat<-alldat[c("species", "year", "province", "number", "dateCalc", "julian")]

# Loading the summary dataset with the ten percent value already calculated
sumdat<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/ncbutterfly.summary.6.25.15.csv")

# Subsetting data for observations made between 1990 to 2013
sumdat <- subset(sumdat, year > 1990)
alldat <-subset(alldat, year > 1990)

# creating a unique ID vector for each species and year
alldat$ID<-paste(alldat$species, alldat$year, sep=".")
sumdat$ID<-paste(sumdat$species, sumdat$year, sep=".")

# sorting each dataset by this new vector
sumdat<-sumdat[order(sumdat$ID),]
alldat<-alldat[order(alldat$ID,alldat$julian),]

# subsetting sumdat before merging
sumdat<-sumdat[c("ID", "percent_10")]

# rounding the ten percent variable
sumdat$percent_10<-round(sumdat$percent_10, digits=0)

# merging alldat and sumdat
df<-merge(alldat, sumdat, by.x="ID")
df<-subset(df, percent_10 > 0)

#load data.table
library(data.table)

# reformating dataframe as a datatable to subset the 10%
require(data.table)
df<-data.table(df)
df<-df[,.SD[order(julian,decreasing=FALSE)[1:percent_10]],by=ID]

# Determining 1st date of appearance and summing total number of butterflies per year
library(plyr)
sum1<-ddply(df, ~ species + year, summarize, earlydate=min(julian),total.year=sum(number))

# Determining summing total number of butterflies per species
sum2<-ddply(df, ~ species, summarize, total.abundance=sum(number))

# Determining the total number of entries per species per year
sum3<-table(df$year,df$species)
sum3<-as.data.frame(sum3)
colnames(sum3)<-c("year","species","total.entries")
# Include rows with greater than "0" in total.entries
sum3<-subset(sum3, (total.entries > 0))

sum<-cbind(sum1,sum3[,3])
colnames(sum)<-c("species", "year", "earlydate","total.year", "total.entries")

# Calculating weighted mean of emergence date per year by number of sightings
library(plyr)
wm<-ddply(df, ~ species + year, function(x) wt.mean(x$julian, x$number))
# Get the same answers between weight.mean and weighted.mean so going to use weighted.mean
colnames(wm)<-c("species", "year", "mean")

# Calculating standard deviation of the weighted mean of emergence date
sd<-ddply(df, ~ species + year, function(x) wt.sd(x$julian, x$number))
colnames(sd)<-c("species", "year", "sd")

# Calculating variance of the weighted mean of emergence date
var<-ddply(df, ~ species + year, function(x) wt.var(x$julian, x$number))
colnames(var)<-c("species", "year", "variance")

# Calculating median of emergence date
med<-ddply(df, ~ species + year, function(x) median(x$julian))
colnames(med)<-c("species", "year", "median")

# Combine wm, sd, and med by sciName with earlydate, total.year, and total.entries
MyData<-cbind(sum, wm[,3],sd[,3],var[,3],med[,3])
colnames(MyData)<-c("species", "year", "earlydate","total", "total.entries","mean","sd", "variance", "median")

# Calculating min, max, mean, median
min<-aggregate(total.entries ~ species, min, data = MyData)
max<-aggregate(total.entries ~ species, max, data = MyData)
mean<-aggregate(total.entries ~ species, mean, data = MyData)
median<-aggregate(total.entries ~ species, median, data = MyData)
total<-ddply(df, ~ species, summarize, total.entries=sum(number))

# Combine min, max, mean, median 
SumData<-cbind(total, min[,2], max[,2], mean[,2], median[,2])
colnames(SumData)<-c("species", "total.entries", "min", "max", "mean", "median")

write.csv(MyData, file = "ncbutterfly.tenpercent.summary.9.29.2015.csv")

#subsetting papilio glaucus
papilio<-MyData[890:911,3]





