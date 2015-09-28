###MAKING A REGRESSION PLOT WITH TEMP AS X-AXIS AND FIRST FLIGHT AS Y-AXIS USING A SINGLE SPP.; PAPILIO GLAUCUS
##x-values- temperature
setwd("~/temp")
files=list.files()
library(raster)

years = 1991:1992

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

#extract tempmean data
tempmean = extract(tmean,1991:1992, fun=mean)
temp[counties_geog@data$NAME == 'ORANGE']
head(tempmean)
print(tempmean)


output1 = data.frame(output)
names(output1) = years
output2 = cbind(counties_geog@data$NAME[2:nrow(output1)], output1[2:nrow(output1),])
names(output2)[1] = "county"

#change 'output 1' to be a vector 
cbind(output1)#cbind the years
NCmean<-data.frame(colMeans(output1,na.rm=T)) #mean each column using colmeans, with na.rm=T
#NCmean is a list, with the avg temp listed for each year. I want to now add the first flight date for each year and add it to this list


####ADDING FIRST FLIGHT DATA TO THE LIST




