##TESTING FLIGHT DATE VS. AVG TEMP (USING P. GLAUCUS IN ORANGE IN 2012-2014 and first appearance as MARCH)

###pull mean monthly (ppt) data for the 8 months prior to avg month of emergence for all years
setwd("C:/Users/lhamon/Documents/clim data")
files1<-paste('C:/Users/lhamon/Documents/clim data/2012/PRISM_ppt_stable_4kmM3_20120',1:3,'_bil.bil',sep='')
files2<-paste('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_20110',8:9,'_bil.bil',sep='')
files3<-paste('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_2011',10:12,'_bil.bil',sep='')
files = c(files1, files2,files3)
tmean<-stack(files)
plot(tmean)

#read in county boundary file
library(rgdal)
counties=readOGR("C:/Users/lhamon/Documents/boundarydata","COUNTYBOUNDARYSHORELINE")

#reading in the raster file
mar<-raster('C:/Users/lhamon/Documents/clim data/2012/PRISM_ppt_stable_4kmM3_201203_bil.bil')
feb<-raster('C:/Users/lhamon/Documents/clim data/2012/PRISM_ppt_stable_4kmM3_201202_bil.bil')
jan<-raster('C:/Users/lhamon/Documents/clim data/2012/PRISM_ppt_stable_4kmM3_201201_bil.bil')
dec<-raster('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_201112_bil.bil')
nov<-raster('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_201111_bil.bil')
oct<-raster('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_201110_bil.bil')
sep<-raster('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_201109_bil.bil')
aug<-raster('C:/Users/lhamon/Documents/clim data/2011/PRISM_ppt_stable_4kmM3_201108_bil.bil')

months<-c("mar","feb","jan","dec","nov","oct","sep","aug")

#check map projections
counties_geog_mar = spTransform(counties, crs(mar))
plot(mar)
plot(counties_geog, add=T)
proj4string(counties_geog)

mean.find.50<-function(dat) {
  out <-sample(dat, 50, replace=TRUE) # randomingly samples 50 lines from the dataset
  out<-sort(out, decreasing=TRUE) # reorders the dataset in ascending order
  out<-out[1:5] # subsets the first 5 lines
  mean(out) # takes the mean of this subset
}

counties_geog_feb = spTransform(counties, crs(feb))
plot(feb)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_jan = spTransform(counties, crs(jan))
plot(jan)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_dec = spTransform(counties, crs(dec))
plot(dec)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_nov = spTransform(counties, crs(nov))
plot(nov)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_oct = spTransform(counties, crs(oct))
plot(oct)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_sep = spTransform(counties, crs(sep))
plot(sep)
plot(counties_geog, add=T)
proj4string(counties_geog)

counties_geog_aug = spTransform(counties, crs(oct))
plot(oct)
plot(counties_geog, add=T)
proj4string(counties_geog)


#crop data and plot
marNC = crop(mar, counties_geog)
plot(marNC)
plot(counties_geog,add=T)

febNC = crop(feb, counties_geog)
plot(febNC)
plot(counties_geog,add=T)

janNC = crop(jan, counties_geog)
plot(janNC)
plot(counties_geog,add=T)

decNC = crop(dec, counties_geog)
plot(decNC)
plot(counties_geog,add=T)

novNC = crop(nov, counties_geog)
plot(novNC)
plot(counties_geog,add=T)

octNC = crop(oct, counties_geog)
plot(octNC)
plot(counties_geog,add=T)

sepNC = crop(sep, counties_geog)
plot(sepNC)
plot(counties_geog,add=T)

augNC = crop(aug, counties_geog)
plot(augNC)
plot(counties_geog,add=T)

#extract tempmean data
tempmean = extract(marNC, counties_geog_mar, fun=mean)
tempmean[counties_geog_mar@data$NAME == 'ORANGE']

tempmean = extract(febNC, counties_geog_feb, fun=mean)
tempmean[counties_geog_feb@data$NAME == 'ORANGE']

tempmean = extract(janNC, counties_geog_jan, fun=mean)
tempmean[counties_geog_jan@data$NAME == 'ORANGE']

tempmean = extract(decNC, counties_geog_dec, fun=mean)
tempmean[counties_geog_dec@data$NAME == 'ORANGE']

tempmean = extract(novNC, counties_geog_nov, fun=mean)
tempmean[counties_geog_nov@data$NAME == 'ORANGE']

tempmean = extract(octNC, counties_geog_oct, fun=mean)
tempmean[counties_geog_oct@data$NAME == 'ORANGE']

tempmean = extract(sepNC, counties_geog_sep, fun=mean)
tempmean[counties_geog_sep@data$NAME == 'ORANGE']

tempmean = extract(augNC, counties_geog_aug, fun=mean)
tempmean[counties_geog_aug@data$NAME == 'ORANGE']

head(tempmean)
print(tempmean)

#designate the year means as the x-axis values
x<-c(82.22523, 89.9073, 95.71481875)




###pull first flight data for each year in question
setwd("C:/Users/lhamon/Dropbox/NC butterfly project")
alldat <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")
colnames(alldat)<-c("Cname", "sciName", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian", "voltinism", "voltinismnotes","diettype", "dietbreadth", "dietnotes","migratory","overwinter")

#subset data for observations made since 1990
alldat$year <-as.numeric(alldat$year) 
alldat <-subset(alldat, year > 1989)

#subset Papilio glaucus into year and julian date
glaucus<-subset(alldat, sciName=="Papilio glaucus")

glaucus_2014<-subset(glaucus, year=="2014")
glaucus_2014_j<-glaucus_2014$julian

glaucus_2013<-subset(glaucus, year=="2013")
glaucus_2013_j<-glaucus_2013$julian

glaucus_2012<-subset(glaucus, year=="2012")
glaucus_2012_j<-glaucus_2012$julian

#load dplyr
library(dplyr)

#sort data in ascending order
glaucus_2014_j<-sort(glaucus_2014_j, decreasing=FALSE)
glaucus_2013_j<-sort(glaucus_2013_j, decreasing=FALSE)
glaucus_2012_j<-sort(glaucus_2012_j, decreasing=FALSE)

# finding the first 10% of observations for each of these subsets
glaucus2014<-glaucus_2014_j[1:34]
glaucus2013<-glaucus_2013_j[1:43]
glaucus2012<-glaucus_2012_j[1:36]

#finding the mean of each of these
mean(glaucus2014)
mean(glaucus2013)
mean(glaucus2012)

y<-c(77.16667,90.39535,93.05882)

# graphing estimated first date by different sample sizes
reg1 <- lm(y~x)
par(cex=.8)
plot(x, y)
abline(reg1)
