#comparing year and temp predictor variables using r (pearson's correlation coefficient)
#first create a table with the slopes and r-squareds for both temp and year for each of the 56 spp
#calculate the correlation coefficient
# it is (the sign of the slope)* the square root of r-squared
#add r back into the table
#proportion of spp for whom year/temp is a better fit (r closer to +/-1)

#load summary data table 
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/year.temp.slopes.csv") 

#calculate r 
#temp
alldat$T.r<-sign(alldat$Tslope)*sqrt(alldat$Trsquared)
#year
alldat$Y.r<-sign(alldat$Yslope)*sqrt(alldat$Yrsquared)
