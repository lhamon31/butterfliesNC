#proportion of spp for whom year/temp is a better fit (r closer to +/-1)

#load summary data table 
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/year.temp.slopes.csv") 

#calculate r 
#temp
alldat$T.r<-sign(alldat$Tslope)*sqrt(alldat$Trsquared)
#year
alldat$Y.r<-sign(alldat$Yslope)*sqrt(alldat$Yrsquared)

#difference between T.r and Y.r
alldat$rdifference<-(alldat$T.r)-(alldat$Y.r)

#is temp a better predictor (y/n)?
alldat$temp.better<-ifelse(abs(alldat$T.r)>abs(alldat$Y.r),"Y","N")

table(alldat$temp.better)

#year is a 'better predictor' in 36/56 spp
#potentially something other than temp an important predictor of arrival
  #photoperiod, traits, etc. 