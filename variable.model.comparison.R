#merging the full dataset and the calculated earlydate/temp values (without province for now)
setwd("~/Documents/Biology/BIOL 692H")
library(plyr)
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8.months.NC.2016.csv")
tempdat<-na.omit(tempdat)

#merge 2: adding the variables in (this bypasses an issue created by the provinces)
variables<-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/Laura's data and scripts/NCbutterflies.65species.June2015.csv")
variables<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/species list 2017.csv")

drop<-c("Cname","county","observer","number","comments","month", "day","dateCalc","province","coid","julian","voltinismnotes","dietnotes")
drop2<-which(names(variables) %in% drop)
variables1<-variables[,-drop2]

variables2<- unique(variables1[,1:7])
variables2<- unique(variables1[,1:6])
names(variables2)[names(variables2)=="sciName"] <- "species"
dat<-merge(variables2,tempdat, by.x=c("species","year"),by.y=c('species','year'), all.x = T, all.y = T)
dat<-merge(variables2,tempdat, by.x=c("species"),by.y=c('species'), all.x = T, all.y = T)

# Subsetting data for observations made between 1990 to 2013
dat<-subset(dat, year > 1989)
dat<-na.omit(dat)
dat$voltinism<-as.factor(dat$voltinism)


## LOOKING FOR YEAR EFFECT ACROSS ENTIRE DATASET (aka alldat) ##
# Required packages
library(nlme)

# defining full model (=most complex) to consider
fullmod<- lme(julian~year+temp+voltinism+diettype+dietbreadth+overwinter+year:voltinism+year:diettype+year:dietbreadth+year:overwinter, random=~1|species, method="ML", data=dat)
fullmod<- lme(julian~year+temp+voltinism+diettype+dietbreadth+overwinter+year:voltinism+year:diettype+year:dietbreadth+year:overwinter+temp:voltinism+temp:diettype+temp:dietbreadth+temp:overwinter, random=~1|species, method="ML", data=dat)
summary(fullmod)
anova(fullmod)

plot.design(julian~year+voltinism+diettype+dietbreadth+overwinter,data=dat)

#in the anova of the full mod,
#year (p<0.0001), temp (p<0.0001), voltinism (p=0.0024), diettype (p=0.0140), year:voltinism (P=0.0127) were significant

##code bit for model averaging
# frequentist model selection
library(MuMIn)

#fit all subsets of full model
fullmod_dredge<-dredge(fullmod)

#fit all subsets of full model
list.good<-get.models(fullmod_dredge, subset = delta < 7)
model1<-model.avg(list.good)
summary(model1) #returns relative 
coef(model1)
importance(model1)

#used linear and mixed:effects
#temp, voltinism, year, overwintering, diettype

#temp, voltinism, year important in 100% of the models
#voltinism:year important in 83% of the models
#diettype important in 78% of the models



#model averaging for the set of best model (delta<7)
model1<-model.avg(get.models(fullmod_dredge, subset = delta < 7))
summary(model1) # returns relative variable importance

#best model
summary(get.models(fullmod_dredge, 1)[[1]])

##refitting model with only significant fixed effects + year:predictor interactions
bestmod<-lme(julian~voltinism+overwinter+migratory+dietbreadth, random=~1|species, method="ML", data=dat)

#fit all subsets of full model
bestmod_dredge<-dredge(bestmod)

#model averaging for the set of best model (delta<7)
model2<-model.avg(get.models(bestmod_dredge, subset = delta < 7))
summary(model2) # returns relative variable importance

#best model
summary(get.models(bestmod_dredge, 1)[[1]])
