##perform ANCOVA to compare mountains, piedmont, coast
library(plyr)
setwd("~/Documents/Biology/butterfly paper 2016/data")

#load in data
mountains<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/mountain.fulldat.csv")
piedmont<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/piedmont.fulldat.csv")
coast<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/coast.fulldat.csv")

#eliminate species such that only the same species are used in each region
#namely, the coast doesn't have "Boloria belladona"
#and the mountains hates its one "Papilio palamedes"
#not enough data for "Chlosyne nycteis" at coast
#not enough data for "Lethe anthedon" at coast
mountains<-mountains[-c(817),]
coast<-coast[-c(290,291,618:623), ]

##Add in appropriate final indicator columns. 
#I'm gonna try it 2-way first.
#piedmont: 0
piedmont$I1 <- rep(0,nrow(piedmont))
#mountains: 1
mountains$I1 <- rep(1,nrow(mountains))
#coast:1
coast$I1<-rep(1,nrow(coast))

#rbind the 2 data sets
fulldat<-rbind(piedmont,mountains)
fulldat<-rbind(piedmont,coast)

#create empty dataframe
output = data.frame(species=character(),
                    beta2=numeric(),
                    p.beta2=numeric(),
                    beta3 =numeric(),
                    p.beta3=numeric())

species<-unique(fulldat$species)

for (s in species) {
  df<-fulldat[fulldat$species==s,]
  ancova.test<-lm (earlydate ~ I1 + year + year*I1 , data = df)
  lm.sub<-summary(ancova.test)
  beta2<-lm.sub$coefficients[3,1]
  p.beta2<-lm.sub$coefficients[3,4]
  beta3<-lm.sub$coefficients[4,1]
  p.beta3<-lm.sub$coefficients[4,4]
  beta.output<- data.frame(species = s, beta2=beta2,p.beta2=p.beta2, beta3=beta3, p.beta3=p.beta3)
  output<-rbind(output,beta.output)
}

#beta 2 is slope for piedmont
#beta 3 is difference in slope
#reporting actual slope for 2 regions and the p-value
#histograms of slopes
#talking about it qualitatively

ok2<-subset(output, output$beta3<0)

#number significant for slope (ß3)
ok<-subset(output, output$p.beta3<0.05)
#temperature
#4/61 slopes significantly different b/w the mountains and the piedmont
#2/61 slopes significantly different b/w the coast and the piedmont
#year
#11/61 slopes significantly different b/w the mountains and the piedmont
#7/61 slopes significantly different b/w the coast and the piedmont

binom.test(38, 63, 0.5, alternative="greater")

#alternatively
alldat<-rbind(piedmont,mountains,coast)
fullmod<- lme(earlydate~province+year+temp+year:province+temp:province, random=~1|species, method="ML", data=alldat)
summary(fullmod)
anova(fullmod)

##plan for the paper
#report the above numbers
#report the direction of the above numbers
#report the proportions of pos to neg slopes between the 3  regions 
#using the histogram.slope script.

###notes:
#"If ß1 turns out to be close to 0,
#then we might conclude that the two groups have very similar intercepts"

#"If ß3 is close to 0,
#then we might conclude that the two groups have very similar slopes" 

#"Note that the last column labeled  "Pr(>|t|)" 
#gives the p-value associated with the null hypothesis 
#that a particular coefficient is equal to 0"
