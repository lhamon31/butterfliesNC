##perform ANCOVA to compare mountains, piedmont, coast
library(plyr)
setwd("~/Biology/butterfly paper 2016/graphs")

#load in data
mountains<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/mountain.fulldat.csv")
piedmont<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/piedmont.fulldat.csv")
coast<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/coast.fulldat.csv")

#Modify the regional data to eliminate species that aren't the same b/w region
#the coast is missing Meadow Fritillary, which tracks
#run this to clean out these spp
mountains<-mountains[ which( ! mountains$species %in% "Boloria bellona") , ]
piedmont<-piedmont[ which( ! piedmont$species %in% "Boloria bellona") , ]
#the mountains has very little data for Wallengrenia otho, Polities vibex, Callophrys gryneus (<6 years)
#maybe run this in addition
alldat<-alldat[ which( ! alldat$species %in% "Wallengrenia otho") , ]
alldat<-alldat[ which( ! alldat$species %in% "Polites vibex") , ]
alldat<-alldat[ which( ! alldat$species %in% "Callophrys gryneus") , ]

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
fulldat<-rbind(mountains,coast)

#create empty dataframe
output = data.frame(species=character(),
                    beta2=numeric(),
                    p.beta2=numeric(),
                    beta3 =numeric(),
                    p.beta3=numeric())

species<-unique(fulldat$species)

for (s in species) {
  df<-fulldat[fulldat$species==s,]
  ancova.test<-lm (julian ~ I1 + year + year*I1 , data = df)
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

#again, it says that [4,1] is out of bounds when you run mtns but it still reports it in the output 
#so not sure why that is

ok2<-subset(output, output$beta3<0)

#number significant for slope (ß3)
ok<-subset(output, output$p.beta3<0.05)

#temperature
    #4/55 slopes significantly different b/w the mountains and the piedmont
    #0/55 slopes significantly different b/w the coast and the piedmont
    #2/55 slopes significantly different b/w the mountains and the piedmont
#year
    #8/55 slopes significantly different b/w the mountains and the piedmont
    #6/55 slopes significantly different b/w the coast and the piedmont
    #4/55 slopes significantly different b/w the mountains and the piedmont

binom.test(38, 63, 0.5, alternative="greater")

#saving the output for the paper appendix
write.csv(output,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/graphs/M.C.temp.compare.csv")

#alternatively
library(nlme)
alldat<-rbind(piedmont,mountains,coast)
fullmod<- lme(julian~province+year+temp+year:province+temp:province, random=~1|species, method="ML", data=alldat)
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
