##perform ANCOVA to compare mountains, piedmont, coast
library(plyr)
setwd("~/Documents/Biology/BIOL 692H")

#load in data
mountains<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/mountain.fulldat.2.24.2016.csv")
piedmont<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/piedmont.fulldat.2.24.2016.csv")
coast<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/coast.fulldat.2.24.2016.csv")

#eliminate species such that only the same species are used in each region
#namely, the coast doesn't have "Boloria belladona"
#and the mountains hates its one "Papilio palamedes"
mountains<-mountains[-c(161:184, 846),]
piedmont<-piedmont[-c(183:192,929:938),]
coast<-coast[-c(874:896), ]

##Add in appropriate final indicator columns. 
#I'm gonna try it straight 2-way first.
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
                    beta1=numeric(),
                    beta3 = numeric())

species<-unique(fulldat$species)

for (s in species) {
  df<-fulldat[fulldat$species==s,]
  ancova.test<-lm (log10(earlydate) ~ I1 + log10(year) + log10(year)*I1 , data = df)
  lm.sub<-summary(ancova.test)
  beta1<-summary(lm.sub)$coefficients[2,4]
  beta3<-summary(lm.sub)$coefficients[4,4]
  beta.output<- data.frame(species = s, beta1=beta1, beta3=beta3)
  output<-rbind(output,beta.output)
}

#number significant for slope (β)
p.beta3<-subset(output, output$beta3<0.05)
#10/63 slopes are significantly different b/w the mountains and the piedmont
#1/63 slopes significantly different b/w the coast and the piedmont
#report the above numbers
#report the direction of the above numbers
#report the proportions of pos to neg slopes between the 3  regions 
#using the histogram.slope script.

###notes:
#"If β1 turns out to be close to 0,
#then we might conclude that the two groups have very similar intercepts"

#"If β3 is close to 0,
#then we might conclude that the two groups have very similar slopes" 

#"Note that the last column labeled  "Pr(>|t|)" 
#gives the p-value associated with the null hypothesis 
#that a particular coefficient is equal to 0"
