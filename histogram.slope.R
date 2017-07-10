#LOAD IN DATA
#merging the full dataset and the calculated earlydate/temp values (without province for now)
library(plyr)
setwd("~/Documents/Biology/BIOL 692H")

#loading data. Any fulldat file may be substituted
#full state
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8.months.NC.2016.csv")
#regional
alldat<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/coast.fulldat.2.24.2016.csv")

#how to modify the regional data to eliminate species that aren't the same b/w region
#mountains
alldat<-alldat[-c(161:184,280:303,636:658,846),]
#piedmont
alldat<-alldat[-c(183:192,310:333,680:698,929:938),]
#coast
alldat<-alldat[-c(292:293,644:645,874:896), ]

##############################################################################

#creating for loop (First I'll just try to get this to read the plots and put them in a pdf)

species<-unique(alldat$species)
pdf("julian.year.7.10.17.pdf",width=10, height=8)
par(mfrow=c(2,3))


for (s in species) {
  df=alldat[alldat$species==s,]
  lm.sub=lm(df$julian~df$temp,xlab="year", ylab="julian", group=species)
  plot(df$julian~df$temp, xlab='year', ylab='Early Date (julian)')
  abline(lm(df$year~df$temp))
  legend("topright", bty="n", legend=paste("R2-",format(summary(lm.sub)$r.squared, digits=4)))
}

#generate a dataframe of species,slope,r-squared and p-value

output = data.frame(species = character(),
                    slope=numeric(),
                    rsquared = numeric(),
                    pvalue = numeric())

species<-unique(alldat$species)

for (s in species) {
  df<-alldat[alldat$species==s,]
  lm.sub<-lm(df$julian~df$year)
  slope<-summary(lm.sub)$coefficients[2,1]
  rsquared<-summary(lm.sub)$r.squared
  pvalue<-summary(lm.sub)$coefficients[2,4]
  tempoutput<- data.frame(species = s, slope=slope, rsquared = rsquared,
                          pvalue = pvalue)
  output<-rbind(output,tempoutput)
}



mean(output$slope)
ok1<-subset(output, output$pvalue<0.05)
ok2<-subset(output,output$slope<0)

hist(output$slope, xlab="Slope (days/year)")
abline(v=-0.5056615,col="red",lwd=2)

histogram1<-ggplot(output, aes(x=slope))+
  geom_histogram(binwidth=0.5)+
  abline(v=-0.5056615,col="red",lwd=2)+
  theme_classic()+theme(axis.line=element_line(colour="grey80"),axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), title=element_text(size=18,face="bold")) +
  xlab("Slope (days/year)")
#aes=aesthetics


#slope proportions
  #year
    #piedmont 40/61 negative mean= -0.3091232
    #mountain 51/61 negative mean=-1.455317
    #coast mean=38/61 negative mean=-0.5056615
  #temperature
    #piedmont 49/61 negative mean=-5.92268
    #mountain 45/61 negative mean=-7.293122
    #coast 43/61 negative, mean=-7.406275


#binomial test to test whether number of negative slopes is greater 
binom.test(44, 65, 0.5, alternative="greater")
