#LOAD IN DATA
#merging the full dataset and the calculated earlydate/temp values (without province for now)
library(plyr)
setwd("~/Biology/butterfly paper 2016/graphs")

#loading data. Any fulldat file may be substituted
#full state
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8months.NC.2016.csv")
#fullstate adjusted
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8months.NC.adjusted.2016.csv") #adjusted

#regional
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/mountain.fulldat.csv")
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/piedmont.fulldat.csv")
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/coast.fulldat.csv")

#Modify the regional data to eliminate species that aren't the same b/w region
#the coast is missing Meadow Fritillary, which tracks
#run this to clean out these spp
alldat<-alldat[ which( ! alldat$species %in% "Boloria bellona") , ]
        #the mountains has very little data for Wallengrenia otho, Polities vibex, Callophrys gryneus (<6 years)
        #maybe run this in addition
        alldat<-alldat[ which( ! alldat$species %in% "Wallengrenia otho") , ]
        alldat<-alldat[ which( ! alldat$species %in% "Polites vibex") , ]
        alldat<-alldat[ which( ! alldat$species %in% "Callophrys gryneus") , ]



##############################################################################

#creating for loop (First I'll just try to get this to read the plots and put them in a pdf)

species<-unique(alldat$species)
pdf("julian.temp.8months.pdf",width=10, height=8)
par(mfrow=c(2,3))


for (s in species) {
  df=alldat[alldat$species==s,]
  lm.sub=lm(df$earlydate~df$temp,xlab="year", ylab="julian", group=species)
  plot(df$earlydate~df$temp, xlab='year', ylab='Early Date (julian)')
  abline(lm(df$earlydate~df$temp))
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
  lm.sub<-lm(df$earlydate~df$temp)
  slope<-summary(lm.sub)$coefficients[2,1] 
  rsquared<-summary(lm.sub)$r.squared
  pvalue<-summary(lm.sub)$coefficients[2,4]
  tempoutput<- data.frame(species = s, slope=slope, rsquared = rsquared,
                          pvalue = pvalue)
  output<-rbind(output,tempoutput)
}

#note that when you run the mountain data you get an error that says the slope subscript is out of bounds
  #why this occurs is unclear. It still generates a list.
  #The data is structured the same as with the piedmont and coast
  #running species individually yields the same slope as generated in the output for loop
  #just something to monitor

mean(output$slope)
ok1<-subset(output, output$pvalue<0.05)
ok2<-subset(output,output$slope<0)

hist(output$slope, xlab="Slope (days/degree Celsius)", ylab="Julian date",main="")
abline(v=-5.852375,col="red",lwd=2)

library(ggplot2)

histogram1<-ggplot(output, aes(x=slope))+
  geom_histogram(binwidth=0.5)+
  abline(v=-0.5056561,col="red",lwd=2)+
  theme_classic()+theme(axis.line=element_line(colour="grey80"),axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), title=element_text(size=18,face="bold")) +
  xlab("Slope (days/year)")+
  ylab("Julian date")
#aes=aesthetics

#whole state
    #4 months
      #year(since the used earlydate is same despite temp window, year results are the same)
        #40/56 negative, 14 significant, mean slope=-0.5056561
      #temp
        #38/56 negative, 9 significant, mean slope=-3.003061
    #6 months
      #temp
        #32/56 negative, 6 significant, mean slope=-1.657351
    #8 months
      #temp
        #36/56 negative, 5 significant, mean slope=-2.029041
      #temp w/ adjusted temp windows (start at previous month of early date is in early half of month)
        #35/56 negative, 5 significant, mean slope=-1.618129
    #12 months 
      #temp 
        #41/56 negative, 6 significant, mean slope=-3.4333367
    
  

#slope proportions regional
  #year
    #mountain 27/55 negative, 10 significant mean=-1.158833
    #piedmont 32/55 negative, 6 significant mean=-0.04517956
    #coast 41/55 negative, 9 significant, mean=-0.6402211
 #temperature
    #mountain 28/55 negative, 5 significant mean=-8.639224
    #piedmont 39/55 negative, 6 significant mean=-4.537344
    #coast 39/55 negative, 4 significant, mean=-5.852375


#binomial test to test whether number of negative slopes is greater 
binom.test(44, 65, 0.5, alternative="greater")
