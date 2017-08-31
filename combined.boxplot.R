#creating a boxplot to demonstrate influence of spp-specific traits on early arrival date

#merging the full dataset and the calculated earlydate/temp values (without province for now)
setwd("~/Documents/Biology/butterfly paper 2016")
library(plyr)
#adjusted
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8months.NC.adjusted.2016.csv") #adjusted

#merge 2: adding the variables in 
variables<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/species list 2017.csv")
drop<-c("Cname","county","observer","number","comments","month", "day","dateCalc","province","coid","julian","voltinismnotes","dietnotes")
drop2<-which(names(variables) %in% drop)
variables1<-variables[,-drop2]
variables2<- unique(variables1[,1:6])
dat<-merge(variables2,tempdat, by.x=c("species"),by.y=c('species'), all.x = T, all.y = T)

#setting voltinism as a factor
dat$voltinism<-as.factor(dat$voltinism)

dat<-na.omit(dat)

#reshape data
#I'm actually just gonna... subset it out by column and make the factor name... a repeating variable
#to get it to do what I want it to do
#there's an easier way I bet
voltinism<-dat[,c("voltinism","earlydate")]
voltinism$variable<- rep("voltinism",length(voltinism))
names(voltinism)<-c("label","earlydate","variable")

diettype<-dat[,c("diettype","earlydate")]
diettype$variable<- rep("diet type",length(diettype))
names(diettype)<-c("label","earlydate","variable")

dietbreadth<-dat[,c("dietbreadth","earlydate")]
dietbreadth$variable<- rep("diet breadth",length(dietbreadth))
names(dietbreadth)<-c("label","earlydate","variable")

overwinter<-dat[,c("overwinter","earlydate")]
overwinter$variable<- rep("overwinter",length(overwinter))
names(overwinter)<-c("label","earlydate","variable")

alldat<-rbind(voltinism,diettype,dietbreadth,overwinter)

# plot
library(ggplot2)
require(ggplot2)
p <- ggplot(data = alldat, aes(x=variable, y=earlydate)) 
p <- p + geom_boxplot(aes(fill = label))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + xlab("variable") + ylab("early date") 
p <- p + scale_fill_manual(values = c("darkcyan","cyan4","cyan3","cyan2","cyan1","cyan","seagreen4","seagreen3","seagreen2",
                                      "indianred4", "indianred3","indianred2","indianred1",
                                      "gold4","gold3","gold2","gold1")
                  , labels = c("2.5" = "2-3", "3.5" = "3-4", "herb"="herbaceous", "larvaeadults"="larvae/adults"))
p <- p + guides(col = guide_legend(nrow = 5))

p <- p + scale_colour_discrete(name="voltinism", labels=c("1", "2","2-3","3","3-4","4"),
                               name="diet type", labels=c("both","herbaceous","woody"),
                               name="diet breadth", labels=c("family","genus","multifamily","species","adults"),
                               name="overwinter", labels=c("adult","larva","adult/larva","pupa"))


       
