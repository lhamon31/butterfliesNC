##ESTIMATING FIRST DATE USING glaucus 2014

setwd("~/Biology/butterfly paper 2016/graphs")

alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")

#Changing column names to match the 2nd summary dataframe
colnames(alldat)<-c("x","Cname", "species", "county", "observer", "number", "comments", "dateCalc", "year", "julian", "voltinism",
                    "voltinismnotes", "diettype", "dietbreadth", "dietnotes", "migratory", "overwinter")

alldat<-alldat[c("year","species","number", "julian")]

alldat <-subset(alldat, year > 1989)

#finding the spp with the most observations in a particular year
  #which species?
    samplespp<-data.frame(table(alldat$species))
    samplespp <- samplespp[order(-samplespp$Freq),] #papilio glaucus most observations
  #which year?
    pglaucus<-subset(alldat, species=="Papilio glaucus")
    pglaucus<-data.frame(table(pglaucus$year))
    pglaucus<-pglaucus[order(-pglaucus$Freq),] #most observations in 2006

# subsetting out Papilio glaucus to test for changes in first date estimation by sample size
glaucus<- alldat[ which(alldat$species=='Papilio glaucus' & alldat$year=="2006"),]
glaucus_j<-subset(glaucus[,3:4])

library(dplyr)
#------------------------------------------------------------------------------------
#pulling a random sample and extracting the date on which the 10th individual appears
mean.find <- function(dat, num) {
  out <-dat[sample(1:nrow(dat), num, replace=FALSE), ] # randomly samples 'num' lines from the dataset
  out = out[order(out$julian, decreasing = F), ]
  out$cum = cumsum(out$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(out$julian[out$cum >= 25])
}

#""""10% individual
mean.find = function(dat, num) {
  out <-dat[sample(1:nrow(dat), num, replace=FALSE), ] # randomly samples 'num' lines from the dataset
  out = out[order(out$julian, decreasing = F), ]
  out$cum = cumsum(out$number)
  out$cumpct = out$cum/sum(out$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(out$julian[out$cumpct >= 0.25])
}
#---------------------------------------------------------------------------------------

#perform this random sample 1000 times
sample_sizes = c(10, 20, 50, 100, 150, 200, 250, 273)

ss_output = c()
for (s in sample_sizes) {
  earlydat= replicate(1000, {
    mm<-mean.find(glaucus_j, s)
  })
  ss_output = cbind(ss_output, earlydat)
}


# detach dplyr to load plyr and run the SummarySE package
detach("package:dplyr", unload=TRUE)

#Creating means
library(plyr)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# adding a species column
list2 <- rep("glaucus rapae",length(1000))
ss_output <- cbind(list2, ss_output)
ss_output<-as.data.frame(ss_output)
colnames(ss_output)<-c("species","c10","c20","c50","c100","c150","c200","c250","c273")

#convert columns to numeric
ss_output$c10<-as.numeric(as.character(ss_output$c10))
ss_output$c20<-as.numeric(as.character(ss_output$c20))
ss_output$c50<-as.numeric(as.character(ss_output$c50))
ss_output$c100<-as.numeric(as.character(ss_output$c100))
ss_output$c150<-as.numeric(as.character(ss_output$c150))
ss_output$c200<-as.numeric(as.character(ss_output$c200))
ss_output$c250<-as.numeric(as.character(ss_output$c250))
ss_output$c273<-as.numeric(as.character(ss_output$c273))

# using summarySE function to calculate mean of the 1000 means from each subset and create standard deviations for plotting
SE_10<-summarySE(ss_output, measurevar="c10", groupvars=c("species"))
SE_10[,"ss"]  <- c("10")
colnames(SE_10)<-c("species","N","julian","sd","se","ci","ss")
SE_20<-summarySE(ss_output, measurevar="c20", groupvars=c("species"))
SE_20[,"ss"]  <- c("20")
colnames(SE_20)<-c("species","N","julian","sd","se","ci","ss")
SE_50<-summarySE(ss_output, measurevar="c50", groupvars=c("species"))
SE_50[,"ss"]  <- c("50")
colnames(SE_50)<-c("species","N","julian","sd","se","ci","ss")
SE_100<-summarySE(ss_output, measurevar="c100", groupvars=c("species"))
SE_100[,"ss"]  <- c("100")
colnames(SE_100)<-c("species","N","julian","sd","se","ci","ss")
SE_150<-summarySE(ss_output, measurevar="c150", groupvars=c("species"))
SE_150[,"ss"]  <- c("150")
colnames(SE_150)<-c("species","N","julian","sd","se","ci","ss")  
SE_200<-summarySE(ss_output, measurevar="c200", groupvars=c("species"))
SE_200[,"ss"]  <- c("200")
colnames(SE_200)<-c("species","N","julian","sd","se","ci","ss")
SE_250<-summarySE(ss_output, measurevar="c250", groupvars=c("species"))
SE_250[,"ss"]  <- c("250")
colnames(SE_250)<-c("species","N","julian","sd","se","ci","ss")
SE_273<-summarySE(ss_output, measurevar="c273", groupvars=c("species"))
SE_273[,"ss"]  <- c("273")
colnames(SE_273)<-c("species","N","julian","sd","se","ci","ss")

# binding means into one object
mean.labels<-c("10", "20", "50", "100", "150", "200", "250","273")
mean.labels<-as.numeric(mean.labels)
mean.julian<-rbind(SE_10, SE_20, SE_50, SE_100, SE_150, SE_200,SE_250, SE_273)
dat25p<-cbind(mean.julian, mean.labels)

dat25th <- dat25th[-1, ] #remove first row of this one (sample size too small)

dat10th$proxy <- rep("10th",nrow(dat10th))
dat25th$proxy <- rep("25th",nrow(dat25th))
dat10p$proxy<-rep("10 percent",nrow(dat10p))
dat25p$proxy<-rep("25 percent",nrow(dat25p))
datf<-rbind(dat10th,dat25th,dat10p,dat25p)

# graphing estimated first date by different sample sizes
library(ggplot2)
plot<-ggplot(datf, aes(x=mean.labels, y=julian, group=proxy, colour=proxy))+
  geom_line()+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=julian-sd, ymax=julian+sd),width=.1)+
  theme(legend.position="none")+
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(legend.text = element_text(size = 16))+
  xlab("Sample size") +
  ylab("Date (julian)")+
  ylim(0,300)+
  theme_bw()+theme(panel.border = element_rect(colour=NA),axis.line=element_line(colour="grey80"), axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold"), title=element_text(size=18,face="bold"))
plot