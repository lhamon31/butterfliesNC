##ESTIMATING FIRST DATE USING 10% DATE FOR PIERIS

setwd("~/Documents/Biology/BIOL 692H")

alldat <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")

#Changing column names to match the 2nd summary dataframe
colnames(alldat)<-c("Cname", "species", "county", "observer", "number", "comments", "year", "month", "day", "dateCalc", "province", "coid", "julian", "voltinism",
                    "voltinismnotes", "diettype", "dietbreadth", "dietnotes", "migratory", "overwinter")

alldat<-alldat[c("species", "year", "number", "julian")]

# subsetting out Pieris to test for changes in first date estimation by sample size
pieris<- alldat[ which(alldat$species=='Pieris rapae' & alldat$year=="2014"),]
pieris_j<-subset(pieris[,3:4])

library(dplyr)

#pulling a random sample
mean.find <- function(dat, num) {
  out <-dat[sample(1:nrow(dat), num, replace=FALSE), ] # randomly samples 'num' lines from the dataset
  out = out[order(out$julian, decreasing = F), ]
  out$cum = cumsum(out$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(out$julian[out$cum >= num])
}


mean.find.10<-function(dat) {
  out <-sample(dat, 10, replace=TRUE) # randomingly samples 10 lines from the dataset
  as.data.frame(out)
  out = out[order(out$julian, decreasing = F), ]
  out$cum = cumsum(out$number)
  #earliest date at which 'num' individuals have cumuluatively been observed
  mindate = min(out$julian[out$cum >= 10])
}

#function to sample 20 lines of the "Pieris 2014"subset and find the mean   
mean.find.20<-function(dat) {
  out <-sample(dat, 20, replace=TRUE) # randomingly samples 20 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[2:2] # subsets the 2nd line
}

#function to sample 50 lines of the "Pieris 2014"subset and find the mean   
mean.find.50<-function(dat) {
  out <-sample(dat, 50, replace=TRUE) # randomingly samples 50 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[5:5] # subsets the 5th line
}

#function to sample 100 lines of the "Pieris 2014"subset and find the mean   
mean.find.100<-function(dat) {
  out <-sample(dat, 100, replace=TRUE) # randomingly samples 100 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[10:10] # subsets 10th line
}

#function to sample 150 lines of the "Pieris 2014"subset and find the mean   
mean.find.150<-function(dat) {
  out <-sample(dat, 150, replace=TRUE) # randomingly samples 150 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[15:15] # subsets the 15th lines
}

#function to sample 200 lines of the "Pieris 2014"subset and find the mean   
mean.find.200<-function(dat) {
  out <-sample(dat, 200, replace=TRUE) # randomingly samples 200 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[20:20] # subsets the 20th lines
}

#function to sample 250 lines of the "Pieris 2014"subset and find the mean   
mean.find.250<-function(dat) {
  out <-sample(dat, 250, replace=TRUE) # randomingly samples 250 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[25:25] # subsets the 25th line
}

#function to sample the first 10% of the full "Pieris 2014"subset and find the mean   
mean.find.273<-function(dat) {
  out <-sample(dat, 273, replace=TRUE) # randomingly samples 200 lines from the dataset
  out<-sort(out, decreasing=FALSE) # reorders the dataset in ascending order
  out<-out[27:27] # subsets the 27th line
}

#perform this random sample 1000 times
#repeat it 1000 times with lapply

sample_sizes = c(10, 20, 50, 100)

ss_output = c()
for (s in sample_sizes) {
  temp= replicate(1000, {
    mm<-mean.find(pieris_j, s)
  })
  ss_output = cbind(ss_output, temp)
}

reps10<-replicate(1000, {
  mm<-mean.find(pieris_j, 10)
})
reps20<-replicate(1000, {
  mm<-mean.find.20(pieris_j)
})
reps50<-replicate(1000, {
  mm<-mean.find.50(pieris_j)
})
reps100<-replicate(1000, {
  mm<-mean.find.100(pieris_j)
})
reps150<-replicate(1000, {
  mm<-mean.find.150(pieris_j)
})
reps200<-replicate(1000, {
  mm<-mean.find.200(pieris_j)
})
reps250<-replicate(1000, {
  mm<-mean.find.250(pieris_j)
})
reps273<-replicate(1000, {
  mm<-mean.find.273(pieris_j)
})

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

# Adding species name back to each subset list to use SummarySE function
sciName<-rep("pieris",1000)
try10<-as.data.frame(cbind(reps10, sciName))
try20<-as.data.frame(cbind(reps20, sciName))
try50<-as.data.frame(cbind(reps50, sciName))
try100<-as.data.frame(cbind(reps100, sciName))
try150<-as.data.frame(cbind(reps150, sciName))
try200<-as.data.frame(cbind(reps200, sciName))
try250<-as.data.frame(cbind(reps250, sciName))
try273<-as.data.frame(cbind(reps273, sciName))


#converting the reps10 column to the correct format
try10$julian <- as.numeric(as.character(try10$reps10))
try20$julian <- as.numeric(as.character(try20$reps20))
try50$julian <- as.numeric(as.character(try50$reps50))
try100$julian <- as.numeric(as.character(try100$reps100))
try150$julian <- as.numeric(as.character(try150$reps150))
try200$julian <- as.numeric(as.character(try200$reps200))
try250$julian <- as.numeric(as.character(try250$reps250))
try273$julian <- as.numeric(as.character(try273$reps273))

# using summarySE function to calculate mean of the 1000 means from each subset and create SE's for plotting
SE_10<-summarySE(try10, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_20<-summarySE(try20, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_50<-summarySE(try50, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_100<-summarySE(try100, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_150<-summarySE(try150, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_200<-summarySE(try200, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_250<-summarySE(try250, measurevar="julian", groupvars=c("sciName"), na.rm=T)
SE_273<-summarySE(try273, measurevar="julian", groupvars=c("sciName"), na.rm=T)


# binding means into one object
mean.labels<-c("10", "20", "50", "100", "150", "200", "250","273")
mean.labels<-as.numeric(mean.labels)
mean.julian<-rbind(SE_10, SE_20, SE_50, SE_100, SE_150, SE_200,SE_250, SE_273)
dat<-cbind(mean.julian, mean.labels)

# graphing estimated first date by different sample sizes
library(ggplot2)
plot<-ggplot(dat, aes(x=mean.labels, y=julian))+
  geom_point()+
  geom_errorbar(aes(ymin=julian-sd, ymax=julian+sd),width=.1)+
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(legend.text = element_text(size = 16))+
  ggtitle("First date calculated by 10 percent date per sample size for Pieris 2014")+
  xlab("Sample size") +
  ylab("Date (julian)")+
  ylim(0,300)+
  theme_bw()+theme(panel.border = element_rect(colour=NA),axis.line=element_line(colour="grey80"), axis.text=element_text(size=16), axis.title=element_text(size=16,face="bold"), title=element_text(size=18,face="bold"))
plot

abline(h=95.117,col="red")


