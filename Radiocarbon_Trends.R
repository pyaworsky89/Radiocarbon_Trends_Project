setwd("D:/Desktop/Dissertation")
library(MASS)
library(Bchron)
library(mgcv)
library(plotfunctions)
library(rcarbon)
library(sf)
library(raster)
library(rcarbon)
library(funModeling)
par(pty="s")

#My question is, what is the best way to use the calibrated radiocarbon distribution of calibrated dates to pick up on archaeological trends. When we work with calibrated dates, we are working with probabilities. Each date has a probability assigned to it about how likely that date. It seems most ppl just look for trends with either the uncalibrated dates, the central tendendency of calibrated dates, or assume a uniform distribution across all the years a calibrated distribution represents (sometimes with threshold probabilities). 

#To see which is better, I am going to simulate some data and trends. To do this, I'll build from my NMC data


alldata<-read.csv("./ALL_NMC_dates.csv")
#dates<-alldata[is.na(alldata$type)==F,]
#
plot(BchronCalibrate(ages=c(sort(alldata$uncal_age,decreasing=F)),
                     ageSds=c(alldata$uncal_err),
                     positions=seq(0,15700,100),
                     calCurves=rep('intcal20',nrow(alldata))),withPositions=T)

#First, I need to select the true year of an event. To do this, I am going to calibrate the dates in above and sample based on the distribution, a "true" date. This will be the simulated correct year associated with the date.

#column to dump our "true" age in.
alldata$trueage<-NA

for (i in 1:nrow(alldata)){
  caldate<-calibrate(x=alldata$uncal_age[i],
                     errors=alldata$uncal_err[i],
                     calCurves='intcal20',
                     verbose=F)
            
  trueage<-sample(x=caldate$grid$'1'[,1],
                            1,
                            prob=caldate$grid$'1'[,2])
  alldata$trueage[i]<-trueage
}

#Next, we need to insert a trend. I am thinking 3 variables. 

#order data by true date
alldata<-alldata[order(alldata$trueage),]
#crop out so its only over 2ky
alldata<-alldata[alldata$trueage<=2000,]

#now to insert our trends
alldata$trend1<-range01(sqrt(alldata$trueage))
plot(trend1~trueage,alldata)
alldata$trend2<-range01((alldata$trueage*2))
plot(trend2~trueage,alldata)
alldata$trend3<-sample(1:1000,nrow(alldata),replace=T)
plot(trend3~trueage,alldata)

#and trend 1 and trend 2 have a relationship
plot(trend1~trend2,alldata)

#####Now to run these data as if we only have the trends and the uncal dates, like archaeological data

#The trend appears quite recognizable 
plot(trend1~uncal_age,alldata)

plot(trend2~uncal_age,alldata)

plot(trend3~uncal_age,alldata)

## The Peter method where we look at the mean values of habitats through time.


df<-data.frame(yearBP=1:2000,
               meantrend1=NA,
               meantrend2=NA,
               meantrend3=NA)

#Trend 1

meantrend<-data.frame(yearBP=1:2000)
for (i in 1:nrow(alldata)){
  #meangde[,1+i]<-paste("gde",i,sep="")
  dates<-calibrate(x=alldata$uncal_age[i],errors=alldata$uncal_err[i],calCurves='intcal20',verbose=F)
  
  x<-data.frame(yearBP=dates$grids$'1'$calBP,trend1=alldata$trend1[i])
                #*dates$grids$"1"$PrDens) 
  meantrend<-merge(meantrend,x,by="yearBP",all=T)
  
  #above i multiply the probability of the density function by gde as a weight of how likely that year is.
}

for (i in 1:nrow(meantrend)){
  test<-meantrend[i,]
  test<-t(test)
  year<-test[1,]
  test<-test[-1,]
  meany<-mean(na.omit(test))
  df[df$yearBP==year,2]<-meany
}

##Trend 2
meantrend<-data.frame(yearBP=1:2000)
for (i in 1:nrow(alldata)){
  #meangde[,1+i]<-paste("gde",i,sep="")
  dates<-calibrate(x=alldata$uncal_age[i],errors=alldata$uncal_err[i],calCurves='intcal20',verbose=F)
  
  x<-data.frame(yearBP=dates$grids$'1'$calBP,trend2=alldata$trend2[i])
                #*dates$grids$"1"$PrDens) 
  meantrend<-merge(meantrend,x,by="yearBP",all=T)
  
  #above i multiply the probability of the density function by gde as a weight of how likely that year is.
}

for (i in 1:nrow(meantrend)){
  test<-meantrend[i,]
  test<-t(test)
  year<-test[1,]
  test<-test[-1,]
  meany<-mean(na.omit(test))
  df[df$yearBP==year,3]<-meany
}

### Trend 3
##Trend 2
meantrend<-data.frame(yearBP=1:2000)
for (i in 1:nrow(alldata)){
  #meangde[,1+i]<-paste("gde",i,sep="")
  dates<-calibrate(x=alldata$uncal_age[i],errors=alldata$uncal_err[i],calCurves='intcal20',verbose=F)
  
  x<-data.frame(yearBP=dates$grids$'1'$calBP,trend3=alldata$trend3[i])
                #*dates$grids$"1"$PrDens) 
  meantrend<-merge(meantrend,x,by="yearBP",all=T)
  
  #above i multiply the probability of the density function by gde as a weight of how likely that year is.
}

for (i in 1:nrow(meantrend)){
  test<-meantrend[i,]
  test<-t(test)
  year<-test[1,]
  test<-test[-1,]
  meany<-mean(na.omit(test))
  df[df$yearBP==year,4]<-meany
}

#Setting these values from 0-1
df$meantrend1<-range01(df$meantrend1)
df$meantrend2<-range01(df$meantrend2)
df$meantrend3<-range01(df$meantrend3)

#comparing to the original trends
par(mfrow=c(1,2))
plot(trend1~trueage,alldata)
plot(meantrend1~yearBP,df)
#plot(trend1~uncal_age,alldata)

plot(trend2~trueage,alldata)
plot(meantrend2~yearBP,df)

plot(trend3~trueage,alldata)
plot(meantrend3~yearBP,df)

plot(trend2~trend1,alldata)
plot(meantrend2~meantrend1,df)

plot(trend3~trend1,alldata)
plot(meantrend3~meantrend1,df)

par(mfrow=c(1,1))
##models
mod1<-gam(meantrend1~s(meantrend2)+s(meantrend3),data=df,family=binomial)
summary(mod1)
ilink <- family(mod1)$linkinv
acf(resid(mod1),lag.max=1000)

plot(mod1,shift=mod1$coefficients[1],shade=T,select=1,seWithMean=TRUE);box()
plot(mod1,shift=mod1$coefficients[2],shade=T,select=2,seWithMean=TRUE);box()

#gamm
mod1<-gamm(meantrend1~s(meantrend2,k=2)+s(meantrend3,k=2),data=df,correlation=corAR1())
summary(mod1$gam)

acf(resid(mod1$lme,type="normalized"),lag.max=1000)

plot(mod1$gam,shift=mod1$gam$coefficients[1],shade=T,select=1,seWithMean=TRUE);box()
plot(mod1$gam,shift=mod1$gam$coefficients[2],shade=T,select=2,seWithMean=TRUE);box()
