#####################################
###### Apply random forest model to make predictions for mesozooplankton biomass

setwd("/Users/kailinliu/Documents/Mesozooplankton")
library(randomForest)
meso_rf<-readRDS("Output_rf_full.rds")
fHarmonic <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

MZ<-read.csv("MZ_working.csv")
library(dplyr)
dat<-MZ%>%
  select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
         "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method","logBiomass_ori") %>%
  mutate_if(is.character,as.factor)
dat1<-na.omit(dat)   ###### remove NA values, then only remians 130,922 data points. 
rownames(dat1)<-1:nrow(dat1)
Mon<-c("1Jan","2Feb","3Mar","4Apr","5May","6Jun","7Jul","8Aug","9Sep","10Oct","11Nov","12Dec")
myfiles <- list.files(path = "Monthly/", pattern = ".csv", full.names = TRUE)
Depths<-0.5:195.5

for(i in 1:length(myfiles)){
  
  monthly<-read.csv(myfiles[i],header=TRUE)
  j<-monthly$month[1] #####make sure which month was selected
  
  ######Data transformation
  #1.latitude & longitude
  monthly1<-monthly%>%
    mutate(latlon1=sin(Latitude*pi/180),
           latlon2=sin(Longitude*pi/180)*cos(Latitude*pi/180),
           latlon3=-cos(Longitude*pi/180)*cos(Latitude*pi/180))
  #2.Day and time
  monthly1<-monthly1%>%
    mutate(month2 = ifelse(Latitude<0,month+6,month))%>%
    mutate(DOY = (month2-1)*30+15)%>%
    mutate(HarmTOD = (Time/24)*2*pi, # Convert to radians
           HarmDOY = (DOY/365)*2*pi, # Convert to radians
           hTime=fHarmonic(HarmTOD,k=1),
           hDoy=fHarmonic(HarmDOY,k=1),
           hTime1=hTime[,1],
           hTime2=hTime[,2],
           hDoy1=hDoy[,1],
           hDoy2=hDoy[,2])%>%
    mutate(Timenight<-rep(24,nrow(monthly1)), ######create night time
           HarmTOD_n=(Timenight/24)*2*pi,
           hTime_n=fHarmonic(HarmTOD_n,k=1),
           hTime1_n=hTime_n[,1],
           hTime2_n=hTime_n[,2])
  
  #3.environmental variables log transformation
  monthly1<-subset(monthly1,monthly$Bottom_Z>0)
  monthly1<-monthly1%>%
    mutate(depth=0.5,####set depth=0.5, calculate the surface biomass
           logChl=log10(Chla),
           logMLD=log10(mld),
           logO2=log10(Oxygen),
           logDepth=log10(depth+10), 
           logBottom=log10(Bottom_Z+10),
           Mesh=as.integer(rep(200,nrow(monthly1))),
           Method=as.factor(rep("dry mass",nrow(monthly1))))
  
  ####daytime
  daytime<-monthly1 %>%
    select("Longitude","Latitude","latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
           "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method")
  
  daytime<-na.omit(daytime)
  latlon<-daytime[,c(1:5)]
  daytime<-daytime[,-c(1,2)]
  levels(daytime$Method)<-levels(dat1$Method) #####factor levels should be the same with training dataset.
  #calculate the surface biomass, as the depth is 1m
  daytime$logmeso<-predict(meso_rf,daytime)
  daytime$Mesobiomass<-10^(daytime$logmeso)
  
  #calculate the biomass below surface to 200m
  #separate the coastal and basin data >200m basin and <200m coastal 
  daytime_basin<-daytime%>%
    filter(logBottom>log10(200+10))
  daytime_coastal<-daytime%>%
    filter(logBottom<=log10(200+10))
  
  #calculate the biomass for basin
  pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
  for(m in 2:length(Depths)){
    setTxtProgressBar(pb, m)
    daytime_basin$logDepth<-log10(Depths[m]+10)
    daytime_basin$Mesobiomass<-daytime_basin$Mesobiomass+(10^predict(meso_rf,daytime_basin))
  }
  #calculate the biomass for coastal
  pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
  cs<-NULL
  mat<-data.frame(meso=rep("0",nrow(daytime_coastal)))
  for(m in 2:length(Depths)){
    setTxtProgressBar(pb, m)
    daytime_coastal$logDepth<-log10(Depths[m]+10)
    cs$meso<-10^(predict(meso_rf,daytime_coastal[,c(1:16)]))
    mat<-cbind(mat,cs)
  }
  mat<-mat[,-1]
  daytime_coastal<-daytime_coastal%>%
    mutate(bottom=floor(10^(logBottom)-10)+0.5)%>%
    mutate(num=findInterval(bottom,Depths)-1)%>%
    filter(bottom>1.5)
  for(n in 1:nrow(daytime_coastal)){
    num<-as.integer(daytime_coastal[n,"num"])
    dmb<-rowSums(mat[n,1:num])
    daytime_coastal[n,"Mesobiomass"]<-daytime_coastal[n,"Mesobiomass"]+dmb
  }
  
  daytime<-rbind(daytime_basin,daytime_coastal[,c(1:18)])
  
  #####nighttime
  night<-monthly1 %>%
    select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1_n","hTime2_n",
           "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method")%>%
    rename(hTime1="hTime1_n",
           hTime2="hTime2_n")
  
  night<-na.omit(night)
  levels(night$Method)<-levels(dat1$Method)
  night$logmeso_n<-predict(meso_rf,night)
  night$Mesobiomass_n<-10^(night$logmeso_n)
  
  #####calculate the biomass below surface to 200m
  night_basin<-night%>%
    filter(logBottom>log10(200+10))
  night_coastal<-night%>%
    filter(logBottom<=log10(200+10))
  #calculate the biomass for basin
  pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
  for(m in 2:length(Depths)){
    setTxtProgressBar(pb, m)
    night_basin$logDepth<-log10(Depths[m]+10)
    night_basin$Mesobiomass_n<-night_basin$Mesobiomass_n+(10^predict(meso_rf,night_basin))
  }
  
  #calculate the biomass for coastal
  pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
  cs<-NULL
  mat<-data.frame(meso=rep("0",nrow(night_coastal)))
  for(m in 2:length(Depths)){
    setTxtProgressBar(pb, m)
    night_coastal$logDepth<-log10(Depths[m]+10)
    cs$meso<-10^(predict(meso_rf,night_coastal))
    mat<-cbind(mat,cs)
  }
  mat<-mat[,-1]
  night_coastal<-night_coastal%>%
    mutate(bottom=floor(10^(logBottom)-10)+0.5)%>%
    mutate(num=findInterval(bottom,Depths)-1)%>%
    filter(bottom>1.5)
  for(n in 1:nrow(night_coastal)){
    num<-as.integer(night_coastal[n,"num"])
    dmb<-rowSums(mat[n,1:num])
    night_coastal[n,"Mesobiomass_n"]<-night_coastal[n,"Mesobiomass_n"]+dmb
  }
  
  
  night<-rbind(night_basin, night_coastal[,c(1:18)])
  
  daytime<-left_join(daytime,night%>%select("latlon1","latlon2","latlon3","Mesobiomass_n"),by=c("latlon1","latlon2","latlon3"))
  daytime$diff<-daytime$Mesobiomass_n-daytime$Mesobiomass
  
  newdata<-left_join(latlon,daytime,by=c("latlon1","latlon2","latlon3"))  
  
  Meso<-newdata%>%
    select("Longitude","Latitude","Mesh","Method","logmeso","Mesobiomass","Mesobiomass_n","diff")
  
  fildata<-left_join(monthly,Meso,by=c("Longitude","Latitude"))
  
  write.csv(fildata,file=paste0(Mon[j],"Prediction",sep=".","csv"))
  
}

##############Annual average mesozooplankton biomass
library(tidyverse)
library(sf)

myfiles <- list.files(path = "MonthlypredictionRF/", pattern = ".csv", full.names = TRUE)
datayear<-data.frame("Latitude"=c(),"Longitude"=c(),"Month"=c(),"Mesobiomass"=c(),"Mesobiomass_n"=c(),"Chla"=c())
for(i in 1:12){
  monthly<-read.csv(myfiles[i])
  monthly1<-monthly%>%
    select("Longitude","Latitude","month","Mesobiomass","Mesobiomass_n","Chla")
  datayear<-rbind(datayear,monthly1)
}
unique(datayear$month)
str(datayear)
newdata<-datayear%>%
  group_by(Latitude,Longitude)%>%
  summarize(annualmean=mean(Mesobiomass,na.rm=TRUE),
            annualsd=sd(Mesobiomass,na.rm=TRUE),
            annualmean_n=mean(Mesobiomass_n,na.rm=TRUE),
            annualsd_n=sd(Mesobiomass_n,na.rm=TRUE),
            annualchl=mean(Chla,na.rm=TRUE))
newdata<-as.data.frame(newdata)
write.csv(newdata,file="MZbiomass_annual2407.csv")