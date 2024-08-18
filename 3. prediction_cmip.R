####################predict based on CMIP6 output
#######Depth data
library(ncdf4)
library(raster)
library(lubridate)
library(dplyr)

setwd("D:/Rcode/World DATA")
depth<-"etopo05.nc"
nc<-nc_open(depth)
print(nc)
nc_close(nc)
rasterdepth<-raster(depth,varname="ROSE")
depth_1deg<-aggregate(rasterdepth,fact=12,fun=mean)
Depth<-cbind(coordinates(depth_1deg),v=values(depth_1deg))
Depth<-as.data.frame(Depth)
names(Depth)<-c("Longitude","Latitude","Bottom_Z")
Depth$Longitude<-round(Depth$Longitude,1)
Depth$Longitude[Depth$Longitude>180]<-Depth$Longitude[Depth$Longitude>180]-360
Depth$Latitude<-round(Depth$Latitude,1)
Depth$Bottom_Z<-Depth$Bottom_Z*(-1)
Depth<-arrange(Depth,Latitude,Longitude)

setwd("D:/Rcode/Mesozooplankton")
load("cmipdata.rda")
library(dplyr)
#########dataset processing
start_time<-Sys.time()
dat2015<-cmip_2015%>%
  rename(Longitude=lon,Latitude=lat,Chl=chl,SST=sst,SSS=sss,Oxygen=oxygen)%>%
  mutate(month=case_when(grepl("-01-",time)~"1",
                         grepl("-02-",time)~"2",
                         grepl("-03-",time)~"3",
                         grepl("-04-",time)~"4",
                         grepl("-05-",time)~"5",
                         grepl("-06-",time)~"6",
                         grepl("-07-",time)~"7",
                         grepl("-08-",time)~"8",
                         grepl("-09-",time)~"9",
                         grepl("-10-",time)~"10",
                         grepl("-11-",time)~"11",
                         grepl("-12-",time)~"12"))%>%
  mutate(Day=rep(15,nrow(cmip_2015)),Time=rep(12,nrow(cmip_2015)),
         depth=rep(200,nrow(cmip_2015)))
dat_2015<-left_join(dat2015,Depth,by=c("Latitude","Longitude"))
dat_2015$month<-as.numeric(dat_2015$month)

library(randomForest)
meso_rf<-readRDS("Output_rf_full0703.rds")
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
         "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass") %>%
  mutate_if(is.character,as.factor)
dat1<-na.omit(dat)   ###### remove NA values, then only remians 130,922 data points. 
rownames(dat1)<-1:nrow(dat1)
Depths<-seq(0.5,195.5)
######Data transformation
#1.latitude & longitude
evndata<-dat_2015%>%
  mutate(latlon1=sin(Latitude*pi/180),
         latlon2=sin(Longitude*pi/180)*cos(Latitude*pi/180),
         latlon3=-cos(Longitude*pi/180)*cos(Latitude*pi/180))
#2.Day and time
evndata<-evndata%>%
  mutate(month2 = ifelse(Latitude<0,month+6,month))%>%
  mutate(DOY = (month2-1)*30+15)%>%
  mutate(HarmTOD = (Time/24)*2*pi, # Convert to radians
         HarmDOY = (DOY/365)*2*pi, # Convert to radians
         hTime=fHarmonic(HarmTOD,k=1),
         hDoy=fHarmonic(HarmDOY,k=1),
         hTime1=hTime[,1],
         hTime2=hTime[,2],
         hDoy1=hDoy[,1],
         hDoy2=hDoy[,2])

#3.environmental variables log transformation
evndata<-subset(evndata,evndata$Bottom_Z>0)
evndata<-subset(evndata,evndata$Chl>0)
evndata<-subset(evndata,evndata$mld>0)

evndata<-evndata%>%
  mutate(depth=0.5,####set depth=0.5, calculate the surface biomass
         logChl=log10(Chl),
         logMLD=log10(mld),
         # logO2=log10(Oxygen),
         logDepth=log10(depth+10), 
         logBottom=log10(Bottom_Z+10),
         # Mesh=as.integer(rep(200,nrow(evndata))),
         #Method=as.factor(rep("dry mass",nrow(evndata)))
  )

mesodata<-evndata%>%
  select("month","Longitude","Latitude","latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
         "logDepth","logBottom","logChl","SST","SSS","logMLD")

#levels(mesodata$Method)<-levels(dat1$Method) #####factor levels should be the same with training dataset.
#calculate the surface biomass, as the depth is 1m
mesodata$logmeso<-predict(meso_rf,mesodata[,-c(1,2,3)])
mesodata$Mesobiomass<-10^(mesodata$logmeso)

#calculate the biomass below surface to 200m
#separate the coastal and basin data >200m basin and <200m coastal 
mesodata_basin<-mesodata%>%
  filter(logBottom>log10(200+10))
mesodata_coastal<-mesodata%>%
  filter(logBottom<=log10(200+10))

#calculate the biomass for basin
pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
for(m in 2:length(Depths)){
  setTxtProgressBar(pb, m)
  mesodata_basin$logDepth<-log10(Depths[m]+10)
  mesodata_basin$Mesobiomass<-mesodata_basin$Mesobiomass+(10^predict(meso_rf,mesodata_basin[,c(4:16)]))
}
#calculate the biomass for coastal
pb <- txtProgressBar(min = 0, max = length(Depths), style = 3)
cs<-NULL
mat<-data.frame(meso=rep("0",nrow(mesodata_coastal)))
for(m in 2:length(Depths)){
  setTxtProgressBar(pb, m)
  mesodata_coastal$logDepth<-log10(Depths[m]+10)
  cs$meso<-10^(predict(meso_rf,mesodata_coastal[,c(4:16)]))
  mat<-cbind(mat,cs)
}
mat<-mat[,-1]
mesodata_coastal<-mesodata_coastal%>%
  mutate(bottom=floor(10^(logBottom)-10)+0.5)%>%
  mutate(num=findInterval(bottom,Depths)-1)%>%
  filter(bottom>1.5)%>%
  filter(num>1)
for(n in 1:nrow(mesodata_coastal)){
  num<-as.integer(mesodata_coastal[n,"num"])
  dmb<-rowSums(mat[n,1:num])
  mesodata_coastal[n,"Mesobiomass"]<-mesodata_coastal[n,"Mesobiomass"]+dmb
}

mesodata<-rbind(mesodata_basin,mesodata_coastal[,1:18])

newdata<-left_join(mesodata,evndata[,1:13],by=c("month","Longitude","Latitude"))  

meso2015<-newdata%>%
  select("Longitude","Latitude","month","Chl","Bottom_Z","Mesobiomass")

end_time<-Sys.time()
runtime<-end_time-start_time

####### the predicion on 2020....2100 is the same with 2015.
