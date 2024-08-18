########## data preparation 
########## predictor selection 

setwd("/Users/kailinliu/Documents/Mesozooplankton")
######## combine meso data and environmental data and create the working files
meso<-read.csv("meso.csv")
envi<-read.csv("meso_envi5.csv")

library(dplyr)
library(lubridate)

MZ<-cbind(meso,envi)
str(MZ)

MZ<-MZ %>%
  select("Longitude","Latitude","Year","Mon","Day","Time.LOC","Lower.Z","Bottom.Z","MesoZooplankton.Carbon.Mass..mg.C.m3..mesh.adjusted",
         "Mesh","calculated.Carbon.Mass.log10.mg.C.m3.","Chl","SST","SSS","mld","Oxygen","bio_method",
         "NO3","PO4","SST_200m","Sal_200m","Oxy_200m","NO3_200m","PO4_200m")%>%
  rename(Depth=Lower.Z,
         logBiomass=MesoZooplankton.Carbon.Mass..mg.C.m3..mesh.adjusted,
         logBiomass_ori=calculated.Carbon.Mass.log10.mg.C.m3.,
         Method=bio_method)
MZ$Date<-as.Date(ISOdate(MZ$Year,MZ$Mon,MZ$Day))
MZ$DOY<-yday(MZ$Date)


############predictor selection
datapar<-MZ%>%
  select("logBiomass","Longitude","Latitude","DOY","Time.LOC","Depth","Bottom.Z","Chl","SST","SSS","mld",
         "NO3","Oxygen")
model<-lm(logBiomass~.,data=datapar)
summary(model)
library(car)
vif(model)
pardata<-MZ%>%
  select("Longitude","Latitude","DOY","Time.LOC","Depth","Mesh","Bottom.Z","Chl","SST","SSS","mld","Oxygen",
         "NO3","PO4","SST_200m","Sal_200m","Oxy_200m","NO3_200m","PO4_200m")%>%
  rename(Bottom_depth=Bottom.Z,
         Mesh_size=Mesh,
         MLD=mld,
         Sampling_depth=Depth)%>%
  filter(Time.LOC>=0)

pardata2<-MZ%>%
  select("Longitude","Latitude","DOY","Time.LOC","Depth","Mesh","Bottom.Z","Chl","SST","SSS","mld","Oxygen","NO3")%>%
  rename(Bottom_depth=Bottom.Z,
         Mesh_size=Mesh,
         MLD=mld)%>%
  filter(Time.LOC>=0)
##calculate pearson correlation
#install.packages("ggcorrplot")
library(caret)
library(randomForest)
library(e1071)
library(ggcorrplot)
cor<-cor(pardata,use="complete.obs",method="pearson")#If use is "complete.obs" then missing values are handled by casewise deletion (and if there are no complete cases, that gives an error)
res1 <- cor.mtest(pardata, conf.level = .95)
p.mat = res1$p
p<-ggcorrplot(cor,method="square",type="lower",outline.color="grey",ggtheme=ggplot2::theme_void,colors=c("#839EDB", "white", "#FF8D8D"),
              lab = T,lab_size=3,p.mat = p.mat, insig= "blank", pch.col = "red", pch.cex = 3, tl.cex = 12)
pdf("Cor_SI.pdf",height=8,width=9)
p
dev.off()
highly_correlated <- findCorrelation(cor, cutoff = 0.75)
print(highly_correlated)
filtered_data <- pardata[, -highly_correlated]
str(filtered_data)

#####1. mesh_size should be deleted, so biomass data should transformed before modelling
#####2. oxygen is highly correlated with SST, considering not use it
#####3. NO3/NO3_200 can be put but not necessary, check R2

###########standardised the month of Southern Hemisphere to Northern Hemisphere
MZ<-MZ%>%
  mutate(Mon2=case_when(
    Latitude<0 & Mon==1~"7",
    Latitude<0 & Mon==2~"8",
    Latitude<0 & Mon==3~"9",
    Latitude<0 & Mon==4~"10",
    Latitude<0 & Mon==5~"11",
    Latitude<0 & Mon==6~"12",
    Latitude<0 & Mon==7~"1",
    Latitude<0 & Mon==8~"2",
    Latitude<0 & Mon==9~"3",
    Latitude<0 & Mon==10~"4",
    Latitude<0 & Mon==11~"5",
    Latitude<0 & Mon==12~"6",
    Latitude>=0 & Mon==1~"1",
    Latitude>=0 & Mon==2~"2",
    Latitude>=0 & Mon==3~"3",
    Latitude>=0 & Mon==4~"4",
    Latitude>=0 & Mon==5~"5",
    Latitude>=0 & Mon==6~"6",
    Latitude>=0 & Mon==7~"7",
    Latitude>=0 & Mon==8~"8",
    Latitude>=0 & Mon==9~"9",
    Latitude>=0 & Mon==10~"10",
    Latitude>=0 & Mon==11~"11",
    Latitude>=0 & Mon==12~"12",
  ))

######calculate the DOY and use harmonic function for DOY and hour of Day.
MZ$Date<-as.Date(ISOdate(MZ$Year,MZ$Mon,MZ$Day))
MZ$DOY<-yday(MZ$Date)
MZ$Date2<-as.Date(ISOdate(MZ$Year,MZ$Mon2,MZ$Day))
MZ$DOY2<-yday(MZ$Date2)

# Harmonic is to fit Hour and DOY

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

MZ<-MZ%>%
  filter(Time.LOC>=0)%>%
  mutate(HarmTOD = (Time.LOC/24)*2*pi, # Convert to radians
         HarmDOY = (DOY2/365)*2*pi, # Convert to radians
         hTime=fHarmonic(HarmTOD,k=1),
         hDoy=fHarmonic(HarmDOY,k=1),
         hTime1=hTime[,1],
         hTime2=hTime[,2],
         hDoy1=hDoy[,1],
         hDoy2=hDoy[,2]) 

######transfer longitude and latitude to xyz coordinates using periodic functions simulated with sine and cosine functions:
######similar with harmonic functions

MZ<-MZ%>%
  mutate(latlon1=sin(Latitude*pi/180),
         latlon2=sin(Longitude*pi/180)*cos(Latitude*pi/180),
         latlon3=-cos(Longitude*pi/180)*cos(Latitude*pi/180))

######log transformation

MZ$logChl<-log10(MZ$Chl) #######log transformation is better
MZ$logSST<-log10(MZ$SST+273.15)######log transfer not changes.
MZ$logSSS<-log10(MZ$SSS) ######no log transformation is better
MZ$logMLD<-log10(MZ$mld)#######log transformation is better
MZ$logO2<-log10(MZ$Oxygen) #######log transformation is better
MZ$logDepth<-log10(MZ$Depth+10) #######log transformation is better
MZ$logBottom<-log10(MZ$Bottom.Z+10) #######log transformation is better
MZ$logNO3<-log10(MZ$NO3)
MZ$logSST_200m<-log10(MZ$SST_200m)
MZ$logSSS_200m<-log10(MZ$Sal_200m)
MZ$logNO3_200m<-log10(MZ$NO3_200m)

write.csv(MZ,file="MZ_working2.csv")

#########further check predictors selection
####model1: mz=lat+lon+DOY+time+S_depth+B_depth+Chl+SST+SSS+MLD
####model2: mz=lat+lon+DOY+time+S_depth+B_depth+Chl+SST+SSS+MLD+NO3
####model3: mz=lat+lon+DOY+time+S_depth+B_depth+Chl+SST+SSS+MLD+NO3_200m
####model4: mz=lat+lon+DOY+time+S_depth+B_depth+Chl+SST_200+SSS_200+MLD
####model5: mz=lat+lon+DOY+time+S_depth+B_depth+Chl+SST_200+SSS_200+MLD+NO3_200m
####model6: mz=S_depth+B_depth+Chl+SST+SSS+MLD+NO3
####model7: mz=S_depth+B_depth+Chl+SST+SSS+MLD

library(randomForest)
rmse   <- function(x,y)sd(x-y)

#model1
mdat1<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
             "logDepth","logBottom","logChl","SST","logBiomass")]
dat1<-na.omit(mdat1)  
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]


model1<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                       logDepth+logBottom+logChl+SST,ntree=1000,mtry=6,data=Train)
rf_p  <- predict(model1,Test)
cor.test(rf_p,Test$logBiomass)
R1<-cor(rf_p,Test$logBiomass)**2
RSME1<-rmse(rf_p,Test$logBiomass)

#model2
mdat2<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
             "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass")]
dat1<-mdat2[is.finite(rowSums(mdat2)),]  
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]

model2<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                       logDepth+logBottom+logChl+SST+SSS+logMLD,ntree=1000,mtry=6,data=Train)

rf_p  <- predict(model2,Test)
cor.test(rf_p,Test$logBiomass)
R2<-cor(rf_p,Test$logBiomass)**2
RSME2<-rmse(rf_p,Test$logBiomass)

#model3
mdat3<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
             "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass","logNO3_200m")]
dat1<-mdat3[is.finite(rowSums(mdat3)),]    ###### remove NA values, then only remians 130,922 data points. 
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
model3<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                       logDepth+logBottom+logChl+SST+SSS+logMLD+logNO3_200m,ntree=1000,mtry=6,data=Train)

rf_p  <- predict(model3,Test)
cor.test(rf_p,Test$logBiomass)
R3<-cor(rf_p,Test$logBiomass)**2
RSME3<-rmse(rf_p,Test$logBiomass)


#model4
mdat4<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
             "logDepth","logBottom","logChl","SST_200m","Sal_200m","logMLD","logBiomass")]
dat1<-mdat4[is.finite(rowSums(mdat4)),]  
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
model4<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                       logDepth+logBottom+logChl+SST_200m+Sal_200m+logMLD,ntree=1000,mtry=6,data=Train)
rf_p  <- predict(model4,Test)
cor.test(rf_p,Test$logBiomass)
R4<-cor(rf_p,Test$logBiomass)**2
RSME4<-rmse(rf_p,Test$logBiomass)

#model5
mdat5<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
             "logDepth","logBottom","logChl","SST_200m","Sal_200m","logMLD","logBiomass","logNO3_200m")]
dat1<-mdat5[is.finite(rowSums(mdat5)),]  
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
model5<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                       logDepth+logBottom+logChl+SST_200m+Sal_200m+logMLD+logNO3_200m,ntree=1000,mtry=6,data=Train)

rf_p  <- predict(model5,Test)
cor.test(rf_p,Test$logBiomass)
R5<-cor(rf_p,Test$logBiomass)**2
RSME5<-rmse(rf_p,Test$logBiomass)


#model6
mdat6<-MZ[,c("logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass","logNO3")]
dat1<-mdat6[is.finite(rowSums(mdat6)),]  
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
model6<-randomForest(logBiomass~logDepth+logBottom+logChl+SST+SSS+logMLD+logNO3,ntree=1000,mtry=6,data=Train)

rf_p  <- predict(model6,Test)
cor.test(rf_p,Test$logBiomass)
R6<-cor(rf_p,Test$logBiomass)**2
RSME6<-rmse(rf_p,Test$logBiomass)


mdat7<-MZ[,c("logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass")]
dat1<-mdat7[is.finite(rowSums(mdat7)),]   
rownames(dat1)<-1:nrow(dat1)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
model7<-randomForest(logBiomass~logDepth+logBottom+logChl+SST+SSS+logMLD,ntree=1000,mtry=6,data=Train)
rf_p  <- predict(model7,Test)
cor.test(rf_p,Test$logBiomass)
R7<-cor(rf_p,Test$logBiomass)**2
RSME7<-rmse(rf_p,Test$logBiomass)

