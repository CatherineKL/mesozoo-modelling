################################################################################
################################################################################
################################################################################
############################ 4 machine learning algorithms #########################

MZ<-read.csv("MZ_working.csv")
dat<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
           "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass")]

str(dat)
dat1<-na.omit(dat)   ###### remove NA values, then only remians 130,922 data points. 
rownames(dat1)<-1:nrow(dat1)

#set training data (70%) and testing data (30%)
set.seed(123)
x      <- sample(rownames(dat1), 0.7*nrow(dat1))
y      <- setdiff(rownames(dat1),x)         #Find the elements of rownames(dat1) which did not belong to x
Train  <- dat1[x,]   #Data for training
Test   <- dat1[y,]
####root mean standard errors (RMSE)
rmse   <- function(x,y)sd(x-y)

################1.boosted regression trees (BRT)
#the package of dismo, the main function is gbm.step(): fits a gbm model to one or more response variables, using cross-validation to estimate the optimal of trees.
library(dismo)
library(ggplot2)
library(foreach)
t1<-Sys.time()
meso_brt<-gbm.step(data=Train,gbm.x=1:13,gbm.y=14,family="gaussian",
                   tree.complexity=15,learning.rate=0.01,bag.fraction=0.5) 
t2<-Sys.time()
t2-t1 ###time:3.98 hours

summary(meso_brt)
saveRDS(meso_brt,file="Output_brt240701.rds")
meso_p  <- predict(meso_brt, Test,n.trees=meso_brt$gbm.call$best.trees, type="response")
cor(meso_p,Test$logBiomass)**2
rmse(meso_p,Test$logBiomass)
mean(meso_p-Test$logBiomass)

####simplying the model
meso_brt_simp<-gbm.simplify(meso_brt,n.drops=3) ####drop method.

####plotting the funcitons and fitted values from the model
par(mfrow=c(4,4))
gbm.plot(meso_brt,n.plots=12, write.title = F)
dev.off()

find.int<-gbm.interactions(meso_brt) ######no interaction

###################################################################################
#########     random forest

####The optimised ntree is 1000, and the mtry is 6. Use these two parameters in the following analysis.
library(randomForest)

t1<-Sys.time()
meso_rf<-randomForest(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                        logDepth+logBottom+logChl+SST+SSS+logMLD,ntree=1000,mtry=6,data=Train)
t2<-Sys.time()
t2-t1 #about38.65mins 
print(meso_rf)
print(importance(meso_rf,type=2))
varImpPlot(meso_rf)
saveRDS(meso_rf, "Output_rf240701.rds")

rf_p  <- predict(meso_rf,Test)
cor.test(rf_p,Test$logBiomass)
summary(lm(rf_p~Test$logBiomass))
cor(rf_p,Test$logBiomass)**2
rmse(rf_p,Test$logBiomass)


###################################################################################
###############support vector machine
library(doSNOW)
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)
library(e1071)
tobj  <- tune.svm(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                    logDepth+logBottom+logChl+SST+SSS+logMLD,data=Train[1:500,],type="eps-regression",gamma=10^(-3:0),cost=10^(0:3))
t1<-Sys.time()
meso_svm  <- svm(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                   logDepth+logBottom+logChl+SST+SSS+logMLD,data=Train,type="eps-regression",
                 gamma=tobj$best.parameters[[1]],cost=tobj$best.parameters[[2]])
t2<-Sys.time()
t2-t1 # 8.693318 mins
saveRDS(meso_svm, "Output_svm240701.rds")

svm_p<-predict(meso_svm,Test)
cor.test(svm_p,Test$logBiomass)
cor(svm_p,Test$logBiomass)**2
rmse(svm_p,Test$logBiomass)
mean(svm_p-Test$logBiomass)

###################################################################################
#########      Neural network
#Neural network
normalize <- function(x) (x-min(x))/(max(x)-min(x))
dat2       <- dat1
for (i in 1:ncol(dat2)){
  dat2[,i]   <- normalize(dat2[,i])
}
##transfer factor variables to dummy variable(虚拟变量)
#dummyV<-model.matrix(~Method,dat1)
#dat3<-cbind(dummyV,dat2)
#colnames(dat3)[1:4]<-c("V1","V2","V3","V4")

library(neuralnet)
x      <- sample(rownames(dat2), 0.7*nrow(dat2))
y      <- setdiff(rownames(dat2),x)         #Find the elements of rownames(NP1) which did not belong to x
Train  <- dat2[x,]   #Data for training
Test   <- dat2[y,]

t1<-Sys.time() 
meso_nn   <- neuralnet(logBiomass~latlon1+latlon2+latlon3+hDoy1+hDoy2+hTime1+hTime2+
                         logDepth+logBottom+logChl+SST+SSS+logMLD,
                       data=Train,hidden=10,stepmax = 1e+08,linear.output=FALSE)
t2<-Sys.time()
t2-t1 # about 15.0644 hours
saveRDS(meso_nn, file="Output_nn240701.rds")
plot(meso_nn)

v      <- c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
            "logDepth","logBottom","logChl","SST","SSS","logMLD")
nn.p    <- compute(meso_nn,Test[,v])$net.result
nn.p    <- nn.p*(max(dat1$logBiomass)-min(dat1$logBiomass))+min(dat1$logBiomass)
Test$logBiomass1<-Test$logBiomass*(max(dat1$logBiomass)-min(dat1$logBiomass))+min(dat1$logBiomass)
cor(nn.p,Test$logBiomass1)**2
rmse(nn.p,Test$logBiomass1)
