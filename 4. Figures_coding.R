setwd("/Users/kailinliu/Documents/Mesozooplankton")

meso<-read.csv("meso.csv")
str(meso)
nrow(meso)

library(ggplot2)

###########################################################################
#######################1. data distribution ###############################

###Fig. S1
Lon<-ggplot(data=meso,aes(x=Longitude))+geom_histogram(breaks=seq(-180,180,30),binwidth =30,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(-180,180,30))+xlab("Longitude")+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(a)")+theme(plot.title = element_text(size = 15,face = "bold"))

Lat<-ggplot(data=meso,aes(x=Latitude))+geom_histogram(breaks=seq(-90,90,10),binwidth =10,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(-90,90,10))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(b)")+theme(plot.title = element_text(size = 15,face = "bold"))

yr<-ggplot(data=meso,aes(x=Year))+geom_histogram(breaks=seq(1930,2020,5),binwidth =5,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(1930,2020,5))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, vjust =0.5))+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(c)")+theme(plot.title = element_text(size = 15,face = "bold"))

nm<-ggplot(data=subset(meso,meso$Latitude>=0),aes(x=Mon))+geom_histogram(breaks=seq(1,12,1),binwidth =1,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+xlab("")+
  annotate("text",x=8,y=19800,label="Northern Hemisphere",cex=6)+ylab("Count")+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(d)")+theme(plot.title = element_text(size = 15,face = "bold"))

sm<-ggplot(data=subset(meso,meso$Latitude<0),aes(x=Mon))+geom_histogram(breaks=seq(1,12,1),binwidth =1,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0),limits=c(0,4000))+scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+xlab("")+
  annotate("text",x=8,y=3800,label="Southern Hemisphere",cex=6)+ylab("Count")+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(e)")+theme(plot.title = element_text(size = 15,face = "bold"))

t<-ggplot(data=meso[-which(meso$Time.LOC<0),],aes(x=round(Time.LOC,0)))+geom_histogram(breaks=seq(0,24,1),binwidth =1,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(0,24,1))+xlab("Local time (h)")+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(f)")+theme(plot.title = element_text(size = 15,face = "bold"))
meso$Lower.Z[meso$Lower.Z>600]<-600
sp<-ggplot(data=meso,aes(x=Lower.Z))+geom_histogram(breaks=seq(0,600,50),binwidth =50,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(0,600,50))+xlab("Sampling Depth (m)")+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(g)")+theme(plot.title = element_text(size = 15,face = "bold"))

#######methods 
library(dplyr)
meso<-meso %>%
  mutate(bio_method=case_when(
    Type...Units=="  DV (ml/m3)" ~"displacement volume",
    Type...Units=="  SV (ml/m3)"~"settled volume",
    Type...Units=="  WM (mg/m3)"~"wet mass",
    Type...Units=="  DM (mg/m3)"|Type...Units=="  DM (mg/100m3)"~"dry mass"
  ))

m<-ggplot(data=meso,aes(x=bio_method))+geom_bar(aes(y=(..count..)/sum(..count..)),colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(labels = scales::percent,expand=c(0,0),limits=c(0,0.7))+ylab("Percentage")+xlab("Biomass measurement")+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(i)")+theme(plot.title = element_text(size = 15,face = "bold"))

mesh<-ggplot(data=meso,aes(x=Mesh))+geom_histogram(breaks=seq(140,660,30),binwidth =30,colour="black",fill="SeaGreen3",alpha=0.3)+
  scale_y_continuous(expand=c(0,0))+scale_x_continuous(breaks=seq(140,660,30))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2,linetype="solid"),axis.text=element_text(size=12,face = "bold"),axis.title=element_text(size=15,face = "bold"))+
  ggtitle("(h)")+theme(plot.title = element_text(size = 15,face = "bold"))+
  theme(axis.text.x=element_text(angle=45, vjust =0.5))+
  xlab(expression(Mesh~size~(mu~m)))


library(gridExtra)
pdf(file="data distribution240703.pdf",width = 18,height=13)
grid.arrange(Lon,Lat,yr,nm,sm,t,sp,mesh,m,ncol=3,nrow=3) ######arrange the figure in different width
dev.off()

###########################################################################
#######################2. data distribution on a map ###############################
#Fig.1

library(dplyr)
meso$longitude<-round(meso$Longitude,2)
meso$latitude<-round(meso$Latitude,2)
df<-meso %>%
  select(longitude, latitude) %>%
  count(longitude, latitude)

library(oceanmap)
?oceanmap::oceanmap
worldmap <- oceanmap:::.get.worldmap(worldmap)
str(worldmap)

Points1<-cbind(lon=df[which(df$n==1),]$longitude,lat=df[which(df$n==1),]$latitude)
Points2<-cbind(lon=df[which(df$n>1&df$n<10),]$longitude,lat=df[which(df$n>1&df$n<10),]$latitude)
Points3<-cbind(lon=df[which(df$n>=10&df$n<50),]$longitude,lat=df[which(df$n>=10&df$n<50),]$latitude)
Points4<-cbind(lon=df[which(df$n>=50&df$n<100),]$longitude,lat=df[which(df$n>=50&df$n<100),]$latitude)
Points5<-cbind(lon=df[which(df$n>=100),]$longitude,lat=df[which(df$n>=100),]$latitude)

lon<-c(-180,180)
lat<-c(90,-90)
pdf('meso samples.pdf',width=20,height=10)
tiff("sampling map.tiff",width =1900,height=900,units="px")
op     <- par(family ="serif",
              mar=c(2,0,2,0),
              oma=c(2,0,2,0),
              mgp=c(3,2,0))
plotmap(lon=lon,lat=lat,cex.ticks=2,axeslabels=FALSE)
points(Points1,pch=19,cex=0.8,col="LightSkyBlue")
points(Points2,pch=19,cex=0.8,col="DodgerBlue")
points(Points3,pch=19,cex=0.8,col="PeachPuff")
points(Points4,pch=19,cex=0.8,col="DarkOrange")
points(Points5,pch=19,cex=0.8,col="OrangeRed")
legend(185,20,c("1","1-10","10-50","50-100",">100"),pch=19,col=c("LightSkyBlue","DodgerBlue","PeachPuff","DarkOrange","OrangeRed"),
       title=c("Number of samples"),cex=1.4)
dev.off()

############################################################################
##################### statistical analysis on depth-interval group 
meso<-read.csv("meso.csv")
str(meso)

library(dplyr)
meso$logbiomass<-meso$MesoZooplankton.Carbon.Mass..mg.C.m3..mesh.adjusted
meso$biomass<-10^meso$logbiomass
summary<-meso%>%
  group_by(zCAT) %>%
  summarize(n=n(),
            mean=mean(biomass,na.rm=TRUE),
            sd=sd(biomass,na.rm=TRUE),
            median=median(biomass,na.rm=TRUE),
            maximum=max(biomass,na.rm=TRUE),
            minimum=min(biomass,na.rm=TRUE))
stat_result<-as.data.frame(summary)
write.csv(stat_result,file="statresult.csv")

###########################################################################
###############Meso-zooplankton biomass VS Environmental variables density plot ########
###Fig.2

MZ<-read.csv("MZ_working.csv")
library(ggplot2)
library(ggpointdensity) # 绘制密度散点图
library(viridis)

chllm<-lm(logBiomass~logChl,data=MZ)
summary(chllm)
###slope=0.61,intercept=0.72
chl<-ggplot(MZ,aes(x=logChl,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(method="lm",se=TRUE,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(log[10]~Chl~italic(a)~(mu~g~L^-1))))+
  annotate("text",x=-1.5,y=2.9,label="(a)",size=5)+
  annotate("text",x=-0.1,y=-3.2,label=expression(log[10]~Biomass==0.61*log[10]~Chl+0.72~(italic(p)<0.001)),fontface = "bold",size=5)

sst<-ggplot(MZ,aes(x=SST,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(SST~(degree~C))))+
  annotate("text",x=0,y=2.9,label="(b)",size=5)

sss<-ggplot(MZ,aes(x=SSS,y=logBiomass_ori))+geom_pointdensity()+xlim(20,40)+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(SSS)))+
  annotate("text",x=5,y=2.9,label="(c)",size=5)

mld<-ggplot(MZ,aes(x=logMLD,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(log[10]~MLD~(m))))+
  annotate("text",x=1,y=2.9,label="(d)",size=5)+xlim(1,3.5)

o2<-ggplot(MZ,aes(x=logO2,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(log[10]~O[2]~(mu~mol~kg^-1))))+
  annotate("text",x=2,y=2.9,label="(e)",size=5)+xlim(2,2.75)

depth<-ggplot(MZ,aes(x=logDepth,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(log[10]~sampling~depth~(m))))+
  annotate("text",x=1,y=2.9,label="(f)",size=5)

D<-ggplot(MZ,aes(x=logBottom,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(log[10]~bottom~depth~(m))))+
  annotate("text",x=1,y=2.9,label="(g)",size=5)

time<-ggplot(MZ,aes(x=Time.LOC,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(Local~time~(hour))))+
  annotate("text",x=0,y=2.9,label="(h)",size=5)+xlim(0,24)

doy<-ggplot(MZ,aes(x=DOY2,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",size=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(Day~of~year~(d))))+
  annotate("text",x=1,y=2.9,label="(i)",size=5) 

Lat<-ggplot(MZ,aes(x=Latitude,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",linewidth=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(Latitude)))+
  annotate("text",x=-75,y=2.9,label="(j)",size=5) 

Lon<-ggplot(MZ,aes(x=Longitude,y=logBiomass_ori))+geom_pointdensity()+
  scale_color_viridis(option = "H",name="Density of Data")+ ####option H is the rainbow colour
  theme_bw(base_size = 14,base_family = 'Helvetica')+geom_smooth(se=T,col="black",linewidth=1.2)+
  theme(legend.title=element_text(size=10),legend.text=element_text(size=10))+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold'))+
  ylab(expression(bold(log[10]~Biomass~(mu~gC~L^-1))))+xlab(expression(bold(Longitude)))+
  annotate("text",x=-150,y=2.9,label="(k)",size=5) 


######use common legend
library(ggpubr)
pdf(file="observation data pattern-240720.pdf",width = 16,height=16)
jpeg(filename = "observation data pattern240703.jpeg",width = 1600, height =1400,quality=300)
ggarrange(chl,sst,sss,mld,o2,depth,D,time,doy,Lat,Lon, nrow = 4, ncol = 3, common.legend=TRUE,legend="right")
dev.off()


####################################################################################################
###############Global distribution of mesozooplankton biomass, monthly climatology

## Fig.4 and Fig. 5

library("ggplot2")
library("sf")
theme_set(theme_bw())
library("rnaturalearth") 
library("rnaturalearthdata")

world<-ne_countries(scale="medium",returnclass="sf") 
class(world)

scale_xy_map <- function(...) {
  list(
    ggplot2::scale_x_continuous("Longitude",
                                expand=c(0,0), minor_breaks=NULL, labels=function(x) {
                                  paste0(abs(x), "\u00B0", c("W","","E")[sign(x)+2])
                                }, ...),
    ggplot2::scale_y_continuous("Latitude",
                                expand=c(0,0), minor_breaks=NULL, labels=function(x) {
                                  paste0(abs(x), "\u00B0", c("S","","N")[sign(x)+2])
                                }, ...)
  )
}

jet.colors   <- colorRampPalette(c("blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  

mons <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
myfiles <- list.files(path = "MonthlypredictionRF/", pattern = ".csv", full.names = TRUE)
plot_list<-list()

for(i in 1:12){
  
  monthly<-read.csv(myfiles[i])
  j=monthly$month[1]
  
  plot_list[[j]]<-ggplot(data=world)+
    geom_sf(fill="grey60")+
    geom_raster(data=monthly,aes(x=Longitude,y=Latitude,fill=Mesobiomass))+
    scale_fill_gradientn(colours= jet.colors(8),na.value="white")+scale_xy_map()+
    theme(panel.background=element_rect(fill="white"),panel.grid.major=element_blank())+
    theme(plot.margin = unit(c(-2, 0, -2, 0),"cm"))+
    theme(axis.title=element_blank(),axis.text = element_text(size=12,face="bold"),
          legend.text=element_text(size=12),legend.key.height=unit(1,"cm"),
          legend.key.width=unit(0.5,"cm"),legend.margin=ggplot2::margin(0,0,0,0),legend.box.margin=ggplot2::margin(-10,10,0,-10))+
    theme(legend.spacing.x =unit(0.1,'cm'))+ ####change the spacing between legend and figures
    labs(fill="",title=mons[j])+
    theme(plot.title=element_text(hjust=0.5,face="bold",size=16))
}

figure <- ggpubr::ggarrange(plotlist = plot_list, nrow = 4, ncol = 3)
ggpubr::ggexport(figure, filename = "Monthly pridiction2407.pdf", width = 16, height =10.5)
ggpubr::ggexport(figure, filename = "Monthly pridiction2407.jpeg", width = 1600, height =1050)


########the monthly distribution when setting time to be 24h 
for(i in 1:12){
  monthly<-read.csv(myfiles[i])
  j=monthly$month[1]
  
  plot_list[[j]]<-ggplot(data=world)+
    geom_sf(fill="grey60")+
    geom_raster(data=monthly,aes(x=Longitude,y=Latitude,fill=Mesobiomass_n/1000,))+
    scale_fill_gradientn(colours= jet.colors(9),na.value=jet.colors(1))+scale_xy_map()+
    theme(panel.background=element_rect(fill=jet.colors(1)),panel.grid.major=element_blank())+
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2),"cm"))+
    theme(axis.title=element_blank(),axis.text = element_text(size=14,face="bold"),
          legend.text=element_text(size=12),legend.key.height=unit(1.2,"cm"),
          legend.key.width=unit(0.5,"cm"),legend.margin=margin(0,0,0,0),legend.box.margin=margin(-10,10,0,-10))+
    theme(legend.spacing.x =unit(0.1,'cm'))+ ####change the spacing between legend and figures
    labs(fill="",title=mons[j])+
    theme(plot.title=element_text(hjust=0.5,face="bold",size=16))
}

figure <- ggpubr::ggarrange(plotlist = plot_list, nrow = 4, ncol = 3)
ggpubr::ggexport(figure, filename = "Monthly pridiction_night0926.jpeg", width = 1200, height =1000)

######## the difference between day and night
mycolour<-colorRampPalette(c("green3", "lightgreen", "#FFFFBF","#FEE090", "#FDAE61", "#F46D43","red","#74ADD1", "#4575B4", "#313695"))
#mycolour<-colorRampPalette(brewer.pal(9,"YlOrBr"))

for(i in 1:12){
  
  monthly<-read.csv(myfiles[i])
  j=monthly$month[1]
  
  plot_list[[j]]<-ggplot(data=world)+
    geom_sf(fill="grey60")+
    geom_raster(data=monthly,aes(x=Longitude,y=Latitude,fill=diff))+
    scale_fill_gradientn(colours= mycolour(10),na.value="white",lim=c(-450,1500))+
    theme(panel.background=element_rect(fill="white"),panel.grid.major=element_blank())+
    scale_xy_map()+
    theme(plot.margin = unit(c(-2, 0, -2, 0),"cm"))+
    theme(axis.title=element_blank(),axis.text = element_text(size=14,face="bold"))+
    theme(legend.spacing.x =unit(0.1,'cm'),legend.text=element_text(size=14,face="bold"),
          legend.key.height=unit(1.2,"cm"),legend.key.width=unit(4.5,"cm"),
          legend.title=element_text(size=16))+ ####change the spacing between legend and figures
    labs(fill=expression(bold(Biomass(night-day)~(mgC~m^-2))),title=mons[j])+
    theme(plot.title=element_text(hjust=0.5,face="bold",size=16))
}

figure <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom",nrow = 4, ncol = 3)
ggpubr::ggexport(figure, filename = "difference_2407.jpeg", width = 1600, height =1250)

#Fig.4
#########annual mean mesozooplankton biomass
setwd("/Users/kailinliu/Documents/Mesozooplankton")
MZ_RF<-read.csv("MZbiomass_annual2407.csv")
MZ_BRT<-read.csv("MZbiomass_annualBRT_2407.csv")
MZ_SVM<-read.csv("MZbiomass_annualSVM_2407.csv")
MZ_NN<-read.csv("MZbiomass_annualANN_240728.csv")
library(raster)
library(terra)
library(graticule)
library(rgdal)
library(rworldmap)

pdf('annual mean0728.pdf',width=22,height=10.5)
op     <- par(font.lab=1.2,
              family ="serif",
              mar=c(0,1,10,8),
              oma=c(0.1,1,0,1),
              mfrow  = c(2,2))

df<-MZ_RF[,c("Longitude","Latitude","annualmean")]
#####convert to raster 
dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
crs(dfr)<-CRS("+proj=longlat +datum=WGS84")
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ####Robinson Projection
raster::crs(dfr)
jp<-terra::rast(dfr$annualmean)
jp<-jp*1# to deal with NAs in this datasaet
new1<-terra::project(jp,crs,mask=TRUE)
#####draw the world map
worldmap<-rworldmap::getMap(resolution="coarse")
raster::crs(worldmap)
#######project the vector data into the Robinson projection
worldmap<-sp::spTransform(worldmap,crs)
# Creates latitude and longitude labels and graticules
lat <- c(-90, -60, -30, 0, 30, 60, 90)
long <- c(-180, -120, -60, 0, 60, 120, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 


jet.colors   <- colorRampPalette(c( "blue", "#007FFF", "cyan",
                                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  
plot(new1,axes=F,col=jet.colors(20),legend=FALSE)
plot(worldmap,add=T,col="grey60")
plot(lines,lty=5,lwd=0.5,col="grey",add=TRUE)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA) # plots latitude labels
plot(dfr, legend.only=TRUE, col=jet.colors(20),legend.mar=2,  #####cannot plot for new the robinson projection's raster
     legend.width=1, legend.shrink=0.5)
title("(a) RF",adj=0,cex.main=2,line=8.8)



###########BRT

df<-MZ_BRT[,c("Longitude","Latitude","annualmean")]
#####convert to raster 
dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
crs(dfr)<-CRS("+proj=longlat +datum=WGS84")
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ####Robinson Projection
raster::crs(dfr)
jp<-terra::rast(dfr$annualmean)
jp<-jp*1# to deal with NAs in this datasaet
new1<-terra::project(jp,crs,mask=TRUE)
#####draw the world map
worldmap<-rworldmap::getMap(resolution="coarse")
raster::crs(worldmap)
#######project the vector data into the Robinson projection
worldmap<-sp::spTransform(worldmap,crs)
# Creates latitude and longitude labels and graticules
lat <- c(-90, -60, -30, 0, 30, 60, 90)
long <- c(-180, -120, -60, 0, 60, 120, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 


jet.colors   <- colorRampPalette(c( "blue", "#007FFF", "cyan",
                                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  
plot(new1,axes=F,col=jet.colors(20),legend=FALSE)
plot(worldmap,add=T,col="grey60")
plot(lines,lty=5,lwd=0.5,col="grey",add=TRUE)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA) # plots latitude labels
plot(dfr, legend.only=TRUE, col=jet.colors(20),legend.mar=2,  #####cannot plot for new the robinson projection's raster
     legend.width=1, legend.shrink=0.5)
title("(b) BRT",adj=0,cex.main=2,line=8.8)

####SVM
#m<-which.max(MZ_SVM$annualmean)
#MZ_SVM<-MZ_SVM[-m,]
df<-MZ_SVM[,c("Longitude","Latitude","annualmean")]
#####convert to raster 
dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
crs(dfr)<-CRS("+proj=longlat +datum=WGS84")
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ####Robinson Projection
raster::crs(dfr)
jp<-terra::rast(dfr$annualmean)
jp<-jp*1# to deal with NAs in this datasaet
new1<-terra::project(jp,crs,mask=TRUE)
#####draw the world map
worldmap<-rworldmap::getMap(resolution="coarse")
raster::crs(worldmap)
#######project the vector data into the Robinson projection
worldmap<-sp::spTransform(worldmap,crs)
# Creates latitude and longitude labels and graticules
lat <- c(-90, -60, -30, 0, 30, 60, 90)
long <- c(-180, -120, -60, 0, 60, 120, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 


jet.colors   <- colorRampPalette(c( "blue", "#007FFF", "cyan",
                                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  
plot(new1,axes=F,col=jet.colors(20),legend=FALSE)
plot(worldmap,add=T,col="grey60")
plot(lines,lty=5,lwd=0.5,col="grey",add=TRUE)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA) # plots latitude labels
plot(dfr, legend.only=TRUE, col=jet.colors(20),legend.mar=2,  #####cannot plot for new the robinson projection's raster
     legend.width=1, legend.shrink=0.5)
title("(c) SVM",adj=0,cex.main=2,line=8.8)

####ANN

df<-MZ_NN[,c("Longitude","Latitude","annualmean")]
#####convert to raster 
dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
crs(dfr)<-CRS("+proj=longlat +datum=WGS84")
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ####Robinson Projection
raster::crs(dfr)
jp<-terra::rast(dfr$annualmean)
jp<-jp*1# to deal with NAs in this datasaet
new1<-terra::project(jp,crs,mask=TRUE)
#####draw the world map
worldmap<-rworldmap::getMap(resolution="coarse")
raster::crs(worldmap)
#######project the vector data into the Robinson projection
worldmap<-sp::spTransform(worldmap,crs)
# Creates latitude and longitude labels and graticules
lat <- c(-90, -60, -30, 0, 30, 60, 90)
long <- c(-180, -120, -60, 0, 60, 120, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 


jet.colors   <- colorRampPalette(c( "blue", "#007FFF", "cyan",
                                    "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  
plot(new1,axes=F,col=jet.colors(20),legend=FALSE)
plot(worldmap,add=T,col="grey60")
plot(lines,lty=5,lwd=0.5,col="grey",add=TRUE)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA) # plots latitude labels
plot(dfr, legend.only=TRUE, col=jet.colors(20),legend.mar=2,  #####cannot plot for new the robinson projection's raster
     legend.width=1, legend.shrink=0.5)
title("(d) ANN",adj=0,line=8.8,cex.main=2)
dev.off()

###########################################################################
#################Permutation-based feature importance################
#install.packages("vip")
library(vip)
library(dplyr)
library(ggplot2)
setwd("/Users/kailinliu/Documents/Mesozooplankton")

MZ<-read.csv("MZ_working.csv")
dat<-MZ[,c("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
           "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass")]

str(dat)
dat1<-na.omit(dat)
rownames(dat1)<-1:nrow(dat1)

meso_brt<-readRDS("Output_brt_full240704.rds")
meso_rf<-readRDS("Output_rf_full0703.rds")  
meso_svm<-readRDS("Output_svm240701.rds")
meso_nn<-readRDS("Output_nn240701.rds")

set.seed(825)  # for reproducibility
pfun <- function(object, newdata) predict(object, newdata = newdata)
vis_svm<- vi(meso_svm, method = "permute", train = dat1, target = "logBiomass",
          nsim = 10, metric = "rmse", pred_wrapper = pfun)
#vis_rf<- vi(meso_rf, method = "permute", train = dat1[1:100,], target = "logBiomass",
#             nsim = 10, metric = "rmse",pred_wrapper = pfun)
#vis_brt<- vi(meso_brt, method = "permute", train = dat1[1:100,], target = "logBiomass",
#           nsim = 10, metric = "rmse", pred_wrapper = pfun)
#vis_nn<- vi(meso_nn, method = "permute", train = dat1[1:100,], target = "logBiomass",
#             nsim = 10, metric = "rmse", pred_wrapper =pfun)

vip(meso_rf)
vip(meso_nn)
vip(meso_svm)
vip(meso_brt)
#RF
vi_rf<-as.data.frame(vi(meso_rf))
vi_rf<-vi_rf %>%
  mutate(factor=case_when(
    Variable=="logChl"~"ep",Variable=="logBottom"~"ep",Variable=="logDepth"~"ep",
    Variable=="SST"~"ep",Variable=="SSS"~"ep",Variable=="logMLD"~"ep"))
vi_rf$factor[is.na(vi_df$factor)]<-"nev"
rf_vip<-ggplot(data=vi_rf,aes(x=reorder(Variable, Importance),y=Importance,fill=factor))+
  geom_col()+coord_flip()+
  theme_bw(base_size = 12,base_family = 'Helvetica')+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold',size=13))+
  scale_y_continuous(expand=c(0,0))+scale_fill_manual(values=c("sandybrown", "lightgrey"))+
  ggtitle("(a)")
#brt
vi_brt<-as.data.frame(vi(meso_brt))
vi_brt<-vi_brt %>%
  mutate(factor=case_when(
    Variable=="logChl"~"ep",Variable=="logBottom"~"ep",Variable=="logDepth"~"ep",
    Variable=="SST"~"ep",Variable=="SSS"~"ep",Variable=="logMLD"~"ep"))
vi_brt$factor[is.na(vi_brt$factor)]<-"nev"
brt_vip<-ggplot(data=vi_brt,aes(x=reorder(Variable, Importance),y=Importance,fill=factor))+
  geom_col()+coord_flip()+
  theme_bw(base_size = 12,base_family = 'Helvetica')+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold',size=13))+
  scale_y_continuous(expand=c(0,0))+scale_fill_manual(values=c("sandybrown", "lightgrey"))+
  ggtitle("(a) Boosted Regression Trees (BRT)")

#ANN
vi_nn<-as.data.frame(vi(meso_nn))
vi_nn<-vi_nn %>%
  mutate(factor=case_when(
    Variable=="logChl"~"ep",Variable=="logBottom"~"ep",Variable=="logDepth"~"ep",
    Variable=="SST"~"ep",Variable=="SSS"~"ep",Variable=="logMLD"~"ep"))
vi_nn$factor[is.na(vi_nn$factor)]<-"nev"
nn_vip<-ggplot(data=vi_nn,aes(x=reorder(Variable, Importance),y=Importance,fill=factor))+
  geom_col()+coord_flip()+
  theme_bw(base_size = 12,base_family = 'Helvetica')+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold',size=13))+
  scale_y_continuous(expand=c(0,0))+scale_fill_manual(values=c("sandybrown", "lightgrey"))+
  ggtitle("(b) Artificial Neural Network (ANN)")

#SVM
vi_svm<-read.csv("vis_svm.csv")
vi_svm<-vi_svm %>%
  mutate(factor=case_when(
    Variable=="logChl"~"ep",Variable=="logBottom"~"ep",Variable=="logDepth"~"ep",
    Variable=="SST"~"ep",Variable=="SSS"~"ep",Variable=="logMLD"~"ep"))
vi_svm$factor[is.na(vi_svm$factor)]<-"nev"
svm_vip<-ggplot(data=vi_svm,aes(x=reorder(Variable, Importance),y=Importance,fill=factor))+
  geom_col()+coord_flip()+
  theme_bw(base_size = 12,base_family = 'Helvetica')+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold',size=13))+
  scale_y_continuous(expand=c(0,0))+scale_fill_manual(values=c("sandybrown", "lightgrey"))+
  ggtitle("(c) Support Vector Machines (SVM)")

library(gridExtra)
pdf(file="VIP.pdf",width =12,height=8)
grid.arrange(brt_vip,nn_vip,svm_vip,ncol=2,nrow=2) ######arrange the figure in different width
dev.off()


###########################################################################
#################Partial effects of environmental variables################

##Fig. 6

setwd("/Users/kailinliu/Documents/Mesozooplankton")
library(dplyr)
MZ<-read.csv("MZ_working.csv")
AD<-read.csv("MZbiomass_annual2407.csv")

dat<-MZ%>%
  select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
         "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method","logBiomass_ori") %>%
  mutate_if(is.character,as.factor)
dat1<-na.omit(dat) 

###### partial effect plot of the interaction of lon and lat
###### set up a new dataset based on AD, set other parameters constant
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
lonlat<-AD %>%
  select("Latitude","Longitude") %>%
  mutate(.,Time=median(MZ$Time.LOC),
         DOY=median(MZ$DOY2,na.rm=TRUE))
lonlat<-lonlat%>%
  mutate(latlon1=sin(Latitude*pi/180),
         latlon2=sin(Longitude*pi/180)*cos(Latitude*pi/180),
         latlon3=-cos(Longitude*pi/180)*cos(Latitude*pi/180)) %>%
  mutate(HarmTOD = (Time/24)*2*pi, 
         HarmDOY = (DOY/365)*2*pi,
         hTime=fHarmonic(HarmTOD,k=1),
         hDoy=fHarmonic(HarmDOY,k=1),
         hTime1=hTime[,1],
         hTime2=hTime[,2],
         hDoy1=hDoy[,1],
         hDoy2=hDoy[,2])%>%
  mutate(logChl=median(MZ$logChl,na.rm=TRUE),
         SST=median(MZ$SST,na.rm=TRUE),
         SSS=median(MZ$SSS,na.rm=TRUE),
         logMLD=median(MZ$logMLD,na.rm=TRUE),
         logO2=median(MZ$logO2,na.rm=TRUE),
         logDepth=median(MZ$logDepth,na.rm=TRUE),
         logBottom=median(MZ$logBottom,na.rm=TRUE),
         Mesh=as.integer(rep(330,nrow(lonlat))),
         Method=as.factor(rep("dry mass",nrow(lonlat))))
lonlat1<-lonlat %>%
  select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
         "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method")
levels(lonlat1$Method)<-levels(dat1$Method) 

########load the RF model
library(randomForest)
meso_rf<-readRDS("Output_rf_full0703.rds")  
lonlat1$logmeso<-predict(meso_rf,lonlat1)
lonlat1$Mesobiomass<-10^(lonlat1$logmeso)
lonlat2<-cbind(lonlat[,c("Latitude","Longitude")],lonlat1)

#######draw the map
library("ggplot2")
library("sf")
library("rnaturalearth") ### rnaturalearth 提供了世界各国的地图， ne_countries()给出城市数据。
library("rnaturalearthdata")
world<-ne_countries(scale="medium",returnclass="sf") #####加载world data
scale_xy_map <- function(...) {
  list(
    ggplot2::scale_x_continuous("Longitude",
                                expand=c(0,0), minor_breaks=NULL, labels=function(x) {
                                  paste0(abs(x), "\u00B0", c("W","","E")[sign(x)+2])
                                }, ...),
    ggplot2::scale_y_continuous("Latitude",
                                expand=c(0,0), minor_breaks=NULL, labels=function(x) {
                                  paste0(abs(x), "\u00B0", c("S","","N")[sign(x)+2])
                                }, ...)
  )
}
jet.colors   <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))  
library(RColorBrewer)
colormap<-colorRampPalette(rev(brewer.pal(11,'Spectral')[2:11]))(80)

latlon<-ggplot(data=world)+
  geom_sf(fill="grey80")+
  geom_raster(data=lonlat2,aes(x=Longitude,y=Latitude,fill=logmeso))+
  scale_fill_gradientn(colours= colormap,lim=c(0,1.2),na.value=jet.colors(1),name=expression(atop(paste(log[10],'Biomass'),(mu~gC~L^-1))))+
  scale_xy_map()+theme_bw(base_size=14,base_family = "Helvetica")+
  theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
  theme(axis.text=element_text(size=14,face="bold",colour="black"))+
  ggtitle("(m)")
 
##### partial effects of other paramets: using partialplot.
#get variable importance measures
imp_df<-data.frame(importance(meso_rf,scale=FALSE,type=2))
imp_df<-imp_df %>%
  mutate(names=rownames(imp_df)) %>%
  arrange(desc(IncNodePurity))
#plot IncNodePurity(分类的指标是mean decreased accuracy)
imp_df<-imp_df %>%
  mutate(factor=case_when(
    names=="logChl"~"ep",names=="logBottom"~"ep",names=="logDepth"~"ep",
    names=="SST"~"ep",names=="SSS"~"ep",names=="logMLD"~"ep"))
imp_df$factor[is.na(imp_df$factor)]<-"nev"
imp<-ggplot(data=imp_df,aes(x=reorder(names,IncNodePurity),y=IncNodePurity,fill=factor))+
  geom_col()+coord_flip()+
  theme_bw(base_size = 13,base_family = 'Helvetica')+
  theme(legend.position = "none")+xlab("")+ylab("IncNodePurity(Importance)")+
  theme(axis.title=element_text(face='bold'),axis.text=element_text(face='bold',size=14))+
  scale_y_continuous(expand=c(0,0))+scale_fill_manual(values=c("sandybrown", "lightgrey"))+
  ggtitle("(a)")+theme(plot.title=element_text(size=14,face="bold"))


############plot the environmental variables, use partial_dependence() in edarf package
#install.packages("pdp")
library(pdp)
#save predictor names as character vector
env<-subset(imp_df,imp_df$factor=="ep")
nm<-as.character(env$names)
xname<-c("logChla (μg/L)","log(Bottom Depth)(m)","logSampling Depth (m)","SST (°C)","logO2 (μmol/kg)","SSS","logMLD (m)" )
title<-c("b","c","d","e","f","g","h")
pdplot<-list()
#get partial dependence values for the predictors

pd_rf<-read.csv("partial effect_rf0726.csv")
pd_brt<-read.csv("partial effect_brt.csv")
pd_rf$method<-rep("RF",nrow(pd_rf))
pd_brt$method<-rep("BRT",nrow(pd_brt))
pd_df<-rbind(pd_rf,pd_brt)
nm<-c("logChl","logDepth","logBottom","SST","SSS","logMLD") 
title<-c("b","c","d","e","f","g")
pdplot<-list()
for(i in 2:6){
  pd_p<-subset(pd_df,pd_df$predictor==nm[i]) 
  
  pdplot[[i]]<-ggplot(pd_p, aes(x = xvalue, y = biomass,col=method))+
    geom_line(aes(size=method))+
    labs(x=nm[i],
         y = expression(Predicted~log[10]~Biomass~mu~gC~L^-1),
         title=paste("(",title[i],")")) +
    theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black",size=2.5,linetype="solid"),
          axis.text=element_text(size=14,face = "bold"),axis.title=element_text(size=14,face = "bold"))+
    theme(legend.position="none")+
    scale_color_manual(values=c('black','blue'))+
    scale_size_manual(values=c(0.8,1.5))+
    theme(plot.title=element_text(size=14,face="bold"))
}
pd_p<-subset(pd_df,pd_df$predictor==nm[1]) 
pdplot[[1]]<-ggplot(pd_p, aes(x = xvalue, y = biomass,col=method))+
  geom_line(aes(size=method))+
  labs(x=nm[i],
       y = expression(Predicted~log[10]~Biomass~mu~gC~L^-1),
       title=paste("(b)")) +
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black",size=2.5,linetype="solid"),
        axis.text=element_text(size=14,face = "bold"),axis.title=element_text(size=14,face = "bold"))+
  theme(legend.position=c(0.8,0.25))+
  scale_color_manual(values=c('black','blue'))+
  scale_size_manual(values=c(0.8,1.5))+
  theme(plot.title=element_text(size=14,face="bold"))+
  xlab(expression(log[10]~Chl~italic(a)~(mu~g~L^-1)))
pdplot[[3]]<-pdplot[[3]]+xlab(expression(log[10]~(Bottom~Depth)~(m)))
pdplot[[2]]<-pdplot[[2]]+xlab(expression(log[10]~(Sampling~Depth)~(m)))
pdplot[[6]]<-pdplot[[6]]+xlab(expression(log[10]~MLD~(m)))



#####interactive effect of chla and temperature
##### each interaction calculation cost about 12 hours 
inter<-partial(meso_rf,pred.var = c("logChl", "SST"))
chlsst<-ggplot(inter,aes(x=logChl,y=SST,z=yhat))+geom_raster(aes(fill=yhat))+
  geom_contour(colour="white")+
  scale_fill_gradientn(colours=colormap,name=expression(atop(paste(log[10],'Biomass'),(mu~gC~L^-1)))) +
  theme_bw(base_size=14,base_family = "Helvetica")+scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  xlab(expression(log[10]~Chl~italic(a)~(mu~g~L^-1)))+ylab(expression(SST~(degree~C)))+
  theme(axis.text=element_text(size=13,colour="black",face="bold"))+
  theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
  ggtitle("(i)")+theme(plot.title=element_text(size=14,face="bold"))

inter2<-partial(meso_rf,pred.var = c("logChl", "logBottom"))
chlbottom<-ggplot(inter2,aes(x=logChl,y=logBottom,z=yhat))+geom_raster(aes(fill=yhat))+
  geom_contour(colour="white")+
  scale_fill_gradientn(colours=colormap,name=expression(atop(paste(log[10],'Biomass'),(mu~gC~L^-1))))+
  theme_bw(base_size=14,base_family = "Helvetica")+scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  xlab(expression(log[10]~Chl~italic(a)~(mu~g~L^-1)))+ylab(expression(log[10]~(Bottom~Depth)~(m)))+
  theme(axis.text=element_text(size=13,colour="black",face="bold"))+
  theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
  ggtitle("(h)")+theme(plot.title=element_text(size=14,face="bold"))

inter3<-partial(meso_rf,pred.var = c("SST", "logBottom"))
sstbottom<-ggplot(inter3,aes(x=SST,y=logBottom,z=yhat))+geom_raster(aes(fill=yhat))+
  geom_contour(colour="white")+
  scale_fill_gradientn(colours=colormap,name=expression(atop(paste(log[10],'Biomass'),(mu~gC~L^-1))))+
  theme_bw(base_size=14,base_family = "Helvetica")+scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  xlab(expression(SST~(degree~C)))+ylab(expression(log[10]~(Bottom~Depth)~(m)))+
  theme(axis.text=element_text(size=13,colour="black",face="bold"))+
  theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
  ggtitle("(j)")+theme(plot.title=element_text(size=14,face="bold"))

############DOY and sampling depth
  depth=seq(1,3,by=0.05) 
  doy<-data.frame(logDepth=depth,DOY=c(rep(1,len=length(depth))))
  for(i in 2:366){
    X<-data.frame(logDepth=depth,DOY=c(rep(i,len=length(depth))))
    doy<-rbind(doy,X)
  }
  
  doy<-doy%>%
    mutate(latlon1=median(MZ$latlon1,na.rm=TRUE),
           latlon2=median(MZ$latlon2,na.rm=TRUE),
           latlon3=median(MZ$latlon3,na.rm=TRUE),
           hTime1=median(MZ$hTime1,na.rm=TRUE),
           hTime2=median(MZ$hTime2,na.rm=TRUE),
           logChl=median(MZ$logChl,na.rm=TRUE),
           SST=median(MZ$SST,na.rm=TRUE),
           SSS=median(MZ$SSS,na.rm=TRUE),
           logMLD=median(MZ$logMLD,na.rm=TRUE),
           logO2=median(MZ$logO2,na.rm=TRUE),
           logBottom=median(MZ$logBottom,na.rm=TRUE),
           Mesh=as.integer(rep(330,nrow(doy))),
           Method=as.factor(rep("dry mass",nrow(doy))))%>%
    mutate(HarmDOY = (DOY/365)*2*pi,
           hDoy=fHarmonic(HarmDOY,k=1),
           hDoy1=hDoy[,1],
           hDoy2=hDoy[,2])
  levels(doy$Method)<-levels(dat1$Method) 
  doy1<-doy %>%
    select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
           "logDepth","logBottom","logChl","SST","SSS","logMLD","logO2","Mesh","Method")
  
  library(randomForest)
  meso_rf<-readRDS("Output_rf_full0703.rds")
  doy1$logmeso<-predict(meso_rf,doy1)
  doy1$Mesobiomass<-10^(doy1$logmeso)
  doy1$DOY<-doy$DOY
  
  ######2d plot
  v<-ggplot(doy1,aes(x=DOY,y=logDepth,z=logmeso))+geom_raster(aes(fill=logmeso))+
    scale_fill_gradientn(colours=colormap,name=expression(atop(paste(log[10],'Biomass'),(mu~gC/L))))+
    geom_contour(colour="white")+theme_bw(base_size = 14,base_family = 'Helvetica')+
    scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
    xlab("The day of year (DOY,d)")+ylab(expression(log[10]~Sampling~depth~(m)))+
    theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
    theme(axis.text=element_text(size=13,colour="black",face="bold"))+
    ggtitle("(k)")+theme(plot.title=element_text(size=14,face="bold"))  
 
  
#######time and sampling depth
  depth=seq(1,3,by=0.05) 
  T=seq(0,24,by=0.2)
  time<-data.frame(logDepth=depth,hour=c(rep(0,len=length(depth))))
  for(i in 2:length(T)){
    X<-data.frame(logDepth=depth,hour=c(rep(T[i],len=length(depth))))
    time<-rbind(time,X)
  }
  
  time<-time%>%
    mutate(latlon1=median(MZ$latlon1,na.rm=TRUE),
           latlon2=median(MZ$latlon2,na.rm=TRUE),
           latlon3=median(MZ$latlon3,na.rm=TRUE),
           hDoy1=median(MZ$hDoy1,na.rm=TRUE),
           hDoy2=median(MZ$hDoy2,na.rm=TRUE),
           logChl=median(MZ$logChl,na.rm=TRUE),
           SST=median(MZ$SST,na.rm=TRUE),
           SSS=median(MZ$SSS,na.rm=TRUE),
           logMLD=median(MZ$logMLD,na.rm=TRUE),
           logO2=median(MZ$logO2,na.rm=TRUE),
           logBottom=median(MZ$logBottom,na.rm=TRUE),
           Mesh=as.integer(rep(330,nrow(time))),
           Method=as.factor(rep("dry mass",nrow(time))))%>%
    mutate(HarmTOD = (hour/24)*2*pi, 
           hTime=fHarmonic(HarmTOD,k=1),
           hTime1=hTime[,1],
           hTime2=hTime[,2])
  levels(time$Method)<-levels(dat1$Method) 
  time1<-time %>%
    select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
           "logDepth","logBottom","logChl","SST","SSS","logMLD")
  time1$logmeso<-predict(meso_rf,time1)
  time1$Mesobiomass<-10^(time1$logmeso)
  time1$hour<-time$hour


  t<-ggplot(time1,aes(x=hour,y=logDepth,z=logmeso))+geom_raster(aes(fill=logmeso))+
    scale_fill_gradientn(colours=colormap,name=expression(atop(paste(log[10],'Biomass'),(mu~gC/L))))+
    geom_contour(colour="white")+theme_bw(base_size = 14,base_family = 'Helvetica')+
    scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
    xlab("Sampling time (h)")+ylab(expression(log[10]~Sampling~depth~(m)))+
    theme(legend.title=element_text(size=12),legend.key.size=unit(15,"pt"),legend.key.height = unit(1.2,"cm"))+
    theme(axis.text=element_text(size=13,colour="black",face="bold"))+
    ggtitle("(l)")+theme(plot.title=element_text(size=14,face="bold")) 
  
 blank<-ggplot()+theme_void() 
  
  
library(gridExtra)
pdf(file="partial plot-2407.pdf",width =14.5, height =18)
#jpeg(filename = "partial plot-2407.jpeg",width =1200, height =1600)
grid.arrange(imp,pdplot[[1]],pdplot[[2]],pdplot[[3]],pdplot[[4]],pdplot[[5]],pdplot[[6]],blank,
                           chlbottom,chlsst,sstbottom,v,t,latlon,
                            layout_matrix=rbind(c(1,2,3,4),c(5,6,7,8),c(9,9,10,10),c(11,11,12,12),c(13,13,14,14)))
dev.off()


###########################################################################
#################Accumulated local effects-"iml"package ###############
setwd("/Users/kailinliu/Documents/Mesozooplankton")
#install.packages("iml")
library("iml")
library("randomForest")
library(ggplot2)
library(dplyr)
MZ<-read.csv("MZ_working.csv")
meso_rf<-readRDS("Output_rf_full0703.rds") 
dat<-MZ%>%
  select("latlon1","latlon2","latlon3","hDoy1","hDoy2","hTime1","hTime2",
         "logDepth","logBottom","logChl","SST","SSS","logMLD","logBiomass") %>%
  mutate_if(is.character,as.factor)
dat1<-na.omit(dat)
X<-dat1[which(names(dat1)!="logBiomass")]
predictor<-Predictor$new(meso_rf,data=X,y=dat1$logBiomass)

#####all predictor
effs<-FeatureEffects$new(predictor)
plot(effs)

######measure interactions
interact<-Interaction$new(predictor)
plot(interact)

chlinteract<-Interaction$new(predictor,feature="logChl")
plot(chlinteract)

sstinteract<-Interaction$new(predictor,feature="SST")
plot(sstinteract)

############CMIP prediction
################ plot##########
load("cmip_prediction.rda")

##################annual mean
yearmean<-data.frame(year=c(2015,seq(2020,2100,by=10)),mean=rep(0,10))
yearmean$mean[1]<-mean(meso2015$Mesobiomass,na.rm=TRUE)
yearmean$mean[2]<-mean(meso2020$Mesobiomass,na.rm=TRUE)
yearmean$mean[3]<-mean(meso2030$Mesobiomass,na.rm=TRUE)
yearmean$mean[4]<-mean(meso2040$Mesobiomass,na.rm=TRUE)
yearmean$mean[5]<-mean(meso2050$Mesobiomass,na.rm=TRUE)
yearmean$mean[6]<-mean(meso2060$Mesobiomass,na.rm=TRUE)
yearmean$mean[7]<-mean(meso2070$Mesobiomass,na.rm=TRUE)
yearmean$mean[8]<-mean(meso2080$Mesobiomass,na.rm=TRUE)
yearmean$mean[9]<-mean(meso2090$Mesobiomass,na.rm=TRUE)
yearmean$mean[10]<-mean(meso2100$Mesobiomass,na.rm=TRUE)
yearmean$Chl[1]<-mean(meso2015$Chl,na.rm=TRUE)
yearmean$Chl[2]<-mean(meso2020$Chl,na.rm=TRUE)
yearmean$Chl[3]<-mean(meso2030$Chl,na.rm=TRUE)
yearmean$Chl[4]<-mean(meso2040$Chl,na.rm=TRUE)
yearmean$Chl[5]<-mean(meso2050$Chl,na.rm=TRUE)
yearmean$Chl[6]<-mean(meso2060$Chl,na.rm=TRUE)
yearmean$Chl[7]<-mean(meso2070$Chl,na.rm=TRUE)
yearmean$Chl[8]<-mean(meso2080$Chl,na.rm=TRUE)
yearmean$Chl[9]<-mean(meso2090$Chl,na.rm=TRUE)
yearmean$Chl[10]<-mean(meso2100$Chl,na.rm=TRUE)

############coastal VS basin
yearmean$coastal[1]<-mean(meso2015[meso2015$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[1]<-mean(meso2015[meso2015$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[2]<-mean(meso2020[meso2020$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[2]<-mean(meso2020[meso2020$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[3]<-mean(meso2030[meso2030$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[3]<-mean(meso2030[meso2030$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[4]<-mean(meso2040[meso2040$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[4]<-mean(meso2040[meso2040$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[5]<-mean(meso2050[meso2050$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[5]<-mean(meso2050[meso2050$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)
yearmean$coastal[6]<-mean(meso2060[meso2060$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[6]<-mean(meso2060[meso2060$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[7]<-mean(meso2070[meso2070$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[7]<-mean(meso2070[meso2070$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[8]<-mean(meso2080[meso2080$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[8]<-mean(meso2080[meso2080$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[9]<-mean(meso2090[meso2090$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[9]<-mean(meso2090[meso2090$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)

yearmean$coastal[10]<-mean(meso2100[meso2100$Bottom_Z<200,]$Mesobiomass,na.rm=TRUE)
yearmean$basin[10]<-mean(meso2100[meso2100$Bottom_Z>200,]$Mesobiomass,na.rm=TRUE)





################## monthly mean
library(dplyr)

#######2015
month2015n<-meso2015%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2015n<-as.data.frame(month2015n)

month2015s<-meso2015%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2015s<-as.data.frame(month2015s)
month2015<-rbind(month2015n,month2015s)

#########2020
month2020n<-meso2020%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2020n<-as.data.frame(month2020n)

month2020s<-meso2020%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2020s<-as.data.frame(month2020s)

#########2030
month2030n<-meso2030%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2030n<-as.data.frame(month2030n)

month2030s<-meso2030%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2030s<-as.data.frame(month2030s)

#########2040
month2040n<-meso2040%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2040n<-as.data.frame(month2040n)

month2040s<-meso2040%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2040s<-as.data.frame(month2040s)

#########2050
month2050n<-meso2050%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2050n<-as.data.frame(month2050n)

month2050s<-meso2050%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2050s<-as.data.frame(month2050s)


#########2060
month2060n<-meso2060%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2060n<-as.data.frame(month2060n)

month2060s<-meso2060%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2060s<-as.data.frame(month2060s)

#########2070
month2070n<-meso2070%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2070n<-as.data.frame(month2070n)

month2070s<-meso2070%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2070s<-as.data.frame(month2070s)

#########2080
month2080n<-meso2080%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2080n<-as.data.frame(month2080n)

month2080s<-meso2080%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2080s<-as.data.frame(month2080s)

#########2090
month2090n<-meso2090%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2090n<-as.data.frame(month2090n)

month2090s<-meso2090%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2090s<-as.data.frame(month2090s)

#########2100
month2100n<-meso2100%>%
  filter(Latitude>0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("North",nrow(.)))
month2100n<-as.data.frame(month2100n)

month2100s<-meso2100%>%
  filter(Latitude<0)%>%
  group_by(month)%>%
  summarize(mean=mean(Mesobiomass,na.rm=TRUE))%>%
  mutate(NS=rep("South",nrow(.)))
month2100s<-as.data.frame(month2100s)

library(RColorBrewer)
mycolour<-brewer.pal(10,"Spectral")

pdf('cmipprediction0909.pdf',width=13,height=12)
#tiff(filename="prediction.tif",width=480,height=800,units="px")
op     <- par(cex.lab=1.5,
              cex.axis=1.3,
              font.lab=2,
              family ="serif",
              mar=c(5,5,3,4),
              oma=c(1,2,2,4),
              layout(matrix(c(1,2,3,3),2,2,byrow=TRUE)))


#########year mean
plot(yearmean$year,yearmean$mean,ylab=expression(Annual~mean~mesozooplankton~biomass~(mgC~m^-2)),
     xlab="Year",mgp=c(2,1,0),pch=18,col="blue",cex=1.5)
a<-lm(yearmean$mean~yearmean$year)
summary(a)
lines(yearmean$year,yearmean$mean,lty=2,lwd=2,col="blue")
par(new=TRUE)
plot(yearmean$year,yearmean$Chl,col="green",pch=15,cex=1.5,yaxt="n",ylab="",xlab="")
axis(4,seq(0.18,0.30,by=0.02))
mtext(expression(Annuanl~mean~Chl~a~(mu~g~L^-1)),side=4,line=2.5,cex=1.3)
lines(yearmean$year,yearmean$Chl,lty=2,lwd=2,col="green")
legend("topright",legend=c("Mesozooplankton","Chl a"),pch=c(18,15),lty=2,col=c("blue","green"))
title("(a)",adj=0,line=0.5,cex=1.5)


plot(month2015n$month,month2015n$mean,ylim=c(200,700),pch=16,cex=1.2,col=mycolour[1],mgp=c(2,1,0),
     ylab=expression(Mesozooplankton~biomass~(mgC~m^-2)),xlab="",xaxt="n")
axis(1,1:12,c("","","","","","","","","","","",""))
labs=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
text(x=seq(1,12),y=150,srt=45,adj=1,cex=1.3,labels=labs,xpd=TRUE)
lines(month2015n$month,month2015n$mean,lty=2,lwd=1.5,col=mycolour[1])
points(month2020n$month,month2020n$mean,pch=16,cex=1.2,col=mycolour[2])
lines(month2020n$month,month2020n$mean,lty=2,lwd=1.5,col=mycolour[2])
points(month2030n$month,month2030n$mean,pch=16,cex=1.2,col=mycolour[3])
lines(month2030n$month,month2030n$mean,lty=2,lwd=1.5,col=mycolour[3])
points(month2040n$month,month2040n$mean,pch=16,cex=1.2,col=mycolour[4])
lines(month2040n$month,month2040n$mean,lty=2,lwd=1.5,col=mycolour[4])
points(month2050n$month,month2050n$mean,pch=16,cex=1.2,col=mycolour[5])
lines(month2050n$month,month2050n$mean,lty=2,lwd=1.5,col=mycolour[5])
points(month2060n$month,month2060n$mean,pch=16,cex=1.2,col=mycolour[6])
lines(month2060n$month,month2060n$mean,lty=2,lwd=1.5,col=mycolour[6])
points(month2070n$month,month2070n$mean,pch=16,cex=1.2,col=mycolour[7])
lines(month2070n$month,month2070n$mean,lty=2,lwd=1.5,col=mycolour[7])
points(month2080n$month,month2080n$mean,pch=16,cex=1.2,col=mycolour[8])
lines(month2080n$month,month2080n$mean,lty=2,lwd=1.5,col=mycolour[8])
points(month2090n$month,month2090n$mean,pch=16,cex=1.2,col=mycolour[9])
lines(month2090n$month,month2090n$mean,lty=2,lwd=1.5,col=mycolour[9])
points(month2100n$month,month2100n$mean,pch=16,cex=1.2,col=mycolour[10])
lines(month2100n$month,month2100n$mean,lty=2,lwd=1.5,col=mycolour[10])
#########ss
points(month2015s$month,month2015s$mean,pch=17,cex=1.2,col=mycolour[1])
lines(month2015s$month,month2015s$mean,lty=3,lwd=1.5,col=mycolour[1])
points(month2020s$month,month2020s$mean,pch=17,cex=1.2,col=mycolour[2])
lines(month2020s$month,month2020s$mean,lty=3,lwd=1.5,col=mycolour[2])
points(month2030s$month,month2030s$mean,pch=17,cex=1.2,col=mycolour[3])
lines(month2030s$month,month2030s$mean,lty=3,lwd=1.5,col=mycolour[3])
points(month2040s$month,month2040s$mean,pch=17,cex=1.2,col=mycolour[4])
lines(month2040s$month,month2040s$mean,lty=3,lwd=1.5,col=mycolour[4])
points(month2050s$month,month2050s$mean,pch=17,cex=1.2,col=mycolour[5])
lines(month2050s$month,month2050s$mean,lty=3,lwd=1.5,col=mycolour[5])
points(month2060s$month,month2060s$mean,pch=17,cex=1.2,col=mycolour[6])
lines(month2060s$month,month2060s$mean,lty=3,lwd=1.5,col=mycolour[6])
points(month2070s$month,month2070s$mean,pch=17,cex=1.2,col=mycolour[7])
lines(month2070s$month,month2070s$mean,lty=3,lwd=1.5,col=mycolour[7])
points(month2080s$month,month2080s$mean,pch=17,cex=1.2,col=mycolour[8])
lines(month2080s$month,month2080s$mean,lty=3,lwd=1.5,col=mycolour[8])
points(month2090s$month,month2090s$mean,pch=17,cex=1.2,col=mycolour[9])
lines(month2090s$month,month2090s$mean,lty=3,lwd=1.5,col=mycolour[9])
points(month2100s$month,month2100s$mean,pch=17,cex=1.2,col=mycolour[10])
lines(month2100s$month,month2100s$mean,lty=3,lwd=1.5,col=mycolour[10])
legend("bottomright",legend=c("2015","2020","2030","2040","2050","2060","2070","2080","2090","2100"),
       lty=1,col=mycolour[1:10],bty="n",inset=-0.001,cex=0.7)
legend("bottomleft",legend=c("Northern Hemisphere","Southern Hemisphere"),pch=c(16,17),lty=c(2,3),
       cex=0.8,bty="n")
title("(b)",adj=0,line=0.5,cex=1.5)



##########difference between 2015 and 2100

load("cmip_prediction.rda")
library(dplyr)
library(raster)
library(terra)
library(graticule)
library(rgdal)
library(rworldmap)


annual2015<-meso2015 %>%
  group_by(Latitude,Longitude)%>%
  summarize(mean2015=mean(Mesobiomass,na.rm=TRUE))
annual2015<-as.data.frame(annual2015)

annual2100<-meso2100 %>%
  group_by(Latitude,Longitude)%>%
  summarize(mean2100=mean(Mesobiomass,na.rm=TRUE))
annual2100<-as.data.frame(annual2100)  

annualdiff<-left_join(annual2015,annual2100,by=c("Longitude","Latitude"))%>%
  mutate(diff=mean2100-mean2015)

df<-annualdiff[,c("Longitude","Latitude","diff")]
#####convert to raster 
dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
crs(dfr)<-CRS("+proj=longlat +datum=WGS84")
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ####Robinson Projection
raster::crs(dfr)
jp<-terra::rast(dfr$diff)
jp<-jp*1# to deal with NAs in this datasaet
new<-terra::project(jp,crs,mask=TRUE)
#####draw the world map
worldmap<-rworldmap::getMap(resolution="coarse")
raster::crs(worldmap)
#######project the vector data into the Robinson projection
worldmap<-sp::spTransform(worldmap,crs)
# Creates latitude and longitude labels and graticules
lat <- c(-90, -60, -30, 0, 30, 60, 90)
long <- c(-180, -120, -60, 0, 60, 120, 180)
labs <- graticule::graticule_labels(lons = long, lats = lat, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lat, proj = crs) # graticules 


mycolor <- colorRampPalette(c("#08306B",  "#2171B5",  "#6BAED6", "#C6DBEF","#DEEBF7","#F7FBFF","#FFFFFF","#FFF5F0",
                              "#FCBBA1"))
plot(new,axes=F,col=mycolor(12),legend=FALSE)
mtext(expression((c)),side=3,cex=1.5,line=-1,adj=0)
plot(worldmap,add=T,col="grey80")
plot(lines,lty=5,lwd=0.5,col="grey",add=TRUE)
text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA) # plots longitude labels
text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA) # plots latitude labels
plot(dfr, legend.only=TRUE, col=mycolor(15),legend.mar=1,  #####cannot plot for new the robinson projection's raster
     legend.width=1, legend.shrink=0.8)

dev.off()







