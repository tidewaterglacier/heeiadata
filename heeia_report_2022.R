library(hydrostats)
#library(EcoHydRology)
library(waterData)
library(dataRetrieval)
library(RNCEP)
library(reshape)
#library(weatherData)
library(rnoaa)

#library(plyr)
library(dplyr)
library(ggplot2)
#library(xlsx)
library(spacetime)
#library(Rmisc) #- this forces plyr to load, which doesn't play nice with dplyr
library(scales)
library(lubridate)
# Install
#install.packages("wesanderson")
# Load
library(wesanderson)
library(EnvStats)
library(zoom)
library(colorspace)
library(lunar)
library(gridExtra)

library(rgdal)
library(rasterVis)
library(RColorBrewer)

#plotting
library(ggpubr)
library(colorspace)
#col<-diverge_hcl(4)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

'%!in%' <- function(x,y)!('%in%'(x,y))  

col<-rainbow_hcl(5)

margins<-c(0.1,0.3,0.1,0)

mytheme<-theme(axis.text.x = element_text(angle=-90,hjust=0, size=12),
               axis.text.y = element_text(size=14),
               #plot.margin = unit(c(2, 1, 1, 1), "lines"),
               plot.title = element_text(hjust = 0.5, size=16),
               plot.margin = margin(margins[1], margins[2], margins[3], 
                                    margins[4], "in"),
               panel.grid.major.x = element_line(color = "grey80"),
               panel.grid.major.y = element_line(color="grey80"),
)

mytheme<-theme_light()+
  theme(axis.text.x = element_text(angle=-90,vjust=1, size=12),
        axis.text.x.top = element_text(hjust = 1),
        axis.text.y = element_text(size=14),
        #axis.title.x = element_blank(),
        #plot.margin = unit(c(2, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size=16),
        plot.margin = margin(margins[1], margins[2], margins[3], 
                             margins[4], "in"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color="grey80"))
mytheme<-mytheme+theme(legend.position="right")


#>>>>>>>>>>>>>>>>>>>>>>>
importCSdata <- function(filename,RetOpt="data"){
  if(RetOpt=="info"){
    # bring in entire header of CSI TOA5 data file for metadata
    stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
    return(stn.info)
  } else {
    # second line of header contains variable names
    header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
    # bring in data
    stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
    names(stn.data) <- header
    # add column of R-formatted date/timestamps
    stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
    return(stn.data)}
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

'%!in%' <- function(x,y)!('%in%'(x,y))

# Gauges ------------------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>Gauges
gaugenum<-"16226200"
gaugename<-"Nuuanu"


gaugenum<-"16275000"
gaugename<-"Heeia"

gaugenum<-"16415600"
gaugename<-"Kawela Gulch"

gaugenum<-"16647000"
gaugename<-"Ukumehame Str"


#>>>>>>>>>>>>>>>>>>>>>>>>Import Data

# Import Campbell ---------------------------------------------------------




wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd5<-"Analysis\\May 2023"
setwd(wd5)

wd4<-"Discharge"
setwd(wd4)

#----------CR200 at 'Auwai 1
wd1<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Turbidity_Data_Campbell"
setwd(wd1)

cr200_Auwai1<-importCSdata("Auwai2_Table1.dat", RetOpt="data")
cr200_Auwai1_part1<-importCSdata("CR200Series_Table1_TurbLvlRain.dat", RetOpt="data")
cr200_Auwai1_part2<-importCSdata("CR200Series_Table1_TurbLvlRain_vWindv3.dat", RetOpt="data")

cr200_Auwai1$timedate<-as.POSIXct(cr200_Auwai1$TIMESTAMP)
cr200_Auwai1_part1$timedate<-as.POSIXct(cr200_Auwai1$TIMESTAMP)
cr200_Auwai1_part2$timedate<-as.POSIXct(cr200_Auwai1_part2$TIMESTAMP)

cr200_Auwai1$TIMESTAMP<-NULL
cr200_Auwai1_part1$TIMESTAMP<-NULL
cr200_Auwai1_part2$TIMESTAMP<-NULL
library(plyr)
cr200_Auwai1_comb<-rbind.fill(list(cr200_Auwai1_part1, cr200_Auwai1_part2, 
                              cr200_Auwai1))
cr200_Auwai1$timedate<-as.POSIXct(cr200_Auwai1$timedate)
detach("package:plyr")

write.csv(cr200_Auwai1_comb, paste("cr200_Auwai1", Sys.Date(),  ".csv"))

cr200_Auwai1_comb<-filter(cr200_Auwai1_comb, timedate>"2016-04-01" & timedate<Sys.Date())


#----------------CR300 at wai 2
wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Turbidity_Data_Campbell\\Wai 2"
setwd(wd2)


  cr300_Wai2<-importCSdata("Wai2_Table1.dat", RetOpt="data")
  cr300_Wai2$timedate<-as.POSIXct(cr300_Wai2$TIMESTAMP)
  cr300_Wai2$TIMESTAMP<-NULL
  cr300_Wai2<-filter(cr300_Wai2, timedate>"2018-10-01")
  

#----------------Weather Data from 2/1/2019
  
  wd5<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\PAR_Campbell"
  setwd(wd5)
  
  cr200_Auwai1_part2<-importCSdata("CR200Series_Table1_TurbLvlRain_vWindv3.dat", RetOpt="data")
  write.csv(cr200_Auwai1_part2, "cr200_Auwai1.csv")
  
  cr200_PAR<-importCSdata("Auwai1_Table4_PAR.dat", RetOpt="data")
  write.csv(cr200_PAR, "cr200_Auwai1_PAR.csv")
#_-----------------YSI at Haptuk

# YSI ---------------------------------------------------------------------

  setwd(wd2)
  setwd("Turbidity_YSI")
    ysi_haptuk<-read.csv("haptuk_YSIMasterv2.csv")
  ysi_haptuk$timedate<-as.POSIXct(ysi_haptuk$Date.Time, format="%m/%d/%Y %H:%M", 
                                  origin="1970-01-01", tz="HST")
  #write.csv(ysi_haptuk, "test.csv")
  
  
# Glazer ------------------------------------------------------------------
  #------------Glazer's DS Loi Water Level
wd3<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\River_Makaha_Water_Level_Glazer"
setwd(wd3)

dsloi_wl<-read.csv("Glazer_Node068_Dec2018.csv") 
dsloi_wl$timedate<-as.POSIXct(dsloi_wl$ReceptionTime, origin="1970-01-01")
dsloi_wl$level<-1500-dsloi_wl$d2w

dsloi_wl<-filter(dsloi_wl, timedate>"2018-02-27")
#write.csv(dsloi_wl, "Node068_Dec18.csv")

hihimanu_wl<-read.csv("poh_nodeHihi_Dec18.csv") 
hihimanu_wl$timedate<-as.POSIXct(hihimanu_wl$ReceptionTime, origin="1970-01-01")
hihimanu_wl<-filter(hihimanu_wl, timedate>"2018-02-01")
hihimanu_wl$level<-1500-hihimanu_wl$d2w

#-------------Rain data------------------
wd<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Weather Station"
setwd(wd)
rain_data<-read.csv("ncdc_rain.csv")
rain_data$timedate<-as.POSIXct(strptime(as.character(rain_data$Date), "%m/%d/%Y"))

rain_data$rainmm<-rain_data$Waihee*25.4

rain_data<-filter(rain_data, !is.na(rain_data$Coconut))

wd<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Rainfall"
setwd(wd)

wd4<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Rainfall"
setwd(wd4)
rain_data<-read.csv("PacIOOS_rainfall_HIMB_2017.csv")

#heeia_d is real water quality analysis
# Water Quality DAta -----------------------------------------------
wd<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\"
setwd(wd)
wd7<-"Water quality sampling\\Analysis 2021_v2"
setwd(wd7)
#wqd<-read.csv("wq_R_2013_2019.csv")

#Process nuts by averaging
wqdsarah<-read.csv("together_SWMP_editedv4.csv")
heeiasiteswq <-group_by(wqdsarah, Session, SiteNum, NEW.SITE)
wq_sarah<- summarise(heeiasiteswq, 
                          TP = gm_mean(TP, na.rm=TRUE),
                          Phosphate=gm_mean(Phosphate, na.rm=TRUE),
                          NH4 = gm_mean(Ammonia, na.rm=TRUE),
                          Nitrate = gm_mean(Nitrate, na.rm=TRUE),
                          TN = gm_mean(TN, na.rm=TRUE), 
                          Silica = gm_mean(Silica, na.rm=TRUE), 
                          Chla = gm_mean(Chla, na.rm=TRUE),
                          TSS = gm_mean(TSS, na.rm=TRUE))
write.csv(wq_sarah, "together_averaged_2021v4.csv")

#Use the real dataset
wqd<-read.csv("WaterQualityData_Heeia_2013_2021v4_DOfixed.csv")
write.csv(wqd, "WaterQualityData_Heeia_2013_2021v4.csv")

wqd$Date<-as.POSIXct(strptime(as.character(wqd$Date), "%m/%d/%Y"))
wqd$Time<-as.POSIXct(strptime(as.character(wqd$Time), "%H:%M:%S"))
wqd$Ammonia<-as.numeric(wqd$NH4)
wqd$Phosphate<-as.numeric(wqd$Phosphate)
wqd$Year<-as.numeric(format(wqd$Date, format="%Y"))

heeiasiteswq <-group_by(wqd, Site2)
heeia_geomean<- summarise(heeiasiteswq, 
                        TP = gm_mean(TP, na.rm=TRUE),
                        Phosphate=gm_mean(Phosphate, na.rm=TRUE),
                        NH4 = gm_mean(NH4, na.rm=TRUE),
                        Nitrate = gm_mean(NNN, na.rm=TRUE),
                        TN = gm_mean(TN, na.rm=TRUE), 
                        Silica = gm_mean(Silicate, na.rm=TRUE), 
                        Chla = gm_mean(Chla, na.rm=TRUE),
                        TSS = gm_mean(TSS, na.rm=TRUE),
                        Turb = gm_mean(Turb, na.rm=TRUE),
                        
                        #SiteNum = unique(SiteNum),
                        TSSsd= round(sd(TSS, na.rm=TRUE),2),
                        TPsd= round(sd(TP, na.rm=TRUE),2),
                        Phosphatesd=round(sd(Phosphate, na.rm=TRUE),2),
                        NH4sd = sd(NH4, na.rm=TRUE),
                        Nitratesd = round(sd(NNN, na.rm=TRUE),2),
                        TNsd = round(sd(TN, na.rm=TRUE),2),
                        Silicasd = round(sd(Silicate, na.rm=TRUE),2),
                        Turbsd = round(sd(Turb, na.rm=TRUE), 2),
                        minDate = min(Date, na.rm=TRUE),
                        maxDate = max(Date, na.rm=TRUE),
                        n = n())
#Sd because dataset not clean???
testsd<-NULL
testsd2<-NULL
testsd3<-NULL
testsd4<-NULL
testsd5<-NULL

for(i in 1:12){                  
  #test<-filter(wqd, Site2==levels(wqd$Site2)[i])
  test<-filter(wqd, Site2==wqd$Site2[i])
  #testsd[i]<-sd(test$TN, na.rm=TRUE)
  testsd[i]<-sd(test$TP, na.rm=TRUE)
  testsd4[i]<-sd(test$TSS, na.rm=TRUE)
  testsd5[i]<-sd(test$TP, na.rm=TRUE)
  testsd2[i]<-sd(test$Phosphate, na.rm=TRUE)
  testsd3[i]<-sd(test$TN, na.rm=TRUE)
  
}
heeia_geomean$TPsd<-testsd
heeia_geomean$Phosphatesd<-testsd2
heeia_geomean$TNsd<-testsd3
heeia_geomean$TSSsd<-testsd4




heeia_geomean$SiteName<-c("Papahana", "Alaloa Bridge", 
                          "Auwai_1", "Haptuk", 
                          "DS_Loi_1", "Mangrove Bridge", 
                          "Wetland", "Wai 2", 
                          "Kako'o Bridge", "Stream Mouth", 
                          "Makahanui", "Reef 9")
#heeia_geomean$id<-rev(c(1, 2, 3, 4, 7, 5, 8, 6, 9, 10))
#set the order of the sites
heeia_geomean$id<-rev(c(3, 2, 1, 6, 7, 4, 8, 5, 9, 10, 11, 12))


heeia_sitesnames<-c("Papahana", "Alaloa Bridge", "Haptuk", "Mangrove Bridge", "Wai 2", "'Auwai WWT", "Down Lo'i",  
                    "Kako'o Bridge", "Wetland", "Stream Mouth", "Makahanui")
reorder(heeia_geomean$SiteNum, heeia_geomean$id)


write.csv(heeia_geomean, paste("heeia_geomean", Sys.Date(), ".csv"))

htheme<-theme_classic()+
  theme(axis.text.x = element_text(angle=-90,hjust=0, size=12),
        axis.text.y = element_text(size=14),
        #plot.margin = unit(c(2, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size=16),
        plot.margin = margin(margins[1], margins[2], margins[3], margins[4], "in"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.major.y = element_line(color="grey80"))

p5<- ggplot(heeia_geomean, aes(x=reorder(SiteName,id), y=Nitrate))+
  geom_bar(stat="identity", position="dodge", fill=col[1])+ #, fill=col[1]
  geom_errorbar(aes(ymin=Nitrate, ymax=Nitrate+Nitratesd), width=.2,
                position=position_dodge(.9))+
  coord_flip(ylim = c(0, 300))+
  #ylim(0,500)+
  ylab("ug/L")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+
  geom_hline(yintercept=3.5, linetype="dashed", color = "red")+
  labs(title="Nitrate + Nitrite", size=12)+
  htheme
  

p5

tiff(filename= paste("nitrateHeeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p5
dev.off()

tiff(filename= paste("TPHeeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p6<- ggplot(heeia_geomean, aes(x=reorder(SiteName, id), y=TP))+
  geom_bar(stat="identity", position="dodge", fill=col[4])+
  geom_errorbar(aes(ymin=TP, ymax=TP+TPsd), width=.2,
                position=position_dodge(.9))+
  coord_flip(ylim = c(0, 80))+
  #ylim(0,40)+
  ylab("ug/L")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+
  geom_hline(yintercept=16, linetype="dashed", color = "red")+
  labs(title="Total P", size=12)+
  htheme
p6
dev.off()

tiff(filename= paste("TNheeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p7<- ggplot(heeia_geomean, aes(x=reorder(SiteName, id), y=TN))+
  geom_bar(stat="identity", position="dodge", fill=col[3])+
  geom_errorbar(aes(ymin=TN, ymax=TN+TNsd), width=.2,
                position=position_dodge(.9))+ 
  coord_flip(ylim = c(0, 500))+
  #ylim(0,300)+
  ylab("ug/L")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+#, labels=abbreviate)+
  geom_hline(yintercept=110, linetype="dashed", color = "red")+
  labs(title="Total Nitrogen", size=12)+
  htheme
p7
dev.off()

tiff(filename= paste("TSSheeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p8<- ggplot(heeia_geomean, aes(x=reorder(SiteName, id), y=Turb))+
  geom_bar(stat="identity", position="dodge", fill=col[2])+
  coord_flip(ylim = c(0, 10))+
  #ylim(0,45)+
  geom_errorbar(aes(ymin=Turb, ymax=Turb + Turbsd), width=.2,
                position=position_dodge(.9))+ 
  geom_hline(yintercept=0.2, linetype="dashed", color="red")+
  #geom_point(data=hui_geomean, aes(x=reorder(SiteName, -lat), y=Sal), group=1, color="blue")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+
  labs(title="TSS", y="mg/L", size=12, fontface="bold")+
  htheme
p8
dev.off()

tiff(filename= paste("TSSheeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p8<- ggplot(heeia_geomean, aes(x=reorder(SiteName, id), y=TSS))+
  geom_bar(stat="identity", position="dodge", fill=col[2])+
  coord_flip(ylim = c(0, 20))+
  #ylim(0,45)+
  # geom_errorbar(aes(ymin=Turb, ymax=Turb + Turbsd), width=.2,
  #               position=position_dodge(.9))+ 
  #geom_hline(yintercept=0.2, linetype="dashed", color="red")+
  #geom_point(data=hui_geomean, aes(x=reorder(SiteName, -lat), y=Sal), group=1, color="blue")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+
  labs(title="TSS", y="mg/L", size=12, fontface="bold")+
  htheme
p8
dev.off()

tiff(filename= paste("Silicaheeia_", Sys.Date(), ".tif", sep=""), 
     width = 8, height = 6, units = 'in', res=300)
p9<- ggplot(heeia_geomean, aes(x=reorder(SiteName, id), y=Silica))+
  geom_bar(stat="identity", position="dodge", fill=col[5])+
  coord_flip()+#ylim = c(0, 80))+
  #ylim(0,45)+
  geom_errorbar(aes(ymin=Silica, ymax=Silicasd+Silica), width=.2,
                position=position_dodge(.9))+ 
  #geom_point(data=hui_geomean, aes(x=reorder(SiteName, -lat), y=Sal), group=1, color="blue")+
  scale_x_discrete(name=NULL)+#, labels=rev(heeia_sitesnames))+
  #scale_x_reverse()+
  ylab("ug/L")+
  labs(title="Silica", size=12, fontface="bold")+
  htheme
p9
dev.off()

tiff(filename= paste("barplots_Heeia_", Sys.Date(), ".tif", sep=""), 
     width = 12, height = 5, units = 'in', res=200)
ggarrange(p5, p6+rremove("y.text"), p7+rremove("y.text"), 
          p8+rremove("y.text"), p9+rremove("y.text"),
          #labels = c("A", "B", "C", "D"),
          font.label=list(size=12),
          #heights = c(1, 1),
          widths = c(1.8,1,1,1,1),
          align = "h",
          ncol = 5, nrow = 1)
dev.off()

wq_heeia_slabs<-subset(wqd, Lab %!in% "DOH")

r1<-ggplot(wqdTNC, aes(x=Silicate, y=NNN))+
  geom_point(wqdTNC, mapping=aes(color=Site2, shape=Site2))+
  geom_smooth(method='lm', fill='NA')+
  scale_color_discrete(name="Site")+
  scale_shape_manual(values=0:11)+
  guides(color=guide_legend(ncol=2), shape=guide_legend(ncol=2))+
  htheme
r1
#----------------Vegetation Surveys


# Veg Survey --------------------------------------------------------------

wd<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\Vegetation Surveys\\Analysis"
setwd(wd)
#vegdata<-read.csv("vegtransALL_012520_vR.csv")
vegdata<-read.csv("vegtransALL_1220_vR.csv")
vegpercentlevels<-read.csv("vegpercentlevels 2022.csv")


estsites<-c("S7", "S5", "S4", "S3", "S2", "S1")
kakoosites<-as.factor(c("C1", "C2", "R1", "R2"))

#subset if only estuary--
#vegdata<-subset(vegdata, Transect %in% kakoosites)

vegpercent<- subset(vegdata, Transect %in% kakoosites) %>% count(Year, Transect, PlantAbbrev) %>% group_by(Year, Transect) %>% transmute(PlantAbbrev, Percentage=n/sum(n)*100)
vegcount<-vegdata %>% count(Year, Transect, PlantAbbrev)
vegpercent$count<-vegcount$n

vegpercentbyyear<- subset(vegdata, Transect %in% kakoosites) %>% count(Year, PlantAbbrev) %>% group_by(Year) %>% transmute(PlantAbbrev, Percentage=n/sum(n)*100)
vegcount2<-vegdata %>% count(Year, PlantAbbrev)
vegpercentbyyear$count<-vegcount2$n



plantabbrevlevels<-levels(vegpercent$PlantAbbrev)


#subset for estuary##
#vegpercentlevels<-subset(vegpercentlevels, Wetlandonly==1)

#write.csv(plantabbrevlevels, paste("vegpercentlevels", Sys.Date(), ".csv"))
#write.csv(vegpercent, paste("vegpercent2", Sys.Date(), ".csv"))        
write.csv(vegpercentbyyear, paste("vegpercent2", Sys.Date(), ".csv"))     

#creates a colorteable
colortable<-tibble(PlantAbbrev = vegpercentlevels$plantcode, Color = vegpercentlevels$plantcolor)

colortablekakoo<-subset(colortable, PlantAbbrev %in% levels(factor(vegpercentbyyear$PlantAbbrev)))
#for figures, order the data
#reorders vegpercent to match the colortable order
vegpercentbyyear$PlantAbbrev<-factor(vegpercentbyyear$PlantAbbrev, 
                               levels = as.character(colortablekakoo$PlantAbbrev))
#reorders vegpercent by transect order
vegpercentbyyear$Transect<-factor(vegpercentbyyear$Transect, 
                            #levels = kakoosites)
                            levels = c("C2", "C1", "R3", "R2", "R1", 
                                       "S7", "S6", "S5", "S4", "S3", "S2", "S1"))



margins<-c(0.1,0.3,0.1,0)   
mythemeveg<-theme(axis.text.x = element_text(angle=-60, hjust=0, size=9),
                  axis.text.y = element_text(size=10),
                  #plot.margin = unit(c(2, 1, 1, 1), "lines"),
                  plot.title = element_text(hjust = 0.5, size=12),
                  plot.margin = margin(margins[1], 0, margins[3], 
                                       margins[4], "in"))

tiff(filename= paste("vegbarplot_byyearkakoo", Sys.Date(), ".tif", sep=""), 
     width = 12, height = 6, units = 'in', res=300)

ggplot(vegpercentbyyear) +
  geom_bar(aes(x = Year, y = Percentage, fill = PlantAbbrev), stat="identity") +
  scale_fill_manual(values = as.character(colortablekakoo$Color),
                    drop = TRUE,
                    labels = as.character(vegpercentlevels$plantname),
                                          name = "Plant Type")+
  #scale_fill_brewer(type="seq", palette=2)+
  #facet_grid(~Transect) +
  labs(size=12, y="Percent of Total", x="", #title="Vegetation Change by Transect over Time"
       fontface="bold")+
  mythemeveg
  #theme(axis.text.y=element_blank())
 # scale_fill_discrete(name = "Plant", labels = as.character(vegpercentlevels$plantname))+
  
  
dev.off()





#-----------------Plot everything including USGS data




#rsets----------------
dirr<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia\\RSETS"
setwd(dirr)
drset<-read.csv("RSETS_0321_r.csv")

raov<-aov(year~diff, data=drset)
summary(raov)
plot(raov)

rset_table<-drset %>% group_by(set_id, session) %>%
  summarise(diff = mean(diff, na.rm=TRUE),
            pin_h = mean(pin_height_mm, na.rm=TRUE),
            diffsd = sd(diff, na.rm=TRUE),
            pin_sd = sd(pin_height_mm, na.rm=TRUE))


p5<- ggplot(drset, aes(x=as.factor(year), y=diff, fill=control))+
  #geom_bar(stat="identity", position="dodge", fill=col[1])
  geom_boxplot(outlier.size=1.5,na.rm=TRUE)+
  labs(y="Height difference (mm)", x="Site")+
  scale_fill_discrete(name="Location")+
  mytheme
p5

labelfile<-"heeia_allnuts_vheeia"
tiff(filename= paste(labelfile, Sys.Date(), ".tif", sep=""), 
     width = 10, height = 6, units = 'in', res=200)
s4
dev.off()

pdf(paste(labelfile, Sys.Date(), ".pdf"))
s4
dev.off()



#Alaloa Data --------------
wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd4<-"Hobo_Heeia\\alaloa"
setwd(wd4)

d_alaloa_dis<-read.csv("hobo_processed_1021_alaloa_forR_v3.csv")
ktime<-as.character(d_alaloa_dis$DateTime)
ktimes<-strptime(ktime, "%m/%d/%Y %H:%M")
d_alaloa_dis$TimeDate<-as.POSIXct(ktimes)



#Haptuk Data----------

wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd4<-"Hobo_Heeia\\Haptuk 3b"
setwd(wd4)

d_haptuk_height<-read.csv("Haptuk_master_1021_PressCorrect_R.csv")
ktime<-as.character(d_haptuk_height$DateTime)
ktimes<-strptime(ktime, "%m/%d/%Y %H:%M")
d_haptuk_height$TimeDate<-as.POSIXct(ktimes)

#Air pressure MCBH-----------------
wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd4<-"Hobo_Heeia\\Air"
setwd(wd4)

d_airmcbh<-read.csv("91176022519.csv")
ktime<-as.character(d_airmcbh$DATE)
ktimes<-strptime(ktime, "%Y-%m-%d T %H:%M:%S")
d_airmcbh$TimeDate<-as.POSIXct(ktimes)
d_airmcbh$SLP<-as.character(d_airmcbh$SLP)
d_airmcbh$SLP<-as.numeric(substr(d_airmcbh$SLP, start = 1, stop = 5))/100
d_airmcbh<-d_airmcbh %>% filter(SLP!=999.99)
d_airmcbh$Press<-d_airmcbh$SLP

write.csv(d_airmcbh, "airpressure_mcbh.csv")

#rainfall----------------
# out<-ncdc_datasets(stationid='GHCND:USC00515655', 
#                startdate = "2013-09-03", enddate = "2023-07-01", limit=30,
#                token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt")
# out<-ncdc_stations(stationid="ghcnd:us1HIHN0027",#91176022519", 
#                    token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt")
# d_ncdc<-ncdc_datacats(locationid=c("zip:96744"), datasetid="Precip_15",#91176022519", 
#                    token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt")


# ncdc_stations(#locationid = c('ZIP:96744'),
#           token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt") #QPCP datatype
# 
# out<-ncdc_stations(extent=c(21,-157.72,21.5,-157.80), token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt")
#kaneohe: 21.45045	-157.76794

#https://www.ncdc.noaa.gov/cdo-web/tokendatatypeid="SLP",

# while (theDate <= end)
# {
#   df <- ncdc(
#     datasetid = 'GHCND',
#     stationid = 'GHCND:ASN00009225',
#     token = "token code", 
#     startdate = theDate,
#     enddate = theDate +1,
#     limit = 1000
#   )$data
#   
#   theDate <- theDate + 1
# }  


library(rnoaa)
library(devtools)

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

site_luluku<-"GHCND:USC00515655"#Precip_15, Precip_HLY; PRCP
site_kaneohe08<-"GHCND:US1H1HN0027"
site_mcbh<-"GHCND:USW00022519"
site_coco<-"GHCND:USC00510350"

sd<-as.Date("20220701", format="%Y%m%d", tz="HST")
ed<-as.Date("20230702", format="%Y%m%d", tz="HST")

#Luluku rainfall
d_ncdc<-ncdc(datasetid="GHCND", datatypeid="PRCP", stationid="GHCND:USW00022519", 
                   token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt", limit=1000, add_units=TRUE,
          startdate = sd, enddate = ed)

d_ncdc_luluku<-d_ncdc$data
d_ncdc_luluku$timedate<-as.POSIXct(d_ncdc_luluku$date, format="%Y-%m-%dT%H:%M:%S")
d_ncdc_luluku$month<-month(d_ncdc$timedate)

d_ncdc_mcbh<-d_ncdc$data
d_ncdc_mcbh$timedate<-as.POSIXct(d_ncdc_mcbh$date, format="%Y-%m-%dT%H:%M:%S")
d_ncdc_mcbh$month<-month(d_ncdc_mcbh$timedate)

d_ncdc<-ncdc(locationid = c('ZIP:96744'), datasetid="GHCND", datatypeid="PRCP",
  token="bBGoGMsdHFIfsJMqbYhKyGlKGVdJmNmt", startdate=sd, enddate=ed, limit=1000) #QPCP datatype

d_ncdc<-d_ncdc$data
d_ncdc$station<-as.factor(d_ncdc$station)
d_ncdc$timedate<-as.POSIXct(d_ncdc$date, format="%Y-%m-%dT%H:%M:%S")
d_ncdc$month<-month(d_ncdc$timedate)

plot(d_ncdc_mcbh$timedate, d_ncdc_mcbh$value)

# Wave data ----------------------

wave_station<-51210
URL<-"https://www.ndbc.noaa.gov//data//realtime2//41002.txt"
download.file(URL, destfile<-"data.csv", method="curl")


# Water Data USGS ---------------------------------------------------------

sdate_C<-"2018-01-01"
edate_C<-Sys.Date()

sd<-as.Date("20220701", format="%Y%m%d", tz="HST")
ed<-as.Date("20230702", format="%Y%m%d", tz="HST")

HeeiaInfo <- readNWISsite(gaugenum)
parameterCd <- "00060"

#using dataRetrieval package
#Raw daily data:
#uv is 15 minute
#dv is daily

d_usgs2 <- readNWISuv(gaugenum,parameterCd,sdate_C,edate_C)
d_usgs2$discharge_cfs <- d_usgs2$X_00060_00000
d_usgs2$timedate<-as.POSIXct(d_usgs2$dateTime, format="%Y-%m-%d %H:%M:%S")
d_usgs2$timedate2<-with_tz(d_usgs2$timedate, tzone="US/Hawaii")
d_usgs2$month<-as.integer(strftime(d_usgs2$timedate2,"%m"))
d_usgs2$day<-as.integer((strftime(d_usgs2$timedate2, "%d")))
d_usgs2$year<-as.integer((strftime(d_usgs2$timedate2, "%Y")))
d_usgs2$date<-as.Date(d_usgs2$timedate2)
d_usgs2$wtryr<-wtr_yr(d_usgs2$date, 10)



wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Olowalu Project\\Data"
setwd(wd2)

wd5<-"Stream data"
setwd(wd5)
write.csv(d_usgs_daily, "heeia_daily.csv")
write.csv(d_usgs_yearly, "ukume_yearly.csv")


#Calculate the Q90 for each sampling session-----------
library(lfstat)
library(lubridate)
library(xts)


d_usgs_daily<-d_usgs2 %>% 
  group_by(date) %>%
  summarise(flow=mean(discharge_cfs, na.rm=TRUE),
            dis_min=min(discharge_cfs),
            dis_max=max(discharge_cfs),
            dateTime=min(timedate2),
            day=min(day),
            month=min(month),
            year=min(year))

d_usgs_yearly<-d_usgs2 %>% 
  group_by(wtryr) %>%
  summarize(flow=mean(discharge_cfs, na.rm=TRUE),
            geoflow=gm_mean(discharge_cfs, na.rm=TRUE),
            dis_min=min(discharge_cfs),
            dis_max=max(discharge_cfs),
            dateTime=min(timedate2),
            day=min(day),
            month=min(month),
            year=min(year))

#create an xts class that is simple out of the time series
temp<-as.xts(d_usgs_daily[,c(-1, -3:-8)], order.by=as.POSIXct(d_usgs_daily$date), na.rm=TRUE)

#create an lf object that includes day/month etc
lftemp<-createlfobj(d_usgs_daily[,c(2,6,7,8)], hyearstart=1)
  #temp$flow, hyearstart=1)

somevalues<-d_usgs_daily[,2]
time<-ts(somevalues)
lftemp<-createlfobj(time,startdate = "01/01/2018", hyearstart=1)

#calculate q90 by month
q90usgs<-Q90(lftemp, year = "any", monthly = FALSE, yearly = TRUE,
    breakdays = NULL, na.rm = TRUE)
q10usgs<-Qxx(lftemp, 10, year = "any", monthly = FALSE, yearly = TRUE,
             breakdays = NULL, na.rm = TRUE)

#plot a hydrograph
hydrograph(lftemp)#,sdate_C,edate_C)

#plot baseflow
bfplot(lftemp,
       year = "any",
       col = "green",
       bfcol = "blue",
       ylog = TRUE)

labelfile=paste("usgs_baseflow_", ".jpg")
ggsave(b1, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)


#We calculated flow variability (Q10:Q90) as the relative size of high to low flows. (Richards, 1989) and a baseflow stability index (Q90:Q50) i



#older methods--------------
flowdata2<-importDVs(gaugenum, code="00060", sdate = sdate_C, edate= edate_C)
seddata2<-importDVs(gaugenum, code="80154", sdate = sdate_C, edate= edate_C)
flowdata2$timedate<-as.POSIXct(flowdata2$dates)


sedload<-importDVs(gaugenum, code="80155", sdate = sdate_C, edate= edate_C)
alldata2<-importDVs(gaugenum, sdate = sdate_C, edate= edate_C)
summaryStats(flowdata2)
summaryStats(seddata2)
summaryStats(sedload)
plotParam(flowdata2, logscale=FALSE, metric=FALSE, ylabel="Q (ft3/s)", title=gaugename)
plotParam(seddata2, logscale=FALSE, metric=TRUE, ylabel="Sed con (mg/L)", title=gaugename)
plotParam(sedload, logscale=FALSE, metric=TRUE, ylabel="Sed con (mg/L)", title=gaugename)
plotAnoms(compAnom(alldata2, which=4))
plot(flowdata2$val[1:2649], seddata2$val)

dev.copy(jpeg, filename= paste("Discharge", Sys.Date(), ".jpg", sep=""), 
         width = 10, height = 5, units = 'in', res=200)
dev.off()

dev.copy(jpeg, filename= paste("sedconc", Sys.Date(), ".jpg", sep=""), 
         width=1800, height=1200)
dev.off()

dev.copy(jpeg, filename= paste("sedload", Sys.Date(), ".jpg", sep=""), 
         width=1800, height=1200)
dev.off()

sdate_before<-"2004-09-23"
edate_before<-"2012-01-01"

sdate_after<-"2012-01-02"
edate_after<-"2018-02-01"

write.csv(d_usgs2, paste("usgsgauge", Sys.Date(), ".csv", sep=""))

#>>>>>>>>>>>>>>> Plot USGS Heeia gauge next to Campbell data
par(mar=c(5,4,4,5)+.1)
plot(timedate, cr200_0916$Lvl_m, col="red", type = "l", xlab="", ylab="Height (m)")
axis(2)
par(new=TRUE)
plot(flowdata2$dates, flowdata2$val, type = "l", col="blue")
axis(4)
mtext("Discharge",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Lo'i","USGS gauge"), cex=0.75)


#Report 2021 Hydro--------------------------------
wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd5<-"Analysis\\Dec 2021"
setwd(wd5)

#needs ggmisc
mytheme<-mytheme+theme(legend.position = "right")

g3<- ggplot(d_usgs2, aes(x=timedate, y=discharge_cfs)) +
  geom_point(color="blue")+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  geom_line()+
  xlab("Date")+
  ylab("USGS Ha'iku Gauge Discharge (cfs)")+
  scale_x_datetime(breaks="14 days", labels=date_format("%m/%d/%y"))+
  theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
g3

sitenxi<-"Haiku"
labelfile=paste("discharge", sitenxi, ".jpg")
ggsave(g3, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)

# tiff(filename= paste(labelfile, Sys.Date(), ".tif", sep=""), 
#      width = 8, height = 6, units = 'in', res=200)
# g3
# dev.off()

g4<- ggplot(d_haptuk_height, aes(x=TimeDate, y=Depth)) +
  geom_point(color="indianred3")+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  geom_line(color="indianred3")+
  xlab("Date")+
  ylab("Haptuk (ft)")+
  scale_x_datetime(breaks="14 days", labels=date_format("%m/%d/%y"))+
  theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  theme(legend.position = "right")
g4
sitenxi<-"Haptuk"
labelfile=paste("discharge", sitenxi, Sys.Date(), ".jpg")
ggsave(g4, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)


g5<- ggplot(d_alaloa_dis, aes(x=TimeDate, y=Discharge_calc_ft3s)) +
  geom_point(color="lightblue")+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  geom_line(color="lightblue")+
  xlab("Date")+
  ylab("Discharge (cfs)")+
  scale_x_datetime(breaks="14 days", labels=date_format("%m/%d/%y"))+
  theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  theme(legend.position = "right")
g5
sitenxi<-"Alaloa"
labelfile=paste("discharge", sitenxi, Sys.Date(), ".jpg")
ggsave(g5, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)


g6<- ggplot(d_usgs2, aes(x=timedate, y=discharge_cfs)) +
  geom_point(data=subset(d_haptuk_height, TimeDate>sdate_C), 
             aes(x=TimeDate, y=Depth*200-140, color="indianred3"))+
  geom_point(color="blue")+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  geom_line(aes(color="a"))+
  geom_point(data=d_alaloa_dis, aes(x=TimeDate, y=Discharge_calc_ft3s), color="lightblue")+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  geom_line(data=subset(d_alaloa_dis, TimeDate>sdate_C), 
            aes(x=TimeDate, y=Discharge_calc_ft3s, color="b"))+
   xlab("Date")+
  ylab("Discharge (cfs)")+
  scale_color_manual(values=c("blue", "lightblue", "indianred3"), labels=c("Ha'iku", "Alaloa", "Haptuk"), name="Site")+
  scale_x_datetime(breaks="14 days", labels=date_format("%m/%d/%y"))+
  theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
g6

sitenxi<-"Alaloa_Haptuk"
labelfile=paste("discharge", sitenxi, Sys.Date(), ".jpg")
ggsave(g6, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)


# pdf(paste(labelfile, Sys.Date(), ".pdf"))
# g3
# dev.off()

#Report 2023 Wai2 EXO--------------------------------
wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd5<-"Water Quality sampling"
setwd(wd5)

dwai2<-read.csv("NERR_WAI2_EXO_2019_2023.csv")            

ktimes<-strptime(dwai2$DateTimeStamp, "%m/%d/%Y %H:%M")
dwai2$TimeDate<-as.POSIXct(ktimes)
dwai2$Year2<-as.factor(dwai2$Year)

#exploring the data
qplot(data=dwai2, Depth, Turb)

sdate<-"2019-09-26"
sdate2<-"2022-12-01"
edate2<-"2023-01-10"
sd1<-as.POSIXct("12/01/2022", format="%m/%d/%Y")
ed1<-as.POSIXct("1/10/2023", format="%m/%d/%Y")


#storm21<-subset(dwai2, TimeDate>sd1 & TimeDate<ed1)

#show USGS + Wai 2 turbidity

g6<- ggplot(subset(dwai2, TimeDate>sd1 & TimeDate<ed1), aes(x=TimeDate, y=Turb)) +
  geom_point(color="indianred3", size=1)+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  #geom_line(aes(color="a"))+
  geom_line(data=subset(d_usgs2, timedate>sdate2 & timedate<edate2), 
            aes(x=timedate, y=discharge_cfs), color="blue")+
  xlab("Date")+
  ylab("Turbidity (FNU), Discharge (cfs)")+
  scale_x_datetime(breaks="1 week", labels=date_format("%m/%d/%y"))+
  mytheme+theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
g6

sitenxi<-"Wai2v2"
labelfile=paste("turb_time_Dec2022", sitenxi, Sys.Date(), ".jpg")
ggsave(g6, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)

sdate2<-"2022-12-01"
edate2<-"2023-01-10"
sd1<-as.POSIXct("09/01/2019", format="%m/%d/%Y")
ed1<-as.POSIXct("04/15/2023", format="%m/%d/%Y")


sc1<- ggplot(subset(dwai2, TimeDate>sd1 & TimeDate<ed1), 
             aes(x=Sal, y=Turb, color=Year)) +
  geom_point(color="brown", size=2)+
  #geom_point(data=hok_d, aes(x=DateTime, y=rep(-10, 48)), color="red")+
  #geom_line(aes(color="a"))+
  #xlab("Date")+
  #ylab("Turbidity (FNU), Discharge (cfs)")+
  #scale_x_datetime(breaks="1 week", labels=date_format("%m/%d/%y"))+
  mytheme+theme_light()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
sc1

sitenxi<-"Wai2"
labelfile=paste("do_depth", sitenxi, Sys.Date(), ".jpg")
ggsave(sc1, file=labelfile, width = 10, height = 4, units = c("in"), dpi=300, device=NULL)

dwai2Year<-dwai2 %>% group_by(as.factor(Year)) %>%
  summarise(DO=mean(DO_Pct, NA.rm=TRUE), 
            Turb=mean(Turb, NA.rm=TRUE),
            Sal=mean(Sal, NA.rm=TRUE),
            Temp=mean(Temp, NA.rm=TRUE),
            Depth=mean(Depth, NA.rm=TRUE))


raov<-aov(Turb~Year2, data=dwai2)
summary(raov)
plot(raov)


wd2<-"C:\\Users\\kim.falinski\\Dropbox\\2. TNC\\Heeia Wetland and Fishpond\\Data_Heeia"
setwd(wd2)

wd5<-"Analysis\\May 2023"
setwd(wd5)


# Plotting GGplot----------------------------------------------------------------

startd<-"2019-01-01"
endd<-"2019-04-15"


q<- ggplot(subset(cr200_Auwai1, timedate>startd & timedate<endd), aes(timedate, Turb_SS)) +
  #geom_point(color="coral4", size=0.5)+
  #geom_point(aes(timedate, Turb_BS), color="chocolate", size=0.5)+
  geom_point(aes(timedate, Lvl_m*500), color="darkblue", size=0.5)+
  geom_point(data=subset(ysi_haptuk, timedate>startd & timedate<endd), 
             aes(timedate, (Depth-0.9)*100), color="aquamarine3", size=0.7)+
  geom_line(data=subset(ysi_haptuk, timedate>startd & timedate<endd), 
             aes(timedate, (Depth-0.9)*100), color="aquamarine3", size=0.7)+
  geom_point(data=subset(ysi_haptuk, timedate>startd & timedate<endd), 
             aes(timedate, Turb), color="chocolate", size=0.4)+
  geom_point(data=subset(flow_data, mdate>startd & mdate<endd), 
             aes(as.POSIXct(mdate), flow_cfs*10), color="blue2")+
  geom_line(data=subset(flow_data, mdate>startd & mdate<endd), 
            aes(as.POSIXct(mdate), flow_cfs*10), color="blue2")+
  #geom_point(data=subset(cr300_Wai1, timedate>startd & timedate<endd), aes(timedate, Lvl_m), color="aquamarine3")+
  #geom_point(data=subset(cr300_Wai1, timedate>startd & timedate<endd), aes(timedate, Turb_SS), color="orange")+
  #eom_point(data=subset(hihimanu_wl, timedate>startd & timedate<endd), aes(timedate, level), color="azure4", size=0.1)+
  #geom_point(data=subset(rain_data, timedate>startd & timedate<endd), aes(timedate, rainmm), color="red",size=5)+
  
  xlab("Date")+
  ylab("Turbidity (NTU) and Water Level (mm)")+
  coord_cartesian(ylim=c(0, 1500))+
  theme_light()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))
q

tiff(filename= paste("wetland_discharge_AugOct2018", Sys.Date(), ".tif", sep=""), 
     width = 6, height = 4, units = 'in', res=300)
q
dev.off()


#Statistics-----------------------------
an_turb<-aov(Turb~as.factor(Year), #data=dchla)
             data=subset(wqdTNC, Year>2016 & Site == 8))
summary.aov(an_turb)
TukeyHSD(an_turb)
