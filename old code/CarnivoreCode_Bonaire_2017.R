# 3/16/2019 Checked by Robert Boenish, robert.boenish@gmail.com
# 4/26/2017 Carnivore analysis
####Using R version 3.3.2 we loaded packages dplyr and ggplot 2 for our analysis
#set working directory to data location
#setwd("C:/Users/Max/Desktop/Max/School/Coral Reefs/Report")
setwd("C:/Users/rboenish/Documents/Papers/Bonaire/carnivorereport")

library(plyr); library(dplyr)
library(ggplot2)
###Read in 2015 raw observation data and 2017 raw observation data in .csv form
carn15<-read.csv("Carn2015.csv", stringsAsFactors=FALSE,header=TRUE)
fish17<-read.csv("Fish 4.3.2.csv", stringsAsFactors=FALSE,header=TRUE)
#Select carnivores and omnivores, and remove data from one diver as it was deemed to be biased
carn17<-fish17[which(fish17$Functional.Feeding.Group %in% c("Carnivores" , "Omnivores")),]
carn17<- carn17 %>% filter(Diver == "MWW") 
carn17 <- carn17[c(1:12,14:18,13)] # move multiplier ("Number") to end
carn17<- carn17[rep(row.names(carn17), carn17$Number), 1:17] # duplicate rows by "Number" col ..makes data in long version
carn17$Year<-"2017"
carn17$Count=1
carn15<-as.data.frame(carn15)
carn15$Biomass.g.<-as.numeric(carn15$Biomass.g.)
#standardize site names between years
carn15$Site <- revalue(carn15$Site, c("No Dive Reserve"="No Dive", "Backadera"="Barcadera", "18 Palm" = "18th Palm", "Bachelor's Beach" = "Bachelor"))
#give sites levels from north to south
carn15$Site <- factor(carn15$Site, levels = c("Bachelor","Windsock","18th Palm","Calabas","Front Porch","Forest","Reef Scientifico","Barcadera","Oil Slick","Karpata","No Dive"))
carn15 <- carn15 %>% filter(Functional.Feeding.Group %in% c("Carnivores","Omnivores"))
carn15 <- carn15 %>% select(-c(5,9)) # drop unnecessary columns
#standardize column names
colnames(carn17)[colnames(carn17)=="Length..cm."] <- "Length"
colnames(carn17)[colnames(carn17)=="weight..g."] <- "Weight"
colnames(carn15)[colnames(carn15)=="Size..cm."] <- "Length"
colnames(carn15)[colnames(carn15)=="Biomass.g."] <- "Weight"
colnames(carn15)[colnames(carn15)=="Transect.."] <- "Transect.number"
# Elegant way to add management column to the dataframe
FPA<-c("18th Palm", "Calabas", "Front Porch", "Reef Scientifico")
#carn15$Management <- ifelse(carn15$Site %in% FPA,"FPA","Control")
carn15 = carn15[!is.na(carn15$Weight),]
carn15$Year<-"2015"
#transnumcarn15 <- carn15 %>% group_by(Site, Transect.number) %>% summarize() %>% count(Site)%>% group_by(Site)
summary(carn15$Site)
#Merging 2015 and 2017 Carnivore data
data<-merge(carn15,carn17, by.x=c("Year","Site","Family","Species.Name", "Transect.number","a","b","Weight","Length","Common.Name"),by.y=c("Year","Site","Family","Species", "Transect.number","a","b","Weight","Length","Common.name"), all=TRUE)
data<-data[-c(11,14:21)]
data$Management <- ifelse(data$Site %in% FPA,"FPA","Control")
data$Site <- factor(data$Site, levels = c("Bachelor","Windsock","18th Palm","Calabas","Front Porch","Forest","Reef Scientifico","Barcadera","Oil Slick","Karpata","No Dive"))
data$Year<-as.factor(data$Year)
data$Count<-1
#####################################################################Analysis You can run 15_17carn.csv from this point on if you like#####
#### Subset code was left above for future years analysis and data preparation #####
#write compiled data into .csv file only need to run once, nothing bad seems to happen if you run it multiple times though
write.csv(data, file = "15_17carn.csv")
#this part you need to run every time, everything below is necessary, all above is preparation of the compiled data
data2<-read.csv("15_17carn.csv", stringsAsFactors=FALSE,header=TRUE)
data2$Site<-as.factor(data2$Site)
#orients north to south
data2$Site <- factor(data2$Site, levels = c("Bachelor","Windsock","18th Palm","Calabas","Front Porch","Forest","Reef Scientifico","Barcadera","Oil Slick","Karpata","No Dive"))
data2$Weight<-as.numeric(data2$Weight)
data2 = data2[!is.na(data2$Weight),]

#count the number of transects in year year and site, aggregate into a new sheet
transnumdata <- data2 %>% group_by(Year,Site,Transect.number) %>% summarize() %>% count(Year,Site)%>% group_by(Year,Site)
#aggregate mean biomass by year, site and transect in grams/100m^2 divide by 1000 if you want kg
Carn.site <- data2 %>% group_by(Year,Management,Site,Transect.number) %>% summarize (Biomass.g100t = sum(Weight), sd = sd(Weight)) %>% left_join(transnumdata, by=c("Year","Site")) %>% mutate(se = sd/sqrt(n)) 


###### 15_17 code for herbs adapted to carns
#Lets look at just 2015-2017
transnumdata2 <- data2 %>% group_by(Year, Site) %>% summarize() %>% count(Year)%>% group_by(Year)
#compare avreage biomass between years by sites
Carn.site.15_17 <- Carn.site %>% group_by(Year, Site,Management) %>% 
  summarize(g100m = mean(Biomass.g100t/1.2), sd = sd(Biomass.g100t/1.2)) %>% left_join(transnumdata2, by=c("Year")) %>% mutate(se = sd/sqrt(n)) 


#Historical Biomass (g/100m^2) of Predators by site (2015-2017)
ggplot(Carn.site.15_17, aes(x=Site, y = g100m,group=Year,fill=factor(Year),ymin=g100m+se,ymax=g100m-se)) +
  geom_bar(width=0.8,position=position_dodge(width=.9), stat="identity")+
  geom_errorbar(width=0.15 ,position=position_dodge(width=1))+
  ggtitle("Historical Biomass (g/100m^2) of Predators by site (2015-2017)")+
  scale_fill_grey()+ guides(fill=guide_legend(title=NULL))+
  theme_bw()+
  theme(plot.title=element_text(size=16),axis.text.x=element_text(angle=80, hjust=1))+
  scale_y_continuous(name="Biomass (g/100m^2)")

# Biomass by management
transnumMm <- Carn.site.15_17 %>% group_by(Year, Management,Site) %>% summarize() %>% count(Year,Management)
Carn.SiteB.tot <- Carn.site.15_17 %>% group_by(Year,Management) %>% summarize (g_100m = mean(g100m), sd = sd(g100m)) %>% left_join(transnumMm, by=c("Year","Management")) %>% mutate(se = sd/sqrt(n)) 
Carn.SiteB.tot$Year<-as.factor(Carn.SiteB.tot$Year)

# Biomass broken up by managment (Fig 5)
perc<-Carn.SiteB.tot[4,3]/Carn.SiteB.tot[2,3]


ggplot(Carn.SiteB.tot, aes(x=Year, y = g_100m,group=Management,fill=factor(Management),ymin=g_100m+se,ymax=g_100m-se)) +
  geom_bar(width=0.8,position=position_dodge(width=.9), stat="identity")+
  geom_errorbar(width=0.15 ,position=position_dodge(width=1))+
  ggtitle("Historical Biomass (g/100m^2) of Predators by Management (2015-2017)")+
  scale_fill_grey()+ guides(fill=guide_legend(title=NULL))+
  theme_bw()+
  theme(plot.title=element_text(size=16),axis.text.x=element_text(angle=80, hjust=1))+
  scale_y_continuous(name="Biomass (g/100m^2)")

# Subsetting out 2015 and 2017 to see if there is a difference between years in FPA or Control Biomass
Carn.s.b15<-Carn.site.15_17[which(Carn.site.15_17$Year=="2015"),]
Carn.s.b15fpa<-Carn.s.b15[which(Carn.s.b15$Management=="FPA"),]
Carn.s.b15con<-Carn.s.b15[which(Carn.s.b15$Management=="Control"),]
Carn.s.b17<-Carn.site.15_17[which(Carn.site.15_17$Year=="2017"),]
Carn.s.b17fpa<-Carn.s.b17[which(Carn.s.b17$Management=="FPA"),]
Carn.s.b17con<-Carn.s.b17[which(Carn.s.b17$Management=="Control"),]

#Paired t-test Carnivore biomass
# 2015-17 FPAs, p=0.125
wilcox.test(Carn.s.b15fpa$g100m,Carn.s.b17fpa$g100m,paired=TRUE,alternative="less")
# 2015-17 Controls, p=0.07813
wilcox.test(Carn.s.b15con$g100m,Carn.s.b17con$g100m,paired=TRUE,alternative="less")
# 2017-17 FPA vs. Control, p=0.08182, Man-whitney U test
wilcox.test(Carn.s.b17con$g100m,Carn.s.b17fpa$g100m,alternative="less")
# 2015-17 Overall, p=0.02783*****, Man-whitney U test
wilcox.test(Carn.s.b15$g100m,Carn.s.b17$g100m,alternative="less")



#Carnivore Density by year and site number of fish per 100m^2
transnumdata3 <- data2 %>% group_by(Year,Site, Transect.number) %>% summarize() %>% count(Year,Site)%>% group_by(Site)
Carn.SiteD <- data2 %>% group_by(Year,Site,Management,Transect.number) %>% summarize(n100m.t=((sum(Count))/1.2)) # total density (#/100m2)
Carn.SiteD.site <- Carn.SiteD %>% group_by(Year,Site,Management) %>% summarize (n_100m=((mean(n100m.t))), sd = sd(n100m.t)) %>% left_join(transnumdata3, by=c("Site","Year")) %>% mutate(se = sd/sqrt(n))
transnumM4 <- Carn.SiteD.site %>% group_by(Year, Site) %>% summarize() %>% count(Year)%>% group_by(Year)
Carn.SiteD.tot <- Carn.SiteD.site %>% group_by(Year) %>% summarize (n100m = mean(n_100m), sd = sd(n_100m)) %>% left_join(transnumM4, by=c("Year")) %>% mutate(se = sd/sqrt(n)) 

#percent dif in density:
perc_dif<-Carn.SiteD.site[12:22,4]/Carn.SiteD.site[1:11,4]

ggplot(Carn.SiteD.site, aes(x=Site, y = n_100m,group=Year,fill=factor(Year),ymin=n_100m+se,ymax=n_100m-se)) +
  geom_bar(width=0.8,position=position_dodge(width=.9), stat="identity")+
  geom_errorbar(width=0.15 ,position=position_dodge(width=1))+
  ggtitle("Historical Density (#/100m^2) of Predators by site (2015-2017)")+
  scale_fill_grey()+ guides(fill=guide_legend(title=NULL))+
  theme_bw()+
  theme(plot.title=element_text(size=16),axis.text.x=element_text(angle=80, hjust=1))+
  scale_y_continuous(name="Density (#/100m^2)")


#Historical Density (g/100m^2) of Herbivores by Year (2011-2017)
#density plot total from year 2015-2017
ggplot(Carn.SiteD.tot, aes(x=Year, y = n100m, ymin=n100m-se,ymax=n100m+se)) +
  geom_bar( width=1.2,stat="identity")+
  geom_errorbar(width=0.15 ,position="identity")+
  ggtitle("Historical Density (g/100m^2) of Carnivores by Year (2015-2017)")+
  theme_bw()+ 
  scale_x_continuous(name="Year",limits = c(2014,2018),breaks=c(2015,2017))+
  scale_y_continuous(name="Density (#/100m^2)")+
  scale_fill_grey()+
  theme(axis.text.x = element_text ( hjust=1),plot.title=element_text(size=rel(1)))

# stats of annual density
Carn.s.D15<-Carn.SiteD.site[which(Carn.SiteD.site$Year=="2015"),]
Carn.s.D17<-Carn.SiteD.site[which(Carn.SiteD.site$Year=="2017"),]
# 2015-17 Density, p=0.2324
wilcox.test(Carn.s.D15$n_100m,Carn.s.D17$n_100m,paired=TRUE,alternative="less")


#Lets look at just 2015-2017 DENSITY PLOT OF CARNIVORES BY MANAGEMENT
transnumdata4 <- data2 %>% group_by(Year, Site, Management,Transect.number) %>% summarize() %>% count(Year,Management)
T_Carn.15_17_2 <- Carn.SiteD %>% group_by(Year, Site, Management) %>% summarize (n100m = mean(n100m.t), sd = sd(n100m.t)) %>% left_join(transnumdata4, by=c("Management","Year","Site" ))%>% group_by(Year, Management,n100m) %>% mutate(se = sd/sqrt(n)) # both 2015 and 2017 density with standard error by site

T_Carn.manD.tot <- T_Carn.15_17_2 %>% group_by(Year, Management) %>% summarize (n_100m = mean(n100m), sd = sd(n100m)) %>% left_join(transnumdata4, by=c("Year", "Management")) %>% mutate(se = sd/sqrt(n)) 
T_Carn.manD.tot$Year<-as.factor(T_Carn.manD.tot$Year)
T_Carn.manD.tot$Management<-as.factor(T_Carn.manD.tot$Management)
D.2017<-T_Carn.15_17_2[which(T_Carn.15_17_2$Year==2017),]
DFPA<-as.vector(D.2017[c(3,4,5,7),4])
DCONt<-as.vector( D.2017[c(1,2,6,8,9,10,11),4] )

# Subsetting out 2015 and 2017 to see if there is a difference between years in FPA or Control Density
Carn.s.d15<-T_Carn.15_17_2 [which(T_Carn.15_17_2$Year=="2015"),]
Carn.s.d15f<-Carn.s.d15[which(Carn.s.d15$Management=="FPA"),]
Carn.s.d15con<-Carn.s.d15[which(Carn.s.d15$Management=="Control"),]
Carn.s.d17<-T_Carn.15_17_2 [which(T_Carn.15_17_2$Year=="2017"),]
Carn.s.d17f<-Carn.s.d17[which(Carn.s.d17$Management=="FPA"),]
Carn.s.d17con<-Carn.s.d17[which(Carn.s.d17$Management=="Control"),]

#Paired t-test carnivore density
# 2015-17 FPAs, p=0.5625
wilcox.test(Carn.s.d15f$n100m,Carn.s.d17f$n100m,paired=TRUE,alternative="less")
# 2015-17 Controls, p=0.1484
wilcox.test(Carn.s.d15con$n100m,Carn.s.d17con$n100m,paired=TRUE,alternative="less")
# 2017-17 FPA vs. Control, p=0.1576, Man-whitney U test
wilcox.test(Carn.s.d17con$n100m,Carn.s.d17f$n100m,alternative="less")
# 2015-17 Overall, p=0.2772, Man-whitney U test
wilcox.test(Carn.s.d15$n100m,Carn.s.d17$n100m,alternative="less")






