library(readxl)

sensordata<- read_excel("sensor_data_combined_risk_v5_final.xlsx")
sensordata$Month[sensordata$Month=="Fall"]<-"Fall - Oct 2023"
sensordata$Month[sensordata$Month=="Spring"]<-"Spring - Apr 2023"
sensordata$Month[sensordata$Month=="Summer"]<-"Summer - Aug 2023"
sensordata$Month[sensordata$Month=="Winter"]<-"Winter - Feb 2024"

sensordata$Month <- factor(sensordata$Month, 
                           levels = c("Spring - Apr 2023", "Summer - Aug 2023", "Fall - Oct 2023", "Winter - Feb 2024"))



#-----------Setting Parameters-------------------------------------------------------------------

C.0<-400/1000000 #assumed ambient CO2 ppm convert to fraction
q<-20 #quantum generation rate per person (quanta/hr), from Iwamura & Tsutsumi 2023
t<-8 #total exposure time (5 min in units of hr)
C.a<-0.038 #Rudnick & Milton (volume fraction of CO2 added to exhaled breath during breathing)

sensordata$avco2<-(as.numeric(sensordata$`Sensor 1 CO2`)+as.numeric(sensordata$`Sensor 2 CO2`))/2
sensordata$f<-(as.numeric(sensordata$avco2)-C.0)/1000000/C.a

dates<-unique(sensordata$Day)
f.dates<-rep(NA,length(dates))
n.dates<-rep(NA,length(dates))
date.type<-rep(NA,length(dates))
season<-rep(NA,length(dates))

for(i in 1:length(dates)){
  f.dates[i]<-sum(sensordata$f[sensordata$Day==dates[i]]*5)/470
  n.dates[i]<-sensordata$n[sensordata$Day==dates[i]][1]
  season[i]<-as.character(sensordata$Month[sensordata$Day==dates[i]][1])
  date.type[i]<-sensordata$`Day Type`[sensordata$Day==dates[i]][1]
}

frame.final<-data.frame(f.dates,n.dates,dates,date.type,season)
frame.final$season<-factor(frame.final$season,levels = c("Spring - Apr 2023", "Summer - Aug 2023", "Fall - Oct 2023", "Winter - Feb 2024"))

#----------------------CASE 1 AIR: Infection Risk assuming 1 Infected Person in Total Occupancy----------------------

I<-1 #number of infectors (assumed)

frame.final$infectionrisk1<-1-exp(-frame.final$f.dates*I*q*t/frame.final$n.date)

#---------------------CASE 2 AIR: Infection Risk assuming P(asymptomatic) x n = I-----------------------------------

P.infect<-0.05
P.asympt<-0.24


frame.final$infectionrisk2<-1-exp(-frame.final$f.dates*(frame.final$n.dates*P.asympt)*q*t/frame.final$n.date)


#---------------------CASE 3 AIR: Infection Risk depending on differences in in-person asymptomatic prob-----------
P.sympt<-0.76
P.inperson.sympt.anchor<-0.75

frame.final$Icase3<-rep(NA,length(dates))
frame.final$Icase3[frame.final$date.type=="Hybrid"]<-(P.asympt)*frame.final$n.dates[frame.final$date.type=="Hybrid"]
frame.final$Icase3[frame.final$date.type=="Anchor"]<-((P.asympt)+(P.inperson.sympt.anchor*P.sympt))*frame.final$n.dates[frame.final$date.type=="Anchor"]
  
frame.final$infectionrisk3<-1-exp(-frame.final$f.dates*(frame.final$Icase3)*q*t/frame.final$n.date)


#---------------------CASE 4 AIR: Infection risk depending on differences in in-person symp or asympt prob--------

P.inperson.sympt.hybrid<-0.05

frame.final$Icase4<-rep(NA,length(dates))
frame.final$Icase4[frame.final$date.type=="Hybrid"]<-((P.asympt)+(P.inperson.sympt.hybrid*P.sympt))*frame.final$n.dates[frame.final$date.type=="Hybrid"]

frame.final$Icase4[frame.final$date.type=="Anchor"]<-((P.asympt)+(P.inperson.sympt.anchor*P.sympt))*frame.final$n.dates[frame.final$date.type=="Anchor"]

frame.final$infectionrisk4<-1-exp(-frame.final$f.dates*(frame.final$Icase4)*q*t/frame.final$n.date)

#---------plotting airborne risks
infection.air<-c(frame.final$infectionrisk1,frame.final$infectionrisk2,frame.final$infectionrisk3,frame.final$infectionrisk4)
date.type<-rep(frame.final$date.type,4)
case<-c(rep("Case 1",length(frame.final$f.dates)),rep("Case 2",length(frame.final$f.dates)),rep("Case 3", length(frame.final$f.dates)),
rep("Case 4",length(frame.final$f.dates)))
season<-rep(frame.final$season,4)
frame.plot.airborne<-data.frame(infect=infection.air,date.type,case,season)

require(ggplot2)
require(ggpubr)

startlunch<-as.POSIXct("1899-12-31 11:30:00")
endlunch<-as.POSIXct("1899-12-31 1:30:00")

windows()
ggplot(frame.plot.airborne)+geom_point(aes(color=season,x=date.type,y=infect))+
  geom_boxplot(aes(color=season,x=date.type,y=infect),alpha=0.2)+
  facet_grid(season~case)+scale_y_continuous(trans="log10",name="Infection Risk")+
  scale_x_discrete(name="")+
  scale_color_discrete(name="")+
  theme_pubr()+
  theme(axis.text=element_text(size=13),axis.title=element_text(size=13),
        legend.position="none",
        strip.text = element_text(size=13),legend.text = element_text(size=13))

ggsave ("figure4.tiff", unit="in", width = 7, height = 9, dpi = 600)


#summary of risk reductions

#Fall
(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])-
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])-
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])-
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Fall - Oct 2023" & frame.plot.airborne$date.type=="Anchor"])*100

#Spring
(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Spring - Apr 2023" & frame.plot.airborne$date.type=="Anchor"])*100

#Winter
(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Winter - Feb 2024" & frame.plot.airborne$date.type=="Anchor"])*100

#Summer
(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 1" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 2" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 3" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])*100

(mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])-
    mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Hybrid"]))/
  mean(frame.plot.airborne$infect[frame.plot.airborne$case=="Case 4" & frame.plot.airborne$season=="Summer - Aug 2023" & frame.plot.airborne$date.type=="Anchor"])*100

#-----------------co2 figure----------------------------------------------

windows()
ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=sensordata$avco2,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name=expression("CO"[2]*phantom(x)*"(ppm)"))+
  scale_color_discrete(name="")+
  theme_pubclean()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0,ymax=1500,alpha=.1,fill="blue")+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13),strip.text = element_text(size=13))

ggsave ("figure2.tiff", unit="in", width = 9, height = 6, dpi = 600)


#Statistical models---------------------------------------------------------

maxs<-rep(NA,length(table(sensordata$Day)))
means<-rep(NA,length(table(sensordata$Day)))
month<-rep(NA,length(table(sensordata$Day)))
ntotal<-rep(NA,length(table(sensordata$Day)))
daytype<-rep(NA,length(table(sensordata$Day)))

days<-unique(sensordata$Day)

for (i in 1:length(maxs)){
  maxs[i]<-max(sensordata$avco2[sensordata$Day==days[i]])
  means[i]<-mean(sensordata$avco2[sensordata$Day==days[i]])
  month[i]<-sensordata$Month[sensordata$Day==days[i]][1]
  ntotal[i]<-sensordata$n[sensordata$Day==days[i]][1]
  daytype[i]<-sensordata$`Day Type`[sensordata$Day==days[i]][1]
}

frame.all<-data.frame(maxs,means,month,ntotal,daytype)
  
sensormodel1<-lm(means~month + ntotal,frame.all)
summary(sensormodel1)

sensormodel2<-lm(maxs~month + ntotal,frame.all)
summary(sensormodel2)

sensormodel3<-lm(means~month + daytype,frame.all)
summary(sensormodel3)

sensormodel4<-lm(maxs~month + daytype,frame.all)
summary(sensormodel4)

sensormodel5<-lm(ntotal~month + daytype,frame.all)
summary(sensormodel5)

