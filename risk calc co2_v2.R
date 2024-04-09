library(readxl)

sensordata<- read_excel("sensor_data_combined_risk_v5_final.xlsx")

#-----------Setting Parameters-------------------------------------------------------------------

C.0<-400/1000000 #assumed ambient CO2 ppm convert to fraction
q<-20 #quantum generation rate per person (quanta/hr), from Iwamura & Tsutsumi 2023
t<-8 #total exposure time (5 min in units of hr)
C.a<-0.038 #Rudnick & Milton (volume fraction of CO2 added to exhaled breath during breathing)

sensordata$avco2<-mean(as.numeric(sensordata$`Sensor 1 CO2`),as.numeric(sensordata$`Sensor 2 CO2`))
sensordata$f<-(as.numeric(sensordata$avco2)-C.0)/1000000/C.a

dates<-unique(sensordata$Day)
f.dates<-rep(NA,length(dates))
n.dates<-rep(NA,length(dates))
date.type<-rep(NA,length(dates))

for(i in 1:length(dates)){
  f.dates[i]<-sum(sensordata$f[sensordata$Day==f.dates[i]]*5)/470
  n.dates[i]<-sensordata$n[sensordata$Day==f.dates[i]][1]
  date.type[i]<-sensordata$`Day Type`[sensordata$Day==f.dates[i]][1]
}

frame.final<-data.frame(f.dates,n.dates,dates,date.type)

#----------------------CASE 1 AIR: Infection Risk assuming 1 Infected Person in Total Occupancy----------------------

I<-1 #number of infectors (assumed)

frame.final$infectionrisk1<-1-exp(-frame.final$f.dates*I*q*t/frame.final$n.date)

#---------------------CASE 2 AIR: Infection Risk assuming P(asymptomatic) x n = I-----------------------------------

P.asympt<-


frame.final$infectionrisk2<-1-exp(-frame.final$f.dates*(frame.final$n.dates*P.asympt)*q*t/frame.final$n.date)


#---------------------CASE 3 AIR: Infection Risk depending on differences in in-person asymptomatic prob-----------
P.sympt<-
P.inperson.sympt.anchor<-0.75

frame.final$Icase3<-rep(NA,length(dates))
frame.final$Icase3[frame.final$date.type=="Hybrid"]<-(P.inperson.asympt.hybrid*P.asympt)*frame.final$n.dates[frame.final$date.type=="Hybrid"]
frame.final$Icase3[frame.final$date.type=="Anchor"]<-((P.asympt)+(P.inperson.sympt.anchor*P.sympt))*frame.final$n.dates[frame.final$date.type=="Anchor"]
  
frame.final$infectionrisk3<-1-exp(-frame.final$f.dates*(frame.final$n.dates*P.asympt)*q*t/frame.final$n.date)


#---------------------CASE 4 AIR: Infection risk depending on differences in in-person symp or asympt prob--------

P.inperson.sympt.hybrid<-0.05

frame.final$Icase4<-rep(NA,length(dates))
frame.final$Icase4[frame.final$date.type=="Hybrid"]<-((P.asympt)+(P.inperson.sympt.hybrid*P.sympt))*frame.final$n.dates[frame.final$date.type=="Anchor"]

frame.final$Icase4[frame.final$date.type=="Anchor"]<-((P.asympt)+(P.inperson.sympt.anchor*P.sympt))*frame.final$n.dates[frame.final$date.type=="Anchor"]

frame.final$infectionrisk4<-1-exp(-frame.final$f.dates*(frame.final$Icase4)*q*t/frame.final$n.date)



require(ggplot2)
require(ggpubr)

startlunch<-as.POSIXct("1899-12-31 11:30:00")
endlunch<-as.POSIXct("1899-12-31 1:30:00")

sensordata$Month[sensordata$Month=="April"]<-"Spring"
sensordata$Month[sensordata$Month=="August"]<-"Summer"
sensordata$Month[sensordata$Month=="October"]<-"Fall"
sensordata$Month[sensordata$Month=="February"]<-"Winter"

A<-ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=avco2,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name="Infection Risk")+
  theme_pubr()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0.002,ymax=0.007,alpha=.1,fill="blue")+
  ggtitle('A')+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13),strip.text = element_text(size=13),plot.title = element_text(size=13),
        legend.text = element_text(size=13))

summary(sensordata$infectionrisk[sensordata$Month=="April" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="April" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="April" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="April" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk)])


summary(sensordata$infectionrisk[sensordata$Month=="August" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="August" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="August" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="August" & sensordata$`Day Type`=="Hybrid"])

#-----------------Infection Risk assuming same prevalence (ratio of infected person to occupancy)-----------

#assumed prevalence: 20% (1/5)

I<-1
n<-5

#calculating infection risk with Rudnick & Milton equation #8
sensordata$infectionrisk2<-1-exp(-sensordata$f*I*q*t/n)

require(ggplot2)
require(ggpubr)

startlunch<-as.POSIXct("1899-12-31 11:30:00")
endlunch<-as.POSIXct("1899-12-31 1:30:00")

B<-ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=infectionrisk2,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name="Infection Risk")+
  theme_pubr()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0.005,ymax=0.02,alpha=.1,fill="blue")+
  ggtitle('B')+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13),strip.text = element_text(size=13),plot.title = element_text(size=13),legend.text = element_text(size=13))

summary(sensordata$infectionrisk2[sensordata$Month=="April" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="April" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="April" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="April" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])

summary(sensordata$infectionrisk2[sensordata$Month=="August" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="August" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="August" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="August" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])

#summary statistics of f----------------------------------------

summary(sensordata$f[sensordata$Month=="August" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="August" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$f[sensordata$Month=="April" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="April" & sensordata$`Day Type`=="Hybrid"])

#What would CO2 be if we assumed 1 ACH------------------------------
anchor<-1141 #(50 people)
peopleanchor<-50
hybrid<-444 #(3 people)
peoplehybrid<-3

#used: https://forhealth.org/tools/co2-calculator/

#calculating f (from Rudnick & Milton) per time point per sensor
anchorf<-(anchor-C.0)/1000000/C.a
hybridf<-(hybrid-C.0)/1000000/C.a


anchorrisk<-1-exp(-anchorf*I*q*t/peopleanchor)
hybridrisk<-1-exp(-hybridf*I*q*t/peoplehybrid)

anchorrisk-hybridrisk

#Statistical models

maxs<-rep(NA,length(table(sensordata$Day)))
means<-rep(NA,length(table(sensordata$Day)))
month<-rep(NA,length(table(sensordata$Day)))
ntotal<-rep(NA,length(table(sensordata$Day)))
  
days<-unique(sensordata$Day)

for (i in 1:length(maxs)){
  maxs[i]<-max(sensordata$`Sensor 2 CO2`[sensordata$Day==days[i]])
  means[i]<-mean(sensordata$`Sensor 2 CO2`[sensordata$Day==days[i]])
  month[i]<-sensordata$Month[sensordata$Day==days[i]][1]
  ntotal[i]<-sensordata$n[sensordata$Day==days[i]][1]
}

frame.all<-data.frame(maxs,means,month,ntotal)
  
sensormodel1<-lm(means~month + ntotal,frame.all)
summary(sensormodel1)

windows()
ggplot(frame.all,aes(x=month,y=means))+
  geom_point()+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") 


sensormodel2<-lm(maxs~month + ntotal,frame.all)
summary(sensormodel2)

library(readr)
occupancy_data <- read_csv("occupancy data.csv")

occupancymodel<-lm(n~Month + `Day Type`,occupancy_data)
summary(occupancymodel)

#------combined plot

windows()
ggarrange(A,B)
