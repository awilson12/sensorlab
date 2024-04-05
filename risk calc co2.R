library(readxl)

sensordata<- read_excel("sensor_data_combined_risk_v5_final.xlsx")

#-----------Setting Parameters-------------------------------------------------------------------

C.0<-400/1000000 #assumed ambient CO2 ppm convert to fraction
I<-1 #number of infectors (assumed)
q<-20 #quantum generation rate per person (quanta/hr), from Iwamura & Tsutsumi 2023
t<-5/60 #total exposure time (5 min in units of hr)
C.a<-0.038 #Rudnick & Milton (volume fraction of CO2 added to exhaled breath during breathing)

#calculating f (from Rudnick & Milton) per time point per sensor
sensordata$f.1<-(as.numeric(sensordata$`Sensor 1 CO2`)-C.0)/1000000/C.a
sensordata$f.2<-(as.numeric(sensordata$`Sensor 2 CO2`)-C.0)/1000000/C.a

#averaging f across the two sensors with equation #3
sensordata$f<-sensordata$f.1+sensordata$f.2/2

#----------------------Infection Risk assuming 1 Infected Person in Total Occupancy----------------------

#calculating infection risk with Rudnick & Milton equation #8
sensordata$infectionrisk<-1-exp(-sensordata$f*I*q*t/sensordata$n)

require(ggplot2)
require(ggpubr)

startlunch<-as.POSIXct("1899-12-31 11:30:00")
endlunch<-as.POSIXct("1899-12-31 1:30:00")

ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=infectionrisk,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name="Infection Risk")+
  theme_pubr()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0.002,ymax=0.007,alpha=.1,fill="blue")+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13))

summary(sensordata$infectionrisk[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk)])


summary(sensordata$infectionrisk[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$infectionrisk[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$infectionrisk[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])

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

ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=infectionrisk2,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name="Infection Risk")+
  theme_pubr()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0.005,ymax=0.02,alpha=.1,fill="blue")+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13))

summary(sensordata$infectionrisk2[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])

summary(sensordata$infectionrisk2[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])

summary(sensordata$infectionrisk2[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])

summary(sensordata$infectionrisk2[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$infectionrisk2[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$infectionrisk2[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$infectionrisk2[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid" & !is.na(sensordata$infectionrisk2)])


#summary statistics of f----------------------------------------

summary(sensordata$f[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$f[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$f[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$f[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
summary(sensordata$f[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])

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
occupancy_data <- read_csv("occupancy data.csv") #NEED TO UPDATE WITH OCT/FEB DATA

occupancymodel<-lm(n~Month + `Day Type`,occupancy_data)
summary(occupancymodel)
