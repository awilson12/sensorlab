library(readxl)

sensordata<- read_excel("sensor_data_combined_risk_v3.xlsx")

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

#-----------------Infection Risk assuming same prevalence (ratio of infected person to occupancy)-----------

#assumed prevalence: 20% (1/5)

I<-1
n<-5

sensordata$infectionrisk2<-1-exp(-sensordata$f*I*q*t/n)

#calculating infectin risk with Rudnick & Milton equation #8
sensordata$infectionrisk<-1-exp(-sensordata$f*I*q*t/sensordata$n)

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