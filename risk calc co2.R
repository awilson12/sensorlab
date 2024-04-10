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

summary(sensordata$f)

timeweighted.f<-rep(NA,(length(table(sensordata$Day))))
occupancy<-rep(NA,(length(table(sensordata$Day))))

days<-unique(sensordata$Day)
type<-c(rep(c("Hybrid", "Anchor", "Anchor", "Hybrid"),1),rep(c("Hybrid","Anchor","Anchor","Hybrid","Hybrid"),3))
season<-c(rep("Spring",4),rep("Summer",5),rep("Fall",5),rep("Winter",5))

for(i in 1:length(table(sensordata$Day))){
  timeweighted.f[i]<-sum(sensordata$f[sensordata$Day==days[i]]*5)/470
  occupancy[i]<-sensordata$n[sensordata$Day==days[i]][1]
}

frame.weighted<-data.frame(timeweighted.f,days,type,season,occupancy)

#Assuming 1 person in occupancy is infected
frame.weighted$infectionrisk<-1-exp(-frame.weighted$timeweighted.f*I*q*t/frame.weighted$occupancy)

A<-ggplot(frame.weighted)+geom_boxplot(aes(x=type,y=infectionrisk,fill=season))+
  scale_y_continuous(name="Case 1 Infection Risk")+
  scale_x_discrete(name="")+
  scale_fill_discrete(name="")+
  facet_wrap(~season)+
  theme_pubclean()+
  ggtitle('A')+
  theme(axis.text = element_text(size=13),strip.text = element_text(size=13),
        axis.title = element_text(size=13),legend.text=element_text(size=13))

#calculating infection risk with Rudnick & Milton equation #8
#sensordata$infectionrisk<-1-exp(-sensordata$f*I*q*t/sensordata$n)

require(ggplot2)
require(ggpubr)

startlunch<-as.POSIXct("1899-12-31 11:30:00")
endlunch<-as.POSIXct("1899-12-31 1:30:00")


#------------------------------------figure 2

windows()
ggplot(data=sensordata[sensordata$`Day Type`!="None",],aes(x=Time,y=sensordata$co2average,group=Day,color=`Day Type`))+geom_point(size=2)+geom_line(size=1)+
  facet_wrap(~Month)+
  scale_x_datetime(name="Time")+
  scale_y_continuous(name=expression("CO"[2]*phantom(x)*"(ppm)"))+
  scale_color_discrete(name="")+
  theme_pubclean()+
  annotate("rect",xmin=sensordata$Time[29],xmax=sensordata$Time[53],ymin=0,ymax=1500,alpha=.1,fill="blue")+
  theme(axis.text.x=element_text(angle=90),axis.title = element_text(size=13),
        axis.text=element_text(size=13),strip.text = element_text(size=13))


#--------------Table 1---------------------------------------------------

library(Metrics)
rmse(sensordata$`Sensor 1 CO2`,sensordata$`Sensor 2 CO2`)

sensordata$co2average<-(sensordata$`Sensor 1 CO2`+sensordata$`Sensor 2 CO2`)/2

summary(sensordata$co2average[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$co2average[sensordata$Month=="Spring" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$co2average[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$co2average[sensordata$Month=="Spring" & sensordata$`Day Type`=="Anchor"])


summary(sensordata$co2average[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$co2average[sensordata$Month=="Summer" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$co2average[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$co2average[sensordata$Month=="Summer" & sensordata$`Day Type`=="Anchor"])


summary(sensordata$co2average[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$co2average[sensordata$Month=="Fall" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$co2average[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$co2average[sensordata$Month=="Fall" & sensordata$`Day Type`=="Anchor"])



summary(sensordata$co2average[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])
sd(sensordata$co2average[sensordata$Month=="Winter" & sensordata$`Day Type`=="Hybrid"])

summary(sensordata$co2average[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])
sd(sensordata$co2average[sensordata$Month=="Winter" & sensordata$`Day Type`=="Anchor"])


#assumed prevalence: 20% (1/5)---------------------------------------------------------------

I<-1
n<-5

frame.weighted$infectionrisk2<-1-exp(-frame.weighted$timeweighted.f*I*q*t/n)

B<-ggplot(frame.weighted)+geom_boxplot(aes(x=type,y=infectionrisk2,fill=season))+
  scale_y_continuous(name="Case 2 Infection Risk")+ 
  scale_x_discrete(name="")+
  scale_fill_discrete(name="")+
  facet_wrap(~season)+
  theme_pubclean()+
  ggtitle('B')+
  theme(axis.text = element_text(size=13),strip.text = element_text(size=13),
        axis.title = element_text(size=13),legend.text=element_text(size=13))

#Figure 3
windows()
ggarrange(A,B,common.legend = TRUE,ncol=1)


#Statistical models----------------------------------------------------

maxs<-rep(NA,length(table(sensordata$Day)))
means<-rep(NA,length(table(sensordata$Day)))
month<-rep(NA,length(table(sensordata$Day)))
ntotal<-rep(NA,length(table(sensordata$Day)))
  
days<-unique(sensordata$Day)

for (i in 1:length(maxs)){
  maxs[i]<-max(sensordata$co2average[sensordata$Day==days[i]])
  means[i]<-mean(sensordata$co2average[sensordata$Day==days[i]])
  month[i]<-sensordata$Month[sensordata$Day==days[i]][1]
  ntotal[i]<-sensordata$n[sensordata$Day==days[i]][1]
}

frame.all<-data.frame(maxs,means,month,ntotal)

#model 2  
sensormodel1<-lm(means~month + ntotal,frame.all)
summary(sensormodel1)

#windows()
#ggplot(frame.all,aes(x=month,y=means))+
#  geom_point()+
#  stat_smooth(method = "lm", 
#              formula = y ~ x, 
#              geom = "smooth") 


sensormodel2<-lm(maxs~month + ntotal,frame.all)
summary(sensormodel2)

library(readr)
occupancy_data <- read_csv("occupancy data.csv") #NEED TO UPDATE WITH OCT/FEB DATA

occupancymodel<-lm(n~Month + `Day Type`,occupancy_data)
summary(occupancymodel)
