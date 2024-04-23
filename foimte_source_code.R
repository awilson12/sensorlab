source("fomite_model.R")
days<-unique(sensordata$Day)


#Case 1
all.case.1<-list()
for(y in 1:length(days)){
  fomite.function(occupancy=frame.final$n.dates[frame.final$dates==days[y]],
                  1,
                  iterations=10000,
                  daytype=frame.final$date.type[frame.final$dates==days[y]])
  all.case.1[[y]]<-all.saves
}



for(j in 1:length(days)){
  for(i in 1:10000){
    if (i==1 & j==1){
      risk<-mean(all.case.1[[j]][[i]]$Risk[!is.na(all.case.1[[j]][[i]]$Risk)])
      dayrecord<-days[j]
      }else{
      risktemp<-mean(all.case.1[[j]][[i]]$Risk[!is.na(all.case.1[[1]][[i]]$Risk)])
      risk<-c(risk,risktemp)
      dayrecord<-c(dayrecord,days[j],length(risktemp))
    }
  }
  
}


data.1<-all.case.1[[1]][[1]]$Risk[!is.na(all.case.1[[1]][[1]]$Risk)]

ggplot(data.1)+geom_line(aes(x=data.1$all.events,y=data.1$C.hand,group=data.1$PersonID),alpha=0.2)+
  scale_y_continuous(trans="log10")


#Case 2
all.case.2<-list()
for(y in 1:length(days)){
  fomite.function(occupancy=frame.final$n.dates[frame.final$dates==days[y]],
                  2,
                  iterations=10000,
                  daytype=frame.final$date.type[frame.final$dates==days[y]])
  all.case.2[[y]]<-all.saves
}

for(i in 1:100){
  if(i==1){
    infectionrisks<-c(all.case.2[[2]][[i]]$Risk)
  }else{
    infectionrisktemp<-c(all.case.2[[2]][[i]]$Risk)
    infectionrisks<-c(infectionrisks,infectionrisktemp)
  }
}

summary(infectionrisks)

#Case 3
all.case.3<-list()
for(y in 1:length(days)){
  fomite.function(occupancy=frame.final$n.dates[frame.final$dates==days[y]],
                  3,
                  iterations=10000,
                  daytype=frame.final$date.type[frame.final$dates==days[y]])
  all.case.3[[y]]<-all.saves
}
