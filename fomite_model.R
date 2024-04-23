#fomite cases

fomite.function<-function(occupancy,casedesignation,iterations,daytype="Hybrid"){
  
  require(triangle)
  require(truncdist)
  
   #rm(list = ls())
  
    all.saves<<-list()
  
    dose.param<-2.46E-3
  
  for(j in 1:iterations){
    
    microwave.interactions<-occupancy
    
    if(casedesignation==1){
      infected.person<-sample(c(1:occupancy),1)
    }
    
    if(daytype=="Hybrid"){
      case2threshold<-(0.05*0.24)
      case3threshold<-(0.05*0.24)+(0.05*0.76*0.05)
    }else{
      case2threshold<-(0.05*0.24)+(0.05*0.76*0.75)
      case3threshold<-(0.05*0.24)+(0.05*0.76*0.75)
    }
    
    for(m in 1:occupancy){
      all.events<-c("start","microwave","food container","microwave","keypad","microwave","food container","microwave","mouth")
      
      TE.surfhand<-rep(NA,length(all.events))
      TE.handsurf<-rep(NA,length(all.events))
      TE.handmouth<-rep(NA,length(all.events))
      A.hand<-rep(NA,length(all.events))
      A.surf<-rep(NA,length(all.events))
      S.H<-rep(NA,length(all.events))
      S.M<-rep(NA,length(all.events))
      C.hand<-rep(NA,length(all.events))
      C.microwave<-rep(NA,length(all.events))
      C.keypad<-rep(NA,length(all.events))
      C.food<-rep(NA,length(all.events))
      Dose<-rep(NA,length(all.events))
      Risk<-rep(NA,length(all.events))
      
      PersonID<-rep(NA,length(all.events))
      
      frame.fomite<-data.frame(all.events,TE.surfhand,TE.handsurf,TE.handmouth,
                               A.hand,A.surf,S.H,S.M,C.hand,C.microwave,C.keypad,C.food,PersonID,Dose,Risk)
      
      frame.fomite$S.H[frame.fomite$all.events=="microwave"]<-runif(length(frame.fomite$S.H[frame.fomite$all.events=="microwave"]),min=0.10,max=0.21)
      frame.fomite$S.H[frame.fomite$all.events=="keypad"]<-runif(length(frame.fomite$S.H[frame.fomite$all.events=="keypad"]),min=0.008,max=0.012)
      frame.fomite$S.H[frame.fomite$all.events=="food container"]<-runif(length(frame.fomite$S.H[frame.fomite$all.events=="food container"]),min=0.04,max=0.21)
      
      frame.fomite$S.M[frame.fomite$all.events=="mouth"]<-runif(length(frame.fomite$S.M  [frame.fomite$all.events=="mouth"]),min=0.008,max=0.012)
      
      frame.fomite$A.surf[frame.fomite$all.events=="microwave"]<-112
      frame.fomite$A.surf[frame.fomite$all.events=="keypad"]<-150
      frame.fomite$A.surf[frame.fomite$all.events=="food container"]<-608
      
      frame.fomite$TE.surfhand[frame.fomite$all.events=="microwave"|
                                 frame.fomite$all.events=="keypad"|
                                 frame.fomite$all.events=="food container"]<-
        rtrunc(length(frame.fomite$TE.surfhand[frame.fomite$all.events=="microwave"|
                                                 frame.fomite$all.events=="keypad"|
                                                 frame.fomite$all.events=="food container"]),
               "norm",mean=0.28,sd=0.23,a=0,b=1)
      
      frame.fomite$TE.handsurf[frame.fomite$all.events=="microwave"|
                                 frame.fomite$all.events=="keypad"|
                                 frame.fomite$all.events=="food container"]<-
        rtrunc(length(frame.fomite$TE.surfhand[frame.fomite$all.events=="microwave"|
                                                 frame.fomite$all.events=="keypad"|
                                                 frame.fomite$all.events=="food container"]),
               "norm",mean=0.17,sd=0.19,a=0,b=1)
      
      frame.fomite$TE.handmouth[frame.fomite$all.events=="mouth"]<-
        rtrunc(length(frame.fomite$TE.surfhand[frame.fomite$all.events=="mouth"]),
               "norm",mean=0.339,sd=0.1318,a=0,b=1)
      
      frame.fomite$A.hand<-runif(1,min=445,max=535)
      
      #looping through events
      #start
      
      #food containers uncontaminated for all
      frame.fomite$C.food[1]<-as.numeric(0)
      
      if(casedesignation==1){
        #Concentration on hands informed by whether they're infected or not
        if(infected.person!=m){
          frame.fomite$C.hand[1]<-as.numeric(0)
        }else{
          frame.fomite$C.hand[1]<-rtriangle(1,a=3E1,b=8.6E7,c=8.2E1)*(1/runif(1,min=100,max=1000))/frame.fomite$A.hand[1]
        }
      }else if (casedesignation==2){
        infected<-runif(1,0,1)
        if(infected<=case2threshold){
          frame.fomite$C.hand[1]<-rtriangle(1,a=3E1,b=8.6E7,c=8.2E1)*(1/runif(1,min=100,max=1000))/frame.fomite$A.hand[1]
        }else{
          frame.fomite$C.hand[1]<-as.numeric(0)
        }
      }else{
        infected<-runif(1,0,1)
        if(infected<=case3threshold){
          frame.fomite$C.hand[1]<-rtriangle(1,a=3E1,b=8.6E7,c=8.2E1)*(1/runif(1,min=100,max=1000))/frame.fomite$A.hand[1]
        }else{
          frame.fomite$C.hand[1]<-as.numeric(0)
        }
      }
    
      
      #Concentration on microwave and keypad depends on previous interactions, unless this is "person 1"
      if(m==1){
        frame.fomite$C.microwave[1]<-as.numeric(0)
        frame.fomite$C.keypad[1]<-as.numeric(0)
      }else{
        frame.fomite$C.microwave[1]<-end.C.microwave
        frame.fomite$C.keypad[1]<-end.C.keypad
      }
      
      
      for (k in 2:length(all.events)){
          
          #changes in conc. based on current event in sequence
          if(all.events[k]=="microwave"){
            
            frame.fomite$C.microwave[k]<-frame.fomite$C.microwave[k-1]+frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1])-
              (frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*(frame.fomite$A.hand[k]/frame.fomite$A.surf[k])*frame.fomite$C.microwave[k-1])
            
            frame.fomite$C.hand[k]<-frame.fomite$C.hand[k-1]+(frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*frame.fomite$C.microwave[k-1])-
              (frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1]))
            
            frame.fomite$C.keypad[k]<-frame.fomite$C.keypad[k-1]
            
            frame.fomite$C.food[k]<-frame.fomite$C.food[k-1]
            
          }else if (all.events[k]=="keypad"){
            
            frame.fomite$C.keypad[k]<-frame.fomite$C.keypad[k-1]+(frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1]))-
              (frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*(frame.fomite$A.hand[k]/frame.fomite$A.surf[k])*frame.fomite$C.keypad[k-1])
            
            frame.fomite$C.hand[k]<-frame.fomite$C.hand[k-1]+(frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*frame.fomite$C.keypad[k-1])-
              (frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1]))
            
            frame.fomite$C.microwave[k]<-frame.fomite$C.microwave[k-1]
            
            frame.fomite$C.food[k]<-frame.fomite$C.food[k-1]
            
          }else if (all.events[k]=="food container"){
            frame.fomite$C.food[k]<-frame.fomite$C.food[k-1]+(frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1]))-
              (frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*(frame.fomite$A.hand[k]/frame.fomite$A.surf[k])*frame.fomite$C.food[k-1])
            
            frame.fomite$C.hand[k]<-frame.fomite$C.hand[k-1]+(frame.fomite$TE.surfhand[k]*frame.fomite$S.H[k]*frame.fomite$C.food[k-1])-
              (frame.fomite$TE.handsurf[k]*frame.fomite$S.H[k]*as.numeric(frame.fomite$C.hand[k-1]))
            
            frame.fomite$C.microwave[k]<-frame.fomite$C.microwave[k-1]
            
            frame.fomite$C.keypad[k]<-frame.fomite$C.keypad[k-1]
            
          }else{
            #hand to mouth contact
            if(m!=infected.person){
              frame.fomite$Dose[k]<-as.numeric(frame.fomite$C.hand[k-1])*frame.fomite$S.M[k]*frame.fomite$TE.handmouth[k]*frame.fomite$A.hand[k]
              frame.fomite$Risk[k]<-1-exp(-frame.fomite$Dose[k]*dose.param)
              
            }

          }
        
      } #end of events loop
      
      frame.fomite$PersonID<-m
      end.C.microwave<-frame.fomite$C.microwave[length(all.events)-1]
      end.C.keypad<-frame.fomite$C.keypad[length(all.events)-1]
      
      if(m==1){
        master.frame<-frame.fomite
      }else{
        master.frame<-rbind(master.frame,frame.fomite)
      }
    } #occupancy loop

    all.saves[[j]]<<-master.frame
  
  } #end of j loop (iterations)
  
} #end of function definition