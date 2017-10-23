# - - - - - - - - - - - - - - - - - - - - - - - - 
# Main Zika output
#
# - - - - - - - - - - - - - - - - - - - - - - - - 

# set up source functions
library(magrittr)
library(mgcv)
  
#setwd("~/Documents/Collaboration/zika/microcephaly_Brazil/simulation_model/")

data.births = data.frame(read.csv("../brasil_nejm_fig2.csv", stringsAsFactors = F))

place.names=c("Brazil-Sao_Paulo",
              "Brazil-Rio_Grande_do_Sul","Brazil-Acre","Brazil-Espirito_Santo",
              "Brazil-Parana",
              "Brazil-Goias","Brazil-Pernambuco",
              "Brazil-Rio_de_Janeiro","Brazil-Mato_Grosso","Brazil-Bahia")

epi.names <- c("SaoPaulo",
               "RioGrande","Acre","EspiritoSanto",
               "Parana",
               "GoianiaCity","Pernambuco",
               "RioJaneiro", "MatoGrosso","Bahia")

# - - - - - - - - 

# Convert into individual level data
output_store_data <- function(all=F){

  store.data.pos=NULL
  store.data.neg=NULL
  for(ii in 1:34){
    # the misc columns exclude pregnancy losses
    if(all==T){
      store.data.pos=c(store.data.pos, rep(data.births[ii,"Week"],data.births[ii,"Pos.misc"]) )
      store.data.neg=c(store.data.neg, rep(data.births[ii,"Week"],data.births[ii,"Neg.misc"]) )
    }else{
     store.data.pos=c(store.data.pos, rep(data.births[ii,"Week"],data.births[ii,"Pos"]) )
      store.data.neg=c(store.data.neg, rep(data.births[ii,"Week"],data.births[ii,"Neg"]) )
    }
  }
  
  store.data=rbind(
    cbind(store.data.pos,rep(1,length(store.data.pos))),
    cbind(store.data.neg,rep(0,length(store.data.neg)))
  )
  
  store.data=data.frame(store.data)
  names(store.data)=c("Week","Positive")
  store.data$Positive=as.factor(store.data$Positive)
  
  store.data

}

#store.data=dataM

# - - - - - - - - - 
# Compare 2 models

compare_models <- function(store.data){

  fitmodel1  = glm(Positive ~ Week ,data=store.data,family=binomial) %>% summary() 
  fitmodel2  = glm(Positive ~ 1 ,data=store.data,family=binomial) %>% summary() 
  
  fitmodel1  = gam(Positive ~ s(Week) ,data=store.data,family=binomial)
  #fitmodel1  = glm(Positive ~ Week ,data=store.data,family=binomial) #smooth.spline(x1,y1,w = NULL,2) # GCV
  
  
  par(mar=c(4,4,1,1))
  par(mgp=c(2,0.9,0))
  
  xx=data.births$Week
  yy=data.births$Pos.misc/(data.births$Pos.misc+data.births$Neg.misc )
  
  plot(data.births$Week,yy,xlab="week" , ylab = "risk",ylim=c(0,1))
  lines(data.births$Week,predict(glm.fitmodel,list(Week=data.births$Week),type="response") ,col="blue")

}


# - - - - - - - - 
# LOAD MICROCEPHALY DATA

get.microcephaly <- function(place.name="Brazil-Rio_de_Janeiro"){
  
  setwd("~/Documents/zika-1/Brazil/COES_Microcephaly/data/")
  
  file.names=list.files()
  
  store.microcephaly=NULL
  total.cases=0
  for(ii in 1:length(file.names)){
    data0=data.frame(read.csv(file.names[ii],stringsAsFactors = F))
    data0=data0[!is.na(data0$location),]
    dataPick0=data0[data0$location==place.name & data0$data_field=="microcephaly_confirmed",c("report_date","value","unit")]
    dataPick=dataPick0
    dataPick$value = ifelse(ii==1, NA, as.numeric(dataPick$value) - total.cases)
    total.cases=as.numeric(dataPick0$value)
    dataPick$unit=total.cases
    
    store.microcephaly=rbind(store.microcephaly, dataPick )
    
  }
  
  setwd("~/Documents/zika/microcephaly_Brazil/simulation_model/")
  
  names(store.microcephaly)=c("date","microcephaly","mc_total")
  store.microcephaly
  
}

# LOAD EPI DATA

load_timeseries <- function(epi.name="RioJaneiro"){
  
  data.cases = data.frame(read.csv(paste("../data/Brazil_",epi.name,".csv",sep=""), stringsAsFactors = F))
  data.cases=data.cases[,c("date","total")]
  data.cases[is.na(data.cases$total),"total"]=0
  data.cases$date=as.Date(data.cases$date)
  
  data.cases
  
}

# - - - - - - - - 
# PLOT MICROCEPHALY AND ZIKA AND ESTIMATED INFECTION RISK

plot_micro_zika <- function(place.name="Brazil-Rio_de_Janeiro", epi.name="RioJaneiro",repRate=0.1,micRate=0.1,micBackground=0,y1lim,y2lim){
  
  # place.name="Brazil-Bahia"; epi.name="Bahia"
  # place.name="Brazil-Rio_de_Janeiro"; epi.name="RioJaneiro"; attackR=0.5
  # place.name="Brazil-Espirito_Santo"; epi.name="EspiritoSanto"; repRate=0.15; micRate=4/49; micBackground=0
  
  micro_timeseries <- get.microcephaly(place.name)
  data.cases <- load_timeseries(epi.name)
  
  pop_size = pop_sizes[pop_sizes$location==place.name,"Pop_2010"]
  
  date.range=c("2015-01-01","2017-12-31") %>% as.Date()
  casesZ=data.cases$total
  casesPlot=casesZ #*1e5/pop_size #1e5*casesZ/(6.32e6)
  
  birthCount= pop_size*14/(52*1000)  # Multiply by birth rate 19.3
  microPlot=as.numeric(micro_timeseries$microcephaly)#1e6*as.numeric(micro_timeseries$microcephaly)/pop_size #/(6.32e6)
  microPlotCum=as.numeric(micro_timeseries$mc_total)#1e6*as.numeric(micro_timeseries$microcephaly)/pop_size #/(6.32e6)
  
  plot(data.cases$date, casesPlot, col="black", lwd=2,
       las=1, tck = -.03, mgp=c(3, .5, 0), cex.axis=0.9, bty="l",
       xlim=date.range, xaxs="i", yaxs="i", xlab="", ylab="", type="l",ylim=y1lim) # \n Microcephaly cases/10,000
  mtext(side = 2, "Zika cases", line = 2,cex=0.7)
  title(main=epi.name) 
  

  for(ii in 1:length(microPlot)){
    if(!is.na(microPlot[ii]) & microPlot[ii]>0){
     dateP=as.Date(micro_timeseries[ii,"date"])
      lines(c(dateP,dateP),c(-1,0.05*max(y1lim)),col=rgb(0,0,1,1),lwd=2) #microPlot[ii]/max(microPlot)
    }
  }
  
  # Define baseline AO risk and plot estimates vs Microcephaly cases
  
  baselineM=micBackground; baselineU=micBackground; baselineL=micBackground;
  
  probability.adverse <- output_risk(attack.rate = (sum(casesZ)/repRate)/pop_size, casesZ)
   
  microPlot[is.na(microPlot)]=0 # Remove NAs for plotting
  ylimM=1.01*(c(0,max(max(microPlot),max(birthCount*probability.adverse$riskG2))))
  #ylimM=c(0,1)
  
  par(new=TRUE)
  date.points=min(data.cases$date)+probability.adverse$birthweek*7 - 52*7
  
  adverseMdian = micRate * birthCount*(probability.adverse$riskG+baselineM)
  adverseMed1st = micRate * birthCount*(probability.adverse$risk+baselineM)
  
  #binom.test(7,57,conf.level=0.95)$conf.int[1]
  
  #adverseUpper =  sapply( round(micRate * birthCount*(probability.adverse$riskG2+baselineU) ) ,function(yy){binom.test(x= yy,n=round(birthCount),conf.level=0.95)$conf.int[1] } ) 

 # adverseLower =  sapply( round(micRate * birthCount*(probability.adverse$riskG2+baselineU)  ),function(yy){binom.test(x= yy,n=round(birthCount),conf.level=0.95)$conf.int[1] } ) 
  
  adverseUpper = micRate * birthCount*(probability.adverse$riskG1+baselineL)
  adverseLower = micRate * birthCount*(probability.adverse$riskG2+baselineU)
  
  # PLOT RAW DATA
  
  plot(date.points,adverseMdian, col="red",yaxs="i",xaxs="i", lwd=2,ylim=y2lim, xlim=date.range,xlab="",ylab="",type="l",yaxt="n",xaxt="n")
  polygon(c(date.points,rev(date.points)),c(adverseLower,rev(adverseUpper)),lty=0 ,col=rgb(1,0,0,0.2))
  # Add first trimester and data
  lines(min(data.cases$date)+probability.adverse$birthweek*7-52*7,adverseMed1st,lty=2 ,col="red", lwd=2,ylim=c(0,1), xlim=date.range,xlab="",ylab="",type="l",yaxt="n",xaxt="n")
  #lines(as.Date(micro_timeseries$date),microPlot,col="blue")

  # PLOT CUMULATIVE DATA
  
  #ylimM=ceiling(c(0,max(sum(micRate* birthCount*probability.adverse$riskG2),max(microPlotCum))))

  #plot(date.points,cumsum(adverseMdian), col="red",yaxs="i",xaxs="i", lwd=2,ylim=ylimM, xlim=date.range,xlab="",ylab="",type="l",yaxt="n",xaxt="n")
  #polygon(c(date.points,rev(date.points)),c(cumsum(adverseLower),rev(cumsum(adverseUpper))),lty=0 ,col=rgb(1,0,0,0.2))
  # Add first trimester and data
  #lines(min(data.cases$date)+probability.adverse$birthweek*7-52*7,cumsum(adverseMed1st),lty=2 ,col="red", lwd=2,ylim=c(0,1), xlim=date.range,xlab="",ylab="",type="l",yaxt="n",xaxt="n")
 
  #lines(as.Date(micro_timeseries$date),microPlotCum,col="blue")
  
  
  axis(4,col="red", col.axis="red", las=1, tck = -.03, mgp=c(3, .5, 0), cex.axis=0.9)
  mtext("Number of births at risk of APO", side=4, line=2,col="red",cex=0.7) # Label for 2nd axis
  
  
}


# - - - - - - - - 
# PLOT ESTIMATES ACROSS MULTIPLE REGIONS

plot_multiple_regions <- function(place.names,epi.names,reportA=0.15,micro.prop=(4/49)){
  
  
  #par(mfrow=c(2,5))
  par(mfrow=c(3,3))
  par(las=0)
  par(mar=c(3.5,3.5,1,3.5))
  par(mgp=c(2,0.7,0))
  
  for(ii in 2:length(place.names)){
    
    # Define ranges for each location
    if(ii>1 & ii<=4){y1lim=c(0,3e2); y2lim=c(0,3)}
    if(ii>4 & ii<=7){y1lim=c(0,1.2e3); y2lim=c(0,10)}
    if(ii>7 & ii<=10){y1lim=c(0,7e3); y2lim=c(0,60)}
    if(length(reportA)==1){reportA1=reportA}else{reportA1=reportA[ii]}
    plot_micro_zika(place.name=place.names[ii],epi.name=epi.names[ii],repRate = reportA1,micRate = 1, micBackground = 0,y1lim,y2lim ) #2/10000)
    title(LETTERS[ii],adj=0)
    
  }
  
  dev.copy(pdf,paste("plots/timeseries_report_",round(10*reportA),".pdf",sep=""),width=8,height=7)
  dev.off()
  
  
}


# Adverse event baseline data
#baselineAOt=binom.test(7,57,conf.level=0.95)
#baselineAO=c(as.numeric(baselineAOt$estimate), baselineAOt$conf.int %>% as.numeric())

#glm.fitmodel=glm(y1 ~ x1)

#par(mfrow=c(1,1))


# - - - - - - - - 
# Bootstrap CI

bootstrap.gam <- function(store.data){
  
  # Fit splines - use 1000 bootstraps
  runs=1000
  
  # Do bootstrap CIs
  predict.x=c(6:39) #seq(min(data.births$Week),max(data.births$Week),0.1)
  cc=length(predict.x)
  cd=length(store.data$Week)
  
  #fitmodel1  = gam(Positive ~ s(Week) ,data=store.data,family=binomial) #smooth.spline(x1,y1,w = NULL,2) # GCV
  fitmodel1  = glm(Positive ~ Week ,data=store.data,family=binomial) #smooth.spline(x1,y1,w = NULL,2) # GCV
  predo = predict(fitmodel1,list(Week=predict.x),type="response")
  


  
  bootstrap=matrix(data = 0, nrow = runs, ncol = cc);
  
  for (i1 in c(1:runs)) {
    
    bootstraprandtable=data.frame(matrix(data = 0, nrow = cd, ncol = 2))
    names(bootstraprandtable)=c("Week","Positive")
    
    for (i2 in c(1:cd)) {
      
      randd=sample(1:cd,1)
      
      bootstraprandtable[i2,1]=store.data[randd,"Week"]
      bootstraprandtable[i2,2]=(store.data[randd,"Positive"]==1 %>% as.numeric())
      
    }
    
    bootstraprandtablesorted=bootstraprandtable[order(bootstraprandtable[,1]),]
    
    #bstspl= gam(Positive ~ s(Week) ,data=bootstraprandtablesorted,family=binomial) #smooth.spline(x1,y1,w = NULL,2) # GCV
    bstspl = glm(Positive ~ Week ,data=bootstraprandtablesorted,family=binomial) #smooth.spline(x1,y1,w = NULL,2) # GCV
    
    predobt = predict(bstspl,list(Week=predict.x),type="response")
    
    bootstrap[i1,]  <- predobt %>% as.numeric()
  }
  
  
  sortboots<-bootstrap
  for (i1 in c(1:cc)) {
    aa<-bootstrap[order(bootstrap[,i1]),]
    sortboots[,i1]<-aa[,i1]
  }
  
  minbts<- sortboots[ceiling(0.025*runs),]
  maxbts<- sortboots[ceiling(0.975*runs),]
  rangebts<- sortboots[ceiling(0.975*runs),]-sortboots[ceiling(0.025*runs),]
  
  # PLOT DATA
  
  #plot(data.births$Week,yy,pch=19,col="white",cex=1, ylab = "risk", xlab="week",ylim=c(0,1),xlim=c(0,39))
  
  #lines(predict.x,predo,col=rgb(0,0,1),lwd = 2)
  #polygon(c(predict.x,rev(predict.x)),c(maxbts,rev(minbts)),col=rgb(0,0,1,0.15),lty=0)
  #oints(data.births$Week,yy,pch=19,col="black",cex=0.5)
  #points(store.data$Week,as.numeric(store.data$Positive==1),pch=19,col=rgb(0,0,0,0.15),cex=0.5)
  
  #lines(xx,minbts,col=rgb(0.8,0,0),lwd = 2)
  #grid(lwd = 1) # grid 
  
  gamCI=data.frame(cbind(predict.x,minbts,maxbts))
  
  gamCI
  #write.csv(gamCI,"glmCI.csv")
  
}

# - - - - - - - - 
# OUTPUT INFECTION RISK FROM CASE DATA

output_risk <- function(attack.rate = 0.5, casesZ){
  # - - - - - - - - - 
  # Calculate proportion with Zika-associated adverse events per week

  
  probability.adverse=NULL
  casesZ = c(rep(0,52),casesZ,rep(0,52)) # Add blank year before and after
  maxT=length(casesZ)
  
  for(ii in 1:maxT){ # cohort with pregnancy at week 6 in week ii
    
    probability.inf.ii=NULL
    probability.adverse.ii=NULL
    probability.adverse.iiG=NULL
    probability.adverse.iiG1=NULL
    probability.adverse.iiG2=NULL
    
    for(jj in ii:(ii+39-shift.t)){ # probability of infection after week ii
      
      week.inf=casesZ[jj]/max(1,sum(casesZ)) # Normalise by future weeks could be infected in
      
      probability.inf.ii = c(probability.inf.ii,attack.rate*week.inf)
      
      probability.adverse.jj = attack.rate*week.inf *uniform.risk(jj-ii+shift.t) # Add 6 as this is first data point
      probability.adverse.ii = c(probability.adverse.ii,probability.adverse.jj)
      probability.adverse.jjG = attack.rate*week.inf *gam.risk(jj-ii+shift.t) # Add 6 as this is first data point
      probability.adverse.iiG = c(probability.adverse.iiG,probability.adverse.jjG)
      
      probability.adverse.jjG1 = attack.rate*week.inf*gam.riskL(jj-ii+shift.t) # Add 6 as this is first data point
      probability.adverse.iiG1 = c(probability.adverse.iiG1,probability.adverse.jjG1)
      
      probability.adverse.jjG2 = attack.rate*week.inf*gam.riskU(jj-ii+shift.t) # Add 6 as this is first data point
      probability.adverse.iiG2 = c(probability.adverse.iiG2,probability.adverse.jjG2)
      
    }
    
    probability.adverse = rbind(probability.adverse, c(ii+39-shift.t, sum(probability.adverse.ii),
                                                       sum(probability.adverse.iiG),
                                                       sum(probability.adverse.iiG1),
                                                       sum(probability.adverse.iiG2),
                                                       1-sum(probability.inf.ii)) )
    
  }
  
  probability.adverse = data.frame(probability.adverse)
  probability.adverse[is.na(probability.adverse)]=0
  names(probability.adverse) = c("birthweek","risk","riskG","riskG1","riskG2","inf")
  
  probability.adverse
  
}


# Supplement Ferguson et al Data with CDC Cumulative tallies -  DEPRECATED

augment_data_files <- function(){
  
  par(mfrow=c(2,4))
  par(las=0)
  par(mar=c(3.5,3.5,1,3.5))
  par(mgp=c(2,0.7,0))
  
  
  for(ii in 1:length(epi.names)){
    
    # Load Ferguson et al Data
    data.epi = read.csv(paste("../data_RAW/Brazil_",epi.names[ii],".csv",sep="") )
    
    # Supplement file with CDC data
    file.names=list.files("~/Documents/zika-1/Brazil/Epidemiological_Bulletin/data/")
    
    store.cases=NULL
    total.cases=0
    
    for(jj in 1:length(file.names)){ # Start with second entry

      data0=data.frame(read.csv(paste("~/Documents/zika-1/Brazil/Epidemiological_Bulletin/data/",file.names[jj],sep=""),stringsAsFactors = F))
      data0=data0[!is.na(data0$location),]
      dataPick0=data0[data0$location==place.names[ii] & data0$data_field=="zika_reported",c("report_date","value")]
      dataPick=dataPick0
      dataPick$value = max( as.numeric(dataPick$value) - total.cases , 0 ) # incidence can't be negative
      total.cases=as.numeric(dataPick0$value)
      
      store.cases=rbind(store.cases,dataPick) # This is new dates and cumulative cases
      
    }
    
    store.cases = store.cases[-1,]
    
    dcheck=data.epi$susp
    dcheck=dcheck[!is.na(dcheck)]
    
    plot(as.Date(data.epi$date),data.epi$susp,type="l",xlim=as.Date(c("2015-01-01","2017-01-01")))
    lines(as.Date(store.cases$report_date),store.cases$value,col="blue")
    title(epi.names[ii])
    
  }
  
  dev.copy(pdf,paste("plots/augmented_timeseries.pdf",sep=""),width=15,height=6)
  dev.off()

  
}

# - - - - - - - - 
# RUN SIMULATED SCENARIOS

simulate_zika <- function(simulated.data=NULL, attackRate=0.5, repRate=0.17, axesLabels=TRUE){ # THIS FUNCTION BUILDS INTO plot_simulated_scenarios
  
  # simulated.data=NULL; attackRate=0.5; repRate=0.15 
  pop_size = 1e6
  date.range=c(0:(52*4)) # NOTE THIS IS IN WEEKS
  
  casesPlot = repRate*attackRate*pop_size*simulated.data/sum(simulated.data) #1e5*casesZ/(6.32e6)
  
  birthCount= pop_size/1000  # Convert to births per 1,000
  
  ylim <- 1.05*max(casesPlot)
  # Plot simulated data
  plot(date.range, casesPlot, col="black", lwd=2,
       las=1, tck = -.03, cex.axis=0.9, bty="l", ylim=c(0, ylim),
       xlim=c(min(date.range), max(date.range)), xlab="week", 
       xaxs="i", yaxs="i", ylab="", type="l") 
  
  if(axesLabels) {
    mtext(side = 2, "Zika cases", line=2.5, cex=0.7) # per 100,000
  }
  
  # Define baseline AO risk and plot estimates vs Microcephaly cases
  micBackground = 0
  baselineM=micBackground; baselineU=micBackground; baselineL=micBackground;
  probability.adverse <- output_risk(attack.rate = attackRate, casesPlot)
  
  ylimM=1.01*(c(0,max(birthCount*probability.adverse$riskG2)))
  
  par(new=TRUE)
  
  adverseMdian = birthCount*(probability.adverse$riskG+baselineM)
  adverseMed1st = birthCount*(probability.adverse$risk+baselineM)
  adverseUpper = birthCount*(probability.adverse$riskG1+baselineL)
  adverseLower =  birthCount*(probability.adverse$riskG2+baselineU)
  
  date.points = probability.adverse$birthweek - 52
  
  # PLOT simulated APOs
  date.rangeP = date.points #c(0:(max(date.range)+104))
  
  plot(date.rangeP, adverseMdian, col="red", yaxs="i", xaxs="i", lwd=2, 
       ylim=ylimM, xlim=c(min(date.range),max(date.range)), xlab="", ylab="", 
       type="l", yaxt="n", xaxt="n", bty="n")
  
  polygon(c(date.rangeP, rev(date.rangeP)), c(adverseLower,rev(adverseUpper)), lty=0 , col=rgb(1,0,0,0.2))
  # Add first trimester and data
  lines(date.rangeP, adverseMed1st, lty=2, col="red", lwd=2, ylim=c(0,1), bty="n")
  
  axis(4, col="red", col.axis="red", las=1, tck = -.03, mgp=c(3, .5, 0), cex.axis=0.9)
  if(axesLabels) {
    mtext("Risk of Zika APO per 1000 births", side=4, line=2, col="red",cex=0.7) # Label for 2nd axis
  }
}

# - - - - - - - - 
# PLOT SIMULATIONS ACROSS MULTIPLE SCENARIOS

plot_simulated_scenarios <- function(reportVal=0.17){
  
  par(mfrow=c(3,1))
  par(las=0)
  par(mar=c(3.5,3.5,1,3.5))
  par(mgp=c(2,0.5,0))
  
  date.range=c(0:(52*4)) # NOTE THIS IS IN WEEKS
  
  simulated.data1 = dnorm(x=date.range,mean = 40 ,sd = 5)
  simulated.data2 = dnorm(x=date.range,mean = 40 ,sd = 15) + dnorm(x=date.range,mean = 92 ,sd = 18)
  simulated.data3 = sin(2*pi*(date.range/52-0.25))+1
  simulated.data3[53:104] <- 0
  simulated.data3[157:209] <- 0
  
  simulate_zika(simulated.data1, axesLabels=F,repRate = reportVal); title(LETTERS[1], adj=0)
  simulate_zika(simulated.data2,repRate = reportVal); title(LETTERS[2], adj=0)
  simulate_zika(simulated.data3, axesLabels=F,repRate = reportVal); title(LETTERS[3], adj=0)
  
  dev.copy(pdf, paste("plots/timeseries_sim.pdf",sep=""), width=4, height=4.5)
  dev.off()
  
}

