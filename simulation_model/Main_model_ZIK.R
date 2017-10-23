# - - - - - - - - - - - - - - - - - - - - - - - - 
# Main Zika output
#
# - - - - - - - - - - - - - - - - - - - - - - - - 

library(deSolve)
library(magrittr)
  
rm(list=ls())

# Set directories and load data

setwd("~/Documents/zika/microcephaly_Brazil/simulation_model/")
#setwd("~/Documents/Collaboration/zika/microcephaly_Brazil/simulation_model/")

source("data_functions.R")


pop_sizes = read.csv("../data/population_sizes.csv",stringsAsFactors = F)

# Load GAM
# Linear model function
store.data <- output_store_data()

gamCI <- bootstrap.gam(store.data)
glm.fitmodel = glm(Positive ~ Week ,data=store.data,family=binomial)
gam.risk <- function(x){predict(glm.fitmodel,list(Week=x),type="response")} # NEED TO LOAD GAM FIRST!
gam.riskL <- function(x){gamCI[x==gamCI$predict.x,"minbts"]} # NEED TO LOAD GAM FIRST!
gam.riskU <- function(x){gamCI[x==gamCI$predict.x,"maxbts"]} # NEED TO LOAD GAM FIRST!


# Uniform in 1st trimester
risk.P = predict(glm.fitmodel,list(Week=6),type="response")
uniform.risk <- function(x,risk=risk.P){ifelse(x<=13,risk,0)}


# Load GAM without 1st trimester miscarriages
# Linear model function
store.dataT <- output_store_data(all = T)

gamCIT <- bootstrap.gam(store.dataT)
glm.fitmodelT = glm(Positive ~ Week ,data=store.dataT,family=binomial)
gam.riskT <- function(x){predict(glm.fitmodelT,list(Week=x),type="response")} # NEED TO LOAD GAM FIRST!
gam.riskLT <- function(x){gamCIT[x==gamCIT$predict.x,"minbts"]} # NEED TO LOAD GAM FIRST!
gam.riskUT <- function(x){gamCIT[x==gamCIT$predict.x,"maxbts"]} # NEED TO LOAD GAM FIRST!

# Uniform in 1st trimester
risk.PT = predict(glm.fitmodelT,list(Week=6),type="response")
uniform.riskT <- function(x,risk=risk.PT){ifelse(x<=13,risk,0)}


# Define pregnancy period - uniform distribution
shift.t=6
pregnancy.period = c(shift.t:39)


# - - - - - - - - - 
# Plot
# - - - - - - - - - 

# Figure 1A

# Plot risk function

par(mfrow=c(1,1))
#layout(matrix(c(1,1,2,2,2,2), 6,1, byrow=F))
par(las=0)
par(mar=c(3,4,1,4))
par(mgp=c(2,0.9,0))


plot(data.births$Week,100*data.births$Pos.misc/(data.births$Pos.misc+data.births$Neg.misc ), col="white",pch=19,cex=0.7, xaxs="i", 
     ylim=c(0,100),xlim=c(0,40), # xlim=c(5.9,39.1), 
     las=1, tck = -.03, mgp=c(3, .5, 0), cex.axis=0.9,
     xlab="",ylab="")
mtext(side = 2, "% with APO after Zika infection", line = 2)
mtext(side = 1, "week of gestation at time of infection", line = 1.5)

lines(pregnancy.period,100*uniform.risk(pregnancy.period), col="red",lty=2, lwd=2 )
lines(pregnancy.period,100*sapply(pregnancy.period,gam.risk),col="red", lwd=2)

polygon(c(pregnancy.period,rev(pregnancy.period)),100*c(sapply(pregnancy.period,gam.riskU),rev(sapply(pregnancy.period,gam.riskL))),lty=0 ,col=rgb(1,0,0,0.2))
points(data.births$Week,100*data.births$Pos.misc/(data.births$Pos.misc+data.births$Neg.misc ) , col="black",pch=19,cex=0.5)

#lines(pregnancy.period,100*uniform.riskT(pregnancy.period), col="blue",lty=2, lwd=2 )
#lines(pregnancy.period,100*sapply(pregnancy.period,gam.riskT),col="blue", lwd=2)

#polygon(c(pregnancy.period,rev(pregnancy.period)),100*c(sapply(pregnancy.period,gam.riskUT),rev(sapply(pregnancy.period,gam.riskLT))),lty=0 ,col=rgb(0,0,1,0.2))
#points(data.births$Week,100*data.births$Pos/(data.births$Pos+data.births$Neg ) , col="blue",pch=1,cex=0.5)
#title(main="B",adj=0) 

dev.copy(pdf,paste("plots/Figure1A.pdf",sep=""),width=12,height=3)
dev.off()



# Figure 1B-J

plot_multiple_regions(place.names, epi.names, reportA=0.17)
plot_multiple_regions(place.names, epi.names, reportA=0.40)


# Figure 2
plot_simulated_scenarios(reportVal = 0.17)
plot_simulated_scenarios(reportVal = 0.4)


# Sensitivity with all data included
gamCI0 = gamCI # Swap function for sensitivity

gamCI = gamCIT # Swap function for sensitivity
glm.fitmodel = glm.fitmodelT
gam.risk = gam.riskT
gam.riskL = gam.riskLT
gam.riskU = gam.riskUT
risk.P = risk.PT
uniform.risk = uniform.riskT

plot_multiple_regions(place.names, epi.names, reportA=0.17)
plot_simulated_scenarios(reportVal = 0.17)


  