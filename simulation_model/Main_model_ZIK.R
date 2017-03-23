# - - - - - - - - - - - - - - - - - - - - - - - - 
# Main Zika output
#
# - - - - - - - - - - - - - - - - - - - - - - - - 

library(deSolve)
library(magrittr)
  
# Set directories and load data

setwd("~/Documents/zika/microcephaly_Brazil/simulation_model/")
#setwd("~/Documents/Collaboration/zika/microcephaly_Brazil/simulation_model/")

source("data_functions.R")

# Load GAM
gamCI <- bootstrap.gam()
#store.microcephaly=data.frame(read.csv("../storemicrocephaly.csv",stringsAsFactors = F))

pop_sizes = read.csv("../data/population_sizes.csv",stringsAsFactors = F)

# Linear model function
gam.risk <- function(x){predict(glm.fit,list(Week=x),type="response")} # NEED TO LOAD GAM FIRST!
gam.riskL <- function(x){gamCI[x==gamCI$predict.x,"minbts"]} # NEED TO LOAD GAM FIRST!
gam.riskU <- function(x){gamCI[x==gamCI$predict.x,"maxbts"]} # NEED TO LOAD GAM FIRST!

# Uniform in 1st trimester
risk.P=predict(glm.fit,list(Week=6),type="response")
uniform.risk <- function(x,risk=risk.P){ifelse(x<=13,risk,0)}

# Define pregnancy period - uniform distribution
shift.t=6
pregnancy.period = c(shift.t:39)


# - - - - - - - - - 
# Plot
# - - - - - - - - - 

# Figure 1B-J

plot_multiple_regions(place.names,epi.names,report=0.15)

# Figure 2

plot_simulated_scenarios()


# Figure 1A

# Plot risk function

par(mfrow=c(1,1))
#layout(matrix(c(1,1,2,2,2,2), 6,1, byrow=F))
par(las=0)
par(mar=c(3,4,1,4))
par(mgp=c(2,0.9,0))


plot(data.births$Week,data.births$Pos.misc/(data.births$Pos.misc+data.births$Neg.misc ), col="white",pch=19,cex=0.7, xaxs="i", 
     ylim=c(0,1),xlim=c(0,40), # xlim=c(5.9,39.1), 
     las=1, tck = -.03, mgp=c(3, .5, 0), cex.axis=0.9,
     xlab="",ylab="")
mtext(side = 2, "Risk of APO after Zika infection", line = 2)
mtext(side = 1, "week of gestation at time of infection", line = 1.5)

lines(pregnancy.period,uniform.risk(pregnancy.period), col="red",lty=2, lwd=2 )

lines(pregnancy.period,sapply(pregnancy.period,gam.risk),col="red", lwd=2)

polygon(c(pregnancy.period,rev(pregnancy.period)),c(sapply(pregnancy.period,gam.riskU),rev(sapply(pregnancy.period,gam.riskL))),lty=0 ,col=rgb(1,0,0,0.2))
points(data.births$Week,data.births$Pos.misc/(data.births$Pos.misc+data.births$Neg.misc ) , col="black",pch=19,cex=0.5)
#title(main="B",adj=0) 

dev.copy(pdf,paste("plots/Risk_fn.pdf",sep=""),width=12,height=3)
dev.off()

  