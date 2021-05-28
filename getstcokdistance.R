
patterntemplate <- function(Segmented){
# source("getstcokPIP.R")
source("testPIP.R")
source("fitness.R")
 # rm(list = ls())
percentile <- ps[,1]
# eight <- quantile(percentile[,1],
# c(.0, .12.5, .25, .37.5, .50, .62.5, .75, .87.5, .100))
percentile <- as.matrix(quantile(percentile))
eight <- rbind(1,percentile[2]/2)
eight <- rbind(eight,percentile[2])
buffer <- rbind(percentile[2],percentile[3])
eight <- rbind(eight,median(buffer))
eight <- rbind(eight,percentile[3])
rm(buffer)
buffer <- rbind(percentile[3],percentile[4])
eight <- rbind(eight,median(buffer))
eight <- rbind(eight,percentile[4])
rm(buffer)
buffer <- rbind(percentile[4],percentile[5])
eight <- rbind(eight,median(buffer))
eight <- rbind(eight,percentile[5])
TDeight <- round(eight)
#取得型態模版temporal所需的八分位點TDeight

# rangeDM <- data.frame(T = TDeight, A = ps[TDeight,2])
# top <- rangeDM[which.max(rangeDM[,2]),2]
# bottom <- rangeDM[which.min(rangeDM[,2]),2]

# template <- quantile(c(-1,1),c(.0, .50, .85, .85, .50, .85, .85, .50, .0))
# template <- data.frame(T = TDeight, A = template)
# 形成模版DoubleTop
SP$A <- 2*(SP$A-min(SP$A))/(max(SP$A)-min(SP$A))-1
# SP標準化

DoubleTop <- quantile(c(-1,1),c(.0, .50, .85, .85, .50, .85, .85, .50, .0))
DoubleTop <- data.frame(T = TDeight, A = DoubleTop)

DouleBottom <- quantile(c(-1,1),c(.100, .50, .15, .15, .50, .15, .15, .50, .100))
DouleBottom <- data.frame(T = TDeight, A = DouleBottom)

TripleTop <- quantile(c(-1,1),c(.0, .50, .100, .50, .100, .50, .100, .50, .0))
TripleTop <- data.frame(T = TDeight, A = TripleTop)

TripleBottom <- quantile(c(-1,1),c(.100, .50, .0, .50, .0, .50, .0, .50, .100))
TripleBottom <- data.frame(T = TDeight, A = TripleBottom)

HeadandShoulderTop <- quantile(c(-1,1),c(.0, .50, .75, .50, .100, .50, .75, .50, .0))
HeadandShoulderTop <- data.frame(T = TDeight, A = HeadandShoulderTop)

HeadandShoulderBottom <- quantile(c(-1,1),c(.100, .50, .25, .50, .0, .50, .25, .50, .100))
HeadandShoulderBottom <- data.frame(T = TDeight, A = HeadandShoulderBottom)

RoundedTop <- quantile(c(-1,1),c(.0, .75, .85, .95, .100, .95, .85, .75, .0))
RoundedTop <- data.frame(T = TDeight, A = RoundedTop)

RoundedBottom <- quantile(c(-1,1),c(.100, .25, .15, .5, .0, .5, .15, .25, .100))
RoundedBottom <- data.frame(T = TDeight, A = RoundedBottom)

SpikeTop <- quantile(c(-1,1),c(.15, .0, .15, .0, .100, .0, .15, .0, .15))
SpikeTop <- data.frame(T = TDeight, A = SpikeTop)

SpikeBottom <- quantile(c(-1,1),c(.85, .100, .85, .100, .0, .100, .85, .100, .85))
SpikeBottom <- data.frame(T = TDeight, A = SpikeBottom)

PennatTop <- quantile(c(-1,1),c(.0, .100, .15, .85, .25, .75, .35, .65, .100))
PennatTop <- data.frame(T = TDeight, A = PennatTop)

PennatBottom <- quantile(c(-1,1),c(.100, .0, .85, .15, .75, .25, .65, .35, .0))
PennatBottom <- data.frame(T = TDeight, A = PennatBottom)

BroadeningTop <- quantile(c(-1,1),c(.0, .35, .65, .25, .75, .15, .85, .0, .100))
BroadeningTop <- data.frame(T = TDeight, A = BroadeningTop)

BroadeningBottom <- quantile(c(-1,1),c(.100, .65, .35, .75, .25, .85, .15, .100, .0))
BroadeningBottom <- data.frame(T = TDeight, A = BroadeningBottom)

DiamondTop <- quantile(c(-1,1),c(.0, .10, .35, .15, .85, .15, .65, .35, .100))
DiamondTop <- data.frame(T = TDeight, A = DiamondTop)

DiamondBottom <- quantile(c(-1,1),c(.100, .35, .65, .15, .85, .15, .35, .60, .0))
DiamondBottom <- data.frame(T = TDeight, A = DiamondBottom)

FlagTop <- quantile(c(-1,1),c(.0, .35, .65, .35, .65, .35, .65, .35, .100))
FlagTop <- data.frame(T = TDeight, A = FlagTop)

FlagBottom <- quantile(c(-1,1),c(.100, .35, .65, .35, .65, .35, .65, .35, .0))
FlagBottom <- data.frame(T = TDeight, A = FlagBottom)

Wedge1 <- quantile(c(-1,1),c(.0, .100, .25, .60, .15, .35, .10, .0, .100))
Wedge1 <- data.frame(T = TDeight, A = Wedge1)

Wedge2 <- quantile(c(-1,1),c(.100, .50, .10, .35, .25, .60, .25, .100, .0))
Wedge2 <- data.frame(T = TDeight, A = Wedge2)

Wedge3 <- quantile(c(-1,1),c(.0, .15, .85, .15, .85, .15, .85, .15, .100))
Wedge3 <- data.frame(T = TDeight, A = Wedge3)

Wedge4 <- quantile(c(-1,1),c(.100, .15, .85, .15, .85, .815, .85, .15, .0))
Wedge4 <- data.frame(T = TDeight, A = Wedge4)

DM <- rbind(c(pattern = "DoubleTop", "fitness" = (fitness(SP,DoubleTop))))
DM <- rbind(DM, c(pattern = "DouleBottom", "fitness" = (fitness(SP,DouleBottom))))
DM <- rbind(DM, c(pattern = "TripleTop", "fitness" = (fitness(SP,TripleTop))))
DM <- rbind(DM, c(pattern = "TripleBottom", "fitness" = (fitness(SP,TripleBottom))))
DM <- rbind(DM, c(pattern = "HeadandShoulderTop", "fitness" = (fitness(SP,HeadandShoulderTop))))
DM <- rbind(DM, c(pattern = "HeadandShoulderBottom", "fitness" = (fitness(SP,HeadandShoulderBottom))))
DM <- rbind(DM, c(pattern = "RoundedTop", "fitness" = (fitness(SP,RoundedTop))))
DM <- rbind(DM, c(pattern = "RoundedBottom", "fitness" = (fitness(SP,RoundedBottom))))
DM <- rbind(DM, c(pattern = "SpikeTop", "fitness" = (fitness(SP,SpikeTop))))
DM <- rbind(DM, c(pattern = "SpikeBottom", "fitness" = (fitness(SP,SpikeBottom))))
DM <- rbind(DM, c(pattern = "PennatTop", "fitness" = (fitness(SP,PennatTop))))
DM <- rbind(DM, c(pattern = "PennatBottom", "fitness" = (fitness(SP,PennatBottom))))
DM <- rbind(DM, c(pattern = "BroadeningTop", "fitness" = (fitness(SP,BroadeningTop))))
DM <- rbind(DM, c(pattern = "BroadeningBottom", "fitness" = (fitness(SP,BroadeningBottom))))
DM <- rbind(DM, c(pattern = "DiamondTop", "fitness" = (fitness(SP,DiamondTop))))
DM <- rbind(DM, c(pattern = "DiamondBottom", "fitness" = (fitness(SP,DiamondBottom))))
DM <- rbind(DM, c(pattern = "FlagTop", "fitness" = (fitness(SP,FlagTop))))
DM <- rbind(DM, c(pattern = "FlagBottom", "fitness" = (fitness(SP,FlagBottom))))
DM <- rbind(DM, c(pattern = "Wedge1", "fitness" = (fitness(SP,Wedge1))))
DM <- rbind(DM, c(pattern = "Wedge2", "fitness" = (fitness(SP,Wedge2))))
DM <- rbind(DM, c(pattern = "Wedge3", "fitness" = (fitness(SP,Wedge3))))
DM <- rbind(DM, c(pattern = "Wedge4", "fitness" = (fitness(SP,Wedge4))))

which.min(DM[,2])

pattern <- DM[which.min(DM[,2]), 1]
}



































































