

# patterntemplate <- function(SP, ps){
      # rm(list = ls())
source("getstcokPIP.R")
source("testPIP.R")
library("scales")
source("fitness.R")
# source("match.R")
SP <- PIP(ps)

  SP <- data.frame(T = SP$x,A = SP$y)
  # SP$T <- rescale(SP$T, to=c(1,length(ps[min(SP$T),1]:ps[max(SP$T),1])))
  # SP$T <- round(SP$T)
  # rm(list = ls())
  # SP$T <- (SP$T-SP[1,1])+1
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
  TDeight <- rescale(TDeight, to=c(0,1))
  # TDeight <- rescale(TDeight, to=c(1,9))
  #取得型態模版temporal所需的八分位點TDeight

  # rangeDM <- data.frame(T = TDeight, A = ps[TDeight,2])
  # top <- rangeDM[which.max(rangeDM[,2]),2]
  # bottom <- rangeDM[which.min(rangeDM[,2]),2]

  # template <- quantile(c(-1,1),c(.0, .50, .85, .85, .50, .85, .85, .50, .0))
  # template <- data.frame(T = TDeight, A = template)
  # 形成模版DoubleTop

  # SP$A <- round(SP$A)
  # SP$A <- 2*(SP$A-min(SP$A))/(max(SP$A)-min(SP$A))-1
  ACSP <- SP
  TCSP <- SP
  SP$A <- rescale(SP$A, to=c(0,1))
  # SP標準化
  SP$T <- rescale(SP$T, to=c(0,1))
  # SP$T <- (SP$T-min(SP$T))/(max(SP$T)-min(SP$T))
  # SP$T <- rescale(SP$T, to=c(1,9))
  # T的標準化
  # windows大小 高0～1 寬1～30

  DoubleTop <- quantile(c(-1,1),c(.0, .50, .85, .85, .50, .85, .85, .50, .0))
  DoubleTop <- data.frame(T = TDeight, A = DoubleTop)
  # DoubleTop$A <- as.numeric(DoubleTop$A)
  # DoubleTop$A <- rescale(DoubleTop$A, to=c(min(SP$A),max(SP$A)))
  # DoubleTop$A <-round(DoubleTop$A)
  DoubleTop$A <- rescale(DoubleTop$A, to=c(0,1))
  # xprot <- table(SP$A>DoubleTop$A)[2]/nrow(SP)
  # xprot <- round(xprot, digits = 2)
  # xprot <- as.numeric(xprot)
  # xprot <- xprot*(1-DoubleTop$A)
  # xprot <- sum(xprot)/nrow(DoubleTop)
  # # -----------------------------
  # nprot <- table(SP$A<DoubleTop$A)[2]/nrow(SP)
  # nprot <- round(nprot, digits = 2)
  # nprot <- as.numeric(nprot)
  # nprot <- nprot*(DoubleTop$A)
  # nprot <- sum(nprot)/nrow(DoubleTop)
  # DT6





  DouleBottom <- quantile(c(-1,1),c(.100, .50, .15, .15, .50, .15, .15, .50, .100))
  DouleBottom <- data.frame(T = TDeight, A = DouleBottom)
  # DouleBottom$A <- as.numeric(DouleBottom$A)
  # DouleBottom$A <- rescale(DouleBottom$A, to=c(min(SP$A),max(SP$A)))
  # DouleBottom$A <- round(DouleBottom$A)
  DouleBottom$A <- rescale(DouleBottom$A, to=c(0,1))

  TripleTop <- quantile(c(-1,1),c(.0, .50, .100, .50, .100, .50, .100, .50, .0))
  TripleTop <- data.frame(T = TDeight, A = TripleTop)
  # TripleTop$A <- as.numeric(TripleTop$A)
  # TripleTop$A <- rescale(TripleTop$A, to=c(min(SP$A),max(SP$A)))
  # TripleTop$A <- round(TripleTop$A)
  TripleTop$A <- rescale(TripleTop$A, to=c(0,1))

  TripleBottom <- quantile(c(-1,1),c(.100, .50, .0, .50, .0, .50, .0, .50, .100))
  TripleBottom <- data.frame(T = TDeight, A = TripleBottom)
  # TripleBottom$A <- as.numeric(TripleBottom$A)
  # TripleBottom$A <- rescale(TripleBottom$A, to=c(min(SP$A),max(SP$A)))
  # TripleBottom$A <- round(TripleBottom$A)
  TripleBottom$A <- rescale(TripleBottom$A, to=c(0,1))

  HeadandShoulderTop <- quantile(c(-1,1),c(.0, .50, .75, .50, .100, .50, .75, .50, .0))
  HeadandShoulderTop <- data.frame(T = TDeight, A = HeadandShoulderTop)
  # HeadandShoulderTop$A <- as.numeric(HeadandShoulderTop$A)
  # HeadandShoulderTop$A <- rescale(HeadandShoulderTop$A, to=c(min(SP$A),max(SP$A)))
  # HeadandShoulderTop$A <- round(HeadandShoulderTop$A)
  HeadandShoulderTop$A <- rescale(HeadandShoulderTop$A, to=c(0,1))

  HeadandShoulderBottom <- quantile(c(-1,1),c(.100, .50, .25, .50, .0, .50, .25, .50, .100))
  HeadandShoulderBottom <- data.frame(T = TDeight, A = HeadandShoulderBottom)
  # HeadandShoulderBottom$A <- as.numeric(HeadandShoulderBottom$A)
  # HeadandShoulderBottom$A <- rescale(HeadandShoulderBottom$A, to=c(min(SP$A),max(SP$A)))
  # HeadandShoulderBottom$A <- round(HeadandShoulderBottom$A)
  HeadandShoulderBottom$A <- rescale(HeadandShoulderBottom$A, to=c(0,1))

  RoundedTop <- quantile(c(-1,1),c(.0, .75, .85, .95, .100, .95, .85, .75, .0))
  RoundedTop <- data.frame(T = TDeight, A = RoundedTop)
  # RoundedTop$A <- as.numeric(RoundedTop$A)
  # RoundedTop$A <- rescale(RoundedTop$A, to=c(min(SP$A),max(SP$A)))
  # RoundedTop$A <- round(RoundedTop$A)
  RoundedTop$A <- rescale(RoundedTop$A, to=c(0,1))

  RoundedBottom <- quantile(c(-1,1),c(.100, .25, .15, .5, .0, .5, .15, .25, .100))
  RoundedBottom <- data.frame(T = TDeight, A = RoundedBottom)
  # RoundedBottom$A <- as.numeric(RoundedBottom$A)
  # RoundedBottom$A <- rescale(RoundedBottom$A, to=c(min(SP$A),max(SP$A)))
  # RoundedBottom$A <- round(RoundedBottom$A)
  RoundedBottom$A <- rescale(RoundedBottom$A, to=c(0,1))

  SpikeTop <- quantile(c(-1,1),c(.15, .0, .15, .0, .100, .0, .15, .0, .15))
  SpikeTop <- data.frame(T = TDeight, A = SpikeTop)
  # SpikeTop$A <- as.numeric(SpikeTop$A)
  # SpikeTop$A <- rescale(SpikeTop$A, to=c(min(SP$A),max(SP$A)))
  # SpikeTop$A <- round(SpikeTop$A)
  SpikeTop$A <- rescale(SpikeTop$A, to=c(0,1))

  SpikeBottom <- quantile(c(-1,1),c(.85, .100, .85, .100, .0, .100, .85, .100, .85))
  SpikeBottom <- data.frame(T = TDeight, A = SpikeBottom)
  # SpikeBottom$A <- as.numeric(SpikeBottom$A)
  # SpikeBottom$A <- rescale(SpikeBottom$A, to=c(min(SP$A),max(SP$A)))
  # SpikeBottom$A <- round(SpikeBottom$A)
  SpikeBottom$A <- rescale(SpikeBottom$A, to=c(0,1))

  PennatTop <- quantile(c(-1,1),c(.0, .100, .15, .85, .25, .75, .35, .65, .100))
  PennatTop <- data.frame(T = TDeight, A = PennatTop)
  # PennatTop$A <- as.numeric(PennatTop$A)
  # PennatTop$A <- rescale(PennatTop$A, to=c(min(SP$A),max(SP$A)))
  # PennatTop$A <- round(PennatTop$A)
  PennatTop$A <- rescale(PennatTop$A, to=c(0,1))

  PennatBottom <- quantile(c(-1,1),c(.100, .0, .85, .15, .75, .25, .65, .35, .0))
  PennatBottom <- data.frame(T = TDeight, A = PennatBottom)
  # PennatBottom$A <- as.numeric(PennatBottom$A)
  # PennatBottom$A <- rescale(PennatBottom$A, to=c(min(SP$A),max(SP$A)))
  # PennatBottom$A <- round(PennatBottom$A)
  PennatBottom$A <- rescale(PennatBottom$A, to=c(0,1))

  BroadeningTop <- quantile(c(-1,1),c(.0, .35, .65, .25, .75, .15, .85, .0, .100))
  BroadeningTop <- data.frame(T = TDeight, A = BroadeningTop)
  # BroadeningTop$A <- as.numeric(BroadeningTop$A)
  # BroadeningTop$A <- rescale(BroadeningTop$A, to=c(min(SP$A),max(SP$A)))
  # BroadeningTop$A <- round(BroadeningTop$A)
  BroadeningTop$A <- rescale(BroadeningTop$A, to=c(0,1))

  BroadeningBottom <- quantile(c(-1,1),c(.100, .65, .35, .75, .25, .85, .15, .100, .0))
  BroadeningBottom <- data.frame(T = TDeight, A = BroadeningBottom)
  # BroadeningBottom$A <- as.numeric(BroadeningBottom$A)
  # BroadeningBottom$A <- rescale(BroadeningBottom$A, to=c(min(SP$A),max(SP$A)))
  # BroadeningBottom$A <- round(BroadeningBottom$A)
  BroadeningBottom$A <- rescale(BroadeningBottom$A, to=c(0,1))

  DiamondTop <- quantile(c(-1,1),c(.0, .10, .35, .15, .85, .15, .65, .35, .100))
  DiamondTop <- data.frame(T = TDeight, A = DiamondTop)
  # DiamondTop$A <- as.numeric(DiamondTop$A)
  # DiamondTop$A <- rescale(DiamondTop$A, to=c(min(SP$A),max(SP$A)))
  # DiamondTop$A <- round(DiamondTop$A)
  DiamondTop$A <- rescale(DiamondTop$A, to=c(0,1))

  DiamondBottom <- quantile(c(-1,1),c(.100, .35, .65, .15, .85, .15, .35, .60, .0))
  DiamondBottom <- data.frame(T = TDeight, A = DiamondBottom)
  # DiamondBottom$A <- as.numeric(DiamondBottom$A)
  # DiamondBottom$A <- rescale(DiamondBottom$A, to=c(min(SP$A),max(SP$A)))
  # DiamondBottom$A <- round(DiamondBottom$A)
  DiamondBottom$A <- rescale(DiamondBottom$A, to=c(0,1))

  FlagTop <- quantile(c(-1,1),c(.0, .35, .65, .35, .65, .35, .65, .35, .100))
  FlagTop <- data.frame(T = TDeight, A = FlagTop)
  # FlagTop$A <- as.numeric(FlagTop$A)
  # FlagTop$A <- rescale(FlagTop$A, to=c(min(SP$A),max(SP$A)))
  # FlagTop$A <- round(FlagTop$A)
  FlagTop$A <- rescale(FlagTop$A, to=c(0,1))

  FlagBottom <- quantile(c(-1,1),c(.100, .35, .65, .35, .65, .35, .65, .35, .0))
  FlagBottom <- data.frame(T = TDeight, A = FlagBottom)
  # FlagBottom$A <- as.numeric(FlagBottom$A)
  # FlagBottom$A <- rescale(FlagBottom$A, to=c(min(SP$A),max(SP$A)))
  # FlagBottom$A <- round(FlagBottom$A)
  FlagBottom$A <- rescale(FlagBottom$A, to=c(0,1))

  Wedge1 <- quantile(c(-1,1),c(.0, .100, .25, .60, .15, .35, .10, .0, .100))
  Wedge1 <- data.frame(T = TDeight, A = Wedge1)
  # Wedge1$A <- as.numeric(Wedge1$A)
  # Wedge1$A <- rescale(Wedge1$A, to=c(min(SP$A),max(SP$A)))
  # Wedge1$A <- round(Wedge1$A)
  Wedge1$A <- rescale(Wedge1$A, to=c(0,1))

  Wedge2 <- quantile(c(-1,1),c(.100, .50, .10, .35, .25, .60, .25, .100, .0))
  Wedge2 <- data.frame(T = TDeight, A = Wedge2)
  # Wedge2$A <- as.numeric(Wedge2$A)
  # Wedge2$A <- rescale(Wedge2$A, to=c(min(SP$A),max(SP$A)))
  # Wedge2$A <- round(Wedge2$A)
  Wedge2$A <- rescale(Wedge2$A, to=c(0,1))

  Wedge3 <- quantile(c(-1,1),c(.0, .15, .85, .15, .85, .15, .85, .15, .100))
  Wedge3 <- data.frame(T = TDeight, A = Wedge3)
  # Wedge3$A <- as.numeric(Wedge3$A)
  # Wedge3$A <- rescale(Wedge3$A, to=c(min(SP$A),max(SP$A)))
  # Wedge3$A <- round(Wedge3$A)
  Wedge3$A <- rescale(Wedge3$A, to=c(0,1))

  Wedge4 <- quantile(c(-1,1),c(.100, .15, .85, .15, .85, .815, .85, .15, .0))
  Wedge4 <- data.frame(T = TDeight, A = Wedge4)
  # Wedge4$A <- as.numeric(Wedge4$A)
  # Wedge4$A <- rescale(Wedge4$A, to=c(min(SP$A),max(SP$A)))
  # Wedge4$A <- round(Wedge4$A)
  Wedge4$A <- rescale(Wedge4$A, to=c(0,1))

  TC <- function(SP){
    slen <- length(TCSP[1,1]:TCSP[nrow(SP),1])
    # 實際分割長度
    dlc <- 2
    # TC控制力度
    dlen <- 180
    theta1 <- dlen/dlc
    d1 <- slen-dlen
    TC <- 1-exp(-(d1/theta1)^2)
    # 1-e^-(d1/theta1)
    return(TC)
  }
  # Amplitude Control
  AC <- function(SP){
    fr <-(ACSP[which.max(SP$A),2]-ACSP[which.min(SP$A),2])/ACSP[which.min(SP$A),2]
    dfr <- 50
    # desired minimum fluctuation rate
    dac <- 2
    d2 <- fr-dfr
    # D2越大越好
    theta2 <- dfr/dac
    AC <- 1-(1/(1+exp(-(d2/theta2))))
    return(AC)
  }

  fitness <- function(SP, template){
    # SP$T <- rescale(SP$T, to=c(0,180))
    # template$T <- rescale(template$T, to=c(0,180))
    AD <- sqrt((1/nrow(SP))*(sum((SP$A - template$A)^2)))
    TD <- sqrt((1/(nrow(SP)-1))*(sum((SP$T - template$T)^2)))
    # AD <- sqrt(sum((SP$A - template$A)^2))
    # TD <- sqrt(sum((SP$T - template$T)^2))
    w <- 0.4
    DM <- (w*AD)+((1-w)*TD)
    return(DM)
  }

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

  # which.min(DM[,2])
  pattern <- data.frame(pattern = DM[which.min(DM[,2]), 1],fitness = DM[which.min(DM[,2]), 2] )
  return(pattern)
# }


