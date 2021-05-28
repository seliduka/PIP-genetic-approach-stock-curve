library(quantmod)
library(xts)
library(zoo)


           #rm(list = ls())
tw2395 <- read.csv("taiwan50/AC_2395.csv", encoding = "UTF-8")

tw2395[,1] <- gsub('/', '-', tw2395[,1])

tw2395 <- data.frame("date" = tw2395[,1], "colsing_price" =  tw2395[,2])


rownames(tw2395) = tw2395[,1]
as.xts(tw2395)
# tw2395 = getSymbols("2330.TW", from="2000-01-01", to="2015-12-31", src="google")
# plot(tw2395)
# chart_Series(tw2395)

# closing <- tw2395[,4]
closing <- tw2395[,2]
closing <- as.matrix(closing)
rownames(closing) <- 1:nrow(closing)
colnames(closing) <- ("y")
plot(closing)
ps <- data.frame(x=1:nrow(closing), y=closing)
row.names(ps) <- row.names(tw2395)







PIP <- function(ps){

  interp <- NULL
  breakpoints <- NULL

  # ps <- as.matrix(p)


  SP <- data.frame(x = ps[1,"x"], y = ps[1,"y"])
  SP <- rbind(SP, c(x = ps[nrow(ps),"x"],y = ps[nrow(ps),"y"]))


  interp <- approx(x=c(ps[1,"x"], ps[nrow(ps),"x"]),
                   # interp內插（approx），x=ps的第一個x到ps最大行的x值的向量（c）
                   y=c(ps[1,"y"],ps[nrow(ps),"y"]), n=nrow(ps))
  # y=第1y到最後行y的向量，x&y分割成nrow(ps)等分
  plot(interp)



  interp <- do.call(cbind, interp)
  # 將interp所有參數執行cbind存到interp，x,y透過column合併<從value變data
  # 現在interp是第一點到最後一點連成的線
  plot(interp)


  breakpoints <- c(1, nrow(ps))
  # breakpoints是1～ps（2664）的column
  breakpoints

  dd <- 0
  while(dd < 7){
    dt <- sqrt(rowSums((ps - interp)^2))
    # 距離dt是加起來的平方根(ps減interp的差)
    plot(dt)


    ind <- which.max(dt)
    if(ind == 1 | ind == nrow(ps)){
      ind <- which.max(dt[dt!=max(dt)] )
    }else{
      ind <- which.max(dt)
    }
    # 19/06/25>>dt等於nrow跟1會失敗
    # ins是距離最大的ds
    # ind在496時距離最大
    # SP <- rbind(SP, c(ind, ps[ind,"y"]))
    SP <- rbind(SP, c(ps[ind,"x"], ps[ind,"y"]))#2017/05/24
    ends <- c(min(ind-breakpoints[breakpoints<ind]), min(breakpoints[breakpoints>ind]-ind))
    # ends是到那點有多少，那點過後有多少
    ends




    leg1 <- approx(x=c(ps[ind-ends[1],"x"], ps[ind,"x"]),
                   y=c(ps[ind-ends[1],"y"], ps[ind,"y"]), n=ends[1]+1)
    leg2 <- approx(x=c(ps[ind,"x"], ps[ind+ends[2],"x"]),
                   y=c(ps[ind,"y"], ps[ind+ends[2],"y"]), n=ends[2])
    interp[(ind-ends[1]):ind, "y"] <- leg1$y
    interp[(ind+1):(ind+ends[2]), "y"] <- leg2$y
    breakpoints <- c(breakpoints, ind)
    dd <- dd+1
  }
  plot(interp)
  SP <- SP[order(SP$x),]
  return(SP)
  # SP <- data.frame(T = sort(SP$x),A = ps[sort(SP$x),2])
}
# ====================================

patterntemplate <- function(SP, ps){

  # source("getstcokPIP.R")
  library(scales)
  # source("fitness.R")
  # SP <- PIP(ps)

  SP <- data.frame(T = SP$x,A = SP$y)
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

  # T的標準，不必要？



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

  fitness <- function(SP, template){
    SP$T <- rescale(SP$T, to=c(0,180))
    template$T <- rescale(template$T, to=c(0,180))
    TD <- sqrt(sum((SP$T - template$T)^2))
    AD <- sqrt(sum((SP$A - template$A)^2))
    w <- 0.4
    DM <- w*AD+(1-w)*TD
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

  which.min(DM[,2])

  pattern <- data.frame(pattern = DM[which.min(DM[,2]), 1],fitness = DM[which.min(DM[,2]), 2] )
  return(pattern)
}

# ====================================

Popsize = 50
Maxgen = 1000
Minfitness = 0
Rc = 0.4
Ra = 0.5
dlen = 180
crossoverrate = 0.4

# Initialization of the Population
ds = round(nrow(ps)/dlen)
# 一個染色體共有幾點
DJ <- nrow(ps)/ds

C1 <- rbind(ps[1,"x"])
for(i in c(1:ds)){
  C1 <- rbind(C1,ps[DJ*i,"x"] )
}
# 共ni+1點的染色體C1
C1 <- data.frame(T = C1[,1], A = ps[C1,"y"])
# 兩點之間是分割
# generation <- function(C1){

CX <- NULL
as.data.frame(CX)
for(x in c(2:50)){
  CX <- rbind(ps[1,"x"])
  for(a in seq(from=1, to=ds)){
    if(length(CX[a-1,1]==C1[a,1])){
      repeat {
        CX[a,1] <- round(runif(1, CX[a-1,1],C1[(a+1),1]))
        if(length(CX[a,1]:CX[a-1,1])>10) break
      }
    }else if(CX[a,1]<C1[a,1]){
      repeat {
        CX[a,1] <- round(runif(1, CX[a,1],C1[(a+1),1]))
        if(length(CX[(a-1),1]:CX[a,1])>10) break
      }
    }else if(CX[a,1]>C1[a,1]){
      repeat {
        CX[a,1] <- round(runif(1, C1[(a-1),1],C1[(a+1),1]))
        if(length(CX[(a-1),1]:CX[a,1])>10) break
      }
    }
    # 隨機在a-1跟a+1之間移動
    CX <- rbind(CX,ps[CX[a,1],"x"])
  }
  CX[1,1] <- (ps[1,"x"])
  # 第一點是1，第二點在1～356之間，第三點在
  if (length(ps[CX[a,1],"x"]:ps[nrow(ps),"x"])<10){
    repeat {
      CX[a,1] <- round(runif(1, CX[a-1,1],ps[nrow(ps),"x"]))
      if(length(CX[a,1]:ps[nrow(ps),"x"])>10) break
    }
  }
  CX[(ds+1),] <- (ps[nrow(ps),"x"])

  CX <- data.frame(T = CX[,1], A = ps[CX[,1],2])
  assign(paste("C", x, sep=""),CX)
}
# 產生第一代

segmentmatch <- function(CXX){
  D <- NULL
  as.data.frame(D)
  for(d in c(1:(length(C1$T)-1))){
    # for(sd in c(1:ds)){

    DXX <- data.frame(ps[CXX[d,1]:CXX[(d + 1),1],])
    DX <- patterntemplate(PIP(DXX), DXX)
    # 分段與22個模版比對的適應值取最小值
    D <- rbind(D,DX)
    # }
  }
  # 17/07/25，BUG D只有2分段
  return(D)
  # 24個分段
}
for(du in c(1:50)){
  U <- paste("C", du, sep="")
  assign(paste("D", du, sep=""),segmentmatch(get(U)))
}
# segmentmatch(C7)


sum_fit <- function(DXXX){
  DXXX[,2] <- as.numeric(as.character(DXXX[,2]))
  DXXX[,1] <- c(1:nrow(D1))
  DSF <- rbind(colSums(DXXX))
  return(DSF)
}
# 取所有分段加起來的平均
DS <- NULL
for(D in c(1:50)){
  DS <- rbind(DS, paste("D", D, sep=""))
}
DSF <- c(NULL,NULL)
for(F in c(1:50)){
  DSF <- rbind(DSF,c(paste("DF", F, sep=""),sum_fit(get(DS[F,1]))[1,2]))
}
# 加總一段染色體的適應值，170726尚未取平均值
# 取平均值---------

DSF[,2]
nrow(DSF)
# Selection best
sel <- DSF[which.min(DSF[,2]),1]
sel <- cbind("pattern" = sel, "fitness" = as.character(min(DSF[,2])))
sel <- get(paste("C",which.min(DSF[,2]), sep=""))

# 比對所有染色體適應值，取平均最低為C1（C1不會經歷交配突變取代）
# 新一代C1先儲存在XXXXXXX裡，等49個子代做完，最後才成為新一代C1
# }
# 將適應值歸一化
# roulette selection
DSF
ADSF <- as.data.frame(DSF)
ADSF$fitness <- as.numeric(as.character(ADSF$fitness))
ADSF$fitness <- (ADSF$fitness-min(ADSF$fitness))/(max(ADSF$fitness)-min(ADSF$fitness))
ADSF$V1 <- as.numeric(row.names(ADSF))
rou <- 1/colSums(ADSF)[2]
ADSF <- as.data.frame(t(ADSF))
sample(c(1:50),size = 2, replace = TRUE,prob = c(ADSF[2,]*rou))
# 全數*上1/和


#



GU <- 2
# repeat {
# Crossover
for(Cr in c(2:11)){
  Cparent <- as.matrix(paste("C", sample(c(1:50),size = 2, replace = TRUE,prob = c(ADSF[2,])), sep=""))
  CutPt <- round(runif(1, 1, nrow(ps)))

  Cnew1 <-c(get(Cparent[1])[which(get(Cparent[1])[,1]<CutPt),1],get(Cparent[2])[which(get(Cparent[2])[,1]>CutPt),1])
  Cnew1 <- as.data.frame(Cnew1)
  Cnew1 <- as.data.frame(cbind(T = Cnew1[,1], A = ps[Cnew1[,1],2]))
  assign(paste("C", Cr, sep=""),Cnew1)

  Cnew2 <-c(get(Cparent[2])[which(get(Cparent[2])[,1]<CutPt),1],get(Cparent[1])[which(get(Cparent[1])[,1]>CutPt),1])
  Cnew2 <- as.data.frame(Cnew2)
  Cnew2 <- as.data.frame(cbind(T = Cnew2, A = ps[Cnew2[,1],2]))
  assign(paste("C", (Cr+10), sep=""),Cnew2)
}
# 做20次成為子代


# Mutation
# for(M in c(22:50)){
# Mparent <- as.matrix(paste("C", sample(c(1:50),size = 2, replace = TRUE,prob = c(ADSF[2,])), sep=""))
# Ra = 0.5
# Rb = 1-Ra
# round(runif(1,1,nrow(get(Mparent))))
# Nr = runif(1,0,1)
# buf <- round(runif(1,1,nrow(ps)))
# Mparent
# if(Nr<=Ra){
  # Cnew3 <- rbind(get(Mparent), c(ps[buf,1],ps[buf,2]))
  # Cnew3 <- Cnew3[order(Cnew3$T),]
# }else{
  # Cnew3 <- get(Mparent)[-round(runif(1,1,nrow(get(Mparent)))), ]
  # rownames(Cnew3) <- (1:nrow(Cnew3))
# }
# Cnew3 <- data.frame(T = Cnew3[,1], A = ps[Cnew3[,1],2])
# assign(paste("C", M, sep=""),Cnew3)
# }
# 做29次

for(du in c(1:50)){
  U <- paste("C", du, sep="")
  assign(paste("D", du, sep=""),segmentmatch(get(U)))
}
# segmentmatch(C7)

sum_fit <- function(DXXX){
  DXXX[,2] <- as.numeric(as.character(DXXX[,2]))
  DXXX[,1] <- c(1:nrow(D1))
  DSF <- rbind(colSums(DXXX))
  return(DSF)
}
DS <- NULL
for(D in c(1:50)){
  DS <- rbind(DS, paste("D", D, sep=""))
}
DSF <- c(NULL,NULL)
for(F in c(1:50)){
  DSF <- rbind(DSF,c(paste("DF", F, sep=""),sum_fit(get(DS[F,1]))[1,2]))
}
# 加總一段染色體的適應值

# Selection best
sel <- DSF[which.min(DSF[,2]),1]
sel <- cbind("pattern" = sel, "fitness" = as.character(min(DSF[,2])))
C1 <- get(paste("C",which.min(DSF[,2]), sep=""))
#

GU <- GU+1

# if (GU==1000) break
# }
print(C1)

