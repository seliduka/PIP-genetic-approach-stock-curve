source("testPIP.R")
source("fitness.R")
source("getpatterntemplate.R")
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
  for(d in c(1:ds)){
    # for(sd in c(1:ds)){

    DXX <- data.frame(ps[CXX[d,1]:CXX[(d+1),1],])
    DX <- patterntemplate(PIP(DXX), DXX)
    # 分段與22個模版比對的適應值取最小值
    D <- rbind(D,DX)
    # }
  }
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
DS <- NULL
for(D in c(1:50)){
  DS <- rbind(DS, paste("D", D, sep=""))
}
DSF <- c(NULL,NULL)
for(F in c(1:50)){
  DSF <- rbind(DSF,c(paste("DF", F, sep=""),sum_fit(get(DS[F,1]))[1,2]))
}
# 加總一段染色體的適應值
# 取平均值---------

# Selection best
sel <- DSF[which.min(DSF[,2]),1]
sel <- cbind("pattern" = sel, "fitness" = as.character(min(DSF[,2])))
C1 <- get(paste("C",which.min(DSF[,2]), sep=""))
# 比對所有染色體適應值
# }
# roulette selection
# for(GU in c(2:1000)){
# Crossover
for(Cr in c(2:11)){
Cparent <- as.matrix(paste("C", round(runif(2,1,50)), sep=""))
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
# Mparent <- as.matrix(paste("C", round(runif(1,1,50)), sep=""))
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
# }


# Fitness Evaluation