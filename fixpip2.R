library(quantmod)
library(xts)
library(zoo)
    # rm(list = ls())
tw2395 <- read.csv("taiwan50/AC_2395.csv", encoding = "UTF-8")
tw2395[,1] <- gsub('/', '-', tw2395[,1])
tw2395 <- data.frame("date" = tw2395[,1], "colsing_price" =  tw2395[,2])
rownames(tw2395) = tw2395[,1]
as.xts(tw2395)
closing <- tw2395[,2]
closing <- as.matrix(closing)
rownames(closing) <- 1:nrow(closing)
colnames(closing) <- ("y")
ps <- data.frame(x=1:nrow(closing), y=closing)
row.names(ps) <- row.names(tw2395)

PIP <- function(input){
  SP <- data.frame(x = ps[1,"x"], y = ps[1,"y"])
  # 第一點
  SP <- rbind(SP, c(x = ps[nrow(input),"x"],y = ps[nrow(input),"y"]))
  # 第二點
  x1<-SP[1,1];x2<-SP[2,1];y1<-SP[1,2];y2<-SP[2,2]
  dlist<-NULL
  for (c in ps[1,1]:ps[nrow(input),1]) {
  x3<-ps[c,1]
  y3<-ps[c,2]
  S <- NULL
  S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
  AB <- sqrt((x2-x1)^2+(y2-y1)^2)
  d <- S/AB
  dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  thed <- dlist[which.max(dlist$distance),1]
  # 第三點
  dlist<-NULL
  x1<-SP[1,1];x2<-SP[3,1];y1<-SP[1,2];y2<-SP[3,2]
  for (c in ps[1,1]:ps[thed,1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-SP[3,1];x2<-SP[2,1];y1<-SP[3,2];y2<-SP[2,2]
  for (c in ps[thed,1]:ps[nrow(input),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第四點
  thed <- dlist[which.max(dlist$distance),1]
  dlist<-NULL
  buf <- rbind(SP[2,1],SP[3,1],SP[4,1])
  x1<-SP[1,1];x2<-min(buf);y1<-SP[1,2];y2<-ps[min(buf),2]
  for (c in ps[1,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第五點
  thed <- dlist[which.max(dlist$distance),1]
  dlist<-NULL
  buf <- rbind(SP[2,1],SP[3,1],SP[4,1],SP[5,1])
  x1<-SP[1,1];x2<-min(buf);y1<-SP[1,2];y2<-ps[min(buf),2]
  for (c in ps[1,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第六點
  thed <- dlist[which.max(dlist$distance),1]
  dlist<-NULL
  buf <- rbind(SP[2,1],SP[3,1],SP[4,1],SP[5,1],SP[6,1])
  x1<-SP[1,1];x2<-min(buf);y1<-SP[1,2];y2<-ps[min(buf),2]
  for (c in ps[1,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第七點
  thed <- dlist[which.max(dlist$distance),1]
  dlist<-NULL
  buf <- rbind(SP[2,1],SP[3,1],SP[4,1],SP[5,1],SP[6,1],SP[7,1])
  x1<-SP[1,1];x2<-min(buf);y1<-SP[1,2];y2<-ps[min(buf),2]
  for (c in ps[1,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第八點
  thed <- dlist[which.max(dlist$distance),1]
  dlist<-NULL
  buf <- rbind(SP[2,1],SP[3,1],SP[4,1],SP[5,1],SP[6,1],SP[7,1],SP[8,1])
  x1<-SP[1,1];x2<-min(buf);y1<-SP[1,2];y2<-ps[min(buf),2]
  for (c in ps[1,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  x1<-min(buf);y1<-ps[min(buf),2];
  minbuf <- min(buf)
  buf <- buf[-which.min(buf)];
  x2<-min(buf);y2<-ps[min(buf),2]
  for (c in ps[minbuf,1]:ps[min(buf),1]) {
    x3<-ps[c,1]
    y3<-ps[c,2]
    S <- NULL
    S <- abs((y3-y1)*(x2-x1)-(x3-x1)*(y2-y1))/2
    AB <- sqrt((x2-x1)^2+(y2-y1)^2)
    d <- S/AB
    dlist <-rbind(dlist,data.frame("x" = ps[c,1], "y" = ps[c,2], "distance" = d))
  }
  SP <- rbind(SP, c(x = dlist[which.max(dlist$distance),1],y = dlist[which.max(dlist$distance),2]))
  # 第九點
  thed <- dlist[which.max(dlist$distance),1]
  SP <- SP[order(SP$x),]
  return(SP)
  # SP <- data.frame(T = sort(SP$x),A = ps[sort(SP$x),2])
}

input <- ps[1,1]:ps[100,1]
input <- data.frame(x = input, y = ps[input,2])
GGG <- PIP(input)
