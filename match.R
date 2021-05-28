library(quantmod)
library(xts)
library(zoo)
          rm(list = ls())
ptm <- proc.time()#當前程序消耗CPU總時間

tw2395 <- read.csv("taiwan50/AC_2395.csv", encoding = "UTF-8")

tw2395[,1] <- gsub('/', '-', tw2395[,1])#把數列1的/替換成-

tw2395 <- data.frame("date" = tw2395[,1], "colsing_price" =  tw2395[,2])#轉換矩陣


rownames(tw2395) = tw2395[,1]#數列名稱改成第一行內容
as.xts(tw2395)
#tw2395 = getSymbols("2330.TW", from="2000-01-01", to="2015-12-31", src="google")
#plot(tw2395)
#chart_Series(tw2395)

# closing <- tw2395[,4]
closing <- tw2395[,2]#把tw2395第二行放進closing
closing <- as.matrix(closing)#把closing矩陣化
rownames(closing) <- 1:nrow(closing)#把closing列名改成1~n(預設就是了，可能多餘)
colnames(closing) <- ("y")#把closing行名改成y
#plot(closing)
ps <- data.frame(x=1:nrow(closing), y=closing)#做資料表ps，行x是1~n，行y是closing值
row.names(ps) <- row.names(tw2395)#把tw2395的行名給ps

# PIP <- function(ps){
#
#   interp <- NULL
#   breakpoints <- NULL
#
#   # ps <- as.matrix(p)
#
#
#   SP <- data.frame(x = ps[1,"x"], y = ps[1,"y"])
#   SP <- rbind(SP, c(x = ps[nrow(ps),"x"],y = ps[nrow(ps),"y"]))
#
#
#   interp <- approx(x=c(ps[1,"x"], ps[nrow(ps),"x"]),
#                    # interp內插（approx），x=ps的第一個x到ps最大行的x值的向量（c）
#                    y=c(ps[1,"y"],ps[nrow(ps),"y"]), n=nrow(ps))
#   # y=第1y到最後行y的向量，x&y分割成nrow(ps)等分
#   # plot(interp)
#
#
#
#   interp <- do.call(cbind, interp)
#   # 將interp所有參數執行cbind存到interp，x,y透過column合併<從value變data
#   # 現在interp是第一點到最後一點連成的線
#   # plot(interp)
#
#
#   breakpoints <- c(1, nrow(ps))
#   # breakpoints是1～ps（2664）的column
#   breakpoints
#
#   dd <- 0
#   while(dd < 7){
#     dt <- sqrt(rowSums((ps - interp)^2))
#     # 距離dt是加起來的平方根(ps減interp的差)
#     # plot(dt)
#
#
#     ind <- which.max(dt)
#     if(ind == 1 | ind == nrow(ps)){
#       ind <- which.max(dt[dt!=max(dt)] )
#     }else{
#       ind <- which.max(dt)
#     }
#     # 17/06/25>>dt等於nrow跟1會失敗
#     # ins是距離最大的ds
#     # ind在496時距離最大
#     # SP <- rbind(SP, c(ind, ps[ind,"y"]))
#     SP <- rbind(SP, c(ps[ind,"x"], ps[ind,"y"]))#2017/05/24
#     ends <- c(min(ind-breakpoints[breakpoints<ind]), min(breakpoints[breakpoints>ind]-ind))
#     # ends是到那點有多少，那點過後有多少
#     ends
#
#
#
#
#     leg1 <- approx(x=c(ps[ind-ends[1],"x"], ps[ind,"x"]),
#                    y=c(ps[ind-ends[1],"y"], ps[ind,"y"]), n=ends[1]+1)
#     leg2 <- approx(x=c(ps[ind,"x"], ps[ind+ends[2],"x"]),
#                    y=c(ps[ind,"y"], ps[ind+ends[2],"y"]), n=ends[2])
#     interp[(ind-ends[1]):ind, "y"] <- leg1$y
#     interp[(ind+1):(ind+ends[2]), "y"] <- leg2$y
#     breakpoints <- c(breakpoints, ind)
#     dd <- dd+1
#   }
#   # plot(interp)
#   SP <- SP[order(SP$x),]
#   return(SP)
#   # SP <- data.frame(T = sort(SP$x),A = ps[sort(SP$x),2])
# }

# ============================================================

PIP <- function(zzz){
  SP <- data.frame(x = ps[1,"x"], y = ps[1,"y"])
  # 第一點
  SP <- rbind(SP, c(x = ps[nrow(zzz),"x"],y = ps[nrow(zzz),"y"]))
  # 第二點
  x1<-SP[1,1];x2<-SP[2,1];y1<-SP[1,2];y2<-SP[2,2]
  dlist<-NULL
  for (c in ps[1,1]:ps[nrow(zzz),1]) {
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
  for (c in ps[thed,1]:ps[nrow(zzz),1]) {
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
# ====================================

patterntemplate <- function(SP, ps){
# 把ps經過pip處理後的SP與模版比對，
# 來得到最佳模版跟該段ps的適應值的函數
  # source("getstcokPIP.R")
  library(scales)
  # source("fitness.R")
  # SP <- PIP(ps)

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
#####################################################
### rescale數值需要檢查 17/11/16
#####################################################

  # TDeight <- rescale(TDeight, to=c(1,9))
  #取得型態模版temporal所需的八分位點TDeight
# windows 高0-1 寬0-1

  # rangeDM <- data.frame(T = TDeight, A = ps[TDeight,2])
  # top <- rangeDM[which.max(rangeDM[,2]),2]
  # bottom <- rangeDM[which.min(rangeDM[,2]),2]

  # template <- quantile(c(-1,1),c(.0, .50, .85, .85, .50, .85, .85, .50, .0))
  # template <- data.frame(T = TDeight, A = template)
  # 形成模版DoubleTop
  TCSP <- SP
  ACSP <- SP
  # SP$A <- round(SP$A)
  # SP$A <- 2*(SP$A-min(SP$A))/(max(SP$A)-min(SP$A))-1
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


  # Temporal Control
  TC <- function(SP){
    slen <- length(TCSP[1,1]:TCSP[nrow(SP),1])
    # 實際分割長度
    dlc <- 2
    # TC控制力度
    theta1 <- dlen/dlc
    d1 <- slen-dlen
    # D1趨近0越好
    TC <- 1-exp(-(d1/theta1)^2)
    # 1-e^-(d1/theta1)
    return(TC)
  }
  # TC正確17/11/02
  # Amplitude Control
  AC <- function(SP){
    fr <-(ACSP[which.max(SP$A),2]-ACSP[which.min(SP$A),2])/ACSP[which.min(SP$A),2]
    dfr <- 50
    # desired minimum fluctuation rate
    # 分段最大Y分段最小Y/分段最小Y
    dac <- 2
    d2 <- fr-dfr
    # D2越大越好
    theta2 <- dfr/dac
    AC <- (1-(1/(1+exp(-(d2/theta2)))))/18
    return(AC)
  }
  # AC正確17/11/02
  # 因其數值以千為變化單位，故AC小余0.1
  fitness <- function(SP, template){
    # SP$T <- rescale(SP$T, to=c(0,180))
    # template$T <- rescale(template$T, to=c(0,180))
    AD <- sqrt((1/nrow(SP))*(sum((SP$A - template$A)^2)))
    TD <- sqrt((1/(nrow(SP)-1))*(sum((SP$T - template$T)^2)))
    # AD <- sqrt(sum((SP$A - template$A)^2))
    # TD <- sqrt(sum((SP$T - template$T)^2))
    w <- 0.4
    DM <- (w*AD)+((1-w)*TD)+TC(SP)+AC(SP)
    # DM <- (w*AD)+((1-w)*TD)
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
}

# ====================================

Popsize = 50 #種群大小
Maxgen = 1000 #最多幾代
Minfitness = 0 #最好子代
Rc = 0.4
Ra = 0.5
dlen = 180 #窗口大小
crossoverrate = 0.4 #突變率

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
G1 <- list("C1" = C1,"C2" = C2,"C3" = C3,
           "C4" = C4,"C5" = C5,"C6" = C6,
           "C7" = C7,"C8" = C8,"C9" = C9,
           "C10" = C10,"C11" = C11,"C12" = C12,
           "C13" = C13,"C14" = C14,"C15" = C15,
           "C16" = C16,"C17" = C17,"C18" = C18,
           "C19" = C19,"C20" = C20,"C21" = C21,
           "C22" = C22,"C23" = C23,"C24" = C24,
           "C25" = C25,"C26" = C26,"C27" = C27,
           "C28" = C28,"C29" = C29,"C30" = C30,
           "C31" = C31,"C32" = C32,"C33" = C33,
           "C34" = C34,"C35" = C35,"C36" = C36,
           "C37" = C37,"C38" = C38,"C39" = C39,
           "C40" = C40,"C41" = C41,"C42" = C42,
           "C43" = C43,"C44" = C44,"C45" = C45,
           "C46" = C46,"C47" = C47,"C48" = C48,
           "C49" = C49,"C50" = C50)

# 產生第一代
segmentmatch <- function(CXX){
  D <- NULL
  as.data.frame(D)
  for(d in c(1:(nrow(CXX)-1))){
  # for(sd in c(1:ds)){
    DXX <- data.frame(ps[CXX[d,1]:CXX[(d + 1),1],])
    DXX[,1] <- 1:length(DXX$x)
    DX <- patterntemplate(PIP(DXX), DXX)
    # 分段與22個模版比對的適應值取最小值
    D <- rbind(D,DX)
  }
  # 17/10/23 BUG 越後面段數fit越高
  # 17/07/25，BUG D只有2分段
  return(D)
  # 24個分段
}
#板模比對↓
for(du in c(1:50)){
  U <- paste("C", du, sep="")
  assign(paste("D", du, sep=""),segmentmatch(get(U)))
}
# 產生D：將所有分段匹配一個最符合pattern並得到適應值
#segmentmatch(C7)


sum_fit <- function(DXXX){
  DXXX[,2] <- as.numeric(as.character(DXXX[,2]))
  DXXX[,1] <- c(1:nrow(DXXX))
  DSF <- rbind(colSums(DXXX)/(nrow(DXXX)))
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

# 取得C1～C50的染色體的適應值
# 取平均值---------


# Selection best
sel <- DSF[which.min(DSF[,2]),1]
sel <- cbind("Ganeration" = 1, "pattern" = sel, "fitness" = as.character(min(DSF[,2])))
MinfitG1 <- sel
sel <- get(paste("C",which.min(DSF[,2]), sep=""))
# 初始最好是CXX，第一世代
MinfitG <- MinfitG1
GD1 <- paste("D",which.min(DSF[,2]), sep="")
GD1 <- get(GD1)
GD2 <- NULL

GU <- 2
assign(paste("G",GU, sep=""),list(C1 = sel))
# 比對所有染色體適應值，取平均最低為C1（C1不會經歷交配突變取代）
# 新一代C1先儲存在XXXXXXX裡，等49個子代做完，最後才成為新一代C1
# }
# 將適應值歸一化
MinC1 <- sel
Gener <- NULL
repeat {
assign(paste("G",GU, sep=""),list(Cr = MinC1))
# roulette selection
ADSF <- as.data.frame(DSF)
ADSF$fitness <- as.numeric(as.character(ADSF$fitness))
ADSF <- ADSF[-which.min(ADSF$fitness),]
row.names(ADSF) <- c(1:nrow(ADSF))
ADSF$V1 <- as.numeric(row.names(ADSF))
ADSF$fitness <- (1/ADSF$fitness)
ADSF$fitness <- (ADSF$fitness/colSums(ADSF)[2])
# ADSF$fitness <- (ADSF$fitness-min(ADSF$fitness))/(max(ADSF$fitness)-min(ADSF$fitness))
# rou <- 1/colSums(ADSF)[2]
# ADSF <- as.data.frame(t(ADSF))
sample(c(1:49),size = 2, replace = TRUE,prob = c(ADSF$fitness))
# 最好的剔除
# 全數*上1/和
# 標準化後做輪盤法則

# 下一代的C1是這代最小的
# Crossover



for(Cr in c(1:10)){
  Cparent <- as.matrix(paste("C", sample(c(1:49),size = 2, replace = TRUE,prob = c(ADSF$fitness)),sep=""))
  Cparent
  # Cparent <- as.matrix(sample(c(1:50),size = 2, replace = TRUE,prob = c(ADSF[2,]*rou)))
  # get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][,1]
  # get(paste("G", GU-1, sep = ""))
  CutPt <- round(runif(1, 1, nrow(ps)))
  # Cnew1 <-c(get(Cparent[1])[which(get(Cparent[1])[,1]<CutPt),1],get(Cparent[2])[which(get(Cparent[2])[,1]>CutPt),1])
  Cnew1 <-c(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][,1]<CutPt),1],get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][,1]>CutPt),1])
  Cnew1 <- as.data.frame(Cnew1)
  Cnew1 <- as.data.frame(cbind(T = Cnew1[,1], A = ps[Cnew1[,1],2]))
   # 不會出現Cparent[1]末端小於Caprent[2]前端的情況，他們都與CutPt比大小
  # Cutpt BUG 17/10/27
  Cnew2 <-c(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][,1]<CutPt),1],get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][,1]>CutPt),1])
  Cnew2 <- as.data.frame(Cnew2)
  Cnew2 <- as.data.frame(cbind(T = Cnew2[,1], A = ps[Cnew2[,1],2]))
  # assign(paste("C", (Cr+10), sep=""),Cnew2)
  # assign(paste("G", GU, sep = "")[Cr+10][[1]],Cnew2)
  tar <- NULL
  LA <- length(Cnew1$T)
  FA <- length(Cnew2$T)
  for (sl in c(2:(LA-1))) {
    tar <- rbind(tar,c(length(Cnew1$T[sl]:Cnew1$T[sl-1]),length(Cnew1$T[sl]:Cnew1$T[sl+1])))
  }
  for (s2 in c(2:(FA-1))) {
    tar <- rbind(tar,c(length(Cnew2$T[s2]:Cnew2$T[s2-1]),length(Cnew2$T[s2]:Cnew2$T[s2+1])))
  }
  if(any(tar<=10)){
    repeat{
      Cparent <- as.matrix(paste("C", sample(c(1:49),size = 2, replace = TRUE,prob = c(ADSF$fitness)),sep=""))
      CutPt <- round(runif(1, 1, nrow(ps)))
      Cnew1 <-c(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][,1]<CutPt),1],get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][,1]>CutPt),1])
      Cnew1 <- as.data.frame(Cnew1)
      Cnew1 <- as.data.frame(cbind(T = Cnew1[,1], A = ps[Cnew1[,1],2]))
      Cnew2 <-c(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[2]][[1]][,1]<CutPt),1],get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][which(get(paste("G", GU-1, sep = ""))[Cparent[1]][[1]][,1]>CutPt),1])
      Cnew2 <- as.data.frame(Cnew2)
      Cnew2 <- as.data.frame(cbind(T = Cnew2[,1], A = ps[Cnew2[,1],2]))
      tar <- NULL
      LA <- length(Cnew1$T)
      FA <- length(Cnew2$T)
        for (sl in c(2:(LA-1))) {
          tar <- rbind(tar,c(length(Cnew1$T[sl]:Cnew1$T[sl-1]),length(Cnew1$T[sl]:Cnew1$T[sl+1])))
        }
        for (s2 in c(2:(FA-1))) {
          tar <- rbind(tar,c(length(Cnew2$T[s2]:Cnew2$T[s2-1]),length(Cnew2$T[s2]:Cnew2$T[s2+1])))
        }
      if(any(tar<=10)==FALSE) break
      }
  }
  assign(paste("G",GU, sep=""),append(get(paste("G",GU, sep="")),list(Cr = Cnew1)))
  assign(paste("G",GU, sep=""),append(get(paste("G",GU, sep="")),list(Cr = Cnew2)))
}

# 更改世代種群裡的染色體名稱

# 不排除重複交配應計數世代
# 有可能交配後長度小於9
# 17/08/05 交配後index不一樣造成length(CXX$T)值為空
# 做20次成為子代


# # Mutation
for(M in c(22:50)){
Mparent <- as.matrix(paste("C", sample(c(1:49),size = 1, replace = TRUE,prob = c(ADSF$fitness)), sep=""))
Mparent <- get(paste("G", GU-1, sep = ""))[[Mparent]]
# # 輪盤C1-50之中的2個
Ra = 0.5
Rb = 1-Ra
Nr = runif(1,0,1)
buf <- round(runif(1,1,nrow(ps)))
# # 隨機增加的那個點

if(Nr<=Ra){
  Cnew3 <- rbind(Mparent, c(ps[buf,1],ps[buf,2]))
  Cnew3 <- Cnew3[order(Cnew3$T),]
  rownames(Cnew3) <- 1:length(Cnew3$A)
  # 如果Cnew3任一點與前後差不到10，repeat
  tar <- NULL
  LA <- length(Cnew3$T)
  for (sl in c(2:(LA-1))) {
    tar <- rbind(tar,c(length(Cnew3$T[sl]:Cnew3$T[sl-1]),length(Cnew3$T[sl]:Cnew3$T[sl+1])))
  }
  if(any(tar<=10)){
  repeat {
    buf <- round(runif(1,1,nrow(ps)))
    Cnew3 <- rbind(Mparent, c(ps[buf,1],ps[buf,2]))
    Cnew3 <- Cnew3[order(Cnew3$T),]
    rownames(Cnew3) <- 1:length(Cnew3$A)
  ###########################
      tar <- NULL
      LA <- length(Cnew3$A)
      for (sl in c(2:(LA-1))) {
      tar <- rbind(tar,c(length(Cnew3$T[sl]:Cnew3$T[sl-1]),length(Cnew3$T[sl]:Cnew3$T[sl+1])))
      }
    if(any(tar<=10)==FALSE) break
    }
  }
# }else if(length(Cnew3[which(Cnew3$T==buf),1]:Cnew3[(which(Cnew3$T==buf)-1),1])<10){
#   repeat {
#     buf <- round(runif(1,1,nrow(ps)))
#     Cnew3 <- rbind(get(Mparent), c(ps[buf,1],ps[buf,2]))
#     Cnew3 <- Cnew3[order(Cnew3$T),]
#     rownames(Cnew3) <- 1:length(Cnew3$A)
#     if(length(Cnew3[which(Cnew3$T==buf),1]:Cnew3[(which(Cnew3$T==buf)-1),1])>10) break
#   }
# }else if(length(Cnew3[which(Cnew3$T==buf),1]:Cnew3[(which(Cnew3$T==buf)+1),1])<10){
#   repeat {
#     buf <- round(runif(1,1,nrow(ps)))
#     Cnew3 <- rbind(get(Mparent), c(ps[buf,1],ps[buf,2]))
#     Cnew3 <- Cnew3[order(Cnew3$T),]
#     rownames(Cnew3) <- 1:length(Cnew3$A)
#     if(length(Cnew3[which(Cnew3$T==buf),1]:Cnew3[(which(Cnew3$T==buf)-1),1])>10) break
#   }
# }
}else{

Cnew3 <- Mparent[-round(runif(1,2,(nrow(Mparent)-1))), ]
rownames(Cnew3) <- 1:length(Cnew3$A)
  # if(round(runif(1,1,nrow(Mparent))) %in% c(1,nrow(ps))){
  #   repeat{
  #   Cnew3 <- Mparent[-round(runif(1,1,nrow(Mparent))), ]
  #   rownames(Cnew3) <- 1:length(Cnew3$A)
  #   if (round(runif(1,1,nrow(Mparent))) %in% c(1,nrow(ps)) == FALSE) break
  #   }
  # }
}
#解決頭跟尾被突變選中消失 17/11/17
# Cnew3 <- data.frame(T = Cnew3[,1], A = ps[Cnew3[,1],2])
assign(paste("G",GU, sep=""),append(get(paste("G",GU, sep="")),list(Cr = Cnew3)))
}
GUU <- get(paste("G",GU, sep=""))
names(GUU) <- c("C1", "C2", "C3", "C4", "C5", "C6",
                "C7", "C8", "C9", "C10", "C11",
                "C12", "C13", "C14", "C15", "C16",
                "C17", "C18", "C19", "C20", "C21",
                "C22", "C23", "C24", "C25", "C26",
                "C27", "C28", "C29", "C30", "C31",
                "C32", "C33", "C34", "C35", "C36",
                "C37", "C38", "C39", "C40", "C41",
                "C42", "C43", "C44", "C45", "C46",
                "C47", "C48", "C49", "C50")
assign(paste("G",GU, sep=""),GUU)
# 做29次
#################################################
######### 有時無限回圈
#################################################
if(is.null(GD2)){
  for(du in c(2:50)){
    # U <- paste("C", du, sep="")
    DG <- paste("G",GU, sep="")
    # assign(paste("D", du, sep=""),segmentmatch(get(U)))
    U <- get(DG)[[du]]
    assign(paste("GD", du, sep=""),segmentmatch(U))
  }
}else{
  which.min(DSF[,2])
  GD1 <- get(paste("GD",which.min(DSF[,2]), sep=""))
  for(du in c(2:50)){
  DG <- paste("G",GU, sep="")
  U <- get(DG)[[du]]
  assign(paste("GD", du, sep=""),segmentmatch(U))
  }
}

# 第一代GD的產生，取代初始的D

DS <- NULL
for(D in c(1:50)){

  DS <- rbind(DS, paste("GD", D, sep=""))
}
DSF <- NULL
for(F in c(1:50)){
  DSF <- rbind(DSF,c(paste("DF", F, sep=""),sum_fit(get(DS[F,1]))[1,2]))
}
#產生GU世代的D



# 加總一段染色體的適應值
# Selection best
sel <- DSF[which.min(DSF[,2]),1]
sel <- cbind("Ganeration" = GU, "pattern" = sel, "fitness" = as.character(DSF[which.min(DSF[,2]),2]))
# as.character(min(DSF[,2]))
MinfitG <- rbind(MinfitG,sel)
MinfitG <- as.data.frame(MinfitG)

NG <- paste("G",GU, sep="")
choo <- which.min(DSF[,2])
Minone <- get(NG)[[choo]]
assign(paste("MGD", 1, sep=""),segmentmatch(Minone))
PTS <- paste("MGD", 1, sep="")
PTS <- get(PTS)
PTS <- PTS[1]
# 取得pattern序列

during <- which.min(DSF[,2])
during <- get(paste("G", GU, sep = ""))[[during]]
Pduring <- NULL
for(d in c(1:(nrow(during)-1))){
  Pduring <- rbind(Pduring, length(during[d,1]:during[(d+1),1]))
}
# 取得pattern持續時間
Pduring <- as.data.frame(Pduring)
PSout <- data.frame("pattern" = PTS$pattern,"during" = Pduring$V1)

MinC1 <- during

GC <- get(paste("G", GU, sep = ""))

Gener[[length(Gener)+1]] <- list(GC)


GU <- GU+1

# if (GU==1001) break
if (GU==2) break
}
rownames(MinfitG) <- c(1:nrow(MinfitG))
print(MinfitG)
print(PSout)
# Minfit <- cbind("pattern" = sel, "fitness" = as.character(min(DSF[,2])))
x <- as.matrix(MinfitG$Ganeration)
y <- as.matrix(MinfitG$fitness)
plot(x, y, type="l", xlab="Ganeration",
     ylab="fitness", ylim = c(0.12,0.2))

proc.time() - ptm
# write.csv(PSout, file = "PSout1124.csv")
# write.csv(MinfitG, file = "MinfitG1124.csv")

# yy <- data.frame(x = ps[U[5,1]:U[6,1],1], y = ps[U[5,1]:U[6,1],2])
# PIP(yy)
#
# ddddd <- data.frame(x = c(1:10),y = c(10,10,10,10,10,10,9,10,10,10))
# PIP(ddddd)
################################################
#21/05/17 問題:突變改變點數，要如何比對板模?
#
#
#

