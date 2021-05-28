#install.packages("quantmod")
#install.packages("xts")
#install.packages("zoo")


library(quantmod) #股票模組
library(xts)
library(zoo)


rm(list = ls())
tw2395 = getSymbols('2395.TW', auto.assign = FALSE) #得到研華資料(來源不明)
plot(tw2395) #資料視覺化
chart_Series(tw2395) #quantmod模組的序列視覺化強化版

head(tw2395) #查看數據前六行

closing <- tw2395[,4] #把矩陣tw2395的第四行取出變成closing
closing <- as.matrix(closing)#轉換數列為矩陣
rownames(closing) <- 1:nrow(closing)#把矩陣排名稱改成1~盡頭
colnames(closing) <- ("y")#把矩陣列名改成y
plot(closing)#檢查成果

p <- data.frame(x=1:nrow(closing), y=closing)#宣告資料表P
p <- as.matrix(p)#轉換資料表p為矩陣

pip <- function(ps, interp=NULL, breakpoints=NULL) {

  counter = 0
  ind = 0
  SP <- data.frame(x = counter, y = ind)


# 函式名pip，引數ps，interp，breakpoints
  if (missing(interp)) {
# if(interp)是測試interp是否為一函數的引數
    interp <- approx(x=c(ps[1,"x"], ps[nrow(ps),"x"]),
# interp內插（approx），x=ps的第一個x到ps最大行的x值的向量（c）
                     y=c(ps[1,"y"],ps[nrow(ps),"y"]), n=nrow(ps))
# y=第1y到最後行y的向量，x&y分割成nrow(ps)等分
        interp <- do.call(cbind, interp)
# 將interp所有參數執行cbind存到interp，x,y透過column合併
    breakpoints <- c(1, nrow(ps))
# breakpoints是1～最大行數的向量
  } else {
    ds <- sqrt(rowSums((ps - interp)^2))
# 距離ds是加起來的平方根
    ind <- which.max(ds)
# ins是距離最大的ds

    counter <- counter+1
    SP <- rbind(SP, c(counter, ind))

    ends <- c(min(ind-breakpoints[breakpoints<ind]), min(breakpoints[breakpoints>ind]-ind))
# ends是
    leg1 <- approx(x=c(ps[ind-ends[1],"x"], ps[ind,"x"]),
                   y=c(ps[ind-ends[1],"y"], ps[ind,"y"]), n=ends[1]+1)
    leg2 <- approx(x=c(ps[ind,"x"], ps[ind+ends[2],"x"]),
                   y=c(ps[ind,"y"], ps[ind+ends[2],"y"]), n=ends[2])
    interp[(ind-ends[1]):ind, "y"] <- leg1$y
    interp[(ind+1):(ind+ends[2]), "y"] <- leg2$y
    breakpoints <- c(breakpoints, ind)
  }
  list(interp=interp, breakpoints=breakpoints)
}

constructPIP <- function(ps, times=10) {
  res <- pip(ps)
  for (i in 2:times) {
    res <- pip(ps, res$interp, res$breakpoints)
  }
  res
}

res <- constructPIP(p, times=10)
plot(p)
points(res$interp, col="blue", type="l")#繪圖
plot(res$interp, col="blue", type="l")

