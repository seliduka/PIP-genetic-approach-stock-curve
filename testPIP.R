
library(quantmod)
library(xts)
library(zoo)


           # rm(list = ls())
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


