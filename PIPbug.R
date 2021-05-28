source("GA.R")

library(quantmod)
library(xts)
library(zoo)


rm(list = ls())
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


# PIP <- function(ps){
  
  interp <- NULL
  breakpoints <- NULL
  
  GG <- data.frame(x = D13[1,"x"], y = D13[1,"y"])
  GG <- rbind(GG, c(x = D13[nrow(D13),"x"],y = D13[nrow(D13),"y"]))
  
  interp <- approx(x=c(D13[1,"x"], D13[nrow(D13),"x"]),
                   # interp內插（approx），x=ps的第一個x到ps最大行的x值的向量（c）
                   y=c(D13[1,"y"],D13[nrow(D13),"y"]), n=nrow(D13))
  # y=第1y到最後行y的向量，x&y分割成nrow(ps)等分
  plot(interp)
  
  interp <- do.call(cbind, interp)
  # 將interp所有參數執行cbind存到interp，x,y透過column合併<從value變data
  # 現在interp是第一點到最後一點連成的線
  plot(interp)
 
  if(D13[1,"x"]==1){
    
    breakpoints <- c(1, nrow(D13))

    dd <- 0
    while(dd < 7){
      dt <- sqrt(rowSums((D13 - interp)^2))
      # 距離dt是加起來的平方根(ps減interp的差)
      plot(dt)
      
      ind <- which.max(dt)
      GG <- rbind(GG, c(ind, D13[ind,"y"]))
      
      ends <- c(min(ind-breakpoints[breakpoints<ind]), min(breakpoints[breakpoints>ind]-ind))
      # ends是到那點有多少，那點過後有多少
      ends

      leg1 <- approx(x=c(D13[ind-ends[1],"x"], D13[ind,"x"]),
                     y=c(D13[ind-ends[1],"y"], D13[ind,"y"]), n=ends[1]+1)
      leg2 <- approx(x=c(D13[ind,"x"], D13[ind+ends[2],"x"]),
                     y=c(D13[ind,"y"], D13[ind+ends[2],"y"]), n=ends[2])
      interp[(ind-ends[1]):ind, "y"] <- leg1$y
      interp[(ind+1):(ind+ends[2]), "y"] <- leg2$y
      breakpoints <- c(breakpoints, ind)
      dd <- dd+1
    }
    plot(interp)
    SP <- data.frame(T = sort(SP$x),A = ps[sort(SP$x),2])
    
  }else{
    
    breakpoints <- c(1, nrow(D13))
    # breakpoints是1～ps（2664）的column
    breakpoints
    
    # dd <- 0
    # while(dd < 7){
    dt <- sqrt(rowSums((D13 - interp)^2))
    # 距離dt是加起來的平方根(ps減interp的差)
    plot(dt)
    
    
    ind <- which.max(dt)
    ind
    # 100
    
    
    
    ind <- D13[ind,"x"]
    ind
    
    GG <- rbind(GG, c(D13[ind,"x"], D13[ind,"y"]))############
    # 455 54.5
    ends <- c(min(D13[ind,"x"]-breakpoints[breakpoints<ind]), min(D13[nrow(D13),"x"]-ind))
    ends
    
    
    leg1 <- approx(x=c(D13[indmin-ends[1],"x"], D13[ind,"x"]),
                   y=c(D13[indmin-ends[1],"y"], D13[ind,"y"]), n=ends[1]+1)
    
    leg2 <- approx(x=c(D13[indmin,"x"], D13[indmin+ends[2],"x"]),
                   y=c(D13[indmin,"y"], D13[indmin+ends[2],"y"]), n=ends[2])
    interp[(indmin-ends[1]):indmin, "y"] <- leg1$y
    interp[(indmin+1):(indmin+ends[2]), "y"] <- leg2$y
    breakpoints <- c(breakpoints, indmin)
    # dd <- dd+1
  }
  
  
  # }
  plot(interp)
  GG <- data.frame(T = sort(GG$x),A = D13[sort(GG$x),2])
  
# }
# SP <- PIP(ps)

