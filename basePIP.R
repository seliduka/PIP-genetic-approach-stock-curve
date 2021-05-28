getPIPs <- function(x, y, distance = "EUK"){

  PIPs <- vector("list", 4)
  PIPs[[1]] <- c(1,length(y))


  for(i in 1:(length(y)-2)){

    switch(distance,
           EUK = (DISTANCE.F  <- EUK.distance.f),
           VD  = (DISTANCE.F  <- VD.distance.f )
    )

    PIPs <- helper.f(PIPs,x,y,DISTANCE.F)


  }

  return(PIPs)

}

helper.f <- function(PIPs,
                     x,
                     y,
                     DISTANCE.F){
  t <- sort(PIPs[[1]])
  gesamt <- NULL

  for(z in 1:(length(t)-1)){
    gesamt <- c(gesamt,DISTANCE.F(x,y,t[z],t[z+1]))
  }
  if(all(gesamt == 0)) return(PIPs)
  else PIPs[[1]] <- append(PIPs[[1]],which.max(gesamt))


  return(PIPs)
}



EUK.distance.f <- function(x,y,sI,eI){

  pointsbetween <- sI:eI

  erg <-
    sqrt((sI-x[pointsbetween])^2 + (y[sI]-y[pointsbetween])^2) +
    sqrt((eI-x[pointsbetween])^2 + (y[eI]-y[pointsbetween])^2)
  erg[1] <- 0
  erg <- erg[-length(erg)]
  return(erg)

}


VD.distance.f <- function(x,y,sI,eI){ #Start und Endindex
  erg <-
    abs(
      y[sI:eI]-(y[sI]+
                  (x[sI:eI]-x[sI]) *
                  ((y[eI]-y[sI]) / (x[eI]-x[sI]))
      )
    )
  erg <- erg[-length(erg)]
  return(erg)
}

#visualize
itertivePlotPIPS.f <- function(x,y,z){
  plot(x,y)
  lines(sort(PIPs[[1]][1:length(x)]),y[sort(PIPs[[1]][1:length(x)])] , col  = "azure3")
  lines(sort(PIPs[[1]][1:z]),y[sort(PIPs[[1]][1:z])])
}

x<-1:100 # "Time" (x)-axis
y<-sample(1:100) # "Data" y-axis
getPIPs(x,y,"EUK")

itertivePlotPIPS.f(x,y,10) # the 10 at the end means "take the first ten PIPs"
