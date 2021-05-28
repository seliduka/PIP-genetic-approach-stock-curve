tst <- data.frame(x=1:100, y=rnorm(100, 4*sin(seq(1,4*pi,len=100)), 1))
tst <- as.matrix(tst)

pip <- function(ps, interp=NULL, breakpoints=NULL) {
  if (missing(interp)) {
    interp <- approx(x=c(ps[1,"x"], ps[nrow(ps),"x"]), 
                     y=c(ps[1,"y"],ps[nrow(ps),"y"]), n=nrow(ps))
    interp <- do.call(cbind, interp)
    breakpoints <- c(1, nrow(ps))
  } else {
    ds <- sqrt(rowSums((ps - interp)^2))  # close by euclidean distance
    ind <- which.max(ds)
    ends <- c(min(ind-breakpoints[breakpoints<ind]), min(breakpoints[breakpoints>ind]-ind))
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

res <- constructPIP(tst, times=5)
plot(tst)
points(res$interp, col="blue", type="l")

