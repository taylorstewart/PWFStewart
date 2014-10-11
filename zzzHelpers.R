pwfHist <- function(df,yr,brks,xlim,ylim,clr,show.xaxis,show.yaxis,len.ticks,freq.ticks,show.yr=TRUE,...) {
  par(mgp=c(0,0.4,0),tcl=-0.25,las=1,xaxs="i",yaxs="i")
  lm <- bm <- 1.1;   lrm <- btm <- 0.8
  if (show.xaxis & show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  } else if (show.xaxis & !show.yaxis) { par(mar=c(bm,lrm,btm,lrm))
  } else if (!show.xaxis & show.yaxis) { par(mar=c(btm,lm,btm,lrm))
  } else if (!show.xaxis & !show.yaxis) { par(mar=c(btm,lrm,btm,lrm))
  }
  tmp <- Subset(df,year==yr)
  h1 <- hist(tmp$tl,breaks=brks,plot=FALSE,right=FALSE)
  # replace counts with percentages for plotting ... this should be the same
  #   as the density if the density was multiplied by 2 (the width of a bar)
  #   This is easier to control the axis limits.
  n <- sum(h1$counts)
  h1$counts <- h1$counts/n*100
  plot(h1,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="",main="",col="gray90")
  tmp2 <- Subset(tmp,vessel!="Coaster")
  h2 <- hist(tmp2$tl,breaks=brks,plot=FALSE,right=FALSE)
  h2$counts <- h2$counts/n*100
  plot(h2,col=clr,add=TRUE)
  if (show.xaxis) {
    axis(1,len.ticks,pos=0)
  } else {
    axis(1,len.ticks,FALSE,pos=0)
  }
  if (show.yaxis) {
    axis(2,freq.ticks)
  } else { 
    axis(2,freq.ticks,FALSE)
  }
  if (show.yr) legend("topright",legend=yr,bty="n")
}
