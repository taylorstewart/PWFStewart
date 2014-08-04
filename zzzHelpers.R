pwfHist <- function(df,yr,breaks,xlim,ylim,show.xaxis,show.yaxis,len.ticks,show.yr=TRUE,...) {
  par(mgp=c(0,0.4,0),tcl=-0.2,las=1,xaxs="i",yaxs="i")
  lm <- bm <- 1;   lrm <- btm <- 0.4
  if (show.xaxis & show.yaxis) { par(mar=c(bm,lm,btm,lrm))
  } else if (show.xaxis & !show.yaxis) { par(mar=c(bm,lrm,btm,lrm))
  } else if (!show.xaxis & show.yaxis) { par(mar=c(btm,lm,btm,lrm))
  } else if (!show.xaxis & !show.yaxis) { par(mar=c(btm,lrm,btm,lrm))
  }
  tmp <- Subset(df,year==yr)
  hist(~tl,data=tmp,breaks=brks,prob=TRUE,xlim=xlmt,ylim=ylmt,xaxt="n",yaxt="n",xlab="",ylab="",...)  
  freq.ticks <- seq(0,max(ylim),0.02)
  if (show.xaxis) { axis(1,len.ticks)
  } else { axis(1,len.ticks,FALSE)
  }
  if (show.yaxis) { axis(2,freq.ticks,freq.ticks*min(diff(breaks))*100)
  } else { axis(2,freq.ticks,FALSE)
  }
  if (show.yr) legend("topright",legend=yr,bty="n")
}
