##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  LENGTH-FREQUENCY ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfLens and pwf data.frames
## ===========================================================
source("Data_Init.R")
str(pwfLens)

## ===========================================================
## Source helper functions ... get pwfHist()
## ===========================================================
source("zzzHelpers.R")


## ===========================================================
## Find characteristics of the Coaster samples in 2008.
## ===========================================================
tmp <- pwfLens %>%
  filter(vessel=="Coaster") %>%
  filter(year==2008)
xtabs(~OP_DATE,data=tmp)
Summarize(~beg_depth,data=tmp)
Summarize(~avg_depth,data=tmp)
Summarize(~end_depth,data=tmp)
## ===========================================================
## Examine how the distribution of haphazardly sampled fish in
##   the pwf data.frame compare to entire sample in pwfLens in
##   2013.
##
## !! The subsample slighly over-represented for the smallest 
## !!   fish (<75 mm), under-represented for the 90-105 mm
## !!   fish, and over-represented for the larger (>110) fish.
## !!   The mis-representation was <5% for all lengths except
## !!   for the 105-mm fish which were under-represented by
## !!   about 15%.
## ===========================================================
## isolate the 2013 fish from pwfLens and add a 5-mm length
##   category variable
brks <- seq(50,155,5)
pwfLens13 <- pwfLens %>% 
  filter(year==2013) %>%
  mutate(lcat5=lencat(tl,breaks=brks,as.fact=TRUE,drop.levels=FALSE))
## add the same length category variable to pwf
pwf <- mutate(pwf,lcat5=lencat(tl,breaks=brks,as.fact=TRUE,drop.levels=FALSE))
## tabulate each group, convert to a percentage
tmp <- prop.table(cbind(sample=xtabs(~lcat5,data=pwfLens13),
                        subsample=xtabs(~lcat5,data=pwf)),
                  margin=2)*100
## find diff
tmp <- cbind(tmp,diff=tmp[,"subsample"]-tmp[,"sample"])
## plots
plot(tmp[,"sample"]~as.numeric(rownames(tmp)),type="l",lwd=2,xlab="TL",ylab="Percentage")
lines(tmp[,"subsample"]~as.numeric(rownames(tmp)),lwd=2,col="red")
plot(tmp[,"diff"]~as.numeric(rownames(tmp)),type="l",lwd=2,xlab="TL",ylab="Subsample-Sample")
abline(h=0,lwd=2,lty=3)


## ===========================================================
## A test of what the density in a histogram is, or, how do
##   you get the percentage of a bar
## What was learned here was used in zzzHelpers.R
## ===========================================================
h <- hist(~tl,data=pwfLens,breaks=seq(20,224,2))
h$counts/sum(h$counts)*100-(2*h$density*100)

h$counts <- h$counts/sum(h$counts)*100
plot(h)


## ===========================================================
## Make a matrix of LF histograms for 2006-2013 (last eight
##   years but not including 2014).  This will be in the
##   manuscript.  See below for histograms of 1991-2013.
## ===========================================================
yrs2 <- 2006:2013
pwfLens2 <- pwfLens %>%
  filter(year %in% yrs2) %>%
  mutate(fyear=droplevels(fyear)) %>%
  arrange(year,tl)
str(pwfLens2)


## -----------------------------------------------------------
## Put the result into a PDF file
## -----------------------------------------------------------
figw <- 24/2.54
figh <- figw*0.8
ptsz <- 18
pdf("Figs/Fig_LF.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
clr <- "gray50"
brks <- seq(20,224,2)
xlmt <- c(30,160)
len.ticks <- c(30,seq(50,175,25))
freq.ticks <- seq(0,10,2)
prob <- TRUE
ylmt <- range(freq.ticks)
# number of rows and cols of actual plots
nrow <- 4
ncol <- 2
# sets the base width for each plot
basew <- 5.0
baseh <- basew*0.6

## -----------------------------------------------------------
## Make a base plot
## -----------------------------------------------------------
# make the layout
layout(rbind(cbind(rep(1,nrow),                           # left column (not bottom row) for y-axis label
                   matrix(3:10,nrow=nrow,byrow=FALSE)),   # middle, the plots
             c(0,rep(2,ncol))),                           # bottom row for x-axis
       widths=c(1,basew,rep(basew,ncol-1),1),             # control widths
       heights=c(rep(baseh,nrow-1),baseh,1),              #   and heights
       respect=TRUE)
# put on some axis labels
par(mar=c(0,0,0,0))
plot.new(); text(0.6,0.5,"Percentage of Catch",srt=90,cex=1.25)
plot.new(); text(0.5,0.6,"Total Length (mm)",cex=1.25)

## -----------------------------------------------------------
## Put on individual histograms
## -----------------------------------------------------------
for (i in 1:length(yrs2)) {
  pwfHist(pwfLens2,yrs2[i],brks,xlmt,ylmt,clr,
          ifelse((i/nrow) %in% (1:ncol),TRUE,FALSE),
          ifelse((i/nrow) <= 1,TRUE,FALSE),
          len.ticks,freq.ticks
  )
  axis(1,seq(20,225,5),labels=NA,tcl=-0.1)
  abline(v=75,col="black",lwd=2,lty=2)
}

## -----------------------------------------------------------
## Close the device to make the actual PDF file
## -----------------------------------------------------------
dev.off()







## ===========================================================
## Make a matrix of LF histograms for every available year.
## ===========================================================
## -----------------------------------------------------------
## From 1974-1989, the PWF were measured only to the nearest
##   5 mm.  Added random 1-mm to each
## -----------------------------------------------------------
#tmp <- pwfLens$year %in% 1974:1989
#pwfLens$tl[tmp] <- pwfLens$tl[tmp] + sample(0:4,length(pwfLens$tl[tmp]),replace=TRUE)

## -----------------------------------------------------------
## Restrict to 1990-2013 when 1-mm TLs were taken.
## -----------------------------------------------------------
yrs <- 2000:2014

clr <- "gray50"
brks <- seq(20,224,2)
xlmt <- c(30,175)
len.ticks <- c(30,seq(50,175,25))
freq.ticks <- seq(0,10,2)
prob <- TRUE
ylmt <- range(freq.ticks)

pdf("Figs/Fig_LF_suppl.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)
pgs <- floor(length(yrs)/8)
if(length(yrs)/8-pgs!=0) pgs <- pgs+1

for (j in 1:pgs) {
  ## -----------------------------------------------------------
  ## Make a base plot
  ## -----------------------------------------------------------
  # make the layout
  layout(rbind(cbind(rep(1,nrow),                           # left column (not bottom row) for y-axis label
                     matrix(3:10,nrow=nrow,byrow=FALSE)),   # middle, the plots
               c(0,rep(2,ncol))),                           # bottom row for x-axis
         widths=c(1,basew,rep(basew,ncol-1),1),             # control widths
         heights=c(rep(baseh,nrow-1),baseh,1),              #   and heights
         respect=TRUE)
  # put on some axis labels
  par(mar=c(0,0,0,0))
  plot.new(); text(0.6,0.5,"Percentage of Catch",srt=90,cex=1.25)
  plot.new(); text(0.5,0.6,"Total Length (mm)",cex=1.25)
  
  ## -----------------------------------------------------------
  ## Put on individual histograms
  ## -----------------------------------------------------------
  yrs2 <- yrs[((j-1)*8+1):(j*8)]
  for (i in 1:length(yrs2)) {
    pwfHist(pwfLens,yrs2[i],brks,xlmt,ylmt,clr,
            ifelse((i/nrow) %in% (1:ncol),TRUE,FALSE),
            ifelse((i/nrow) <= 1,TRUE,FALSE),
            len.ticks,freq.ticks
    )
    axis(1,seq(20,225,5),labels=NA,tcl=-0.1)
    abline(v=75,col="black",lwd=2,lty=2)
  }
}

## -----------------------------------------------------------
## Close the device to make the actual PDF file
## -----------------------------------------------------------
dev.off()
