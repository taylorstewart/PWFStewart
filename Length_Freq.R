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
pwfLens

## ===========================================================
## Source helper functions ... get pwfHist()
## ===========================================================
source("zzzHelpers.R")

## ===========================================================
## Get list of years available, restrict to last eight years,
##   order by year and length
## ===========================================================
( yrs <- as.numeric(levels(pwfLens$fyear)) )
yrs <- yrs[(length(yrs)-7):length(yrs)]
pwfLens <- pwfLens %>%
  filter(year %in% yrs) %>%
  arrange(year,tl)
pwfLens

## ===========================================================
## Make a matrix of LF histograms for the last eight years
## ===========================================================
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
xlmt <- c(40,150)
len.ticks <- seq(50,150,25)
prob <- TRUE
ylmt <- c(0,0.06)
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
for (i in 1:length(yrs)) {
  pwfHist(pwfLens,yrs[i],brks,xlmt,ylmt,col=clr,
          ifelse((i/nrow) %in% (1:ncol),TRUE,FALSE),
          ifelse((i/nrow) <= 1,TRUE,FALSE),
          len.ticks
  )
  abline(v=75,col="black",lwd=2,lty=2)
}

## -----------------------------------------------------------
## Close the device to make the actual PDF file
## -----------------------------------------------------------
dev.off()


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
