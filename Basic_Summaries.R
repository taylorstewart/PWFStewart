##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  BASIC SUMMARIES ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfLens and pwfWL data.frames
## ===========================================================
source("Data_Init.R")
pwfWL
pwfLens

## ===========================================================
## Lengths of all PWF in 2013
## ===========================================================
Summarize(~tl,data=subset(pwfLens,year==2013))

## ===========================================================
## Lengths and Weights in subsample of PWF
## ===========================================================
Summarize(~tl,data=pwfWL)
Summarize(~wt,data=pwfWL)

## ===========================================================
## Sex ratio of subsampled fish
## ===========================================================
Summarize(~sex,data=pwfWL)
## -----------------------------------------------------------
## With N/A fish removed
## -----------------------------------------------------------
Summarize(~sex,data=Subset(pwfWL,sex!="N/A"))

## ===========================================================
## Comparison of lengths between male and females
## ===========================================================
## -----------------------------------------------------------
## Isolate males and females
## -----------------------------------------------------------
tmpF <- filter(pwfWL,sex=="Female")
tmpM <- filter(pwfWL,sex=="Male")
## -----------------------------------------------------------
## Kolmogorov-Smirnov test of the distribution
## -----------------------------------------------------------
ks.test(tmpF$tl,tmpM$tl)
plot(ecdf(tmpF$tl),pch=".",verticals=TRUE,lwd=2,main="",xlab="Total Length (mm)")
plot(ecdf(tmpM$tl),add=TRUE,pch=".",verticals=TRUE,lwd=2,col="red")
legend("topleft",c("Female","Male"),lwd=2,col=c("black","red"),bty="n")
## -----------------------------------------------------------
## Wilcoxon test of the medians
## -----------------------------------------------------------
wilcox.test(tmpF$tl,tmpM$tl)
Summarize(tl~sex,data=pwfWL)
