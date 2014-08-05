##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  BASIC SUMMARIES ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfLens and wf data.frames
## ===========================================================
source("Data_Init.R")
pwf
pwfLens

## ===========================================================
## Lengths of all PWF in 2013
## ===========================================================
Summarize(~tl,data=subset(pwfLens,year==2013))

## ===========================================================
## Lengths and Weights in subsample of PWF
## ===========================================================
Summarize(~tl,data=pwf)
Summarize(~wt,data=pwf)

## ===========================================================
## Sex ratio of subsampled fish
## ===========================================================
Summarize(~sex,data=pwf)
## -----------------------------------------------------------
## With N/A fish removed
## -----------------------------------------------------------
Summarize(~sex,data=subset(pwf,sex!="N/A"))

## ===========================================================
## 
## ===========================================================
tmpF <- filter(pwf,sex=="Female")
tmpM <- filter(pwf,sex=="Male")
ks.test(tmpF$tl,tmpM$tl)
plot(ecdf(tmpF$tl))
plot(ecdf(tmpM$tl), add = TRUE, lty = "dashed")
wilcox.test(tmpF$tl,tmpM$tl)
Summarize(tl~sex,data=pwf)
