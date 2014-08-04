##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  AGE-LENGTH KEY COMPARISONS ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfGrow data.frame
## ===========================================================
source("Data_Init.R")
head(pwfGrow,n=10)

## ===========================================================
## Load Additional Packages
## ===========================================================
library(nnet)      # for multinom()


## ===========================================================
## Isolate the male and female fish (remove the unknowns)
## ===========================================================
pwfALK <- filter(pwfGrow,sex!="Unknown")
xtabs(~sex,data=pwfALK)

## ===========================================================
## Statistically compare the observed ALKs using multinomial
##   regression models (as in Gerritsen et al. (2006)).  This
##   requires fitting three models -- one with length, sex, 
##   and the interaction; a second with just length and sex;
##   and a third with just length.  A likelihood ratio test
##   is then computed between the "fullest" and "simplest"
##   models first.
## !! The results suggest no significant sex effect in the ALKs.
## ===========================================================
mdlA <- multinom(oto~lcat*sex,data=pwfALK,maxit=500)
mdlB <- multinom(oto~lcat+sex,data=pwfALK,maxit=500)
mdlC <- multinom(oto~lcat,data=pwfALK,maxit=500)
anova(mdlA,mdlC,test="Chisq")

## ===========================================================
## Visually compare the ALKs for each sex
## ===========================================================
## -----------------------------------------------------------
## Isolate the two groups for construction of separate ALKs ...
## -----------------------------------------------------------
pwfALK.FEM <- filter(pwfALK,sex=="Female")
nrow(pwfALK.FEM)
pwfALK.MAL <- filter(pwfALK,sex=="Male")
nrow(pwfALK.MAL)

## -----------------------------------------------------------
## Make and view the OBSERVED ALKs ...
## -----------------------------------------------------------
tmp <- xtabs(~lcat+oto,data=pwfALK.FEM)
addmargins(tmp,margin=2)
oALK.FEM <- prop.table(tmp,margin=1)
round(oALK.FEM*100,0)
tmp <- xtabs(~lcat+oto,data=pwfALK.MAL)
addmargins(tmp,margin=2)
oALK.MAL <- prop.table(tmp,margin=1)
round(oALK.MAL*100,0)

ageKeyPlot(oALK.FEM,type="bubble",col=rgb(0,0,0,0.2),ylim=c(1.8,9.2),ylab="Otolith Age")
ageKeyPlot(oALK.MAL,type="bubble",col=rgb(1,0,0,0.2),add=TRUE)
legend("topleft",legend=c("female","male"),col=c(rgb(0,0,0,0.2),rgb(1,0,0,0.2)),
       pch=19,cex=1.25,pt.cex=2,bty="n")

## -----------------------------------------------------------
## Make and view the MODELED ALKs ...
## -----------------------------------------------------------
ALKmdl.FEM <- multinom(oto~lcat,data=pwfALK.FEM)
pALK.FEM <- predict(ALKmdl.FEM,data.frame(lcat=as.numeric(rownames(oALK.FEM))),type="probs")
rownames(pALK.FEM) <- rownames(oALK.FEM)
round(pALK.FEM*100,0)

ALKmdl.MAL <- multinom(oto~lcat,data=pwfALK.MAL)
pALK.MAL <- predict(ALKmdl.MAL,data.frame(lcat=as.numeric(rownames(oALK.MAL))),type="probs")
rownames(pALK.MAL) <- rownames(oALK.MAL)
round(pALK.MAL*100,0)

ageKeyPlot(pALK.FEM,type="bubble",col=rgb(0,0,0,0.2),ylim=c(1.8,9.2),ylab="Otolith Age")
ageKeyPlot(pALK.MAL,type="bubble",col=rgb(1,0,0,0.2),add=TRUE)
legend("topleft",legend=c("female","male"),col=c(rgb(0,0,0,0.2),rgb(1,0,0,0.2)),
       pch=19,cex=1.25,pt.cex=2,bty="n")

