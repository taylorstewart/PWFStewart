##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  GROWTH ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfAge data.frame
## ===========================================================
source("Data_Init.R")
pwfGrow

## ===========================================================
## Load Additional Packages
## ===========================================================
library(nlstools)  # in VonB analysis

## ===========================================================
## Set some constants used throughout
## ===========================================================
# axis labels
xlbl <- "Otolith Age"
ylbl <- "Total Length (mm)"
# custom colors
colF <- rgb(0,0,0,1/3)
colM <- rgb(1,0,0,1/3)
colU <- rgb(0,0,1,1/3)
# jitter amount for plots with male and females
sep <- 0.08/2


## ===========================================================
## Preliminary exploratory plot
## ===========================================================
plot(tl~I(oto-sep),data=subset(pwfGrow,sex=="Female"),pch=19,col=colF,
     xlab=xlbl,ylab=ylbl,ylim=c(50,150),xlim=c(1,9))
points(tl~I(oto+sep),data=subset(pwfGrow,sex=="Male"),pch=19,col=colM)
points(tl~oto,data=subset(pwfGrow,sex=="Unknown"),pch=19,col=colU)
legend("topleft",legend=c("Female","Male","Unknown"),
       pch=16,col=c("black","red","blue"),bty="n")


## ===========================================================
## Handle small (<75-mm) fish by assigning an age of 2 (in a
##   new age variable, otoX) and randomly allocating them to
##   be male or female.  This should help anchor the left
##   side of the growth trajectory.
## ===========================================================
## -----------------------------------------------------------
## Create a new age variable, otoX, that contains all of the
##   old otolith ages except that the unknown fish are made
##   to be age-2
## -----------------------------------------------------------
pwfGrow <- mutate(pwfGrow,otoX=oto)
pwfGrow$otoX[pwfGrow$sex=="Unknown"] <- 2

## -----------------------------------------------------------
## Create a new sex variable (sex2) where the unknown sex
##   fish are partitioned equally and randomly between male
##   and female.
## -----------------------------------------------------------
# which rows have unknown sex individuals
tmp <- which(pwfGrow$sex=="Unknown")
( nUnk <- length(tmp) )
# randomly order the values
tmp <- sample(tmp,nUnk)
# create new variable & replace first half of random indivs
#   with Female and second half with Male
pwfGrow$sex2 <- pwfGrow$sex
pwfGrow$sex2[tmp[1:round(nUnk/2,0)]] <- "Female"
pwfGrow$sex2[tmp[(round(nUnk/2,0)+1):nUnk]] <- "Male"
# check -- should be none in "Unknown" and "N/A" columns,
#   should be roughly 50% of "Unknown" row in Female and Male
xtabs(~sex+sex2,data=pwfGrow)

## -----------------------------------------------------------
## Recreate the preliminary plot with the "new" variables
## -----------------------------------------------------------
plot(tl~I(otoX-sep),data=subset(pwfGrow,sex=="Female"),pch=19,col=colF,
     xlab=xlbl,ylab=ylbl,ylim=c(50,150),xlim=c(1,9))
points(tl~I(otoX+sep),data=subset(pwfGrow,sex=="Male"),pch=19,col=colM)
points(tl~I(otoX-sep),data=subset(pwfGrow,sex=="Unknown" & sex2=="Female"),pch=1,col="black")
points(tl~I(otoX+sep),data=subset(pwfGrow,sex=="Unknown" & sex2=="Male"),pch=1,col="red")
legend("topleft",legend=c("Female","Male"),pch=16,col=c("black","red"),bty="n")


## ===========================================================
## Compare Von B fits between males and females (where unknown
##   sex fish were given age-2 and partitioned as above).
## ===========================================================
## -----------------------------------------------------------
## Use the ages in common for both sexes -- 3 and 7 (and, thus,
##   5).  Did not use age-2 b/c assumed to be same given the
##   decision with unknown fish from above.
## -----------------------------------------------------------
age1 <- 3
age3 <- 7

## -----------------------------------------------------------
## The most general model (all parameters differ) and the
##   three one parameter in common models are declared below.
##   The numbers in the model name correspond to the parameters
##   that vary between sexes in that model.
## -----------------------------------------------------------
# most general model --- all parameters differ
vbGen <- tl~L1[sex2]+(L3[sex2]-L1[sex2])*((1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1[sex2]))^(2*(otoX-age1)/(age3-age1)))/(1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1[sex2]))^2))
# assume that L1 is in common
vb23 <- tl~L1+(L3[sex2]-L1)*((1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1))^(2*(otoX-age1)/(age3-age1)))/(1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1))^2))
# assume that L2 is in common
vb13 <- tl~L1[sex2]+(L3[sex2]-L1[sex2])*((1-((L3[sex2]-L2)/(L2-L1[sex2]))^(2*(otoX-age1)/(age3-age1)))/(1-((L3[sex2]-L2)/(L2-L1[sex2]))^2))
# assume that L3 is in common
vb12 <- tl~L1[sex2]+(L3-L1[sex2])*((1-((L3-L2[sex2])/(L2[sex2]-L1[sex2]))^(2*(otoX-age1)/(age3-age1)))/(1-((L3-L2[sex2])/(L2[sex2]-L1[sex2]))^2))

## -----------------------------------------------------------
## Get starting values ... close to mean lengths-at-age with
##   some adjustments so that L1<L2<L3.
## -----------------------------------------------------------
# get mean lengths-at-age and sex
pwfGrow %>% group_by(sex2,otoX) %>% summarize(mean(tl))
svGen  <- list(L1=c(88,88),L2=c(114,100),L3=c(138,108))
sv23 <- list(L1=88,L2=c(114,100),L3=c(138,108))
sv13 <- list(L1=c(88,88),L2=107,L3=c(138,108))
sv12 <- list(L1=c(88,88),L2=c(114,100),L3=123)

## -----------------------------------------------------------
## The Port algorithm was used to help model fitting ... can
##   set constraints so the algorithm does not wander off into
##   unreasonable values.  Constraints are set below.
## -----------------------------------------------------------
lowCom <- c( 70, 90,100)
upCom <-  c(110,130,150)
upGen <- unlist(lapply(upCom,rep,2))
lowGen <- unlist(lapply(lowCom,rep,2))
up23 <- unlist(mapply(rep,upCom,c(1,2,2)))
low23 <- unlist(mapply(rep,lowCom,c(1,2,2)))
up13 <- unlist(mapply(rep,upCom,c(2,1,2)))
low13 <- unlist(mapply(rep,lowCom,c(2,1,2)))
up12 <- unlist(mapply(rep,upCom,c(2,2,1)))
low12 <- unlist(mapply(rep,lowCom,c(2,2,1)))

## -----------------------------------------------------------
## Fit the four models
## -----------------------------------------------------------
fitGen <- nls(vbGen,data=pwfGrow,start=svGen,algorithm="port",lower=lowGen,upper=upGen)
fit23 <- nls(vb23,data=pwfGrow,start=sv23)
fit13 <- nls(vb13,data=pwfGrow,start=sv13,algorithm="port",lower=low13,upper=up13)
fit12 <- nls(vb12,data=pwfGrow,start=sv12,algorithm="port",lower=low12,upper=up12)

## -----------------------------------------------------------
## Gather the ANOVA results for a simple output
## !! No difference at age-3
## !! Apparent difference at age-5 and age-7
## -----------------------------------------------------------
extraSS(fit23,fit13,fit12,com=fitGen)

## -----------------------------------------------------------
## Repeat the analysis above, but the comparisons will be 
##   between the two models with L1 and either L2 or L3 in
##   common and the model with only L1 in common.
## -----------------------------------------------------------
# assume that L1 and L2 are in common
vb3 <- tl~L1+(L3[sex2]-L1)*((1-((L3[sex2]-L2)/(L2-L1))^(2*(otoX-age1)/(age3-age1)))/(1-((L3[sex2]-L2)/(L2-L1))^2))
# assume that L1 and L3 are in common
vb2 <- tl~L1+(L3-L1)*((1-((L3-L2[sex2])/(L2[sex2]-L1))^(2*(otoX-age1)/(age3-age1)))/(1-((L3-L2[sex2])/(L2[sex2]-L1))^2))
# starting values
sv3 <- list(L1=88,L2=107,L3=c(138,108))
sv2 <- list(L1=88,L2=c(114,100),L3=123)
# constraints
up3 <- unlist(mapply(rep,upCom,c(1,1,2)))
low3 <- unlist(mapply(rep,lowCom,c(1,1,2)))
up2 <- unlist(mapply(rep,upCom,c(1,2,1)))
low2 <- unlist(mapply(rep,lowCom,c(1,2,1)))
# fits
fit3 <- nls(vb3,data=pwfGrow,start=sv3,algorithm="port",lower=low3,upper=up3)
fit2 <- nls(vb2,data=pwfGrow,start=sv2,algorithm="port",lower=low2,upper=up2)
# anova
extraSS(fit3,fit2,com=fit23)
## -----------------------------------------------------------
## !! Confirms apparent difference at age-5 and age-7
## -----------------------------------------------------------

## ===========================================================
## Bootstrap separate models for each sex
## ===========================================================
## -----------------------------------------------------------
## Isolate males and females (using the sex2 variable)
## -----------------------------------------------------------
pwfGrow.FEM <- filter(pwfGrow,sex2=="Female")
pwfGrow.MAL <- filter(pwfGrow,sex2=="Male")

## -----------------------------------------------------------
## Create the Francis model as a function
## -----------------------------------------------------------
vbFrancis <- vbFuns("Francis")

## -----------------------------------------------------------
## Fit the Francis model separately
## -----------------------------------------------------------
# starting values -- use coefficients from general fit
svFEM <- coef(fitGen)[c(1,3,5)]
svMAL <- coef(fitGen)[c(2,4,6)]
names(svFEM) <- names(svMAL) <- c("L1","L2","L3")
# fit the models
vbFEM <- nls(tl~vbFrancis(otoX,L1,L2,L3,t1=age1,t3=age3),data=pwfGrow.FEM,start=svFEM)
vbMAL <- nls(tl~vbFrancis(otoX,L1,L2,L3,t1=age1,t3=age3),data=pwfGrow.MAL,start=svMAL)
# compare coefficients from these to the general model
cbind(svFEM,coef(vbFEM))
cbind(svMAL,coef(vbMAL))

## -----------------------------------------------------------
## Perform the bootstrap sampling
## -----------------------------------------------------------
rsmpls <- 1000  # be patient
bootFEM <- nlsBoot(vbFEM,niter=rsmpls)
bootMAL <- nlsBoot(vbMAL,niter=rsmpls)

## -----------------------------------------------------------
## Summarize the coefficients
## -----------------------------------------------------------
coefFEM <- cbind(coef(vbFEM),confint(bootFEM))
coefMAL <- cbind(coef(vbMAL),confint(bootMAL))
colnames(coefMAL)[1] <- colnames(coefFEM)[1] <- "Estimate"
round(coefFEM,1)
round(coefMAL,1)

## -----------------------------------------------------------
## Predict mean length-at-age
## -----------------------------------------------------------
# Predicted lengths at various ages
bootFEM1 <- bootFEM$coefboot
bootMAL1 <- bootMAL$coefboot
for (i in 2:9) {
  tmp <- apply(bootFEM$coefboot,MARGIN=1,FUN=vbFrancis,t=i,t1=c(age1,age3))
  bootFEM1 <- cbind(bootFEM1,tmp)
  tmp <- apply(bootMAL$coefboot,MARGIN=1,FUN=vbFrancis,t=i,t1=c(age1,age3))
  bootMAL1 <- cbind(bootMAL1,tmp)  
  colnames(bootFEM1)[ncol(bootFEM1)] <- colnames(bootMAL1)[ncol(bootMAL1)] <- paste("predL",i,sep="")
}
# CIs for predicted lengths at various ages
pLenFEM <- t(apply(bootFEM1[,-(1:3)],MARGIN=2,FUN=quantile,probs=c(0.025,0.975)))
pLenMAL <- t(apply(bootMAL1[,-(1:3)],MARGIN=2,FUN=quantile,probs=c(0.025,0.975)))
# Append predicted lengths-at-age
pLenFEM <- cbind(predict(vbFEM,data.frame(otoX=2:9)),pLenFEM)
pLenMAL <- cbind(predict(vbMAL,data.frame(otoX=2:9)),pLenMAL)
colnames(pLenMAL)[1] <- colnames(pLenFEM)[1] <- "Estimate"
# Note that ages 3, 5, and 7 are the same as the model coefficients
round(pLenFEM,1)
round(pLenMAL,1)

## ===========================================================
## Fitplot for each sex
## ===========================================================
# Base plot
plot(-1,-1,xlab=xlbl,ylab=ylbl,xlim=c(2,9),ylim=c(55,150),yaxt="n")
curve(vbFrancis(x,L1=coef(vbFEM),t1=c(age1,age3)),from=2,to=9,lwd=3,lty=2,col="black",add=TRUE)
polygon(c(2:9,rev(2:9)),c(pLenFEM[,"2.5%"],rev(pLenFEM[,"97.5%"])),col=rgb(0,0,0,0.3),border=NA)
polygon(c(2:7,rev(2:7)),c(pLenMAL[1:6,"2.5%"],rev(pLenMAL[1:6,"97.5%"])),col=rgb(1,0,0,0.3),border=NA)
curve(vbFrancis(x,L1=coef(vbMAL),t1=c(age1,age3)),from=2,to=7,lwd=3,col=rgb(1,0,0,0.6),add=TRUE)
points(tl~I(otoX-sep),data=pwfGrow.FEM,pch=16,col=colF)
points(tl~I(otoX+sep),data=pwfGrow.MAL,pch=16,col=colM)
legend("topleft",c("Female","Male"),lwd=2,col=c("black","red"),pch=16,bty="n")
axis(2,seq(60,140,20))

## ===========================================================
## Fitplot for each sex -- for publication (if decided to use)
## ===========================================================
## -----------------------------------------------------------
## Put the result into a PDF file
## -----------------------------------------------------------
figw <- 5 # inches
figh <- figw
ptsz <- 12
pdf("Figs/FigGROW.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
# base line width
lwid <- 2
sep <- 0.08
# plotting parameters
par(mar=c(3.5,3.5,0.5,0.5),mgp=c(1.8,0.4,0),tcl=-0.2,las=1,xaxs="i",yaxs="i")

## -----------------------------------------------------------
## Make the figure
## -----------------------------------------------------------
# base plot
plot(-1,-1,xlab=xlbl,ylab=ylbl,xlim=c(1.8,9.2),ylim=c(50,155),yaxt="n")
# curve and CI for Females
curve(vbFrancis(x,L1=coef(vbFEM),t1=c(age1,age3)),from=2,to=9,lwd=lwid+1,lty=1,col="black",add=TRUE)
lines((2:9),pLenFEM[,"2.5%"],lty=2,lwd=lwid,col="black")
lines((2:9),pLenFEM[,"97.5%"],lty=2,lwd=lwid,col="black")
# curve and CI for Males
curve(vbFrancis(x,L1=coef(vbMAL),t1=c(age1,age3)),from=2,to=7,lwd=lwid+1,lty=1,col="black",add=TRUE)
lines((2:7),pLenMAL[1:6,"2.5%"],lty=2,lwd=lwid,col="black")
lines((2:7),pLenMAL[1:6,"97.5%"],lty=2,lwd=lwid,col="black")
# put female points on with dots
points(tl~I(otoX-sep),data=subset(pwfGrow.FEM,sex=="Female"),pch=16)
points(tl~I(otoX-sep),data=subset(pwfGrow.FEM,sex=="Unknown"),pch=1)
# put male points on with squares
points(tl~I(otoX+sep),data=subset(pwfGrow.MAL,sex=="Male"),pch=15)
points(tl~I(otoX+sep),data=subset(pwfGrow.MAL,sex=="Unknown"),pch=0)
# put on legend
legend("topleft",c("Female","Male"),pch=c(16,15),bty="n")
# add y-axis labels
axis(2,seq(60,140,20))

dev.off()



## ===========================================================
## Table of predicted lengths-at-age with results from 
##   Eschmeyer and Bailey included
## ===========================================================
predLens <- cbind(age=1:9,
                  predF=c(NA,pLenFEM[,"Estimate"]),
                  lciF=c(NA,pLenFEM[,"2.5%"]),
                  uciF=c(NA,pLenFEM[,"97.5%"]),
                  KB53F=c(46,69,88,107,117,123,130,NA,NA),
                  IR53F=c(37,64,82,96,NA,NA,NA,NA,NA),
                  AI53Ft=c(46,71,94,112,122,126,136,NA,NA),
                  LFP53F=c(46,68,90,106,118,123,NA,NA,NA),
                  
                  predM=c(NA,pLenMAL[1:6,"Estimate"],NA,NA),
                  lciM=c(NA,pLenMAL[1:6,"2.5%"],NA,NA),
                  uciM=c(NA,pLenMAL[1:6,"97.5%"],NA,NA),
                  KB53M=c(49,71,87,98,106,NA,NA,NA,NA),
                  IR53M=c(40,68,81,88,NA,NA,NA,NA,NA),
                  AI53Mt=c(50,75,95,108,NA,NA,NA,NA,NA),
                  LFP53M=c(48,79,92,102,106,NA,NA,NA,NA)
)
rownames(predLens) <- NULL
print(predLens,digits=1,na.print="-")

predLens <- data.frame(predLens)
par(mfrow=c(1,2),mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2)
plot(-1,-1,xlim=c(1,9),ylim=c(35,140),xlab="Age",ylab="Total Length")
lines(KB53F~age,data=predLens,lwd=2,lty=1,col="gray90")
lines(IR53F~age,data=predLens,lwd=2,lty=1,col="gray80")
lines(AI53Ft~age,data=predLens,lwd=2,lty=1,col="gray70")
lines(LFP53F~age,data=predLens,lwd=2,lty=1,col="gray60")
lines(predF~age,data=predLens,lwd=3,lty=1)
legend("topleft",legend="Females",bty="n",cex=1.25)
plot(-1,-1,xlim=c(1,9),ylim=c(35,140),xlab="Age",ylab="Total Length")
lines(KB53M~age,data=predLens,lwd=2,lty=1,col="gray90")
lines(IR53M~age,data=predLens,lwd=2,lty=1,col="gray80")
lines(AI53Mt~age,data=predLens,lwd=2,lty=1,col="gray70")
lines(LFP53M~age,data=predLens,lwd=2,lty=1,col="gray60")
lines(predM~age,data=predLens,lwd=3,lty=1)
legend("topleft",legend="Males",bty="n",cex=1.25)


## ===========================================================
## Table of predicted lengths-at-age with results from 
##   other life history studies included
## ===========================================================
predLens <- cbind(age=1:9,
                  predF=c(NA,pLenFEM[,"Estimate"]),
                  lciF=c(NA,pLenFEM[,"2.5%"]),
                  uciF=c(NA,pLenFEM[,"97.5%"]),
                  FLF=c(116,140,154,168,NA,NA,NA,NA,NA),
                  BKLF=c(57,70,75,NA,NA,NA,NA,NA,NA),
                  SBF=c(76,112,127,138,155,NA,NA,NA,NA),
                  LMF=c(62,92,108,NA,NA,NA,NA,NA,NA),
                  BLF=c(60,119,152,NA,NA,NA,NA,NA,NA),
                  CLF=c(53,91,112,121,136,152,NA,NA,NA),
                  TLF=c(58,89,105,114,124,134,NA,NA,NA),
                  MLF=c(NA,87,141,191,228,232,250,NA,262),
                  MLLF=c(NA,95,148,178,185,NA,NA,NA,NA),
                  
                  predM=c(NA,pLenMAL[1:6,"Estimate"],NA,NA),
                  lciM=c(NA,pLenMAL[1:6,"2.5%"],NA,NA),
                  uciM=c(NA,pLenMAL[1:6,"97.5%"],NA,NA),
                  FLM=c(117,128,140,NA,NA,NA,NA,NA,NA),
                  BKLM=c(57,64,71,NA,NA,NA,NA,NA,NA),
                  SBM=c(77,109,118,133,NA,NA,NA,NA,NA),
                  LMM=c(64,88,101,108,NA,NA,NA,NA,NA),
                  BLM=c(59,113,130,NA,NA,NA,NA,NA,NA),
                  CLM=c(64,96,107,112,118,NA,NA,NA,NA),
                  TLM=c(58,80,100,NA,NA,NA,NA,NA,NA),
                  MLM=c(NA,87,133,188,174,215,NA,NA,NA),
                  MLLM=c(NA,95,146,167,NA,NA,NA,NA,NA)
)
rownames(predLens) <- NULL
print(predLens,digits=1,na.print="-")

predLens <- data.frame(predLens)
par(mfrow=c(1,2),mar=c(3,3,0.5,0.5),mgp=c(1.7,0.5,0),tcl=-0.2)
plot(-1,-1,xlim=c(1,9),ylim=c(50,275),xlab="Age",ylab="Total Length")
lines(FLF~age,data=predLens,lwd=2,lty=1,col="gray90")
lines(BKLF~age,data=predLens,lwd=2,lty=1,col="gray80")
lines(SBF~age,data=predLens,lwd=2,lty=1,col="gray70")
lines(LMF~age,data=predLens,lwd=2,lty=1,col="gray60")
lines(BLF~age,data=predLens,lwd=2,lty=1,col="gray50")
lines(CLF~age,data=predLens,lwd=2,lty=1,col="gray40")
lines(TLF~age,data=predLens,lwd=2,lty=1,col="gray30")
lines(MLF~age,data=predLens,lwd=2,lty=1,col="gray20")
lines(MLLF~age,data=predLens,lwd=2,lty=1,col="gray10")
lines(predF~age,data=predLens,lwd=3,lty=1)
legend("topleft",legend="Females",bty="n",cex=1.25)
plot(-1,-1,xlim=c(1,9),ylim=c(50,275),xlab="Age",ylab="Total Length")
lines(FLM~age,data=predLens,lwd=2,lty=1,col="gray90")
lines(BKLM~age,data=predLens,lwd=2,lty=1,col="gray80")
lines(SBM~age,data=predLens,lwd=2,lty=1,col="gray70")
lines(LMM~age,data=predLens,lwd=2,lty=1,col="gray60")
lines(BLM~age,data=predLens,lwd=2,lty=1,col="gray50")
lines(CLM~age,data=predLens,lwd=2,lty=1,col="gray40")
lines(TLM~age,data=predLens,lwd=2,lty=1,col="gray30")
lines(MLM~age,data=predLens,lwd=2,lty=1,col="gray20")
lines(MLLM~age,data=predLens,lwd=2,lty=1,col="gray10")
lines(predM~age,data=predLens,lwd=3,lty=1)
legend("topleft",legend="Males",bty="n",cex=1.25)
