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
plot(tl~I(ageO-sep),data=subset(pwfGrow,sex=="Female"),pch=19,col=colF,
     xlab=xlbl,ylab=ylbl,ylim=c(50,150),xlim=c(1,9))
points(tl~I(ageO+sep),data=subset(pwfGrow,sex=="Male"),pch=19,col=colM)
points(tl~ageO,data=subset(pwfGrow,sex=="Unknown"),pch=19,col=colU)
legend("topleft",legend=c("Female","Male","Unknown"),
       pch=16,col=c("black","red","blue"),bty="n")


## ===========================================================
## Cross-tabs of length category and age, separeately for each
##   sex (i.e., an age-length key if converted to percentages)
## ===========================================================
xtabs(~lcat+ageO+sex,data=Subset(pwfGrow,sex %in% c("Female","Male")))


## ===========================================================
## Handle small (<75-mm) unknown sexed fish by assigning an
##   age of 1 (in a new age variable, ageOX) and randomly
##   allocating them to be male or female.  This should help
##   anchor the left side of the growth trajectory.
## ===========================================================
## -----------------------------------------------------------
## Create a new age variable, ageOX, that contains all of the
##   old otolith ages except that the unknown fish are made
##   to be age-1
## -----------------------------------------------------------
pwfGrow %<>% mutate(ageOX=ageO)
pwfGrow$ageOX[pwfGrow$sex=="Unknown"] <- 1

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
plot(tl~I(ageOX-sep),data=subset(pwfGrow,sex=="Female"),pch=19,col=colF,
     xlab=xlbl,ylab=ylbl,ylim=c(50,150),xlim=c(1,9))
points(tl~I(ageOX+sep),data=subset(pwfGrow,sex=="Male"),pch=19,col=colM)
points(tl~I(ageOX-sep),data=subset(pwfGrow,sex=="Unknown" & sex2=="Female"),pch=1,col="black")
points(tl~I(ageOX+sep),data=subset(pwfGrow,sex=="Unknown" & sex2=="Male"),pch=1,col="red")
legend("topleft",legend=c("Female","Male"),pch=16,col=c("black","red"),bty="n")


## ===========================================================
## Compare Von B fits between males and females (where unknown
##   sex fish were given age-2 and partitioned as above).
## ===========================================================
## -----------------------------------------------------------
## Use the ages in common for both sexes -- 2 and 6 (and, thus,
##   4).  Did not use age-1 b/c assumed to be same given the
##   decision with unknown fish from above.  Did not use age-7
##   b/c only one male there.
## -----------------------------------------------------------
age1 <- 2
age3 <- 6

## -----------------------------------------------------------
## The most general model (all parameters differ) and the
##   three one parameter in common models are declared below.
##   The numbers in the model name correspond to the parameters
##   that vary between sexes in that model.
## -----------------------------------------------------------
# most general model --- all parameters differ
vbGen <- tl~L1[sex2]+(L3[sex2]-L1[sex2])*((1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1[sex2]))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1[sex2]))^2))
# assume that L1 is in common
vb23 <- tl~L1+(L3[sex2]-L1)*((1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3[sex2]-L2[sex2])/(L2[sex2]-L1))^2))
# assume that L2 is in common
vb13 <- tl~L1[sex2]+(L3[sex2]-L1[sex2])*((1-((L3[sex2]-L2)/(L2-L1[sex2]))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3[sex2]-L2)/(L2-L1[sex2]))^2))
# assume that L3 is in common
vb12 <- tl~L1[sex2]+(L3-L1[sex2])*((1-((L3-L2[sex2])/(L2[sex2]-L1[sex2]))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3-L2[sex2])/(L2[sex2]-L1[sex2]))^2))

## -----------------------------------------------------------
## Get starting values ... close to mean lengths-at-age with
##   some adjustments so that L1<L2<L3.
## -----------------------------------------------------------
# get mean lengths-at-age and sex
pwfGrow %>% group_by(sex2,ageOX) %>% summarize(mean(tl))
svGen  <- list(L1=c(84,84),L2=c(110,99),L3=c(140,118))
sv23   <- list(L1=  84,    L2=c(110,99),L3=c(140,118))
sv13   <- list(L1=c(84,84),L2=  105,    L3=c(140,118))
sv12   <- list(L1=c(84,84),L2=c(110,99),L3=  129)

## -----------------------------------------------------------
## The Port algorithm was used to help model fitting ... can
##   set constraints so the algorithm does not wander off into
##   unreasonable values.  Constraints are set below.
## -----------------------------------------------------------
lowCom <- c( 70, 90,108)
upCom <-  c(110,130,160)
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
fit23 <- nls(vb23,data=pwfGrow,start=sv23,algorithm="port",lower=low23,upper=up23)
fit13 <- nls(vb13,data=pwfGrow,start=sv13,algorithm="port",lower=low13,upper=up13)
fit12 <- nls(vb12,data=pwfGrow,start=sv12,algorithm="port",lower=low12,upper=up12)

## -----------------------------------------------------------
## Gather the ANOVA results for a simple output
## !! No difference at age-2
## !! Apparent difference at age-4 and age-6
## -----------------------------------------------------------
extraSS(fit23,fit13,fit12,com=fitGen)

## -----------------------------------------------------------
## Repeat the analysis above, but the comparisons will be 
##   between the two models with L1 and either L2 or L3 in
##   common and the model with only L1 in common.
## -----------------------------------------------------------
# assume that L1 and L2 are in common
vb3 <- tl~L1+(L3[sex2]-L1)*((1-((L3[sex2]-L2)/(L2-L1))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3[sex2]-L2)/(L2-L1))^2))
# assume that L1 and L3 are in common
vb2 <- tl~L1+(L3-L1)*((1-((L3-L2[sex2])/(L2[sex2]-L1))^(2*(ageOX-age1)/(age3-age1)))/(1-((L3-L2[sex2])/(L2[sex2]-L1))^2))
# starting values
sv3 <- list(L1=84,L2=  105,     L3=c(140,118))
sv2 <- list(L1=84,L2=c(110,99), L3=  129)
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
## !! Confirms apparent difference at age-4 and age-6
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
vbFEM <- nls(tl~vbFrancis(ageOX,L1,L2,L3,t1=age1,t3=age3),data=pwfGrow.FEM,start=svFEM)
vbMAL <- nls(tl~vbFrancis(ageOX,L1,L2,L3,t1=age1,t3=age3),data=pwfGrow.MAL,start=svMAL)
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
for (i in 1:9) {
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
pLenFEM <- cbind(predict(vbFEM,data.frame(ageOX=1:9)),pLenFEM)
pLenMAL <- cbind(predict(vbMAL,data.frame(ageOX=1:9)),pLenMAL)
colnames(pLenMAL)[1] <- colnames(pLenFEM)[1] <- "Estimate"
# Note that ages 2, 4, and 6 are the same as the model coefficients
round(pLenFEM,1)
round(pLenMAL,1)

## ===========================================================
## Fitplot for each sex
## ===========================================================
# Base plot
plot(-1,-1,xlab=xlbl,ylab=ylbl,xlim=c(1,9),ylim=c(55,150),yaxt="n")
curve(vbFrancis(x,L1=coef(vbFEM),t1=c(age1,age3)),from=1,to=9,lwd=3,lty=2,col="black",add=TRUE)
polygon(c(1:9,rev(1:9)),c(pLenFEM[,"2.5%"],rev(pLenFEM[,"97.5%"])),col=rgb(0,0,0,0.3),border=NA)
polygon(c(1:7,rev(1:7)),c(pLenMAL[1:7,"2.5%"],rev(pLenMAL[1:7,"97.5%"])),col=rgb(1,0,0,0.3),border=NA)
curve(vbFrancis(x,L1=coef(vbMAL),t1=c(age1,age3)),from=1,to=7,lwd=3,col=rgb(1,0,0,0.6),add=TRUE)
points(tl~I(ageOX-sep),data=pwfGrow.FEM,pch=16,col=colF)
points(tl~I(ageOX+sep),data=pwfGrow.MAL,pch=16,col=colM)
legend("topleft",c("Female","Male"),lwd=2,col=c("black","red"),pch=16,bty="n")
axis(2,seq(60,140,20))



## ===========================================================
## Fitplot for each sex -- for publication (if decided to use)
##   see below for a different version.
## ===========================================================
## -----------------------------------------------------------
## Put the result into a PDF file
## -----------------------------------------------------------
figw <- 5 # inches
figh <- figw
ptsz <- 12
#pdf("Figs/Figure4.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)
png("Figs/Figure4.PNG",width=figw,height=figh,units="in",pointsize=ptsz,family="Times",res=300)

## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
# base line width
lwid <- 2
sep <- 0.04
# plotting parameters
par(mar=c(3.5,3.5,0.5,0.5),mgp=c(1.8,0.4,0),tcl=-0.2,las=1,xaxs="i",yaxs="i")
bg <- "gray50"
# labels
xlbl <- "Consensus Otolith Age"
ylbl <- "Total Length (mm)"

## -----------------------------------------------------------
## Make the figure
## -----------------------------------------------------------
# confidence bands (re-compute to make more smooth)
# females
sagesFEM <- seq(1,9,length.out=100)
bootFEM2 <- NULL
for (i in sagesFEM) bootFEM2 <- cbind(bootFEM2,apply(bootFEM$coefboot,MARGIN=1,FUN=vbFrancis,t=i,t1=c(age1,age3)))
pLenFEM2 <- t(apply(bootFEM2,MARGIN=2,FUN=quantile,probs=c(0.025,0.975)))
# males
sagesMAL <- seq(1,7,length.out=100)
bootMAL2 <- NULL
for (i in sagesMAL) bootMAL2 <- cbind(bootMAL2,apply(bootMAL$coefboot,MARGIN=1,FUN=vbFrancis,t=i,t1=c(age1,age3)))  
pLenMAL2 <- t(apply(bootMAL2,MARGIN=2,FUN=quantile,probs=c(0.025,0.975)))

# base plot
plot(-1,-1,xlab=xlbl,ylab=ylbl,xlim=c(0.8,9.2),ylim=c(50,155),yaxt="n",xaxt="n")
# confidence polygons
trnsp <- 0.2
polygon(c(sagesFEM,rev(sagesFEM)),c(pLenFEM2[,"2.5%"],rev(pLenFEM2[,"97.5%"])),col=rgb(0,0,0,trnsp),border=NA)
polygon(c(sagesMAL,rev(sagesMAL)),c(pLenMAL2[,"2.5%"],rev(pLenMAL2[,"97.5%"])),col=rgb(0,0,0,trnsp),border=NA)
# best-fitcurves
clr <- "gray40"
curve(vbFrancis(x,L1=coef(vbFEM),t1=c(age1,age3)),from=1,to=9,lwd=lwid+1,lty=1,col=clr,add=TRUE)
curve(vbFrancis(x,L1=coef(vbMAL),t1=c(age1,age3)),from=1,to=7,lwd=lwid+1,lty=1,col=clr,add=TRUE)
# put points on with dots
bg <- "gray50"
lwd <- 1
# females
points(tl~I(ageOX-sep),data=subset(pwfGrow.FEM,sex=="Female"),pch=21,bg=bg,lwd=lwd)
points(tl~I(ageOX-sep),data=subset(pwfGrow.FEM,sex=="Unknown"),pch=1,bg=bg,lwd=lwd)
# put male points on with squares
points(tl~I(ageOX+sep),data=subset(pwfGrow.MAL,sex=="Male"),pch=22,bg=bg,lwd=lwd)
points(tl~I(ageOX+sep),data=subset(pwfGrow.MAL,sex=="Unknown"),pch=0,bg=bg,lwd=lwd)
# put on legend
legend("topleft",c("Female","Male"),pch=c(21,22),pt.bg=bg,pt.lwd=lwd,bty="n")
# add axis labels
axis(2,seq(60,140,20))
axis(1,1:9)

dev.off()


## ===========================================================
## Fitplot for each sex -- for publication (if decided to use)
## ===========================================================
## -----------------------------------------------------------
## Put the result into a PDF file
## -----------------------------------------------------------
figw <- 5 # inches
figh <- figw
ptsz <- 12
pdf("Figs/Figure4_alt.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)

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
plot(-1,-1,xlab=xlbl,ylab=ylbl,xlim=c(0.8,9.2),ylim=c(50,155),yaxt="n")
# curve and CI for Females
curve(vbFrancis(x,L1=coef(vbFEM),t1=c(age1,age3)),from=1,to=9,lwd=lwid+1,lty=1,col="black",add=TRUE)
lines(sagesFEM,pLenFEM2[,"2.5%"],lty=2,lwd=lwid,col="black")
lines(sagesFEM,pLenFEM2[,"97.5%"],lty=2,lwd=lwid,col="black")
# curve and CI for Males
curve(vbFrancis(x,L1=coef(vbMAL),t1=c(age1,age3)),from=2,to=7,lwd=lwid+1,lty=1,col="black",add=TRUE)
lines(sagesMAL,pLenMAL2[,"2.5%"],lty=2,lwd=lwid,col="black")
lines(sagesMAL,pLenMAL2[,"97.5%"],lty=2,lwd=lwid,col="black")
# put points on with dots
bg <- "gray50"
lwd <- 1
# femals
points(tl~I(ageOX-sep),data=subset(pwfGrow.FEM,sex=="Female"),pch=21,bg=bg,lwd=lwd)
points(tl~I(ageOX-sep),data=subset(pwfGrow.FEM,sex=="Unknown"),pch=1,bg=bg,lwd=lwd)
# put male points on with squares
points(tl~I(ageOX+sep),data=subset(pwfGrow.MAL,sex=="Male"),pch=22,bg=bg,lwd=lwd)
points(tl~I(ageOX+sep),data=subset(pwfGrow.MAL,sex=="Unknown"),pch=0,bg=bg,lwd=lwd)
# put on legend
legend("topleft",c("Female","Male"),pch=c(21,22),pt.bg=bg,pt.lwd=lwd,bty="n")
# add y-axis labels
axis(2,seq(60,140,20))

dev.off()



## ===========================================================
## Table of predicted lengths-at-age with OBSERVED (not
##   back-calculated) results from other life history studies.
##   Table 3 and 4 in manuscript
##
##, Location,Abbreviations,Source,Units,Type,From,Structure,DHO
##, "Keewenaw Bay, Lake Superior (MI, USA)",KB,Eschmeyer and Bailey (1955),TL,at-capture,Table 11,Scales,confirmed
##, "Isle Royale, Lake Superior (MI, USA)",IR,Eschmeyer and Bailey (1955),TL,at-capture,Table 13,Scales,confirmed
##, "Flathead Lake (MT, USA)",FL,Weisel et al.  (1973),TL,at-capture,Table 6,Scales,confirmed
##, "Brooks Lake (AK, USA)",BKL,Heard and Hartman (1973),FL,at-capture,Table 6,Scales,confirmed
##, "Naknek Lake (AK, USA)",NL,Heard and Hartman (1973),FL,at-capture,Table 6,Scales,confirmed
##, "Cluculz Lake (BC, CAN)",CL,McCart (1963),FL,at-capture,Table 11,Scales,confirmed
##, "Tacheeda Lake (BC, CAN)",TL,McCart (1963),FL,at-capture,Table 12,Scales,confirmed
##, "MacLure Lake (BC, CAN)",ML,McCart (1963),FL,at-capture,Table 13,Scales,confirmed
##, "McLeese Lake (BC, CAN)",MLL,McCart (1963),FL,at-capture,Table 14,Scales,confirmed
##, "Dina Lake #1 (BC, CAN)",DL1,McPhail and Zemlak (2001),FL,at-capture,Table 4,Scales/Otos??,confirmed
##, "Apostle Islands, Lake Superior (WI, USA)",DID NOT USE BECAUSE  THEY ONLY SHOWED BACK_CALCULATED GROWTH,,,,,,
##, "Laughing Fish Point, Lake Superior (MI, USA)",DID NOT USE BECAUSE  THEY ONLY SHOWED BACK_CALCULATED GROWTH,,,,,,
##, "Lake McDonald",DID NOT USE COULD NOT FIND SOURCE AND ESCHMEYER_BAILEY VALUES WERE BACK_CALCULATED,,,,,,
##, "Bull Lake",DID NOT USE COULD NOT FIND SOURCE AND ESCHMEYER_BAILEY VALUES WERE BACK_CALCULATED,,,,,,

## ===========================================================
convFL2TL <- function(d,digits=0) {
  for (i in 1:length(d)) d[i] <- round(ifelse(d[i]<100,1.077*d[i],1.0845*d[i]),0)
  d
}

predLensF <- cbind(pred=pLenFEM[,"Estimate"],
                   lci=pLenFEM[,"2.5%"],
                   uci=pLenFEM[,"97.5%"],
                   KB53=         c( 77,101,106,120,126,128,136, NA, NA),
                   IR53=         c( 59, 81, 88,100, NA, NA, NA, NA, NA),
                   FL= convFL2TL(c(116,140,154,168, NA, NA, NA, NA, NA)),
                   BKL=convFL2TL(c( 57, 70, 75, NA, NA, NA, NA, NA, NA)),
                   NL= convFL2TL(c( 76,112,127,138,155, NA, NA, NA, NA)),
                   CL= convFL2TL(c( 81,110,121,127,142,156, NA, NA, NA)),
                   TL= convFL2TL(c( 84,106,114,120,126,136, NA, NA, NA)),
                   ML= convFL2TL(c( NA,117,190,219,245,248,257, NA,271)),
                   MLL=convFL2TL(c( NA,109,158,194,194, NA, NA, NA, NA)),
                   DL1=convFL2TL(c( 84,105,111,118,122,123,122, NA, NA))
             )
rownames(predLensF) <- paste0("age-",1:9,"+")
predLensF <- rbind(predLensF,minTL=c(NA,NA,NA, 57,NA, 89,convFL2TL(c(48, 51, 83, 78,105,105, 27))),
                             maxTL=c(NA,NA,NA,138,NA,150,convFL2TL(c(78,155,158,138,275,195,215)))
                   )
print(predLensF,digits=1,na.print="-")
                  
                  
predLensM <- cbind(pred=c(pLenMAL[1:7,"Estimate"]),
                   lci=c(pLenMAL[1:7,"2.5%"]),
                   uci=c(pLenMAL[1:7,"97.5%"]),
                   KB53=         c( 76, 94,102,106,110, NA, NA),
                   IR53=         c( 63, 78, 85, 92, NA, NA, NA),
                   FL=           c(117,128,140, NA, NA, NA, NA),
                   BKL=convFL2TL(c( 57, 64, 71, NA, NA, NA, NA)),
                   NL= convFL2TL(c( 77,109,118,133, NA, NA, NA)),
                   CL= convFL2TL(c( 92,113,116,116,122, NA, NA)),
                   TL= convFL2TL(c( 82, 90,108, NA, NA, NA, NA)),
                   ML= convFL2TL(c( NA,121,192,210,185,225, NA)),
                   MLL=convFL2TL(c( NA,111,153,178, NA, NA, NA)),
                   DL1=convFL2TL(c( 73, 94,101,105, NA, NA, NA))
             )
rownames(predLensM) <- paste0("age-",1:7,"+")
predLensM <- rbind(predLensM,minTL=c(NA,NA,NA, 67,NA, 84,convFL2TL(c(45, 51, 88, 78,105,105, 62))),
                             maxTL=c(NA,NA,NA,118,NA,170,convFL2TL(c(75,141,128,113,235,175,122)))
)
print(predLensM,digits=1,na.print="-")

