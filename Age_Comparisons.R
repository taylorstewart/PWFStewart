##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  AGE COMPARISONS ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfAgeS, pwfAgeO, pwfAgeSO
## ===========================================================
source("Data_Init.R")



## ===========================================================
## Compare scale ages (Taylor Stewart v. Dalton Lebeda)
## ===========================================================
## -----------------------------------------------------------
## How many fish were deemed to have unuseable scales?
## How were unuseable scales related to length
## Remove those fish from further analysis.
## -----------------------------------------------------------
Summarize(~useS,data=pwfAgeS)
xtabs(~useS+lcat,data=pwfAgeS)
pwfAgeS %<>% filter(useS=="YES")

## -----------------------------------------------------------
## How many fish did not have a consensus scale age?
## -----------------------------------------------------------
( numS.NoConsensus <- length(which(is.na(pwfAgeS$ageS))) )
numS.NoConsensus/nrow(pwfAgeS)*100

## -----------------------------------------------------------
## Sample size for comparing scale ages
## -----------------------------------------------------------
( numS <- nrow(pwfAgeS) )
xtabs(~sex+lcat,data=pwfAgeS)

## -----------------------------------------------------------
## Scales -- Bias Between Readers
## !! No signficant bias between TRS and DL
## -----------------------------------------------------------
abS <- ageBias(ageS1~ageS2,data=pwfAgeS,ref.lab="TRS Age",nref.lab="DL Age")
summary(abS,what=c("n","table","symmetry"))
plot(abS)

## -----------------------------------------------------------
## Scales -- Precision Between Readers
## !! CV relatively high
## -----------------------------------------------------------
apS <- agePrecision(ageS1~ageS2,data=pwfAgeS)
summary(apS,what=c("precision","difference","absolute difference"),digits=2)
# percentage difference within X years
cumsum(apS$absdiff)/sum(apS$absdiff)*100



## ===========================================================
## Compare otolith ages (Taylor Stewart v. The Master)
## ===========================================================
## -----------------------------------------------------------
## How many fish were deemed to have unuseable otos?
## How were unuseable otos related to length
## Remove those fish from further analysis.
## -----------------------------------------------------------
Summarize(~useO,data=pwfAgeO)
tmp <- xtabs(~lcat+useO,data=pwfAgeO)
round(prop.table(tmp,margin=1)*100,1)
pwfAgeO %<>% filter(useO=="YES")

## -----------------------------------------------------------
## How many fish did not have a consensus scale age?
## -----------------------------------------------------------
( numO.NoConsensus <- length(which(is.na(pwfAgeO$ageO))) )
numO.NoConsensus/nrow(pwfAgeO)*100

## -----------------------------------------------------------
## Sample size for comparing scale ages
## -----------------------------------------------------------
( numO <- nrow(pwfAgeO) )
xtabs(~sex+lcat,data=pwfAgeO)

## -----------------------------------------------------------
## Otos -- Bias Between Readers
## !! No signficant bias between TRS and DHO
## -----------------------------------------------------------
abO <- ageBias(ageO1~ageO2,data=pwfAgeO,ref.lab="TRS Age",nref.lab="DHO Age")
summary(abO,what=c("n","table","symmetry"))
plot(abO)

## -----------------------------------------------------------
## Otos -- Precision Between Readers
## !! CV relatively high
## -----------------------------------------------------------
apO <- agePrecision(ageO1~ageO2,data=pwfAgeO)
summary(apO,what=c("precision","difference","absolute difference"),digits=2)
# percentage difference within X years
cumsum(apO$absdiff)/sum(apO$absdiff)*100



## ===========================================================
## Compare scale and otolith (consensus) ages
## ===========================================================
## -----------------------------------------------------------
## Bias Between Structures
## !! Very strong evidence that scales under-estimate age
## !!   relative to otolith ages starting, possibly, as early
## !!   as otolith age-2.
## -----------------------------------------------------------
abSO <- ageBias(ageO~ageS,data=pwfAgeSO,ref.lab="Consensus Otolith Age",nref.lab="Consensus Scale Age")
summary(abSO,what=c("n","table","symmetry"))
plot(abSO)


## ===========================================================
## If we assume that all fish less than 75 mm are age-1, how
##   do scale and oto ages compare to that.
## !! Otos not so good, scales very good
## ===========================================================
Summarize(~factor(ageO),data=Subset(pwfAgeO,tl<75))
Summarize(~factor(ageS),data=Subset(pwfAgeS,tl<75))



## ===========================================================
## Publication quality graphic -- scale/otolith age-bias plot
## ===========================================================
## -----------------------------------------------------------
## Put the result into a PDF file
## -----------------------------------------------------------
figw <- 5 # inches
figh <- figw
ptsz <- 12
#pdf("Figs/Figure2.PDF",width=figw,height=figh,pointsize=ptsz,family="Times",onefile=TRUE)
png("Figs/Figure2.PNG",width=figw,height=figh,units="in",pointsize=ptsz,family="Times",res=300)
## -----------------------------------------------------------
## Set some constants for plotting
## -----------------------------------------------------------
# plotting parameters
par(mar=c(3.5,3.5,0.5,0.5),mgp=c(1.8,0.4,0),tcl=-0.2,las=1,xaxs="i",yaxs="i")

## -----------------------------------------------------------
## Make the figure
## -----------------------------------------------------------
plot(abSO,xlim=c(0,9.2),ylim=c(0,9.2),col.CI="black",col.CIsig="black",col.agree="black",nYpos=0.025)

dev.off()



## ===========================================================
## Info for Publication quality table
## ===========================================================
tmp <- summary(abS)
tmpS <- c(apS$n,tmp$p,apS$ACV,apS$APE,apS$absdiff/sum(apS$absdiff)*100,NA,NA)
tmp <- summary(abO)
tmpO <- c(apO$n,tmp$p,apO$ACV,apO$APE,apO$absdiff/sum(apO$absdiff)*100,NA)
tmp <- summary(abSO)
apSO <- agePrecision(ageO~ageS,data=pwfAgeSO)
tmpSO <- c(apSO$n,tmp$p,NA,NA,apSO$absdiff/sum(apSO$absdiff)*100)
tmp <- rbind(tmpS,tmpO,tmpSO)
colnames(tmp) <- c("n","McN","E-H","Bow","ACV","APE","0","1","2","3","4")
round(tmp,4)
sum(tmp["tmpSO",c("3","4")])
