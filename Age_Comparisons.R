##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  AGE COMPARISONS ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfAge data.frame
## ===========================================================
source("Data_Init.R")
pwfAge


## ===========================================================
## Compare scale ages (Taylor Stewart v. Dalton Lebeda)
## ===========================================================
## -----------------------------------------------------------
## Create specific data.frame for structure comparison
##   i.e., both scale variables are not missing
## -----------------------------------------------------------
pwfScales <- filter(pwfAge,!is.na(scale1) & !is.na(scale2))

## -----------------------------------------------------------
## Sample size for comparing scale ages
## -----------------------------------------------------------
( numScales <- nrow(pwfScales) )

## -----------------------------------------------------------
## Number (and percent) of fish where a scale age could not
##  be agreed upon -- i.e., rows in pwfScales with NA in 
##  scale variable
## -----------------------------------------------------------
( numScalesDisagree <- length(which(is.na(pwfScales$scale))) )
numScalesDisagree/numScales*100

## -----------------------------------------------------------
## Scales -- Bias Between Readers
## !! No signficant bias between TRS and DL
## -----------------------------------------------------------
abS <- ageBias(scale1~scale2,data=pwfScales,ref.lab="TRS Age",nref.lab="DL Age")
summary(abS,what=c("n","table","symmetry"))
plot(abS)

## -----------------------------------------------------------
## Scales -- Precision Between Readers
## !! CV relatively high
## -----------------------------------------------------------
apS <- agePrecision(scale1~scale2,data=pwfScales)
summary(apS,what=c("precision","difference","absolute difference"),digits=2)
# percentage difference within one year
sum(apS$absdiff[1:2])/sum(apS$absdiff)*100


## ===========================================================
## Compare otolith ages (Taylor Stewart v. The Master)
## ===========================================================
## -----------------------------------------------------------
## Create specific data.frame for structure comparison
##   i.e., both otolith variables are not missing
## -----------------------------------------------------------
pwfOtos <- filter(pwfAge,!is.na(oto1) & !is.na(oto2))

## -----------------------------------------------------------
## Sample size for comparing otolith ages
## -----------------------------------------------------------
( numOtos <- nrow(pwfOtos) )

## -----------------------------------------------------------
## Number (and percent) of fish where an otolith age could not
##  be agreed upon -- i.e., rows in pwfOtos with NA in oto
##  variable
## -----------------------------------------------------------
( numOtosDisagree <- length(which(is.na(pwfOtos$oto))) )
numOtosDisagree/numOtos*100

## -----------------------------------------------------------
## Otoliths -- Bias Between Readers
## !! Weak evidence that DHO over-estimated relative to TRS
## -----------------------------------------------------------
abO <- ageBias(oto1~oto2,data=pwfOtos,ref.lab="TRS Age",nref.lab="DHO Age")
summary(abO,what=c("n","table","symmetry"))
plot(abO)

## -----------------------------------------------------------
## Otoliths -- Precision Between Readers
## !! CV relatively high, % agreement relatively low
## -----------------------------------------------------------
apO <- agePrecision(oto1~oto2,data=pwfOtos)
summary(apO,what=c("precision","difference","absolute difference"),digits=2)
# percentage difference within one year
sum(apO$absdiff[1:2])/sum(apO$absdiff)*100


## ===========================================================
## Compare scale and otolith (consensus) ages
## ===========================================================
## -----------------------------------------------------------
## Create specific data.frame for structure comparison
##   i.e., both otolith variables are not missing
## -----------------------------------------------------------
pwfSO <- filter(pwfAge,!is.na(scale) & !is.na(oto))
nrow(pwfSO)

## -----------------------------------------------------------
## Bias Between Structures
## !! Very strong evidence that scales under-estimate age
## !!   relative to otolith ages starting, possibly, as early
## !!   as otolith age-2.
## -----------------------------------------------------------
abSO <- ageBias(oto~scale,data=pwfSO,ref.lab="Otolith Age",nref.lab="Scale Age")
summary(abSO,what=c("n","table","symmetry"))
plot(abSO)


## ===========================================================
## If we assume that all fish less than 75 mm are age-2, how
##   do scale and oto ages compare to that.
## !! Otos (36%) and scales (8.3%) do not match age-2
## !! Scales (83.3%) suggest age-1 ... do scales
## !!   underestimate by 1 year?
## ===========================================================
otos75 <- xtabs(~oto,data=subset(pwfOtos,tl<75))
otos75["2"]/sum(otos75)*100
scales75 <- xtabs(~scale,data=subset(pwfScales,tl<75))
scales75["2"]/sum(scales75)*100
scales75["1"]/sum(scales75)*100

## ===========================================================
## Explore the  bias between scales and otoliths if all scale
##   ages were one greater (simulating missing the inner annulus).
## !! Symmetry is equivocal (Bowker's says no, others say yes)
## !! Plot suggests that scale under-estimation starts at
## !!   age-5 (though not significant until age-7).
## ===========================================================
pwfSO <- mutate(pwfSO,scaleAdj=scale+1)
abSO2 <- ageBias(oto~scaleAdj,data=pwfSO,ref.lab="Otolith Age",nref.lab="Adj Scale Age")
summary(abSO2,what=c("n","table","symmetry"))
plot(abSO2)
