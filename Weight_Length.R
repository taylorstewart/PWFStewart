##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  WEIGHT-LENGTH ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfWL data.frame
## ===========================================================
source("Data_Init.R")
pwfWL

## ===========================================================
## Load Additional Packages
## ===========================================================
library(NCStats)   # rSquared()


## ===========================================================
## Delete the N/A fish in the sex variable (thus, all "unknown"
##   fish are also immature fish), append logwt and logtl
##   variables, and remove three outliers ...
##   FishID #184 -- wt too heavy for tl
##   FishID #214 -- wt too light for tl
##   FishID #216 -- wt too light for tl
## ===========================================================
pwfWL %<>% filter(sex!="N/A") %>%
           mutate(logtl=log10(tl),logwt=log10(wt)) %>%
           filter(!(fishID %in% c(184,214,216)))
pwfWL

## ===========================================================
## Fit the W-L DVR using sex to see if there is a significant
##   difference in W-L between female, male, and unknown sex
##   fish.
## 
## !! Slight indiciation for a difference in slopes among the
## !!   the three "sexes."  However, the post-hoc FDR analysis
## !!   shows no differences.  Thus, conclude that the three
## !!   W-L regressions are statistically similar.
## ===========================================================
## -----------------------------------------------------------
## Typical DVR ... see full vs. simple model comparison below
## -----------------------------------------------------------
lm1 <- lm(logwt~logtl*sex,data=pwfWL)
anova(lm1)
compSlopes(lm1)
fitPlot(lm1,legend="topleft")
residPlot(lm1,resid.type="student")

## ===========================================================
## Fit the W-L SLR to get the overall coefficients
## ===========================================================
lm2 <- lm(logwt~logtl,data=pwfWL)
fitPlot(lm2)
residPlot(lm2,resid.type="student")
summary(lm2)
(cf <- coef(lm2) )
10^(cf[["(Intercept)"]])
rSquared(lm2)

## -----------------------------------------------------------
## full vs. simple model comparison
## -----------------------------------------------------------
anova(lm2,lm1)
