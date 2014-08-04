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
library(NCStats)   # compSlopes()


## ===========================================================
## Delete the N/A fish in the sex variable (thus, all "unknown"
##   fish are also immature fish), append logwt and logtl
##   variables, and remove three outliers ...
##   FishID #184 -- wt too heavy for tl
##   FishID #214 -- wt too light for tl
##   FishID #216 -- wt too light for tl
## ===========================================================
pwfWL <- pwfWL %>% 
  filter(sex!="N/A") %>%
  mutate(logtl=log(tl),
         logwt=log(wt)) %>%
  filter(!(fish %in% c(184,214,216)))
pwfWL

## ===========================================================
## Fit the W-L DVR using sex to see if there is a significant
##   difference in W-L between female, male, and uknown sex
##   fish.
## 
## !! Slight indiciation for a difference in slopes among the
## !!   the three "sexes."  However, the post-hoc FDR analysis
## !!   shows no differences.  Thus, conclude that the three
## !!   W-L regressions are statistically similar.
## ===========================================================
lm1 <- lm(logwt~logtl*sex,data=pwfWL)
anova(lm1)
compSlopes(lm1)
fitPlot(lm1,legend="topleft")
residPlot(lm1,student=TRUE)

## ===========================================================
## Fit the W-L SLR to get the overall coefficients
## ===========================================================
lm2 <- lm(logwt~logtl,data=pwfWL)
fitPlot(lm2)
residPlot(lm2,student=TRUE)
summary(lm2)
(cf <- coef(lm2) )
exp(cf["(Intercept)"])
rSquared(lm2)

