##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  AGE DISTRIBUTIONS ANALYSIS SCRIPT
##
##  NOTE: ALKs did not differ between sexes (see ALK_Comparisons.R)
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get pwfGrow and PWFLens data.frames
## ===========================================================
source("Data_Init.R")
pwfGrow
pwfLens

## ===========================================================
## Isolate 2013 fish from pwfLens
## ===========================================================
pwfLens13 <- filter(pwfLens,year==2013)

## ===========================================================
## Create a modeled ALK (not separated by sex) from pwfAge
## ===========================================================
lvls <- rownames(xtabs(~lcat+oto,data=pwfGrow))
ALKmdl <- multinom(oto~lcat,data=pwfGrow)
pALK <- predict(ALKmdl,data.frame(lcat=as.numeric(lvls)),type="probs")
rownames(pALK) <- lvls
round(pALK*100,0)

## ===========================================================
## Apply the ALK to the lengths in pwfLens13
## ===========================================================
## -----------------------------------------------------------
## Apply to all fish ...
## -----------------------------------------------------------
pwfLens13 <- ageKey(pALK,~tl,data=pwfLens13)

## -----------------------------------------------------------
## but then make all fish <75 mm age-2
## -----------------------------------------------------------
pwfLens13$age[pwfLens13$tl<75] <- 2
head(pwfLens13)

## ===========================================================
## Expanded Age Distribution
## !! Displays variable recruitment (or ageing issues)
## ===========================================================
( ad.raw <- xtabs(~age,data=pwfLens13) )
barplot(ad.raw)



## ===========================================================
## EXPERIMENTAL --- develop a year-class strength index as the
##   number of PWF caught between 50 and 75 mm.  This is
##   reprsentative of the age-2 fish in the sample.  This 
##   vector of catches can then be scaled to serve as a
##   relative correction factor for variable recruitment in
##   the cross-sectional catch-curve.
##
## Below will only work with catches if effort was the same
##   each year.  Need number of trawls each year to make a CPE.
## ===========================================================
ycs <- pwfLens %>%
  filter(year>=2006 & year<=2012) %>%
  filter(tl>=50 & tl <=75) %>%
  arrange(year) %>%
  group_by(fyear) %>%
  summarize(count=n())
ycs

yccf <- max(ycs$count)/ycs$count
yccf <- c(1,rev(yccf))

barplot(ad.raw*yccf)
