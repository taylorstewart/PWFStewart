##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript
##
##  ANALYSIS SETUP SCRIPT
##
##############################################################
##############################################################

## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(XLConnect) # reading data
library(dplyr)     # manipulating data
library(FSA)       # lots of stuff

## ===========================================================
## Set the random seed for reproducibility (i.e., randomization
##   is used in the "new" sex variable below and in application
##   of the age-length-key.
set.seed(84621684)

## ===========================================================
## Load and Initial Manipulations of Length Frequency Data
## ===========================================================
## -----------------------------------------------------------
## Load the LF data
## -----------------------------------------------------------
wb <- loadWorkbook("data/PWFLengths.xlsx")
pwfLF <- readWorksheet(wb,sheet="AllYears")

## -----------------------------------------------------------
## Expend the LF data ... the LF data are recorded as the
##   frequency of fish for each length by year.  These data
##   must be expanded to individual lengths year.
## -----------------------------------------------------------
pwfLens <- with(pwfLF,data.frame(tl=rep(LENGTH,EXP_N),
                                 year=rep(YEAR,EXP_N)))

## -----------------------------------------------------------
## Convert to a dplyr tbl_df type
## -----------------------------------------------------------
pwfLens <- tbl_df(pwfLens)

## -----------------------------------------------------------
## Factor the year variable
## -----------------------------------------------------------
pwfLens <- mutate(pwfLens,fyear = factor(year))
pwfLens

## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load the Fish data
## -----------------------------------------------------------
wb <- loadWorkbook("data/PWF 2013.xlsx")
pwf <- readWorksheet(wb,sheet="PWF 2013")

## -----------------------------------------------------------
## Convert to a dplyr tbl_df type
## -----------------------------------------------------------
pwf <- tbl_df(pwf)

## -----------------------------------------------------------
## Eliminate variables that will not be used elsewhere
## -----------------------------------------------------------
pwf <- select(pwf,-c(Serial:Location,Scales,Otoliths,Scale_Mag,Comments))

## -----------------------------------------------------------
## Rename variables for easier use later
## -----------------------------------------------------------
names(pwf) <- c("fish","tl","wt","sex","mat","scale1","scale2","scale","oto1","oto2","oto")

## -----------------------------------------------------------
## Factor the sex variable, add a 10-mm length bin variable
## -----------------------------------------------------------
pwf <- mutate(pwf,sex=factor(sex),
              lcat=lencat(tl,w=10))
pwf


## ===========================================================
## Create Data.Frame for Weight-Length and Sex Ratio Analysis
## ===========================================================
## -----------------------------------------------------------
## Just remove the age data and order by sex category, length,
##   and weight
## -----------------------------------------------------------
pwfWL <- pwf %>%
  select(-(scale1:oto)) %>%
  arrange(sex,tl,wt)
pwfWL


## ===========================================================
## Create Data.frame for Age Comparison Analyses
## ===========================================================
## -----------------------------------------------------------
## Remove fish with no scale and otolith ages, remove unused
##    other variables, and order by sex category, otolith
##    age, and length.
## -----------------------------------------------------------
pwfAge <- pwf %>% 
  filter(!(is.na(scale1) & is.na(scale2) & is.na(oto1) & is.na(oto2))) %>%
  select(-c(wt,mat)) %>%
  arrange(sex,oto,tl)
pwfAge


## ===========================================================
## Create Data.frame for Growth and ALK Analyses
## ===========================================================
## -----------------------------------------------------------
## Get all fish with otolith ages, remove unused other
##    variables, and order by sex category, otolith age, and
##    length.
## -----------------------------------------------------------
pwfGrow <- pwf %>% 
  filter(!is.na(oto)) %>%
  select(-c(wt,mat,scale1:oto2)) %>%
  arrange(sex,oto,tl)
pwfGrow


## ===========================================================
## Clean-up items that are not needed for later
## ===========================================================
rm(wb,pwfLF)


## ***********************************************************
## ===========================================================
## Note that the following data.frames are created and
##   returned if this script is sourced.
##
## pwf: Original fish sample data.frame.  Used in length
##        frequency and sex ratio analysis.
## pwfAge: Used in the age comparisons analysis.
## pwfGrow: Used in the growth analysis.
## pwfWL: Used in the weight-length analysis.
## pwfLens: Used in the length frequency analysis.
## ===========================================================
## ***********************************************************
dim(pwf)
dim(pwfAge)
dim(pwfGrow)
dim(pwfWL)
dim(pwfLens)
