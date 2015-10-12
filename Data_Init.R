##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript work
##
##  ANALYSIS SETUP SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Clear the environment first
## ===========================================================
rm(list = ls(all.names=TRUE))
gc()

## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(readxl)    # reading data
library(FSA)       # lots of stuff
library(magrittr)  # for %<>%
library(dplyr)     # manipulating data
library(lubridate) # to handle dates



## ===========================================================
## Set the random seed for reproducibility (i.e., randomization
##   is used in the "new" sex variable below and in application
##   of the age-length-key.
## ===========================================================
set.seed(84621684)



## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load the Fish data, convert to a dplyr tbl_df type
## -----------------------------------------------------------
pwf <- read_excel("data/PWF 2013.xlsx",sheet="PWF 2013")
pwf <- tbl_df(pwf)

## -----------------------------------------------------------
## Eliminate variables that will not be used elsewhere
## Factor the sex, useS, and useO variables.
## Add a 10-mm length bin variable
## -----------------------------------------------------------
pwf %<>% select(-c(serial:location,comments:mat)) %>%
         mutate(sex=factor(sex),useS=factor(useS),
                useO=factor(useO),lcat=lencat(tl,w=10))
pwf



## ===========================================================
## Create Data.Frame for Weight-Length and Sex Ratio Analysis
## ===========================================================
pwfWL <- pwf %>%
  select(fishID:sex,lcat) %>%
  arrange(sex,tl,wt)
pwfWL



## ===========================================================
## Create separate data.frames for SCALE and OTOLITH age
## comparison analyses -- i.e., 
##   Remove fish with NA in useX variable (i.e., not analyzed)
##   Remove unused other variables
##   Order by sex category, length, and consensus age.
## ===========================================================
pwfAgeS <- pwf %>% 
  filter(!is.na(useS)) %>%
  select(-wt,-contains("ageO"),-useO) %>%
  arrange(sex,tl,ageS)
pwfAgeS

pwfAgeO <- pwf %>% 
  filter(!is.na(useO)) %>%
  select(-wt,-contains("ageS"),-useS) %>%
  arrange(sex,tl,ageO)
pwfAgeO



## ===========================================================
## Create a data.frames for comparing consensus SCALE and
## OTOLITH ages -- i.e., 
##   Isolate fish with both an ageS and ageO (consensus ages)
##   Remove fish for which either the scale or oto is unuseable
##   Remove unused other variables
##   Order by sex category, length, and ageO.
## ===========================================================
pwfAgeSO <- pwf %>% 
  filter(!is.na(ageS), !is.na(ageO),
         useS=="YES", useO=="YES") %>%
  select(-c(wt,ageS1,ageS2,useS,ageO1,ageO2,useO)) %>%
  arrange(sex,tl,ageO)
pwfAgeSO



## ===========================================================
## Create Data.frame for Growth and ALK Analyses
## ===========================================================
## -----------------------------------------------------------
## Get all fish with useable otolith ages, remove unused other
##    variables, and order by sex category, otolith age, and
##    length.
## -----------------------------------------------------------
pwfGrow <- pwf %>% 
  filter(!is.na(ageO),useO=="YES") %>%
  select(-c(wt,ageS1:ageO2,useO)) %>%
  arrange(sex,ageO,tl)
pwfGrow



## ***********************************************************
## ===========================================================
## Note that the following data.frames are created and
##   returned if this script is sourced.
##
## pwfLens: Used in the length frequency analysis.
## pwfWL: Used in the weight-length analysis, length frequency
##        analysis, and sex-ratio analysis.
## pwfAgeS: Used in the scale age comparisons analysis.
## pwfAgeO: Used in the otolith age comparisons analysis.
## pwfAgeSO: Used in the scale-otolith age comparisons analysis.
## pwfGrow: Used in the growth analysis.
## ===========================================================
## ***********************************************************
tmp <- rbind(dim(pwfWL),dim(pwfAgeS),
             dim(pwfAgeO),dim(pwfAgeSO),dim(pwfGrow))
tmp <- data.frame(paste0("pwf",c("WL","AgeS","AgeO","AgeSO","Growth")),tmp)
colnames(tmp) <- c("data.frame","indivs","vars")
tmp



## ===========================================================
## Remove data.frames that are not needed in other scripts.
## ===========================================================
rm(tmp)
