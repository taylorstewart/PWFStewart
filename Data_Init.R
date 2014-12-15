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

## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(XLConnect) # reading data
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
## Load and Initial Manipulations of Length Frequency Data
## ===========================================================
## -----------------------------------------------------------
## Load the LF data
## -----------------------------------------------------------
wb <- loadWorkbook("data/1967_to_present_pygmy_whitefish_lengths_LSBS.xlsx")
pwfLF <- readWorksheet(wb,sheet="Export Worksheet")

## -----------------------------------------------------------
## Expend the LF data ... the LF data are recorded as the
##   frequency of fish for each length by year.  These data
##   must be expanded to individual lengths year.
## -----------------------------------------------------------
# if EXP_N row is blank then put N value in it
pwfLF$EXP_N[pwfLF$EXP_N==""] <- pwfLF$N[pwfLF$EXP_N==""]
# repeat row index as many times as EXP_N
reprows <- rep(1:nrow(pwfLF),pwfLF$EXP_N)
# make a data.frame with those row indices (will repeat as
#  necessary), drop the "N" and "EXP_N" columns, create some
#  date related variabvles, recode some categorical things
#  that are coded as numbers, convert depths from character
#  to numeric (because of blanks??), and change LENGTH to tl
#  so we don't have to change some of the other code.
pwfLens <- pwfLF[reprows,] %>%
  select(-N,-EXP_N) %>%
  mutate(op_date=dmy(OP_DATE),
         year=year(op_date),
         year=ifelse(year>2020,year-100,year),
         fyear=factor(year),
         mon=month(op_date,label=TRUE),
         day=day(op_date),
         vessel=mapvalues(VESSEL,from=c("1","11","25","95"),to=c("Siscowet","Grayling","Kiyi","Coaster")),
         tr_design=mapvalues(TR_DESIGN,from=c("25","4","26"),to=c("Large","Large","Small")),
         target=mapvalues(TARGET,from=c("2","130","122","113","104"),to=c("nearshore","inshore","deepwater","bathymetric","YOY LKT")),
         beg_depth=as.numeric(BEG_DEPTH),
         end_depth=as.numeric(END_DEPTH),
         avg_depth=(beg_depth+end_depth)/2,
         tl=LENGTH)

## ===========================================================
## Reduce length frequency sample to only those fish caught
##   with the Kiyi and only in May, June, or July ... to 
##   be consistent with Taylor's samples.  However, kept 
##   Coaster samples (which are only from 2008) to show the
##   age-1+ fish in 2008.
## ===========================================================
pwfLens %<>% filter(vessel %in% c("Kiyi","Coaster"),
                    mon %in% c("May","Jun","Jul"))
pwfLens <- droplevels(pwfLens)

pwfLens <- tbl_df(pwfLens)



## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load the Fish data, convert to a dplyr tbl_df type
## -----------------------------------------------------------
wb <- loadWorkbook("data/PWF 2013.xlsx")
pwf <- readWorksheet(wb,sheet="PWF 2013")
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



## ===========================================================
## Clean-up items that are not needed for later
## ===========================================================
rm(wb,pwfLF,reprows)



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
tmp <- rbind(dim(pwfLens),dim(pwfWL),dim(pwfAgeS),
             dim(pwfAgeO),dim(pwfAgeSO),dim(pwfGrow))
tmp <- data.frame(paste0("pwf",c("Lens","WL","AgeS","AgeO","AgeSO","Growt")),tmp)
colnames(tmp) <- c("data.frame","indivs","vars")
tmp
