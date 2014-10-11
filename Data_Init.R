##############################################################
##############################################################
##  PWF (Taylor Stewart et al.) manuscript work
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
library(lubridate) # to handle dates

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
#wb <- loadWorkbook("data/PWFLengths.xlsx")  # this was original
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
  mutate(op_date=dmy(OP_DATE)) %>%
  mutate(year=year(op_date),mon=month(op_date,label=TRUE),day=day(op_date)) %>%
  mutate(year=ifelse(year>2020,year-100,year)) %>%
  mutate(fyear=factor(year)) %>%
#  mutate(mon=factor(mon,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
  mutate(vessel=recodeF(as.character(VESSEL),c("1","11","25","95"),c("Siscowet","Grayling","Kiyi","Coaster"))) %>%
  mutate(tr_design=recodeF(as.character(TR_DESIGN),c("25","4","26"),c("Large","Large","Small"))) %>%
  mutate(target=recodeF(as.character(TARGET),c("2","130","122","113","104"),c("nearshore","inshore","deepwater","bathymetric","YOY LKT"))) %>%
  mutate(beg_depth=as.numeric(BEG_DEPTH)) %>%
  mutate(end_depth=as.numeric(END_DEPTH)) %>%
  mutate(avg_depth=(beg_depth+end_depth)/2) %>%
  mutate(tl=LENGTH)

## ===========================================================
## Reduce length frequency sample to only those fish caught
##   with the Kiyi and only in May, June, or July ... to 
##   be consistent with Taylor's samples.  However, kept 
##   Coaster samples (which are only from 2008) to show the
##   age-1+ fish in 2008.
## ===========================================================
pwfLens <- pwfLens %>%
  filter(vessel %in% c("Kiyi","Coaster")) %>%
  filter(mon %in% c("May","Jun","Jul"))
pwfLens <- droplevels(pwfLens)

tbl_df(pwfLens)

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
