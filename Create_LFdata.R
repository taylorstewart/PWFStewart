## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(readxl)    # reading data
library(FSA)       # mapvalues, filterD
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
pwfLF <- read_excel("data/1967_to_present_pygmy_whitefish_lengths_LSBS.xlsx",
                    sheet="Export Worksheet")

## -----------------------------------------------------------
## Clean up a little
##   handle dates (create year and month variables),
##   recode vessel codes
##   rename some variables so not all caps
##   create an average depth
##   remove unused variables
## -----------------------------------------------------------
pwfLF %<>% 
  mutate(op_date=dmy(OP_DATE),
         year=year(op_date),
         year=ifelse(year>2020,year-100,year),
         fyear=factor(year),
         mon=month(op_date,label=TRUE),
         vessel=mapvalues(VESSEL,from=c("1","11","25","95","4"),
                          to=c("Siscowet","Grayling","Kiyi","Coaster","OTHER")),
         tl=LENGTH,
         beg_depth=BEG_DEPTH,
         end_depth=END_DEPTH,
         avg_depth=(beg_depth+end_depth)/2) %>%
  select(-c(OP_DATE,VESSEL,BEG_DEPTH,END_DEPTH,LENGTH,TARGET,TR_DESIGN))


## -----------------------------------------------------------
## Expend the LF data ... the LF data are recorded as the
##   frequency of fish for each length by year.  These data
##   must be expanded to individual lengths for each year.
##
## if EXP_N row is blank then put N value in it
## repeat row index as many times as EXP_N
## make a new data.frame with those row indices (will repeat as necessary)
## drop the "N" and "EXP_N" columns
## Reduce length frequency sample to only those fish caught
##   with the Kiyi and only in May, June, or July ... to 
##   be consistent with Taylor's samples.  However, kept 
##   Coaster samples (which are only from 2008) to show the
##   age-1+ fish in 2008.
## -----------------------------------------------------------
pwfLF$EXP_N[is.na(pwfLF$EXP_N)] <- pwfLF$N[is.na(pwfLF$EXP_N)]
reprows <- rep(1:nrow(pwfLF),pwfLF$EXP_N)
pwfLens <- pwfLF[reprows,] %>%
  select(-c(N,EXP_N)) %>%
  filterD(vessel %in% c("Kiyi","Coaster"),mon %in% c("May","Jun","Jul"))


## -----------------------------------------------------------
## Write out to a CSV file
## -----------------------------------------------------------
write.csv(pwfLens,"data/pwfLens.csv",row.names=FALSE)


## -----------------------------------------------------------
## Clean objects from memory
## -----------------------------------------------------------
rm(pwfLens,pwfLF,reprows)
