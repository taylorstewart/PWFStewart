PWFStewart
==========

This repository contains data files, analysis scripts, and a draft manuscript for *Life History Aspects of Lake Superior Pygmy Whitefish, Including Precision and Bias in Age Assessments* by Taylor R. Stewart, Derek H. Ogle, and Mark R. Vinson.

**These data are as of yet unpublished and should not be used by anyone other than the authors.**

# Data files (not in the repo)
* `data/PWFLengths.xlsx` -- Excel file containing measured lengths of PWF from the last 25 years.
* `data/PWF 2013.xlsx` -- Excel file containing the biological data specific to this project.

# Scripts
* `Data_Init.R` -- Initial loading of packages and data, initial preparation of data for other scripts.  This is sourced by all other scripts.
* `Basic_Summaries.R` -- Basic summaries of length, weight, age, and sex variables..
* `Age_Comparisons.R` -- Precision and bias analysis between readers of scales and otoliths, bias analysis between scales and otoliths.
* `Length_Freq.R` -- Length frequency analysis for last 8 years of data, including showing a break nearly every year at 75-mm and a break that is apparent in only one year near 50 mm.
* `Weight-Length.R` -- Weight-length relationship analysis including comparing between sexes (F,M,U) and developing an overall W-L relationship.
* `ALK_Comparisons.R` -- Compare age-length-keys for otolith ages between females and males.
* `Growth.R` -- von Bertalanffy growth analyses.
* `Age_Distributions.R` -- Applied an overall age-length-key to all measured lengths in 2013 to estimate an age distribution.


# Other Files
* `zzzHelpers.R` -- Helper files used in the above scripts.
* `PWFStewart.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file
