PWFStewart
==========

This repository contains data files, analysis scripts, and a draft manuscript for *Age, growth, and size of Lake Superior Pygmy Whitefish (Prosopium coulterii) in 2013* by Taylor R. Stewart, Derek H. Ogle, Owen T. Gorman, and Mark R. Vinson.

**These data are as of yet unpublished and should not be used by anyone other than the authors.**

# Data files (not in the repo)
* `data/1967_to_present_pygmy_whitefish_lengths_LSBS.xlsx` -- Excel file containing measured lengths of PWF from the last 27 years.
* `data/PWF 2013.xlsx` -- Excel file containing the biological data specific to this project.

# Scripts
* `Data_Init.R` -- Initial loading of packages and data, initial preparation of data for other scripts.  This is sourced by all other scripts except `Create_LFdata.R`.
* `Basic_Summaries.R` -- Basic summaries of length, weight, age, and sex variables.
* `Weight-Length.R` -- Weight-length relationship analysis including comparing between sexes (F,M,U) and developing an overall W-L relationship.
* `Age_Comparisons.R` -- Precision and bias analysis between readers of scales and otoliths, bias analysis between scales and otoliths.
* `Growth.R` -- von Bertalanffy growth analyses.
* `Create_LFdata.R` -- Process the original length frequency data to produce `pwfLens.csv` which is then loaded in the the `Length_Freq.R` script.  This is only run once as running it multiple times seems to run into a memory problem that crashes R/RStudio.
* `Length_Freq.R` -- Length frequency analysis for last 8 years of data, including showing a break nearly every year at 75-mm and a break that is apparent in only one year near 50 mm.


# Other Files
* `zzzHelpers.R` -- Helper files used in the above scripts.
* `PWFStewart.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file
* `literature_resources` -- Folder of PDFs of relevant literature.
