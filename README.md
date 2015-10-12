PWFStewart
==========

This repository contains data files, analysis scripts, and the draft manuscript for *Age, growth, and size of Lake Superior Pygmy Whitefish (Prosopium coulterii)* by Taylor R. Stewart, Derek H. Ogle, Owen T. Gorman, and Mark R. Vinson that has been accepted for publication in the American Midland Naturalist.


# Data files (not in the repo)
* `data/PWF 2013.xlsx` -- Excel file containing the biological data specific to this project.
* `data/1967_to_present_pygmy_whitefish_lengths_LSBS.xlsx` -- Excel file containing measured lengths of PWF from the last 27 years.
* `data/pwfLens.csv` -- CSV file that contains lengths from `data/1967_to_present_pygmy_whitefish_lengths_LSBS.xlsx` that have been expanded.  Created in `Create_LFdata.R` and used in `Length_Freq.R` and `Basic_Summaries.R`.


# Scripts
## Initialization
* `Data_Init.R` -- Initial loading of packages and data, initial preparation of data for other scripts.  This is sourced by all other scripts except `Create_LFdata.R` and creates the following data.frames.
    * `pwfLens`: Used in the length frequency analysis.
    * `pwfWL`: Used in the weight-length analysis, length frequency analysis, and sex-ratio analysis.
    * `pwfAgeS`: Used in the scale age comparisons analysis.
    * `pwfAgeO`: Used in the otolith age comparisons analysis.
    * `pwfAgeSO`: Used in the scale-otolith age comparisons analysis.
    * `pwfGrow: Used in the growth analysis.`

## Length Frequency Analysis
* `Create_LFdata.R` -- Process the original length frequency data to produce `pwfLens.csv` which is then loaded in the `Length_Freq.R` script.  This is only run once as running it multiple times seems to run into a memory problem that crashes R/RStudio.
* `Length_Freq.R` -- Length frequency analysis for last 8 years of data, including showing a break nearly every year at 75-mm and a break that is apparent in only one year near 50 mm.  This makes **Figure 3** in the manuscript (and a supplemental figure of length frequency histograms for 2000-2014).

## Age, Growth, and Size Analyses
* `Age_Comparisons.R` -- Precision and bias analysis between readers of scales and otoliths, bias analysis between scales and otoliths.  This makes **Table 1**, **Figure 2**, and the results in the **AGE** section of the manuscript.
* `Basic_Summaries.R` -- Basic summaries of length, weight, age, and sex variables.  These results are in the **SIZE** section of the manuscript.
* `Growth.R` -- von Bertalanffy growth analyses.  This makes **Table 2**, **Table 3**, **Table 4**,  **Figure 4**, and the results in the **GROWTH** section of the manuscript.
* `Weight-Length.R` -- Weight-length relationship analysis including comparing between sexes (F,M,U) and developing an overall W-L relationship.  These results are in the **WEIGHT-LENGTH RELATIONSHIP** section of the manuscript.

## Helper Files
* `zzzHelpers.R` -- Helper files used in the above scripts.


# Other Files
* `PWFStewart.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
* `literature_resources` -- Folder of PDFs of relevant literature.
