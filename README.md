<!-- rmarkdown v1 -->


## Research compendium for: The Effects of a 16-week Aerobic Exercise and Mindfulness-based Intervention on Chronic Psychosocial Stress: A Nonrandomized Pilot and Feasibility Trial.

[![Last-changedate](https://img.shields.io/badge/last%20change-2019--10--16-brightgreen.svg)](https://github.com/adamhsparks/rice.awd.pests/commits/master)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.1-brightgreen.svg)](https://cran.r-project.org/)

* Compendium Author: [Guy A. Prochilo](http://www.guyprochilo.com)

## Preprint

- [:earth_asia: Preprint](https://psyarxiv.com/): Link to manuscript preprint on PsyArXiv.

## Overview of main contents

- [:page_facing_up: Supplementary Materials](/02_supplementary_materials.pdf)
- [:page_facing_up: Supplementary Results](03_supplementary_results.pdf)
- [:white_check_mark: CONSORT Checklist for Trials](/04_CONSORT_checklist.pdf)
- [:file_folder: Data](/data): Data entered into the analysis*.
- [:file_folder: Plots](/plots): A directory hierarchy that will store the output of plots generated by the analysis.
- [:file_folder: Results](/results): A directory hierarchy that will store `.csv` file results  generated by the analysis.
- [:scroll: Script File](/ProcFun_stress-v1.R): R source code for reproducing all analyses
- [:key: License](/LICENSE.txt): The scripts within this repository are free software provided under the terms of the GNU General Public License (v >= 3) unless otherwise specified. 

*_Note_: This directory is empty. Data are not publicly available on the advice of the Monash University Human Research Ethics Committee. The deidentified data are available from the compendium author, upon a reasonable request. 

## R Markdown report

- [:earth_asia: Report](http://gprochilo.github.io/stress_trial): An R Markdown report that contains the results of all analyses performed in the manuscript*.

*_Note_: This file can be independently reproduced by following the instructions below.

## Instructions for reproducing results

**1. Check software dependencies:**

* Version:  `R version = 3.5.1 (2018-07-02)`
* OS:       `Windows = 10 x64`   
* UI:       `RStudio = 1.1.463`

_Note_: This analysis pipeline has not been tested on Mac OS or other Unix systems

**2. Download analysis pipeline:**

* Download or clone the current GitHub Repository
* _Note_: do not change the directory hierarchy

**3. Open the R Studio project file:**

* Open `analysis.Rproj` to begin an R Studio session in the current working directory

**4. Install R package dependencies:**

* This analysis requires the following packages:


```r
install.packages(c(
  "ggplot2",
  "MBESS",
  "scales",
  "HLMdiag",
  "userfriendlyscience",
  "lmerTest",
  "emmeans",
  "ggpubr",
  "here",
  "pwr",
  "DT")
)
```

**5. Knit the R Markdown file:**

* Open `rm_report.Rmd` in R Studio and select **Knit**
* This action will reproduce all reported analyses

**_Optional_: Run analyses from the `commands.R` script file**

* Open `commands.R` in R Studio
* Run the following commands to prepare the R session


```r
source("libraries.R")
source("Rallfun-v35.txt")
source("import_data.R")
source("ProcFun_stress-v1.R")
```

* Run any script from the `commands.R` file to reproduce individual analyses

## Contact
* Guy A. Prochilo, PhD Candidate, Melbourne School of Psychological Sciences
* University of Melbourne, Parkville, VIC 3010
* guy.prochilo@gmail.com
* https://www.guyprochilo.com


<!--- ## Citation

This [research compendium](doi link here) accompanies the paper: 

> Paper. (Year). Title. doi.

An open-access version of the paper is available as a preprint on PsyArXiv: <link here>. --->
