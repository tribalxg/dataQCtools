dataQCtools: automate data cropping, QC, and more
====

Both the code and documentation in this repository are being actively developed for use by Tribes throughout the Tribal Exchange Network.

## Overview

This code was developed by [Jess Kunke](https://github.com/jpierkunke), Kim Bray, and [Angie Reed](https://github.com/angiereed04468) to automate several steps of the Hoh Tribe's high-frequency water and air temperature data processing and quality control (QC). Specifically, this code does the following tasks:

- Reads in many different csv files from HOBO sensors at different sites and combines the data together into a common dataset
- Crops the data to remove any measurements recorded before sensor deployment (between the time the sensor is turned on and the time it is fully installed in the field) or after sensor retrieval (between the time the sensor is removed from the field and brought back to the lab).
- Plots the cropped and raw data together to visualize how much data was cropped for each site.
- Generates QC plots for identifying suspicious values to be manually corrected. This step could be fully automated, but the current script allows the user to manually explore the results and make decisions.
- Reads in quality-controlled data (after the user has removed or corrected any spurious values), computes the seven-day average daily maximum temperature (7DADM), compares these values against water quality standards to determine which sites were impaired on which dates, and outputs a table to be included in the Tribal assessment report (TAR).


## Setting up

You will need to install several following R packages once before using this code for the first time on a given computer. To do this, you can run this line of code in RStudio:

```r
install.packages(c("zoo", "grid", "plotly", "readxl", "scales", "runner", 
"ggthemes", "tidyverse", "lubridate", "gridExtra", "flextable", "RColorBrewer", "magrittr"))
```

## Practice example

You can use the provided example data to practice using the code, then substitute your own data. Please see the vignettes/articles and the help pages for examples.

