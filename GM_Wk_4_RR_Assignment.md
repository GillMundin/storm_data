---
title: "Effect of severe storms on US health and Economy"
author: "Gill Mundin"
date: "15 July 2019"
output: 
  html_document:
    keep_md: true
---


##Synopsis
TBC - Describe analysis briefly in 10 sentences


##Data processing
The sections below describe how the data were loaded and processed for analysis

###Packages used

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
library(tidyr)
```

###Load data

A link to the storm data is found on the Coursera website:
https://www.coursera.org/learn/reproducible-research/peer/OMZ37/course-project-2

Right click the Storm Data hyperlink and copy link address:
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

Assign the link address to variable url, and use to download file to working directory.

Use the chunk option cache = TRUE to save knit time.


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "storm-data.csv.bz2"  # Saved file name, saved to working directory
download.file(url, filename)
data <- read.csv(filename)
```


###Process data for analysis
Process an analysis dataset for health question
Relevant variables are STATE, EVTYPE, FATALITIES, INJURIES

Process an analysis dataset for economy question
Relevant variables are BGN_DATE, BGN_TIME, STATE, EVTYPE, PROPDMG:CROPDMGEXP, REMARKS, REFNUM




```r
health <- data %>% select(STATE, EVTYPE, FATALITIES, INJURIES)

econ <- data %>% select(BGN_DATE, BGN_TIME, STATE, EVTYPE, PROPDMG:CROPDMGEXP, REMARKS, REFNUM)
```

###Determine which weather event caused most injuries and fatalities across entire US


##Results
