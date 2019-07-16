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
tidyhealth <- gather(health, outcome, count, -STATE, -EVTYPE)
tidyhealth$outcome <- as.factor(tidyhealth$outcome)

str(tidyhealth)
```

```
## 'data.frame':	1804594 obs. of  4 variables:
##  $ STATE  : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ outcome: Factor w/ 2 levels "FATALITIES","INJURIES": 1 1 1 1 1 1 1 1 1 1 ...
##  $ count  : num  0 0 0 0 0 0 0 0 1 0 ...
```

```r
econ <- data %>% select(BGN_DATE, BGN_TIME, STATE, EVTYPE, PROPDMG:CROPDMGEXP, REMARKS, REFNUM)

head(tidyhealth, 3)
```

```
##   STATE  EVTYPE    outcome count
## 1    AL TORNADO FATALITIES     0
## 2    AL TORNADO FATALITIES     0
## 3    AL TORNADO FATALITIES     0
```

###Determine which weather event caused most injuries and fatalities across entire US
Use tapply to determine the number of injuries and fatalities for each weather event.
The number of unique weather events is large, with 985 different types of weather event.
A meaningful barchart is difficult to format with this many events therefore equations have 
been used to determine the weather event responsible for the most injuries and fatalities.


```r
harm_event <- with(tidyhealth,
                   tapply(count, 
                          list(EVTYPE, outcome), 
                          sum,
                          na.rm = TRUE)) %>% 
        as.data.frame()


fatal <- harm_event[which.max(harm_event$FATALITIES), ] # harm_event must be df for this to work
fatalevent <- rownames(fatal)
nfatal <- fatal[1,1]

injury <- harm_event[which.max(harm_event$INJURIES), ]
injuryevent <- rownames(injury)
ninjury <- injury[1,2]
```

Rank the data so that a sensible number of events can be plotted


```r
fatal_rank <- harm_event[order(harm_event$FATALITIES, decreasing = TRUE), ]
fatal_top_10 <- fatal_rank[1:10, ]

injury_rank <- harm_event[order(harm_event$INJURIES, decreasing = TRUE), ]
injury_top_10 <- injury_rank[1:10, ]

str(fatal_top_10)
```

```
## 'data.frame':	10 obs. of  2 variables:
##  $ FATALITIES: num  5633 1903 978 937 816 ...
##  $ INJURIES  : num  91346 6525 1777 2100 5230 ...
```

```r
str(injury_top_10)
```

```
## 'data.frame':	10 obs. of  2 variables:
##  $ FATALITIES: num  5633 504 470 1903 816 ...
##  $ INJURIES  : num  91346 6957 6789 6525 5230 ...
```

```r
fatal_top_10
```

```
##                FATALITIES INJURIES
## TORNADO              5633    91346
## EXCESSIVE HEAT       1903     6525
## FLASH FLOOD           978     1777
## HEAT                  937     2100
## LIGHTNING             816     5230
## TSTM WIND             504     6957
## FLOOD                 470     6789
## RIP CURRENT           368      232
## HIGH WIND             248     1137
## AVALANCHE             224      170
```

```r
injury_top_10
```

```
##                   FATALITIES INJURIES
## TORNADO                 5633    91346
## TSTM WIND                504     6957
## FLOOD                    470     6789
## EXCESSIVE HEAT          1903     6525
## LIGHTNING                816     5230
## HEAT                     937     2100
## ICE STORM                 89     1975
## FLASH FLOOD              978     1777
## THUNDERSTORM WIND        133     1488
## HAIL                      15     1361
```

Code for barplots displayed in the results


```r
par(mar = c(12, 8, 4, 2))

fatalplot <- barplot(fatal_top_10$FATALITIES, 
                     names.arg = rownames(fatal_top_10), 
                     las = 2,
                     col = "light blue",
                     main = "Top 10 events causing fatalities over period studied",
                     ylab = "# deaths")
```

![](GM_Wk_4_RR_Assignment_files/figure-html/healthplots-1.png)<!-- -->

```r
injuryplot <- barplot(injury_top_10$INJURIES, 
                     names.arg = rownames(injury_top_10), 
                     las = 2,
                     col = "orange",
                     main = "Top 10 events causing injuries over period studied",
                     ylab = "# injuries")
```

![](GM_Wk_4_RR_Assignment_files/figure-html/healthplots-2.png)<!-- -->


##Results
####Across the United States, which types of events (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable) are most harmful with respect to population health?

TORNADO was the weather event responsible for the most injuries and fatalities across the United States:


```r
fatal
```

```
##         FATALITIES INJURIES
## TORNADO       5633    91346
```

```r
injury
```

```
##         FATALITIES INJURIES
## TORNADO       5633    91346
```

TORNADO was responsible for 5633 fatalities and 91346 injuries in the period studied.
