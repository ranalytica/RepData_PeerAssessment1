---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    code_folding: show
---



## Environment Settings

Not all packages are in one chunk code due to conflicts with lubridate either with VIM, mice or misforest package. 


```r
library(tidyverse)
library(reshape2)
library(lubridate)
library(gt)
library(glue)
library(paletteer)
library(scales)
```


```r
sessionInfo()
```

```
## R version 3.6.3 (2020-02-29)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Catalina 10.15.4
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] scales_1.1.0    paletteer_1.1.0 glue_1.3.2      gt_0.2.0.5     
##  [5] lubridate_1.7.8 reshape2_1.4.3  forcats_0.5.0   stringr_1.4.0  
##  [9] dplyr_0.8.5     purrr_0.3.3     readr_1.3.1     tidyr_1.0.2    
## [13] tibble_3.0.0    ggplot2_3.3.0   tidyverse_1.3.0
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.4        lattice_0.20-40   assertthat_0.2.1  digest_0.6.25    
##  [5] R6_2.4.1          cellranger_1.1.0  plyr_1.8.6        backports_1.1.5  
##  [9] reprex_0.3.0      oompaBase_3.2.9   evaluate_0.14     httr_1.4.1       
## [13] pillar_1.4.3      rlang_0.4.5       readxl_1.3.1      rstudioapi_0.11  
## [17] rmarkdown_2.1     munsell_0.5.0     broom_0.5.5       compiler_3.6.3   
## [21] modelr_0.1.6      xfun_0.12         pkgconfig_2.0.3   scico_1.1.0      
## [25] htmltools_0.4.0   tidyselect_1.0.0  viridisLite_0.3.0 fansi_0.4.1      
## [29] crayon_1.3.4      dbplyr_1.4.2      withr_2.1.2       jcolors_0.0.4    
## [33] grid_3.6.3        nlme_3.1-145      jsonlite_1.6.1    gtable_0.3.0     
## [37] lifecycle_0.2.0   DBI_1.1.0         magrittr_1.5      palr_0.2.0       
## [41] pals_1.6          cli_2.0.2         stringi_1.4.6     mapproj_1.2.7    
## [45] fs_1.3.2          xml2_1.2.5        ellipsis_0.3.0    generics_0.0.2   
## [49] vctrs_0.2.4       rematch2_2.1.1    tools_3.6.3       dichromat_2.0-0  
## [53] maps_3.3.0        hms_0.5.3         yaml_2.2.1        colorspace_1.4-1 
## [57] cluster_2.1.0     rvest_0.3.5       knitr_1.28        haven_2.2.0
```

## Load Data

Datasets are downloaded on `dateDownloaded`. 

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fname <- c("./data/pamd.zip")

source("LoadUnzip.R")
dateDownloaded
```

```
## [1] "Thu Apr 16 17:41:51 2020"
```

```r
# if(!file.exists("data")){
#         dir.create("data")
# }
# 
# download.file(fileUrl,destfile="./data/pamd.zip",method="curl")
# 
# dateDownloaded <- date()
# 
# maindf <- read_delim(fname, delim = ",", col_names = TRUE)
```

Range of Date


```r
start_date <- min(maindf$date)
end_date <- max(maindf$date)
range(maindf$date)
```

```
## [1] "2012-10-01" "2012-11-30"
```

## What is **mean** total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day?
Answer: MeanTotal gives us the breakdown of steps each day.

2. If you do not understand the difference between a histogram and a barplot, 
research the difference between them.  Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


GTMeanTotal is using the (gt) grammar for tables, scale, glue, and paletteer package. The yellow hightlighted portion gives as the Total steps - mean and median for 53 observations (I used *drop_na* from dplyr package to remove the NA). 


```r
MeanTotal <- maindf %>% drop_na() %>% group_by(date) %>%  summarize(Steps = sum(steps)) %>% mutate(m=month(date))
head(MeanTotal)
```

```
## # A tibble: 6 x 3
##   date       Steps     m
##   <date>     <dbl> <dbl>
## 1 2012-10-02   126    10
## 2 2012-10-03 11352    10
## 3 2012-10-04 12116    10
## 4 2012-10-05 13294    10
## 5 2012-10-06 15420    10
## 6 2012-10-07 11015    10
```
Mean and Median


```r
StepsMeanMed <- MeanTotal %>% summarize(Obs = length(date), SumofSteps = sum(Steps), StepsMean = mean(Steps), StepsMedian = median(Steps) ) 

GTMeanTotal <- StepsMeanMed %>% gt::gt() %>% data_color(
        columns = vars(Obs,SumofSteps, StepsMean,StepsMedian),
        colors = c("yellow")) %>% 
        tab_header(title = "Step Mean and Median Summary",
        subtitle = glue("{start_date} to {end_date}"))

GTMeanTotal
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xgwhbszczp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xgwhbszczp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xgwhbszczp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xgwhbszczp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xgwhbszczp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xgwhbszczp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xgwhbszczp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xgwhbszczp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xgwhbszczp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xgwhbszczp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xgwhbszczp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xgwhbszczp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#xgwhbszczp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xgwhbszczp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xgwhbszczp .gt_from_md > :first-child {
  margin-top: 0;
}

#xgwhbszczp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xgwhbszczp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xgwhbszczp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#xgwhbszczp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xgwhbszczp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#xgwhbszczp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xgwhbszczp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xgwhbszczp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xgwhbszczp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xgwhbszczp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#xgwhbszczp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xgwhbszczp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#xgwhbszczp .gt_left {
  text-align: left;
}

#xgwhbszczp .gt_center {
  text-align: center;
}

#xgwhbszczp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xgwhbszczp .gt_font_normal {
  font-weight: normal;
}

#xgwhbszczp .gt_font_bold {
  font-weight: bold;
}

#xgwhbszczp .gt_font_italic {
  font-style: italic;
}

#xgwhbszczp .gt_super {
  font-size: 65%;
}

#xgwhbszczp .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="xgwhbszczp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Step Mean and Median Summary</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SumofSteps</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMedian</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">53</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">570608</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10766.19</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10765</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

Histogram of Steps taken Each Day


```r
g1<-ggplot(data = MeanTotal, aes(x=Steps,fill=factor(m)))
gg1 <- g1+geom_histogram(bins = 9, alpha=.5)+geom_vline(xintercept = mean(MeanTotal$Steps))+
  labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Total Steps Each Day",
         x="STEPS",
         y="FREQUENCY",
         fill="MONTH")
gg1
```

![](PAMD_files/figure-html/Mean Steps histogram-1.png)<!-- -->


## What is the average daily activity pattern?
I'm using the gt (grammar for tables) package to analyze 5 minute interval average for the steps, see below: This table has 288 observations from 0-2355 with 5 minute interval.  

```r
SummaryIntervalSteps <- maindf %>% drop_na() %>% group_by(TimeInterval=interval)%>% 
        summarize(Total_Obs = length(interval), 
        SumOfSteps = sum(steps),
        MeanOfSteps = mean(steps)) 

GTInterval <- SummaryIntervalSteps %>% gt::gt() %>% data_color(
    columns = vars(SumOfSteps, MeanOfSteps),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
        ) %>% as.character(),
      domain = NULL
      )
  ) %>% tab_header(title = "Personal Movement Activity Monitoring Device",
        subtitle = glue("{start_date} to {end_date}"))
    
GTInterval    
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#slduhgqjyr .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#slduhgqjyr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#slduhgqjyr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#slduhgqjyr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#slduhgqjyr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#slduhgqjyr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#slduhgqjyr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#slduhgqjyr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#slduhgqjyr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#slduhgqjyr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#slduhgqjyr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#slduhgqjyr .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#slduhgqjyr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#slduhgqjyr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#slduhgqjyr .gt_from_md > :first-child {
  margin-top: 0;
}

#slduhgqjyr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#slduhgqjyr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#slduhgqjyr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#slduhgqjyr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#slduhgqjyr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#slduhgqjyr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#slduhgqjyr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#slduhgqjyr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#slduhgqjyr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#slduhgqjyr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#slduhgqjyr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#slduhgqjyr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#slduhgqjyr .gt_left {
  text-align: left;
}

#slduhgqjyr .gt_center {
  text-align: center;
}

#slduhgqjyr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#slduhgqjyr .gt_font_normal {
  font-weight: normal;
}

#slduhgqjyr .gt_font_bold {
  font-weight: bold;
}

#slduhgqjyr .gt_font_italic {
  font-style: italic;
}

#slduhgqjyr .gt_super {
  font-size: 65%;
}

#slduhgqjyr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="slduhgqjyr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Personal Movement Activity Monitoring Device</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">TimeInterval</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Total_Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SumOfSteps</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">MeanOfSteps</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">0</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">91</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.7169811</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">5</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">18</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">10</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">7</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">15</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">8</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">20</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">4</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">25</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">111</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.0943396</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">30</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">28</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.5283019</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">35</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">46</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.8679245</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">40</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">45</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">78</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.4716981</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">50</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">16</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">55</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">7</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">100</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">17</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">105</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">36</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">110</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">8</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">115</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">18</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">120</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">125</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">59</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">1.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">130</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">97</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.8301887</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">135</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">9</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">140</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">9</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">145</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">20</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3773585</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">150</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">14</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.2641509</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">155</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">200</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">205</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">210</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">60</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">1.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">215</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">220</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">225</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">7</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">230</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">235</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">12</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.2264151</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">240</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">245</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">250</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">82</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">255</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">50</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">300</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">305</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">310</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">315</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">320</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">11</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">325</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">33</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">330</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">86</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">335</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">31</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.5849057</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">340</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">26</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.4905660</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">345</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">4</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">350</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">355</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">400</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9ED; color: #000000;">63</td>
      <td class="gt_row gt_right" style="background-color: #FFE9ED; color: #000000;">1.1886792</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">405</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">50</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">410</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">136</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.5660377</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">415</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">420</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">18</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">425</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">19</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3584906</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">430</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">218</td>
      <td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">4.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">435</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">35</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">440</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE6EA; color: #000000;">185</td>
      <td class="gt_row gt_right" style="background-color: #FFE6EA; color: #000000;">3.4905660</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">445</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">44</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.8301887</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">450</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">165</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">3.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">455</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">59</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">1.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">500</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">505</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">83</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.5660377</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">510</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">159</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">3.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">515</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">119</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.2452830</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">520</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">176</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">3.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">525</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">157</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">2.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">530</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">111</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.0943396</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">535</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE3E7; color: #000000;">321</td>
      <td class="gt_row gt_right" style="background-color: #FFE3E7; color: #000000;">6.0566038</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">540</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">849</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">16.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">545</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD3D8; color: #000000;">972</td>
      <td class="gt_row gt_right" style="background-color: #FFD3D8; color: #000000;">18.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">550</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A8A9; color: #000000;">2091</td>
      <td class="gt_row gt_right" style="background-color: #F4A8A9; color: #000000;">39.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">555</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">2358</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">44.4905660</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">600</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">1669</td>
      <td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">31.4905660</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">605</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9494; color: #000000;">2611</td>
      <td class="gt_row gt_right" style="background-color: #EE9494; color: #000000;">49.2641509</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">610</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8C; color: #000000;">2850</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8C; color: #000000;">53.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">615</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">3363</td>
      <td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">63.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">620</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">2648</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">49.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">625</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9898; color: #000000;">2495</td>
      <td class="gt_row gt_right" style="background-color: #EF9898; color: #000000;">47.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">630</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">2764</td>
      <td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">52.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">635</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A8AA; color: #000000;">2085</td>
      <td class="gt_row gt_right" style="background-color: #F4A8AA; color: #000000;">39.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">640</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">2333</td>
      <td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">44.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">645</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">2341</td>
      <td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">44.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">650</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">1980</td>
      <td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">37.3584906</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">655</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9594; color: #000000;">2599</td>
      <td class="gt_row gt_right" style="background-color: #EE9594; color: #000000;">49.0377358</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">700</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">2322</td>
      <td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">43.8113208</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">705</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">2352</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">44.3773585</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">710</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9292; color: #000000;">2677</td>
      <td class="gt_row gt_right" style="background-color: #ED9292; color: #000000;">50.5094340</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">715</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">2889</td>
      <td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">54.5094340</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">720</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">2646</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">49.9245283</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">725</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9191; color: #000000;">2702</td>
      <td class="gt_row gt_right" style="background-color: #ED9191; color: #000000;">50.9811321</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">730</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">2951</td>
      <td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">55.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">735</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">2349</td>
      <td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">44.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">740</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">2770</td>
      <td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">52.2641509</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">745</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E57272; color: #000000;">3686</td>
      <td class="gt_row gt_right" style="background-color: #E57272; color: #000000;">69.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">750</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA8685; color: #000000;">3066</td>
      <td class="gt_row gt_right" style="background-color: #EA8685; color: #000000;">57.8490566</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">755</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8988; color: #000000;">2976</td>
      <td class="gt_row gt_right" style="background-color: #EB8988; color: #000000;">56.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">800</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E76D6C; color: #000000;">3889</td>
      <td class="gt_row gt_right" style="background-color: #E76D6C; color: #000000;">73.3773585</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">805</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E57474; color: #000000;">3615</td>
      <td class="gt_row gt_right" style="background-color: #E57474; color: #000000;">68.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">810</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA3D35; color: #FFFFFF;">6860</td>
      <td class="gt_row gt_right" style="background-color: #EA3D35; color: #FFFFFF;">129.4339623</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">815</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #D53030; color: #FFFFFF;">8349</td>
      <td class="gt_row gt_right" style="background-color: #D53030; color: #FFFFFF;">157.5283019</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">820</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #CD2C2C; color: #FFFFFF;">9071</td>
      <td class="gt_row gt_right" style="background-color: #CD2C2C; color: #FFFFFF;">171.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">825</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #D73130; color: #FFFFFF;">8236</td>
      <td class="gt_row gt_right" style="background-color: #D73130; color: #FFFFFF;">155.3962264</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">830</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #C92A2A; color: #FFFFFF;">9397</td>
      <td class="gt_row gt_right" style="background-color: #C92A2A; color: #FFFFFF;">177.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">835</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #B71C1C; color: #FFFFFF;">10927</td>
      <td class="gt_row gt_right" style="background-color: #B71C1C; color: #FFFFFF;">206.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">840</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #BE2221; color: #FFFFFF;">10384</td>
      <td class="gt_row gt_right" style="background-color: #BE2221; color: #FFFFFF;">195.9245283</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">845</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #C82929; color: #FFFFFF;">9517</td>
      <td class="gt_row gt_right" style="background-color: #C82929; color: #FFFFFF;">179.5660377</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">850</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #C62828; color: #FFFFFF;">9720</td>
      <td class="gt_row gt_right" style="background-color: #C62828; color: #FFFFFF;">183.3962264</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">855</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #CF2D2D; color: #FFFFFF;">8852</td>
      <td class="gt_row gt_right" style="background-color: #CF2D2D; color: #FFFFFF;">167.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">900</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E03633; color: #FFFFFF;">7603</td>
      <td class="gt_row gt_right" style="background-color: #E03633; color: #FFFFFF;">143.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">905</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE3F36; color: #FFFFFF;">6574</td>
      <td class="gt_row gt_right" style="background-color: #EE3F36; color: #FFFFFF;">124.0377358</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">910</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F3473C; color: #FFFFFF;">5783</td>
      <td class="gt_row gt_right" style="background-color: #F3473C; color: #FFFFFF;">109.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">915</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F3483D; color: #FFFFFF;">5730</td>
      <td class="gt_row gt_right" style="background-color: #F3483D; color: #FFFFFF;">108.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">920</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F24B42; color: #FFFFFF;">5497</td>
      <td class="gt_row gt_right" style="background-color: #F24B42; color: #FFFFFF;">103.7169811</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">925</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F0504B; color: #000000;">5086</td>
      <td class="gt_row gt_right" style="background-color: #F0504B; color: #000000;">95.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">930</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E67777; color: #000000;">3509</td>
      <td class="gt_row gt_right" style="background-color: #E67777; color: #000000;">66.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">935</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">2397</td>
      <td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">45.2264151</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">940</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FEC9CD; color: #000000;">1314</td>
      <td class="gt_row gt_right" style="background-color: #FEC9CD; color: #000000;">24.7924528</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">945</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">2054</td>
      <td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">38.7547170</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">950</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F7B2B4; color: #000000;">1854</td>
      <td class="gt_row gt_right" style="background-color: #F7B2B4; color: #000000;">34.9811321</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">955</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">1116</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">21.0566038</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1000</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F3A6A7; color: #000000;">2150</td>
      <td class="gt_row gt_right" style="background-color: #F3A6A7; color: #000000;">40.5660377</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1005</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">1430</td>
      <td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">26.9811321</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1010</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">2248</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">42.4150943</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1015</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8F8E; color: #000000;">2791</td>
      <td class="gt_row gt_right" style="background-color: #EC8F8E; color: #000000;">52.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1020</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">2063</td>
      <td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">38.9245283</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1025</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">2692</td>
      <td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">50.7924528</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1030</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">2347</td>
      <td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">44.2830189</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1035</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">1983</td>
      <td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">37.4150943</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1040</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">1839</td>
      <td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">34.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1045</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">1502</td>
      <td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">28.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1050</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FEC8CD; color: #000000;">1330</td>
      <td class="gt_row gt_right" style="background-color: #FEC8CD; color: #000000;">25.0943396</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1055</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F9B9BC; color: #000000;">1693</td>
      <td class="gt_row gt_right" style="background-color: #F9B9BC; color: #000000;">31.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1100</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">1662</td>
      <td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">31.3584906</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1105</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FBBEC1; color: #000000;">1573</td>
      <td class="gt_row gt_right" style="background-color: #FBBEC1; color: #000000;">29.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1110</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">1130</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">21.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1115</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">1354</td>
      <td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">25.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1120</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">1504</td>
      <td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">28.3773585</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1125</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">1403</td>
      <td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">26.4716981</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1130</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">1772</td>
      <td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">33.4339623</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1135</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">2649</td>
      <td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">49.9811321</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1140</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A3; color: #000000;">2228</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A3; color: #000000;">42.0377358</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1145</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">2364</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">44.6037736</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1150</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">2440</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">46.0377358</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1155</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">3137</td>
      <td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">59.1886792</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1200</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">3385</td>
      <td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">63.8679245</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1205</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE5956; color: #000000;">4648</td>
      <td class="gt_row gt_right" style="background-color: #EE5956; color: #000000;">87.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1210</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F0514C; color: #000000;">5027</td>
      <td class="gt_row gt_right" style="background-color: #F0514C; color: #000000;">94.8490566</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1215</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF524F; color: #000000;">4917</td>
      <td class="gt_row gt_right" style="background-color: #EF524F; color: #000000;">92.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1220</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">3360</td>
      <td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">63.3962264</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1225</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9392; color: #000000;">2659</td>
      <td class="gt_row gt_right" style="background-color: #ED9392; color: #000000;">50.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1230</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">2887</td>
      <td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">54.4716981</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1235</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F9B8BA; color: #000000;">1718</td>
      <td class="gt_row gt_right" style="background-color: #F9B8BA; color: #000000;">32.4150943</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1240</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">1406</td>
      <td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">26.5283019</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1245</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F5ACAD; color: #000000;">2000</td>
      <td class="gt_row gt_right" style="background-color: #F5ACAD; color: #000000;">37.7358491</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1250</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09C9C; color: #000000;">2388</td>
      <td class="gt_row gt_right" style="background-color: #F09C9C; color: #000000;">45.0566038</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1255</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E67675; color: #000000;">3566</td>
      <td class="gt_row gt_right" style="background-color: #E67675; color: #000000;">67.2830189</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1300</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">2244</td>
      <td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">42.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1305</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">2114</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">39.8867925</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1310</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F1A0A0; color: #000000;">2293</td>
      <td class="gt_row gt_right" style="background-color: #F1A0A0; color: #000000;">43.2641509</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1315</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">2172</td>
      <td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">40.9811321</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1320</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">2451</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">46.2452830</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1325</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">2991</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">56.4339623</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1330</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F1A1A1; color: #000000;">2266</td>
      <td class="gt_row gt_right" style="background-color: #F1A1A1; color: #000000;">42.7547170</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1335</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FEC8CC; color: #000000;">1332</td>
      <td class="gt_row gt_right" style="background-color: #FEC8CC; color: #000000;">25.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1340</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">2118</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">39.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1345</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">2838</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">53.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1350</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">2508</td>
      <td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">47.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1355</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E98180; color: #000000;">3223</td>
      <td class="gt_row gt_right" style="background-color: #E98180; color: #000000;">60.8113208</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1400</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">2955</td>
      <td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">55.7547170</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1405</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED908F; color: #000000;">2754</td>
      <td class="gt_row gt_right" style="background-color: #ED908F; color: #000000;">51.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1410</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">2310</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">43.5849057</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1415</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9595; color: #000000;">2581</td>
      <td class="gt_row gt_right" style="background-color: #EE9595; color: #000000;">48.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1420</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">1880</td>
      <td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">35.4716981</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1425</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F5ACAE; color: #000000;">1990</td>
      <td class="gt_row gt_right" style="background-color: #F5ACAE; color: #000000;">37.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1430</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F2A3A3; color: #000000;">2218</td>
      <td class="gt_row gt_right" style="background-color: #F2A3A3; color: #000000;">41.8490566</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1435</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">1458</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">27.5094340</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1440</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD5D9; color: #000000;">907</td>
      <td class="gt_row gt_right" style="background-color: #FFD5D9; color: #000000;">17.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1445</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC6CA; color: #000000;">1382</td>
      <td class="gt_row gt_right" style="background-color: #FDC6CA; color: #000000;">26.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1450</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">2312</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">43.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1455</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">2320</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">43.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1500</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">1591</td>
      <td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">30.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1505</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F6B0B1; color: #000000;">1912</td>
      <td class="gt_row gt_right" style="background-color: #F6B0B1; color: #000000;">36.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1510</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">1881</td>
      <td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">35.4905660</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1515</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">2059</td>
      <td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">38.8490566</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1520</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">2436</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">45.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1525</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">2531</td>
      <td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">47.7547170</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1530</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EE9696; color: #000000;">2551</td>
      <td class="gt_row gt_right" style="background-color: #EE9696; color: #000000;">48.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1535</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E77979; color: #000000;">3462</td>
      <td class="gt_row gt_right" style="background-color: #E77979; color: #000000;">65.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1540</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC605D; color: #000000;">4394</td>
      <td class="gt_row gt_right" style="background-color: #EC605D; color: #000000;">82.9056604</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1545</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F14E48; color: #000000;">5229</td>
      <td class="gt_row gt_right" style="background-color: #F14E48; color: #000000;">98.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1550</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F14C44; color: #FFFFFF;">5412</td>
      <td class="gt_row gt_right" style="background-color: #F14C44; color: #FFFFFF;">102.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1555</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC5F5C; color: #000000;">4450</td>
      <td class="gt_row gt_right" style="background-color: #EC5F5C; color: #000000;">83.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1600</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E87E7E; color: #000000;">3293</td>
      <td class="gt_row gt_right" style="background-color: #E87E7E; color: #000000;">62.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1605</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">3399</td>
      <td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">64.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1610</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">3951</td>
      <td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">74.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1615</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E87D7C; color: #000000;">3348</td>
      <td class="gt_row gt_right" style="background-color: #E87D7C; color: #000000;">63.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1620</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8787; color: #000000;">3016</td>
      <td class="gt_row gt_right" style="background-color: #EB8787; color: #000000;">56.9056604</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1625</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E98282; color: #000000;">3168</td>
      <td class="gt_row gt_right" style="background-color: #E98282; color: #000000;">59.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1630</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">2325</td>
      <td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">43.8679245</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1635</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">2044</td>
      <td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">38.5660377</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1640</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">2367</td>
      <td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">44.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1645</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">2409</td>
      <td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">45.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1650</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">2449</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">46.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1655</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">2315</td>
      <td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">43.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1700</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">2471</td>
      <td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">46.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1705</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">2984</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">56.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1710</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">2688</td>
      <td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">50.7169811</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1715</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E98080; color: #000000;">3245</td>
      <td class="gt_row gt_right" style="background-color: #E98080; color: #000000;">61.2264151</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1720</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E76E6D; color: #000000;">3854</td>
      <td class="gt_row gt_right" style="background-color: #E76E6D; color: #000000;">72.7169811</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1725</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA6663; color: #000000;">4184</td>
      <td class="gt_row gt_right" style="background-color: #EA6663; color: #000000;">78.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1730</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E57373; color: #000000;">3654</td>
      <td class="gt_row gt_right" style="background-color: #E57373; color: #000000;">68.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1735</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E98382; color: #000000;">3162</td>
      <td class="gt_row gt_right" style="background-color: #E98382; color: #000000;">59.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1740</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E86B69; color: #000000;">3980</td>
      <td class="gt_row gt_right" style="background-color: #E86B69; color: #000000;">75.0943396</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1745</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">2995</td>
      <td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">56.5094340</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1750</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">1843</td>
      <td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">34.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1755</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F5ADAE; color: #000000;">1985</td>
      <td class="gt_row gt_right" style="background-color: #F5ADAE; color: #000000;">37.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1800</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">2156</td>
      <td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">40.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1805</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">3075</td>
      <td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">58.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1810</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E86B6A; color: #000000;">3959</td>
      <td class="gt_row gt_right" style="background-color: #E86B6A; color: #000000;">74.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1815</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">4522</td>
      <td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">85.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1820</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">3141</td>
      <td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">59.2641509</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1825</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E57575; color: #000000;">3592</td>
      <td class="gt_row gt_right" style="background-color: #E57575; color: #000000;">67.7735849</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1830</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">4118</td>
      <td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">77.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1835</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">3935</td>
      <td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">74.2452830</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1840</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">4523</td>
      <td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">85.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1845</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F14E47; color: #000000;">5271</td>
      <td class="gt_row gt_right" style="background-color: #F14E47; color: #000000;">99.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1850</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED5B58; color: #000000;">4589</td>
      <td class="gt_row gt_right" style="background-color: #ED5B58; color: #000000;">86.5849057</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1855</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #ED5C59; color: #000000;">4537</td>
      <td class="gt_row gt_right" style="background-color: #ED5C59; color: #000000;">85.6037736</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1900</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC5D5A; color: #000000;">4498</td>
      <td class="gt_row gt_right" style="background-color: #EC5D5A; color: #000000;">84.8679245</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1905</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">4125</td>
      <td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">77.8301887</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1910</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">3076</td>
      <td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">58.0377358</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1915</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">2828</td>
      <td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">53.3584906</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1920</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F6AFB1; color: #000000;">1925</td>
      <td class="gt_row gt_right" style="background-color: #F6AFB1; color: #000000;">36.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1925</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD0D5; color: #000000;">1098</td>
      <td class="gt_row gt_right" style="background-color: #FFD0D5; color: #000000;">20.7169811</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1930</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">1452</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">27.3962264</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1935</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">2121</td>
      <td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">40.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1940</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">1601</td>
      <td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">30.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1945</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">1354</td>
      <td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">25.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1950</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">2420</td>
      <td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">45.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">1955</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F8B5B8; color: #000000;">1777</td>
      <td class="gt_row gt_right" style="background-color: #F8B5B8; color: #000000;">33.5283019</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2000</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">1040</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">19.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2005</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D7; color: #000000;">1008</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D7; color: #000000;">19.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2010</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">1025</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">19.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2015</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">1767</td>
      <td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">33.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2020</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">1421</td>
      <td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">26.8113208</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2025</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">1122</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">21.1698113</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2030</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">1447</td>
      <td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">27.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2035</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">1131</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">21.3396226</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2040</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">1036</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">19.5471698</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2045</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">1130</td>
      <td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">21.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2050</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #F9B8BB; color: #000000;">1712</td>
      <td class="gt_row gt_right" style="background-color: #F9B8BB; color: #000000;">32.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2055</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D5; color: #000000;">1068</td>
      <td class="gt_row gt_right" style="background-color: #FFD1D5; color: #000000;">20.1509434</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2100</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">845</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">15.9433962</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2105</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD4D9; color: #000000;">913</td>
      <td class="gt_row gt_right" style="background-color: #FFD4D9; color: #000000;">17.2264151</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2110</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFCCD1; color: #000000;">1243</td>
      <td class="gt_row gt_right" style="background-color: #FFCCD1; color: #000000;">23.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2115</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">1020</td>
      <td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">19.2452830</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2120</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFDBDF; color: #000000;">660</td>
      <td class="gt_row gt_right" style="background-color: #FFDBDF; color: #000000;">12.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2125</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">425</td>
      <td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">8.0188679</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2130</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD8DC; color: #000000;">777</td>
      <td class="gt_row gt_right" style="background-color: #FFD8DC; color: #000000;">14.6603774</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2135</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">864</td>
      <td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">16.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2140</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">460</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">8.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2145</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">413</td>
      <td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">7.7924528</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2150</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">431</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">8.1320755</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2155</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">139</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.6226415</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2200</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">77</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.4528302</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2205</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">195</td>
      <td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">3.6792453</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2210</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">255</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">4.8113208</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2215</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">451</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">8.5094340</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2220</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE2E5; color: #000000;">375</td>
      <td class="gt_row gt_right" style="background-color: #FFE2E5; color: #000000;">7.0754717</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2225</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">461</td>
      <td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">8.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2230</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFDEE2; color: #000000;">517</td>
      <td class="gt_row gt_right" style="background-color: #FFDEE2; color: #000000;">9.7547170</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2235</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">117</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.2075472</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2240</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">17</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.3207547</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2245</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">6</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.1132075</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2250</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">85</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.6037736</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2255</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">244</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">4.6037736</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2300</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">175</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">3.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2305</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">151</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">2.8490566</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2310</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2315</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">44</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.8301887</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2320</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">51</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.9622642</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2325</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">84</td>
      <td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">1.5849057</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2330</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">138</td>
      <td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">2.6037736</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2335</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">249</td>
      <td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">4.6981132</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2340</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">175</td>
      <td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">3.3018868</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2345</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">34</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">0.6415094</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2350</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">12</td>
      <td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">0.2264151</td>
    </tr>
    <tr>
      <td class="gt_row gt_right">2355</td>
      <td class="gt_row gt_center">53</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">57</td>
      <td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">1.0754717</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 
5-minute interval (x-axis) and the average number of steps taken, averaged 
across all days (y-axis)


```r
g2<-ggplot(data = SummaryIntervalSteps, aes(y=MeanOfSteps, x=TimeInterval))
g2+geom_line(color="red")+
        labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Average Steps Each Day",
         x="Time Interval ",
         y="Average Steps",
         fill="MONTH") 
```

![](PAMD_files/figure-html/Time Series-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps? 8:35 a.m.


```r
SummaryIntervalSteps %>%  filter(MeanOfSteps==max(MeanOfSteps)) %>% select(TimeInterval, MeanOfSteps)
```

```
## # A tibble: 1 x 2
##   TimeInterval MeanOfSteps
##          <dbl>       <dbl>
## 1          835        206.
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
Yeardf <- maindf %>% group_by(`Year`=year(date)) %>%  
        summarize(ISNA_Count = sum(is.na(steps)), Mean_ISNA=mean(is.na(steps)), 
        Total_Obs = length(maindf$date)) %>% ungroup() %>% gt() %>% 
        tab_header(title = "Year 'IS.NA' Summary",
        subtitle = glue("{start_date} to {end_date}"))
Yeardf
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hdnfegzsxr .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hdnfegzsxr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hdnfegzsxr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hdnfegzsxr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hdnfegzsxr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hdnfegzsxr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hdnfegzsxr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hdnfegzsxr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hdnfegzsxr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hdnfegzsxr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hdnfegzsxr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hdnfegzsxr .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hdnfegzsxr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hdnfegzsxr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hdnfegzsxr .gt_from_md > :first-child {
  margin-top: 0;
}

#hdnfegzsxr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hdnfegzsxr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hdnfegzsxr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hdnfegzsxr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hdnfegzsxr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hdnfegzsxr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hdnfegzsxr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hdnfegzsxr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hdnfegzsxr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hdnfegzsxr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hdnfegzsxr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hdnfegzsxr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hdnfegzsxr .gt_left {
  text-align: left;
}

#hdnfegzsxr .gt_center {
  text-align: center;
}

#hdnfegzsxr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hdnfegzsxr .gt_font_normal {
  font-weight: normal;
}

#hdnfegzsxr .gt_font_bold {
  font-weight: bold;
}

#hdnfegzsxr .gt_font_italic {
  font-style: italic;
}

#hdnfegzsxr .gt_super {
  font-size: 65%;
}

#hdnfegzsxr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="hdnfegzsxr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Year 'IS.NA' Summary</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Year</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">ISNA_Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Mean_ISNA</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Total_Obs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_right">2012</td>
      <td class="gt_row gt_center">2304</td>
      <td class="gt_row gt_right">0.1311475</td>
      <td class="gt_row gt_center">17568</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

Monthly Missing Data


```r
Monthdf <- maindf %>% group_by(M=month(date,label=TRUE, abbr=TRUE)) %>% 
  summarize(Total_Obs = length(date), ISNA_Count = sum(is.na(steps)),
            Mean_ISNA=mean(is.na(steps))) 

MonthdfGT <- Monthdf %>% gt() %>% 
  tab_header(
    title = "Monthly 'IS.NA' Summary",
    subtitle = glue("{start_date} to {end_date}"))
MonthdfGT
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yudbhtmfch .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yudbhtmfch .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yudbhtmfch .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yudbhtmfch .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yudbhtmfch .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yudbhtmfch .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yudbhtmfch .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yudbhtmfch .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yudbhtmfch .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yudbhtmfch .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yudbhtmfch .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yudbhtmfch .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#yudbhtmfch .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yudbhtmfch .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yudbhtmfch .gt_from_md > :first-child {
  margin-top: 0;
}

#yudbhtmfch .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yudbhtmfch .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yudbhtmfch .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yudbhtmfch .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yudbhtmfch .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yudbhtmfch .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yudbhtmfch .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yudbhtmfch .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yudbhtmfch .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yudbhtmfch .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yudbhtmfch .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yudbhtmfch .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yudbhtmfch .gt_left {
  text-align: left;
}

#yudbhtmfch .gt_center {
  text-align: center;
}

#yudbhtmfch .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yudbhtmfch .gt_font_normal {
  font-weight: normal;
}

#yudbhtmfch .gt_font_bold {
  font-weight: bold;
}

#yudbhtmfch .gt_font_italic {
  font-style: italic;
}

#yudbhtmfch .gt_super {
  font-size: 65%;
}

#yudbhtmfch .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="yudbhtmfch" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Monthly 'IS.NA' Summary</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">M</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Total_Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">ISNA_Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Mean_ISNA</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">Oct</td>
      <td class="gt_row gt_center">8928</td>
      <td class="gt_row gt_center">576</td>
      <td class="gt_row gt_right">0.06451613</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Nov</td>
      <td class="gt_row gt_center">8640</td>
      <td class="gt_row gt_center">1728</td>
      <td class="gt_row gt_right">0.20000000</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->


```r
dailydf <-
    maindf %>%  group_by(
      Daily = wday(date, label = TRUE, abbr = TRUE)) %>%
    summarize(
      Total_Obs = length(date),
      ISNA_Count = sum(is.na(steps)),
      Mean_ISNA = mean(is.na(steps))
  )

  dailydf %>% gt() %>%
    tab_header(title = "By day 'IS.NA' Summary",
               subtitle = glue("{start_date} to {end_date}"))
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ghtjqxrlue .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ghtjqxrlue .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ghtjqxrlue .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ghtjqxrlue .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ghtjqxrlue .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ghtjqxrlue .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ghtjqxrlue .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ghtjqxrlue .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ghtjqxrlue .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ghtjqxrlue .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ghtjqxrlue .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ghtjqxrlue .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ghtjqxrlue .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ghtjqxrlue .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ghtjqxrlue .gt_from_md > :first-child {
  margin-top: 0;
}

#ghtjqxrlue .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ghtjqxrlue .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ghtjqxrlue .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ghtjqxrlue .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ghtjqxrlue .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ghtjqxrlue .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ghtjqxrlue .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ghtjqxrlue .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ghtjqxrlue .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ghtjqxrlue .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ghtjqxrlue .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ghtjqxrlue .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ghtjqxrlue .gt_left {
  text-align: left;
}

#ghtjqxrlue .gt_center {
  text-align: center;
}

#ghtjqxrlue .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ghtjqxrlue .gt_font_normal {
  font-weight: normal;
}

#ghtjqxrlue .gt_font_bold {
  font-weight: bold;
}

#ghtjqxrlue .gt_font_italic {
  font-style: italic;
}

#ghtjqxrlue .gt_super {
  font-size: 65%;
}

#ghtjqxrlue .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ghtjqxrlue" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>By day 'IS.NA' Summary</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Daily</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Total_Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">ISNA_Count</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Mean_ISNA</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">Sun</td>
      <td class="gt_row gt_center">2304</td>
      <td class="gt_row gt_center">288</td>
      <td class="gt_row gt_right">0.1250000</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Mon</td>
      <td class="gt_row gt_center">2592</td>
      <td class="gt_row gt_center">576</td>
      <td class="gt_row gt_right">0.2222222</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Tue</td>
      <td class="gt_row gt_center">2592</td>
      <td class="gt_row gt_center">0</td>
      <td class="gt_row gt_right">0.0000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Wed</td>
      <td class="gt_row gt_center">2592</td>
      <td class="gt_row gt_center">288</td>
      <td class="gt_row gt_right">0.1111111</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Thu</td>
      <td class="gt_row gt_center">2592</td>
      <td class="gt_row gt_center">288</td>
      <td class="gt_row gt_right">0.1111111</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Fri</td>
      <td class="gt_row gt_center">2592</td>
      <td class="gt_row gt_center">576</td>
      <td class="gt_row gt_right">0.2222222</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">Sat</td>
      <td class="gt_row gt_center">2304</td>
      <td class="gt_row gt_center">288</td>
      <td class="gt_row gt_right">0.1250000</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



```r
g3 <- ggplot(data = dailydf, aes(Daily))

g3 + geom_bar(aes(weight = ISNA_Count))+
        labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Missing data in Week Days",
         x="Daily",
         y="Frequency") 
```

![](PAMD_files/figure-html/histogram by days-1.png)<!-- -->

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# load these library at these section because it affects lubridate labels
# and abbr
library(mice)
library(missForest)
library(VIM)
```

Using aggr function to plot NA.  


```r
aggr(maindf, col=c('navyblue', 'red'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(maindf), cex.axis=.8,
     gap=5, ylab=c("Missing Data", "Pattern"))
```

![](PAMD_files/figure-html/aggr vim-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##  Variable     Count
##     steps 0.1311475
##      date 0.0000000
##  interval 0.0000000
```
I'm using Predictive Mean Matching (PMM) model to impute the missing data.
1. m = number of multiple imputations. Default is 5 and I picked 3.  
2. maxit = A scalar giving the number of imputations. the default is 5 and I chose 50
3. Method = pmm
4. seed = set.seed


```r
imputed_maindf1 <- mice(maindf, m=3, maxit = 50, 
                        method = 'pmm', seed = 420)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   6   1  steps
##   6   2  steps
##   6   3  steps
##   7   1  steps
##   7   2  steps
##   7   3  steps
##   8   1  steps
##   8   2  steps
##   8   3  steps
##   9   1  steps
##   9   2  steps
##   9   3  steps
##   10   1  steps
##   10   2  steps
##   10   3  steps
##   11   1  steps
##   11   2  steps
##   11   3  steps
##   12   1  steps
##   12   2  steps
##   12   3  steps
##   13   1  steps
##   13   2  steps
##   13   3  steps
##   14   1  steps
##   14   2  steps
##   14   3  steps
##   15   1  steps
##   15   2  steps
##   15   3  steps
##   16   1  steps
##   16   2  steps
##   16   3  steps
##   17   1  steps
##   17   2  steps
##   17   3  steps
##   18   1  steps
##   18   2  steps
##   18   3  steps
##   19   1  steps
##   19   2  steps
##   19   3  steps
##   20   1  steps
##   20   2  steps
##   20   3  steps
##   21   1  steps
##   21   2  steps
##   21   3  steps
##   22   1  steps
##   22   2  steps
##   22   3  steps
##   23   1  steps
##   23   2  steps
##   23   3  steps
##   24   1  steps
##   24   2  steps
##   24   3  steps
##   25   1  steps
##   25   2  steps
##   25   3  steps
##   26   1  steps
##   26   2  steps
##   26   3  steps
##   27   1  steps
##   27   2  steps
##   27   3  steps
##   28   1  steps
##   28   2  steps
##   28   3  steps
##   29   1  steps
##   29   2  steps
##   29   3  steps
##   30   1  steps
##   30   2  steps
##   30   3  steps
##   31   1  steps
##   31   2  steps
##   31   3  steps
##   32   1  steps
##   32   2  steps
##   32   3  steps
##   33   1  steps
##   33   2  steps
##   33   3  steps
##   34   1  steps
##   34   2  steps
##   34   3  steps
##   35   1  steps
##   35   2  steps
##   35   3  steps
##   36   1  steps
##   36   2  steps
##   36   3  steps
##   37   1  steps
##   37   2  steps
##   37   3  steps
##   38   1  steps
##   38   2  steps
##   38   3  steps
##   39   1  steps
##   39   2  steps
##   39   3  steps
##   40   1  steps
##   40   2  steps
##   40   3  steps
##   41   1  steps
##   41   2  steps
##   41   3  steps
##   42   1  steps
##   42   2  steps
##   42   3  steps
##   43   1  steps
##   43   2  steps
##   43   3  steps
##   44   1  steps
##   44   2  steps
##   44   3  steps
##   45   1  steps
##   45   2  steps
##   45   3  steps
##   46   1  steps
##   46   2  steps
##   46   3  steps
##   47   1  steps
##   47   2  steps
##   47   3  steps
##   48   1  steps
##   48   2  steps
##   48   3  steps
##   49   1  steps
##   49   2  steps
##   49   3  steps
##   50   1  steps
##   50   2  steps
##   50   3  steps
```

Summary of the Imputed Data.  


```r
summary(imputed_maindf1)
```

```
## Class: mids
## Number of multiple imputations:  3 
## Imputation methods:
##    steps     date interval 
##    "pmm"       ""       "" 
## PredictorMatrix:
##          steps date interval
## steps        0    1        1
## date         1    0        1
## interval     1    1        0
```

Multiple imputations (m=3)


```r
imputed_maindf1$imp$steps
```

```
##         1   2   3
## 1       0  47  47
## 2       0   0   0
## 3       0   0   0
## 4       0   0   0
## 5       0   0   0
## 6       0   0   0
## 7       0   0   0
## 8       0  47  47
## 9       0   0   0
## 10      0   0   0
## 11      0  47  47
## 12      0   0   0
## 13      0   0   0
## 14      0   0   0
## 15      0   0   0
## 16      0   0   0
## 17      0   0   0
## 18      0   0   0
## 19      0   0   0
## 20      0   0   0
## 21      0   0   0
## 22      0   0   0
## 23      0  47  47
## 24      0   0  47
## 25      0   0   0
## 26      0   0   0
## 27      0   0   0
## 28      0   0  47
## 29      0   0   0
## 30      0   0  47
## 31      0   0   0
## 32      0  47   0
## 33      0   0   0
## 34      0   0  47
## 35      0   0   0
## 36      0   0   0
## 37     22   0   0
## 38     10  47   0
## 39      0  47   0
## 40      0  47   0
## 41    280   0   0
## 42      0   0   0
## 43      0   0   0
## 44      0   0   0
## 45     54   0   0
## 46     20   0   0
## 47      0   0   0
## 48    181   0   0
## 49    167   0   0
## 50    401   0   0
## 51     49   0   0
## 52      0   0   0
## 53      0   0   4
## 54      0   0   0
## 55      0   0   0
## 56      0   0   0
## 57      0   0   0
## 58      7   0   0
## 59      0   0   0
## 60    758   0   0
## 61    748   0   0
## 62      0   0   0
## 63    377   0   0
## 64      0   0   0
## 65    665   0   0
## 66     47   0   0
## 67      0   0   0
## 68     15   0   0
## 69    245   0   0
## 70     59   0   0
## 71      0   0   0
## 72      0   0   0
## 73      0   0  33
## 74    319   0   0
## 75     60   0   0
## 76    123   0   0
## 77      0   0   0
## 78    517   0   0
## 79      0   0   0
## 80      0   0   0
## 81      0   0   0
## 82      0   0   0
## 83      0   0  12
## 84      0   0   0
## 85      0   0   0
## 86      0   0   0
## 87      0   0   0
## 88      0   0  10
## 89     29   0   0
## 90      0   0  55
## 91      0   0   0
## 92      0   0   0
## 93      9   0   0
## 94     11   4   8
## 95      5   0   0
## 96      0   0   0
## 97     10   0   0
## 98     34   0   0
## 99     47   0   0
## 100     0   0  24
## 101     0   0  60
## 102     0   9  28
## 103     0   0   0
## 104     0   0   0
## 105     0   0  11
## 106    45   0   0
## 107     0   0   0
## 108    85   0   0
## 109     0   0 556
## 110     0   0   0
## 111     0   0   0
## 112   485   0   0
## 113     0   0 513
## 114    66   0   0
## 115    10  16 530
## 116     0   0  13
## 117     0   0   0
## 118     0   0  36
## 119     0   0  12
## 120    99   0 119
## 121     0   0   0
## 122     0  50 148
## 123     0   0 508
## 124     0   0   0
## 125     0  45   0
## 126     0  25   0
## 127   181   0  46
## 128     0 251   0
## 129   489   0   0
## 130   463   0   0
## 131     0   0   0
## 132     0   0   0
## 133     0   9   0
## 134     0   0   0
## 135     0  43   0
## 136     0  13   0
## 137     0  60 116
## 138     0   0   0
## 139     0  65   0
## 140     0   0   0
## 141     0   0  20
## 142     4   0   0
## 143     0   0   0
## 144   360   0 320
## 145     0   0   0
## 146     0  35   0
## 147    27   0  27
## 148     0  99 450
## 149     0 423   0
## 150     0  75   0
## 151    28  34   0
## 152    19   0   0
## 153   532 770  28
## 154    54   0 511
## 155     0   0   0
## 156     0   0  19
## 157    24   0   0
## 158     0 173   0
## 159     0  43   7
## 160     0   0   0
## 161   197   0   0
## 162   427   0   0
## 163    40   0   0
## 164     0   0   0
## 165     0   0  44
## 166     0   0   0
## 167     0   0  17
## 168     0   0   0
## 169    39   0   0
## 170     0  73   0
## 171    49   0   4
## 172     0   0   0
## 173    43   0   0
## 174     0   0   0
## 175     0   0 533
## 176    25   0   0
## 177    32   0 334
## 178     0   0   0
## 179     0   0   0
## 180    17   0   0
## 181    21   0   0
## 182     0   0  10
## 183    45 444   0
## 184     6 150   0
## 185     0   0 411
## 186   103   0 533
## 187   159   0   0
## 188    32 143   0
## 189     0  66   0
## 190     0   0   0
## 191     0   0   0
## 192     0   0   0
## 193    76   0 487
## 194    68   0 279
## 195     0   0   0
## 196    24   0 189
## 197     0   0   0
## 198     0  45  43
## 199   136 303  28
## 200     0   0  15
## 201     0  11  58
## 202     0   0  22
## 203     0  58  63
## 204     0   0   8
## 205   349   0  46
## 206     0   0   0
## 207     0  23  32
## 208     0   0   0
## 209     0 306   0
## 210     0   0   0
## 211     0   0  32
## 212     0   0   0
## 213     0   0   0
## 214     0   0 439
## 215    74   0  65
## 216     0   0  31
## 217     0  41   0
## 218     8   0  30
## 219     0  26   0
## 220    46  22  88
## 221    62  61  64
## 222     0   0   0
## 223     0   0   0
## 224     0   0   0
## 225     0   0   0
## 226     0  55  58
## 227   174   0   0
## 228     0  78  25
## 229     0   0   0
## 230     0   0   0
## 231     0   0   0
## 232     0  79   0
## 233     0  39   0
## 234     0   8   0
## 235     0  46   0
## 236     0   0   0
## 237     0   0   0
## 238     0   0   0
## 239     0 332   0
## 240     0   0   0
## 241     0   0   0
## 242     0   0   0
## 243     0  15   0
## 244     0  12   0
## 245     0  25 101
## 246     0  44   0
## 247     0  61   9
## 248     0   0  16
## 249     0   0   0
## 250     0   0   0
## 251     0   0   0
## 252     0   0   0
## 253     0   0   0
## 254     0   0   0
## 255     0   0   0
## 256     0   0   0
## 257     0  93   0
## 258     0   0   0
## 259     0   0   0
## 260     0  13   0
## 261     0   0   0
## 262     0   0   0
## 263     0   0   0
## 264     0  21   0
## 265     0   0   0
## 266     0 142   0
## 267     0  81   0
## 268     0 515   0
## 269     0  29   0
## 270     0   8   0
## 271     0 176   0
## 272     0 167   0
## 273     0  54   0
## 274     0   0   0
## 275     0  63   0
## 276     0   0   0
## 277     0   0   0
## 278     0   0   0
## 279     0   0   0
## 280     0 110   0
## 281     0  16   0
## 282     0 437   0
## 283     0  50   0
## 284     0  16   0
## 285     0   0   0
## 286     0  11   0
## 287     0  33   0
## 288     0  53   0
## 2017    0   0   0
## 2018    0   0   0
## 2019    0   0   0
## 2020    0  47   0
## 2021   38   0   0
## 2022    0   0   0
## 2023    0  47   0
## 2024    0   0   0
## 2025    0  47  47
## 2026    0   0   0
## 2027    0   0   0
## 2028    0   0   0
## 2029    0   0  47
## 2030    0  47   0
## 2031    0   0   0
## 2032    0  47   0
## 2033    0   0   0
## 2034    0   0  47
## 2035    0   0   0
## 2036    0   0   0
## 2037    0   0   0
## 2038    0  47   0
## 2039    0   0   0
## 2040    0   0   0
## 2041    0   0   0
## 2042    0  47   0
## 2043    0   0   0
## 2044    0  47   0
## 2045    0   0   0
## 2046    0  47   0
## 2047    0   0   0
## 2048    0   0   0
## 2049    0   0   8
## 2050    0   0  13
## 2051    0   0   0
## 2052    0   0   0
## 2053    0   0   0
## 2054    0   0   0
## 2055    0   0   0
## 2056    0   0   0
## 2057   18  47   0
## 2058    0   0   0
## 2059    0   0   0
## 2060    0   0   0
## 2061    0   7   0
## 2062    0   0   0
## 2063    0   0   0
## 2064   36   0   0
## 2065    0   0   0
## 2066    0   0   0
## 2067    0   0   0
## 2068    0   0   0
## 2069    0   0   0
## 2070   44   0   0
## 2071    0   0   0
## 2072   40   0   0
## 2073  301   0   0
## 2074   43   0   0
## 2075  358   0   0
## 2076   20   0   0
## 2077  224   0   0
## 2078   32   0   0
## 2079    8   0   0
## 2080  608   0   0
## 2081    0   0   0
## 2082   34   0   0
## 2083    0   0   0
## 2084    0   0   0
## 2085    0   0   0
## 2086    0   0   0
## 2087  340   0   0
## 2088  534   0   0
## 2089    0   0   0
## 2090    0   0   0
## 2091    0   0   0
## 2092    0   0   0
## 2093   26   0   0
## 2094    0   0   0
## 2095    0   0   0
## 2096    0   0   0
## 2097    0   0   0
## 2098   27   0   0
## 2099    0   0   0
## 2100   40   0   0
## 2101    0   0   0
## 2102    0   0  29
## 2103    0   0 533
## 2104   48   0 628
## 2105    0   0   0
## 2106    0   0   0
## 2107    0   0   0
## 2108    0   0   0
## 2109    0   0   0
## 2110    0   0   0
## 2111    0   0   0
## 2112    0   0   0
## 2113   38   0   0
## 2114    0   0   0
## 2115    0   0   0
## 2116  114   0  23
## 2117   20   0  24
## 2118   29   0  36
## 2119    0   0   0
## 2120    0   0  33
## 2121    0   0   0
## 2122  160   0  66
## 2123    7   0 393
## 2124    0   0 759
## 2125    0   0 241
## 2126    0   0   0
## 2127    0  27  51
## 2128    0   0   0
## 2129    0   0 476
## 2130   51   0  64
## 2131    0   0   0
## 2132  261   0   0
## 2133   35   0  16
## 2134    0   0   7
## 2135    0  10  19
## 2136   28   8   0
## 2137    0   0   0
## 2138    0  17   0
## 2139    0   0   0
## 2140    0   0   0
## 2141    7  43   0
## 2142    0   0   0
## 2143    0   0 483
## 2144    0   0   0
## 2145    0 770   0
## 2146    0   0   0
## 2147    0   9   0
## 2148   67   0   0
## 2149    0   0  69
## 2150    9   0   0
## 2151   12  20   0
## 2152    0   0   0
## 2153   83  15  13
## 2154   17   0   0
## 2155    0  31   0
## 2156    0   0   0
## 2157    0   0  47
## 2158    0   0   0
## 2159    0   0  29
## 2160    0   0 303
## 2161  223 746   0
## 2162  260   0  69
## 2163    0 129 356
## 2164  504   0   0
## 2165    0 179 158
## 2166  506  39   0
## 2167    0   0   0
## 2168    0  18   0
## 2169    0   0   0
## 2170    0   0   0
## 2171    0   0  28
## 2172    0   0   0
## 2173    0   0   0
## 2174    0  78   0
## 2175  203   0   0
## 2176  347 108   0
## 2177    0  27   0
## 2178    0   0   0
## 2179  785   0   0
## 2180    0   0  23
## 2181    0   0   0
## 2182  198  44   0
## 2183  758   0   0
## 2184    0  73   0
## 2185    0 323   0
## 2186   38   0   0
## 2187  414   0   0
## 2188  158   0  56
## 2189    0   0   0
## 2190   42  17   0
## 2191   71   0   0
## 2192   63   0  35
## 2193   79   0   0
## 2194  183   0   0
## 2195   73   0   0
## 2196    7   0   0
## 2197   75   0  20
## 2198   55  38  67
## 2199   60   0   0
## 2200   89   0   0
## 2201   70   6   0
## 2202    0   0   0
## 2203   83 202   0
## 2204   14   0  34
## 2205    0   0   0
## 2206    0   0   9
## 2207  396   0   0
## 2208    0   0   4
## 2209    0  58   0
## 2210  104  78  35
## 2211   37  41   0
## 2212   24  21   0
## 2213   25 403   0
## 2214   31   0  60
## 2215    0   7  29
## 2216    0   0  70
## 2217    0   0  81
## 2218   46 404  99
## 2219   26   0 410
## 2220    0   0   2
## 2221    0   0   0
## 2222    0   0   0
## 2223    0   0 112
## 2224    0   0   0
## 2225   81   0   6
## 2226   27  39  49
## 2227    0 168   0
## 2228    0  95  83
## 2229   35   0   0
## 2230   77   0 553
## 2231   36   0   0
## 2232    0   0  55
## 2233    0   0   0
## 2234    0  78   0
## 2235    0   0   0
## 2236    0   0  18
## 2237    0   0   0
## 2238    0   0   0
## 2239    0   0  38
## 2240    0   0   0
## 2241    0  15   0
## 2242    0   0   0
## 2243    0  45   0
## 2244    0   0   0
## 2245    0   0  62
## 2246    0   0   0
## 2247    0   0   0
## 2248    0  31   0
## 2249    0 130   0
## 2250    0   0   0
## 2251    0   0   0
## 2252    0 506   0
## 2253    0  36   0
## 2254    0   0   0
## 2255    0   0   0
## 2256    0   0   0
## 2257    0   0   0
## 2258    0   0   0
## 2259    0  20   0
## 2260    0   0   0
## 2261    0   0   0
## 2262    0   0   0
## 2263    0   0   0
## 2264    0  20   0
## 2265    0  47   0
## 2266    0   0   0
## 2267    0  22   0
## 2268    0   4   0
## 2269    0  39   0
## 2270    0  17   0
## 2271    0   0   0
## 2272    0  17   0
## 2273    0   0   0
## 2274    0  52   0
## 2275    0  96   0
## 2276    0  71   0
## 2277    0  30   0
## 2278    0  30   0
## 2279    0  37   0
## 2280    0   9   0
## 2281    0   0   0
## 2282    0  80   0
## 2283    0   0   0
## 2284    0   0   0
## 2285    0  41   0
## 2286    0  70   0
## 2287    0   0   0
## 2288    0  21   0
## 2289    0   0   0
## 2290    0  37   0
## 2291    0  15   0
## 2292    0  14   0
## 2293    0   0   0
## 2294    0   0   0
## 2295    0   0   0
## 2296    0   0   0
## 2297    0   0   0
## 2298    0  33   0
## 2299    0  32   0
## 2300    0 469   0
## 2301    0 353   0
## 2302    0   0   0
## 2303    0   0   0
## 2304    0   0   0
## 8929    0   0  47
## 8930    0  47   0
## 8931    0   0   0
## 8932    0   0   0
## 8933    0  47   0
## 8934    0   0  47
## 8935    0  47   0
## 8936    0   0   0
## 8937    0  47   0
## 8938    0   0   0
## 8939    0   0  47
## 8940    0   0   0
## 8941    0   0   0
## 8942    0   0   0
## 8943    0   0  47
## 8944    0   0  47
## 8945    0   0   0
## 8946    0   0   0
## 8947    0   0   0
## 8948    0   0   0
## 8949    0   0   0
## 8950    0   0   0
## 8951    0   0   0
## 8952    0   0  52
## 8953    0   0   0
## 8954    0   0   0
## 8955    0   0   0
## 8956    0   0   0
## 8957    0   0   0
## 8958    0   0   0
## 8959    0   0   0
## 8960    0   0   0
## 8961    0   0  42
## 8962    0   0   4
## 8963   20   0   0
## 8964    0   0   0
## 8965    0   0   0
## 8966    0   0   0
## 8967    0   0   0
## 8968    0   0   0
## 8969    0   0   0
## 8970    6   0   0
## 8971    0   0   0
## 8972    0   0   0
## 8973    0   0   0
## 8974    0   0   0
## 8975    0   0   0
## 8976    0   0   0
## 8977   32   0   0
## 8978   34   0   0
## 8979    0   0   0
## 8980    0   0   0
## 8981    0   0   0
## 8982    0   0   0
## 8983    0   0   0
## 8984   38   0   0
## 8985    0   0   0
## 8986   76   0   0
## 8987    0   0   0
## 8988    0   0   0
## 8989    0   0   0
## 8990    0   0   0
## 8991  127   0   0
## 8992  443   0   0
## 8993  161   0   0
## 8994    0   0   0
## 8995    0   0   0
## 8996    0   0  13
## 8997    0   0   0
## 8998    0   0   0
## 8999   23   0   0
## 9000    0   0   0
## 9001   78   0   0
## 9002   34   0  20
## 9003    0   0   0
## 9004    0   0  65
## 9005   23   0   0
## 9006   16   0  63
## 9007   85   0  33
## 9008   40   0   0
## 9009   56   0   0
## 9010    0   0   0
## 9011    0   0   0
## 9012   33   0   0
## 9013  734   0   0
## 9014    0   0   0
## 9015    0   0   0
## 9016    0   0  34
## 9017  179   6   0
## 9018    0   0  31
## 9019    0   0   0
## 9020   16   0   0
## 9021    0   0   0
## 9022  269   0  73
## 9023   10   0 727
## 9024  205   0   0
## 9025  528  67   0
## 9026    0   0 745
## 9027    0   0   0
## 9028    0  22   0
## 9029    0   0  78
## 9030    0   0  12
## 9031    0  10 119
## 9032    0   0 519
## 9033    0  21  67
## 9034    0   0   0
## 9035    0   0   0
## 9036    0  15  78
## 9037    0   0   0
## 9038    0 107   0
## 9039    0   0   0
## 9040    0  28   0
## 9041    0   0   0
## 9042    0   0   0
## 9043    0  15 160
## 9044    0   0  30
## 9045    0  37   0
## 9046    0   7   0
## 9047    0  29   0
## 9048   27   0   0
## 9049    0 470   0
## 9050   96  24   0
## 9051    0   0   0
## 9052   56   0  20
## 9053    0  51  88
## 9054   21 755   0
## 9055    0  39   0
## 9056    0   0   0
## 9057    0 385   0
## 9058   74   0  19
## 9059   29   0   0
## 9060    0   0   0
## 9061    0   0   0
## 9062    0   0  80
## 9063  103   9   0
## 9064    0 144   0
## 9065    0 198   0
## 9066    0   0   0
## 9067    0   0   0
## 9068    0   0   0
## 9069  289 461 496
## 9070    0  42   0
## 9071    0   0 103
## 9072    0 499  17
## 9073   55 524  61
## 9074    0   0  11
## 9075   44   0   0
## 9076    0 533   0
## 9077    0   0   0
## 9078  124   0   0
## 9079    0 146   0
## 9080  242   0   0
## 9081    0   0   0
## 9082    0   0  55
## 9083    0   0   0
## 9084    0   0   0
## 9085   43   0 402
## 9086   51   6  89
## 9087    7   0   0
## 9088  468   0   0
## 9089   15  79   0
## 9090    0   0 114
## 9091    0 117   0
## 9092   60   0   0
## 9093    0  27   0
## 9094    0   0   7
## 9095    0  91   0
## 9096    0   0   0
## 9097    0   0   0
## 9098    0   0   0
## 9099    0   0   0
## 9100   14 129   0
## 9101  476   0 523
## 9102  124 104   0
## 9103    0   0   0
## 9104    0  13  62
## 9105    0 298   0
## 9106    0   0  39
## 9107    0   0  19
## 9108    0  14   0
## 9109    0   0   0
## 9110    0   0  28
## 9111   24   0  33
## 9112   74   0  58
## 9113    0 146   0
## 9114    0  35   0
## 9115    0   0  60
## 9116    0   0 318
## 9117   71 147 178
## 9118   46 143 144
## 9119   68   7 176
## 9120  183   0  69
## 9121   51 119   0
## 9122   30   0  54
## 9123   75   0   0
## 9124   20   0   0
## 9125   93 104 146
## 9126   59 125  38
## 9127   82   0  38
## 9128    0   0   0
## 9129    0   0   0
## 9130  231  22   0
## 9131    0  83   0
## 9132    0   0  29
## 9133    0   0   0
## 9134   63   0   0
## 9135   39   0   0
## 9136    0   0   0
## 9137    0   0   0
## 9138   24   0   0
## 9139   50   0   0
## 9140   31   0   0
## 9141    0   0   0
## 9142   36   0   0
## 9143    0   0  60
## 9144   46   0   0
## 9145    0   0   0
## 9146    0   0   0
## 9147   30 258   0
## 9148    0   0   0
## 9149    0   0   0
## 9150    0 310   0
## 9151   81   0   0
## 9152    0   0   0
## 9153    0   0   0
## 9154    0   0   0
## 9155    0   0  17
## 9156    0   0   0
## 9157    0 106   0
## 9158    0 639   0
## 9159    0   0   0
## 9160   17   0   0
## 9161   45  79   0
## 9162    0   0   0
## 9163    0  73   0
## 9164    0  19   0
## 9165   44   0   0
## 9166    0 237   0
## 9167    0   0   0
## 9168    0   0   0
## 9169    0   0   0
## 9170    0   0   0
## 9171    0  83   0
## 9172    0  26   0
## 9173    0  22   0
## 9174    0   0   0
## 9175    0   0   0
## 9176    0   0   0
## 9177    0   0   0
## 9178    0   0   0
## 9179    0   0   0
## 9180    0   0   0
## 9181    0   0   0
## 9182    0   0   0
## 9183    0   0   0
## 9184    0   7   0
## 9185    0   0   0
## 9186    0  77   0
## 9187    0  38   0
## 9188    0   0   0
## 9189    0 174   0
## 9190    0  29   0
## 9191    0  12   0
## 9192    0   0   0
## 9193    0  27   0
## 9194    0  35   0
## 9195    0   0   0
## 9196    0   0   0
## 9197    0  85   0
## 9198    0   0   0
## 9199    0 516   0
## 9200    0   0   0
## 9201    0   0   0
## 9202    0   0   0
## 9203    0   0   0
## 9204    0  29   0
## 9205    0  30   0
## 9206    0   0   0
## 9207    0   0   0
## 9208    0  90   0
## 9209    0   0   0
## 9210    0   0   0
## 9211    0   0   0
## 9212    0  34   0
## 9213    0   0   0
## 9214    0   0   0
## 9215    0  20   0
## 9216    0   0   0
## 9793    0   0   0
## 9794    0   0   0
## 9795    0   0   0
## 9796    0  47   0
## 9797    0   0   0
## 9798    0   0   0
## 9799    0   0  47
## 9800    0   0   0
## 9801    0   0   0
## 9802    0   0   0
## 9803    0   0  47
## 9804    0   0   0
## 9805    0   0   0
## 9806    0   0  47
## 9807    0   0  47
## 9808    0   0   0
## 9809    0   0   0
## 9810    0   0   0
## 9811    0   0   0
## 9812    0   0  10
## 9813    0   0   0
## 9814    0   0   0
## 9815    0   0   0
## 9816    0   0   0
## 9817    0   0   0
## 9818    0   0   0
## 9819    0   0   0
## 9820    0   0   0
## 9821    0   0  38
## 9822    0   0   0
## 9823    0   0   0
## 9824    0   0   0
## 9825    0   0   0
## 9826    0   0   0
## 9827    0   0   0
## 9828    0   0   0
## 9829    0   0   0
## 9830    0   0   0
## 9831    0   0   0
## 9832    0   0   0
## 9833    0   0   0
## 9834    0   0   0
## 9835    0   0   0
## 9836    0   0   0
## 9837    0   0   0
## 9838    0   0   0
## 9839    0   0   0
## 9840    0   0   0
## 9841   50   0   0
## 9842    0   0   0
## 9843    0   0   0
## 9844    0   0   0
## 9845    0   0   0
## 9846    0   0   0
## 9847    0   0   0
## 9848    0   0   0
## 9849   63   0   0
## 9850    0   0   0
## 9851    0   0   0
## 9852    0   0   0
## 9853    0   0   0
## 9854    0   0   0
## 9855    0   0   0
## 9856    0   0   0
## 9857    0   0   0
## 9858   24   0   0
## 9859   28   0   0
## 9860   74   0   0
## 9861   51   0   0
## 9862    0   0   0
## 9863    0   0   0
## 9864    0   0   0
## 9865    0   0   0
## 9866    0   0   8
## 9867   35  68   0
## 9868  750   0 149
## 9869    8  20   0
## 9870    0   0   0
## 9871   51   0   0
## 9872   40   0   0
## 9873   39   0 280
## 9874    1   0 611
## 9875    0   0   0
## 9876  748   0   0
## 9877   26   0   0
## 9878  161   0   0
## 9879    7   0  22
## 9880    0   0 738
## 9881    0   0   0
## 9882   19   0  16
## 9883    0   0   4
## 9884    0   0   0
## 9885   78   0 371
## 9886    0   0 225
## 9887    0   0  12
## 9888    0   0  25
## 9889    0   0   0
## 9890    0   0 343
## 9891    0  55  37
## 9892    0  83   0
## 9893    0   0   0
## 9894    0   0  61
## 9895    0   0  18
## 9896    0   0 540
## 9897    0   0   0
## 9898   24 611  16
## 9899    0   0   0
## 9900    0  43   0
## 9901   98   0   0
## 9902    6  29   0
## 9903  116 141   0
## 9904    0   0   0
## 9905    5   0 238
## 9906    0  40   0
## 9907    0   0  23
## 9908    0  43   0
## 9909    0 109   0
## 9910    0   0   0
## 9911    0 756   0
## 9912    0 559   0
## 9913  121  78   0
## 9914    0 556   0
## 9915    0   0 202
## 9916    0   0   0
## 9917    0  31   0
## 9918    0   0   0
## 9919    0   0   0
## 9920    0 522   0
## 9921    0 190  19
## 9922    0 530 127
## 9923    0 698 168
## 9924    0 252   0
## 9925    0   0  12
## 9926  247   0   0
## 9927    0   0   0
## 9928  108   0   0
## 9929    0   0   0
## 9930    0   0   0
## 9931    0   0  17
## 9932    0 106 179
## 9933    0   0   0
## 9934   39   0   0
## 9935  168   0   0
## 9936   77   0   0
## 9937    0   0 412
## 9938    0   0   0
## 9939    0   0  59
## 9940    0  85   0
## 9941    0   0   0
## 9942    0   0 241
## 9943   12   0  41
## 9944  120   0   0
## 9945    0   0   0
## 9946   89 207  18
## 9947    0   0   0
## 9948    0 172 378
## 9949  168   0   0
## 9950    0   0   0
## 9951    0   4   0
## 9952  332   0  42
## 9953  351   0   0
## 9954    0   0   0
## 9955   37  30   0
## 9956    0   0   9
## 9957    0   0   0
## 9958    0  28   0
## 9959    0   0   0
## 9960    0   0   0
## 9961  520  53   0
## 9962    0   0   0
## 9963   26   0   0
## 9964    0   0  32
## 9965    5 351  19
## 9966  191   0  19
## 9967    0   6   7
## 9968    0   0  25
## 9969    0   0   0
## 9970   20   0   0
## 9971  533   0  40
## 9972   24   0  28
## 9973   90 285  69
## 9974    0 140   0
## 9975    0  48   0
## 9976   92   0   0
## 9977    0   0   0
## 9978   49   0  46
## 9979    0   0   0
## 9980    0   0  18
## 9981    0   0  48
## 9982    0   0   0
## 9983   23   0  44
## 9984  128   0   0
## 9985    0 143  32
## 9986    0  30  62
## 9987   26   0  35
## 9988   26 350  17
## 9989    0   0   0
## 9990   56   0   0
## 9991  119 166   0
## 9992   16   0   0
## 9993   60   0 249
## 9994   38   0   0
## 9995    6   0  85
## 9996  185   0   0
## 9997    0   0   0
## 9998    0  33  10
## 9999    0  33  70
## 10000   0   0   0
## 10001  55   0  52
## 10002   0 432   0
## 10003   0   0   0
## 10004 171 276   0
## 10005   0  87   0
## 10006  17 114  32
## 10007   0   0   0
## 10008   0  60   0
## 10009 516  35   0
## 10010   0   0   0
## 10011   0   0   0
## 10012   0 400   0
## 10013   0  25   0
## 10014   0  14   0
## 10015   0 112   0
## 10016   0   0   0
## 10017   0   0   0
## 10018   0   0   0
## 10019   0  15   0
## 10020   0   0   0
## 10021   0   0   0
## 10022  60   0   0
## 10023   0  26   0
## 10024   0  11   0
## 10025   0   0   0
## 10026  42   0   0
## 10027  82   0   0
## 10028  39   0   0
## 10029   0   0   0
## 10030   0   0  17
## 10031   0  79   0
## 10032   0  39   0
## 10033   0  32   0
## 10034   0   0   0
## 10035 389   0   0
## 10036   0   0   0
## 10037   0  33   0
## 10038   0  41   0
## 10039   0   0   0
## 10040   0 171   0
## 10041   0  62   0
## 10042   0   0   0
## 10043   0   0   0
## 10044   0  10   0
## 10045   0  68   0
## 10046   0   0   0
## 10047   0  38   0
## 10048   0   0   0
## 10049   0   0   0
## 10050   0   0   0
## 10051   0  62   0
## 10052   0   0   0
## 10053   0   0   0
## 10054   0   0   0
## 10055   0  65   0
## 10056   0  31   0
## 10057   0   0   0
## 10058   0  15   0
## 10059   0  24   0
## 10060   0   0   0
## 10061   0   0   0
## 10062   0 184   0
## 10063   0   0   0
## 10064   0   0   0
## 10065   0   0   0
## 10066   0   0   0
## 10067   0   0   0
## 10068   0  42   0
## 10069   0   0   0
## 10070   0  36   0
## 10071   0   0   0
## 10072   0  14   0
## 10073   0 279   0
## 10074   0 153   0
## 10075   0   0   0
## 10076   0   0   0
## 10077   0 148   0
## 10078   0   2   0
## 10079   0   0   0
## 10080   0   8   0
## 11233   0   0   0
## 11234   0   0   0
## 11235   0   0  47
## 11236   0  47  47
## 11237   0   0   0
## 11238   0  47   0
## 11239   0   0  47
## 11240   0  47   0
## 11241   0   0   0
## 11242   0  47  47
## 11243   0   0   0
## 11244   0   0   0
## 11245   0   0   0
## 11246   0   0   0
## 11247   0   0   0
## 11248   0   0   0
## 11249   0   0   0
## 11250   0   0   0
## 11251   0   0   0
## 11252   0   0   0
## 11253   0   0   0
## 11254   0   0   0
## 11255   0   0   0
## 11256   0   0   0
## 11257   0   0   0
## 11258   0   0   0
## 11259   0   0   0
## 11260   0   0   0
## 11261   0   0   0
## 11262   0   0   0
## 11263   0   0   0
## 11264   0   0   0
## 11265   0   9   0
## 11266   0   0   0
## 11267   0   0   0
## 11268   0   0   0
## 11269   0   0   0
## 11270   0   0   0
## 11271   0   0   0
## 11272   0   0   0
## 11273   0   0   0
## 11274   0   0   0
## 11275   0   0   0
## 11276   0   0   0
## 11277   0   0   0
## 11278   0   0   0
## 11279   0   0   0
## 11280  30   0   0
## 11281   0   0   0
## 11282  18   0   0
## 11283   0   0   0
## 11284   0   0   0
## 11285   9   0   0
## 11286  36   0   0
## 11287   7  40   0
## 11288   0   0   0
## 11289   0   0   0
## 11290   0   0   0
## 11291   1   0   0
## 11292  23   0   0
## 11293  11   0   0
## 11294   0   0   0
## 11295  84   0  30
## 11296   0   0   0
## 11297   0   0  11
## 11298  57   0   0
## 11299 126   0   0
## 11300   0   0  70
## 11301  33   0   0
## 11302   0   0   0
## 11303   0  29   0
## 11304   8   0   0
## 11305   0   0   0
## 11306  37   0  21
## 11307   0   0   0
## 11308 149   0   0
## 11309 540   0   7
## 11310  15   0   0
## 11311   6   0   0
## 11312 652   0   0
## 11313 635   0 706
## 11314 732  13   0
## 11315  50   0  35
## 11316   9   0  52
## 11317   0   0 121
## 11318  70   0  43
## 11319   0   0   9
## 11320   0   0   0
## 11321 686 613 446
## 11322 592   0 667
## 11323 119   0   0
## 11324 500 534  25
## 11325  21   0 187
## 11326   0  34 608
## 11327  56  32   0
## 11328   0   0 489
## 11329   0   0   3
## 11330  14   0 159
## 11331  47  44  75
## 11332   0 637 540
## 11333   0   0 143
## 11334 488   0   0
## 11335   0 706  16
## 11336  45   0   0
## 11337   7   0   0
## 11338   0  97  27
## 11339   0   0 260
## 11340   0   0 400
## 11341  57  67   0
## 11342   0  57  46
## 11343   0  99   0
## 11344   0  53  17
## 11345   0  51  34
## 11346   0 748   0
## 11347 157 743   0
## 11348 429 687   0
## 11349   0 393  25
## 11350   0   0   0
## 11351 160 208 122
## 11352   0 199 485
## 11353 106  56  17
## 11354 319 190 211
## 11355  97 665  28
## 11356   0 127   0
## 11357   0  46   0
## 11358 127   0   0
## 11359   0   0  39
## 11360   0 253 443
## 11361 209  64   0
## 11362  38  14  78
## 11363   0  39   0
## 11364 101   0   0
## 11365   0   0   0
## 11366   0 275   0
## 11367   0   0   0
## 11368   0   0   0
## 11369   0  14   0
## 11370   0   0   0
## 11371   0   0   0
## 11372   0   0   0
## 11373  11 414   0
## 11374  38 110  15
## 11375 139   0   0
## 11376  16   0   0
## 11377   0   0   0
## 11378   0   0   0
## 11379   0   0 549
## 11380   0   0   0
## 11381   0   0   0
## 11382   0   0   0
## 11383  31   0   0
## 11384  27   0 419
## 11385   0   0  25
## 11386  78  13   0
## 11387   0   0   0
## 11388  20  64   0
## 11389  27   0  42
## 11390   0 415   0
## 11391   0   0  50
## 11392   0   0   0
## 11393  26   0   0
## 11394  65 555   0
## 11395  41 345   0
## 11396   0   0   0
## 11397   0   0   0
## 11398   0   0 400
## 11399   0   0   0
## 11400 432   0   0
## 11401   7   0   0
## 11402  25   0   0
## 11403  30   0 190
## 11404  50   0  25
## 11405  27   0   0
## 11406   0   0   0
## 11407   0  16  53
## 11408   0   0  96
## 11409   0   0 306
## 11410  71   0  17
## 11411  15   0  15
## 11412   0   0  68
## 11413  53   0  65
## 11414   0   0  46
## 11415   0   0   0
## 11416   0   0  18
## 11417  19   7  48
## 11418  57   0   0
## 11419  20   0 223
## 11420   0   7   0
## 11421 403  32  31
## 11422 263   0   0
## 11423   0   0  16
## 11424   0   0  48
## 11425   0   0 123
## 11426   0   0   0
## 11427   0   0   0
## 11428   0   0  42
## 11429  17   0   0
## 11430  16  57   6
## 11431 264   0   0
## 11432  63   0 123
## 11433  16   0   0
## 11434   0   0   0
## 11435   0   0   0
## 11436   0   0   0
## 11437  46   0   0
## 11438   0   0  14
## 11439  46   0  42
## 11440   0  11   0
## 11441   6   0   0
## 11442  50   0   0
## 11443   0   0   0
## 11444  23   0   0
## 11445  20   0   0
## 11446  12   0  24
## 11447   0   0  36
## 11448   0   0  87
## 11449  11   0   0
## 11450   4  22   0
## 11451   0   0   0
## 11452   0 121   0
## 11453   0   0   0
## 11454   0  16   0
## 11455  24   0   0
## 11456   0   0   0
## 11457   0   0  11
## 11458   0  68   0
## 11459   0   0   0
## 11460   0   0   0
## 11461  95   0   0
## 11462   0   7   0
## 11463  30   9   0
## 11464  46  42   0
## 11465  31   0   0
## 11466   0   0  17
## 11467   0   7   0
## 11468   0   0   0
## 11469   0   0   0
## 11470   0   0   0
## 11471   0   0   0
## 11472   0   0   0
## 11473   0   0   0
## 11474   0 112   0
## 11475   0  70   0
## 11476   0  26   0
## 11477   0  25   0
## 11478   0   8   0
## 11479   0  92   0
## 11480   0  32   0
## 11481   0  45   0
## 11482   0   0   0
## 11483   0   0   0
## 11484   0 112   0
## 11485   0   0   0
## 11486   0   0   0
## 11487   0  54   0
## 11488   0  50   0
## 11489   0  53   0
## 11490   0   0   0
## 11491   0   0   0
## 11492   8   0   0
## 11493   0   0   0
## 11494   0  47   0
## 11495   0 249   0
## 11496  35  27   0
## 11497   0 129   0
## 11498   0   0   0
## 11499   0  15   0
## 11500   0   0   0
## 11501   0   0   0
## 11502   0   0   0
## 11503   0   0   0
## 11504   0   0   0
## 11505   0   0   0
## 11506   0   0   0
## 11507   0   8   0
## 11508   0   0   0
## 11509   0   0   0
## 11510   0   0   0
## 11511   0   0   0
## 11512   0   0   0
## 11513   0   0   0
## 11514   0   0   0
## 11515   0   0   0
## 11516   0   0   0
## 11517   0  74   0
## 11518   0   0   0
## 11519   0   0   0
## 11520   0   0   0
## 11521   0   0  47
## 11522   0   0   0
## 11523   0   0   0
## 11524   0   0  47
## 11525   0   0   0
## 11526   0   0   0
## 11527   0  47   0
## 11528   0   0  47
## 11529   0   0   0
## 11530   0   0   0
## 11531   0   0   0
## 11532   0   0   0
## 11533   0  18   0
## 11534   0   0   0
## 11535   0   0   0
## 11536   0   0   0
## 11537   0   0   0
## 11538   0   0   0
## 11539   0   0   0
## 11540   0   0   0
## 11541   0   0   0
## 11542   0   7   8
## 11543   0   0   0
## 11544   0   0   0
## 11545   0   0   0
## 11546   0   0   0
## 11547   0   0   0
## 11548   0   0   0
## 11549   0   0   0
## 11550   0   0   0
## 11551   0   0   0
## 11552   0   0   0
## 11553   0   0   0
## 11554   0   0   0
## 11555   0   0   0
## 11556   0   0   0
## 11557   0   0   0
## 11558   0   0   0
## 11559   0   0   0
## 11560   0   0   0
## 11561   0   0   0
## 11562   0   0   0
## 11563   0   0   0
## 11564   0   0   0
## 11565   0   0   0
## 11566   0   0   0
## 11567  13   0   0
## 11568 101   0   0
## 11569   0   0   0
## 11570   0   0   0
## 11571   0   0   0
## 11572   0   0   0
## 11573   0   0   0
## 11574   0   0   0
## 11575  35   0   0
## 11576   0   0   0
## 11577   0   0   0
## 11578   0   0   0
## 11579 709  12   0
## 11580  69   0   0
## 11581  31   0   0
## 11582  11   0   0
## 11583  33   0   0
## 11584  84   0   0
## 11585   0   0   0
## 11586   0   0   0
## 11587   0   0   0
## 11588   0   0   0
## 11589   0   0   0
## 11590   0   0   0
## 11591   0   0   0
## 11592  23   0   0
## 11593 109   0   0
## 11594 789   0   0
## 11595 211   0   0
## 11596 257   0  10
## 11597  24   0  30
## 11598  55   0   0
## 11599   0   0  13
## 11600 614   0  99
## 11601 667   0   0
## 11602  79   0   0
## 11603  26   0  64
## 11604  35   0  32
## 11605 138   0  47
## 11606  23   0  17
## 11607   0   0  55
## 11608   0   0   0
## 11609   0  13 748
## 11610 542   0  79
## 11611  32  59 618
## 11612   0  49  95
## 11613   0   0 655
## 11614   0  29   0
## 11615   0  45 385
## 11616   0  90   0
## 11617  13   0   0
## 11618   0   0 108
## 11619   0  16 555
## 11620   0   0 625
## 11621   0  36   0
## 11622   0  54   0
## 11623  16  21   0
## 11624   0   0 106
## 11625   0  44 275
## 11626   0   0 399
## 11627   0  23   0
## 11628   0   0   0
## 11629 475 114   0
## 11630   0 102   0
## 11631 172   0   0
## 11632  11   0   0
## 11633   0 146   0
## 11634   9  15 197
## 11635   0   0   0
## 11636   8  24   0
## 11637   0   0   0
## 11638   0   0 143
## 11639 186  81   0
## 11640   0  60   0
## 11641 301   0   0
## 11642   0  71 319
## 11643   0   0   0
## 11644   0 802   0
## 11645   0   0   0
## 11646   0   0   0
## 11647  39 592 489
## 11648 193 362   0
## 11649  35   0  59
## 11650  53 528   0
## 11651   0   0   0
## 11652  59 198   6
## 11653   0   0   0
## 11654  76   0   0
## 11655   0 508   0
## 11656   0  93   0
## 11657   0   0  16
## 11658   0 468   0
## 11659   0   0   0
## 11660   0   0 309
## 11661   0   0   0
## 11662   0   0   0
## 11663 180   0   0
## 11664  28   0   0
## 11665   0   0 281
## 11666   0   0  83
## 11667   0   0 471
## 11668   0   0   0
## 11669   0   0   0
## 11670   0   0   0
## 11671   0   9   0
## 11672   0  98 198
## 11673   0   0   0
## 11674   0  35   0
## 11675   0 118   0
## 11676   0 179   4
## 11677  20   0   0
## 11678   0 320  15
## 11679  64  70   0
## 11680 161 106   0
## 11681   0 319   0
## 11682   0  19   0
## 11683   0 163 504
## 11684   0  15   0
## 11685   0   0   0
## 11686  33   0   0
## 11687   0 141   0
## 11688   0  96 168
## 11689 760   0   0
## 11690 744   0  85
## 11691   0 284  40
## 11692 758   0   0
## 11693   0   0   0
## 11694  66   0   0
## 11695   0   0   0
## 11696   0   0  21
## 11697   0   0   0
## 11698   0   0   0
## 11699   0  21  17
## 11700   0  81  26
## 11701 746   0  67
## 11702  18   0   0
## 11703   0   0   0
## 11704   0   0  84
## 11705   0   0  25
## 11706   0   0  74
## 11707  88 103   0
## 11708  30   0  16
## 11709   0   0  28
## 11710   0   0  61
## 11711   0   0  78
## 11712 411   0   0
## 11713  73   0   0
## 11714  44   0  49
## 11715  82  39   0
## 11716 504  49 175
## 11717   0  26   0
## 11718 115   0   0
## 11719   0  64  94
## 11720   0   0   0
## 11721   0 120   0
## 11722  12   0   0
## 11723  41 505   0
## 11724  30   0  75
## 11725  63  75 128
## 11726 401   0   0
## 11727   7   0  23
## 11728   0   0   0
## 11729   0  35   0
## 11730   0  50   0
## 11731  23 143   0
## 11732   0   0   0
## 11733   0   0   0
## 11734   0   0  67
## 11735   0  66   0
## 11736   0   0   0
## 11737   0   0   0
## 11738   0   0   0
## 11739   0  18   0
## 11740   8   0   0
## 11741  71 411   0
## 11742   0 387   0
## 11743   0  47   0
## 11744  24   0   0
## 11745  33   0   0
## 11746   0   0   0
## 11747   0  37   0
## 11748   0  34   4
## 11749   0 306   0
## 11750  22 111   0
## 11751  90  10   0
## 11752  30   0   0
## 11753   0 476   0
## 11754   0  38   0
## 11755   0   0   0
## 11756   0  91   0
## 11757   0   0   0
## 11758  16   0   0
## 11759   0   0   0
## 11760   0  17   0
## 11761  47  60   0
## 11762   0   0   0
## 11763   0   6   0
## 11764   0   0   0
## 11765   0   0   0
## 11766   0  22   0
## 11767   0   0   0
## 11768   0   0   0
## 11769   0   0   0
## 11770   0   0   0
## 11771   0 364   0
## 11772   0 533   0
## 11773   0   0   0
## 11774   0   0   0
## 11775   0  55   0
## 11776   0   0   0
## 11777   0   0   0
## 11778   0  13   0
## 11779  56  71   0
## 11780   0   0   0
## 11781   0 179   0
## 11782   0  81   0
## 11783   0   0   0
## 11784   0   0   0
## 11785   0   0   0
## 11786   0   0   0
## 11787   0   0   0
## 11788   0   0   0
## 11789   0   0   0
## 11790   0   0   0
## 11791   0   0   0
## 11792   0   0   0
## 11793   0   0   0
## 11794   0  95   0
## 11795   0   0   0
## 11796   0   0   0
## 11797   0   0   0
## 11798   0  39   0
## 11799   0   0   0
## 11800   0   0   0
## 11801   0   0   0
## 11802   0   0   0
## 11803   0   0   0
## 11804   0   0   0
## 11805   0   0   0
## 11806   0   0   0
## 11807   0   0   0
## 11808   0   0   0
## 12673   0   0   0
## 12674   0   0   0
## 12675   0  47   0
## 12676   0   0   0
## 12677   0   0   0
## 12678   0   0  47
## 12679   0   0   0
## 12680   0   0   0
## 12681   0   0   0
## 12682   0   0   0
## 12683   0   0   0
## 12684   0   0  47
## 12685   0  35  28
## 12686   0   0   0
## 12687   0   4   0
## 12688   0   0   0
## 12689   0   0   0
## 12690   0   0   0
## 12691   0   6   0
## 12692   0   0   0
## 12693   0   0   0
## 12694   0   0   0
## 12695   0   0   0
## 12696   0   0   0
## 12697   0   0   0
## 12698   0   0   0
## 12699   0   0   0
## 12700   0   0   0
## 12701   0   0   0
## 12702   0   0   0
## 12703   0   0  18
## 12704   0   0   0
## 12705   0   0   0
## 12706   0   0   0
## 12707   0   0   0
## 12708   0   0   0
## 12709   6   0   0
## 12710   0   0   0
## 12711   0   0   0
## 12712   0   0   0
## 12713   0   0   0
## 12714   0   0   0
## 12715   0   0   0
## 12716   0   0   0
## 12717   0   0   0
## 12718   0   0   0
## 12719   0   0   0
## 12720   0   0   0
## 12721   0   0   0
## 12722   0   0   0
## 12723   0   0   0
## 12724   0   0   0
## 12725   0   8   0
## 12726   0   0   0
## 12727   0   4   0
## 12728   0   0   0
## 12729   0   0   0
## 12730 562   0   0
## 12731   0   0   0
## 12732   0   0   0
## 12733   0   0  18
## 12734   0   0   0
## 12735   0   0   0
## 12736   0   0  70
## 12737  10   0   0
## 12738   0   0   0
## 12739  37   0   0
## 12740   0   0   0
## 12741   0   0   9
## 12742   0   0   0
## 12743  32   0   0
## 12744  19   0  19
## 12745   0   0   0
## 12746   0   0   0
## 12747   0   0  16
## 12748  35 101   0
## 12749  17  67  43
## 12750   0   0   0
## 12751  73   0  35
## 12752   0   0   0
## 12753   0   0   0
## 12754   0   0  57
## 12755  48  40 119
## 12756  43   0   0
## 12757  82   0  98
## 12758   0   0  40
## 12759   0  43  12
## 12760 418  14 739
## 12761 159   0   0
## 12762 736   0  68
## 12763 252   0   0
## 12764 153   0 146
## 12765  16   0 188
## 12766 253   0 100
## 12767 187   0  51
## 12768   0  43  71
## 12769   0   0  92
## 12770   0  38 539
## 12771   0   6  16
## 12772   0   0   0
## 12773   0  33   0
## 12774  26   0  27
## 12775  30  88   0
## 12776  64  40   0
## 12777  92   0  69
## 12778   0 105 526
## 12779   0 167   0
## 12780   0   0   0
## 12781 135  48   0
## 12782  17   0 475
## 12783   0   0   0
## 12784   0 259   0
## 12785   0  15  25
## 12786   0 770   0
## 12787   0   8   5
## 12788   0  19  46
## 12789 180   0   0
## 12790  68  85   0
## 12791   0 128   0
## 12792   0   0   0
## 12793   0  59  19
## 12794   0  15   0
## 12795   0   0   0
## 12796   0   0 143
## 12797   0   0   0
## 12798   0  16  78
## 12799   0   0   0
## 12800  74   0   0
## 12801  10   0   0
## 12802   0   0  52
## 12803   0   0   0
## 12804  39 175   0
## 12805 444   0   0
## 12806 131  30   0
## 12807   0   0 465
## 12808  69   0   0
## 12809   0  40   0
## 12810   0   0   0
## 12811   0   0   0
## 12812 158  19   0
## 12813   0   0   0
## 12814   0   0   0
## 12815   0   0   0
## 12816  56   0   0
## 12817  95   0   0
## 12818   0   0 354
## 12819   0 364   0
## 12820   0  40   0
## 12821  66   0  24
## 12822   0  22   0
## 12823   0 186   0
## 12824   0   0   0
## 12825   0   0   0
## 12826  77   0   0
## 12827 412   0  56
## 12828 194   0   0
## 12829   0   0  30
## 12830 511  31   0
## 12831   0 121  54
## 12832  34   0  13
## 12833   0   0   0
## 12834   0   0   0
## 12835   0   0   0
## 12836   0   0   0
## 12837   0  78   0
## 12838   9   0   0
## 12839   0  21   0
## 12840   0  38   0
## 12841  17  62   0
## 12842   0   0   0
## 12843   0 435  21
## 12844  17   0 465
## 12845   0 204   0
## 12846   0   0  79
## 12847   0   0   8
## 12848   0   0   0
## 12849 465   0   0
## 12850   0   0 115
## 12851 759 131   0
## 12852 535  80   0
## 12853  29  22  60
## 12854 111   0   0
## 12855   0   0  12
## 12856 541   0  57
## 12857   0  51   0
## 12858   0   0  17
## 12859   0   0   8
## 12860   0   0 533
## 12861   0   0  28
## 12862   0   0  53
## 12863 190   0 174
## 12864 256   0   0
## 12865 128   0 176
## 12866 285   0   0
## 12867  68 211   0
## 12868 750   0   0
## 12869  30   0 480
## 12870   0 326  36
## 12871   0  23  66
## 12872 163   0   0
## 12873   0   0   0
## 12874  58   0   0
## 12875 190   0   0
## 12876   0  15   0
## 12877   0   0   0
## 12878  74   0   0
## 12879  22   0   0
## 12880  75  61   0
## 12881   0   0   0
## 12882   0   0   0
## 12883   0   0   0
## 12884   0   0  87
## 12885   0   0   0
## 12886   0   0   0
## 12887   0   0   0
## 12888   0   0  10
## 12889  21   0   0
## 12890  16   0   0
## 12891  97   0   0
## 12892   0   0   0
## 12893   0   0   0
## 12894   0 432   0
## 12895  73  39   0
## 12896  33   0   0
## 12897 262 160   0
## 12898   0   0   0
## 12899   0 263   0
## 12900  35  60   0
## 12901   0   0   0
## 12902 106   0   0
## 12903 122  86   0
## 12904  39 176  26
## 12905  22  25   0
## 12906   0   0   0
## 12907   4   0   0
## 12908   9   0   0
## 12909   0  24   0
## 12910   0   0   0
## 12911 152  53   0
## 12912   0  26   0
## 12913  87   0   0
## 12914 117 393   0
## 12915   0 437   0
## 12916  46 188   0
## 12917  62   0   0
## 12918   0   0   0
## 12919   0   0   0
## 12920   9   0   0
## 12921  19   0   0
## 12922   0   0   0
## 12923  21  54   0
## 12924   0 503   0
## 12925  55  97   0
## 12926   0   0   0
## 12927   0   0   0
## 12928   0  46   0
## 12929   0   0   0
## 12930   0   0   0
## 12931   0   0   0
## 12932   0   0   0
## 12933 101   0   0
## 12934   0   0   0
## 12935   0   0   0
## 12936   0  66   0
## 12937   0   0   0
## 12938   0   0   0
## 12939   0   0   0
## 12940   0   0   0
## 12941  17   0   0
## 12942   0   0   0
## 12943   0   0   0
## 12944   0   0   0
## 12945   0   0   0
## 12946   0   0   0
## 12947   0   0   0
## 12948   0  32   0
## 12949   0   0   0
## 12950   0   0   0
## 12951   0   0   0
## 12952   0   0   0
## 12953   0   0   0
## 12954   0   0   0
## 12955   0   0   0
## 12956   0   0   0
## 12957   0   0   0
## 12958   0   0   0
## 12959   0   0   0
## 12960   0   0   0
## 17281   0   0   0
## 17282   0   0   0
## 17283   0   0   0
## 17284   0   0   0
## 17285   0   0   0
## 17286   0   0   0
## 17287   0   0   0
## 17288   0   0   0
## 17289   0   0   0
## 17290   0   0   0
## 17291   0   0   0
## 17292   0   0   0
## 17293   0   0   0
## 17294   0   0   0
## 17295   0   0   0
## 17296   0   0   0
## 17297   0   0   0
## 17298   0   0   0
## 17299   0   0   0
## 17300   0   0   0
## 17301   0   0   0
## 17302   0   0   0
## 17303   0   0   0
## 17304   0   0   0
## 17305   0   0  12
## 17306   0   0   0
## 17307   0   0   0
## 17308   0   0   0
## 17309   0   0   0
## 17310   0   0   0
## 17311   0   0   0
## 17312   0   0   0
## 17313   0   0   0
## 17314   0   0   0
## 17315   0   0   0
## 17316   0   0   0
## 17317   0   0   0
## 17318   8   0   0
## 17319   0   0   0
## 17320   0   0   0
## 17321   0   0   0
## 17322   0   0   0
## 17323   0   0   0
## 17324   0   0   0
## 17325   0   0   0
## 17326   0   0   0
## 17327   0   0   0
## 17328   0   0   0
## 17329   0   0   0
## 17330   0   0   0
## 17331   0   9   0
## 17332   0   0   0
## 17333   0   0   0
## 17334   0   0  53
## 17335  39   0   0
## 17336   0   0  11
## 17337  27   0   0
## 17338   0   0   0
## 17339   0   0   0
## 17340   0   0   0
## 17341   0   0 777
## 17342   0  30   0
## 17343   0  92   0
## 17344   0   0   0
## 17345   0   0   0
## 17346   0   0   0
## 17347  38   0   0
## 17348  55   0   0
## 17349  23   0   0
## 17350  20   0   0
## 17351  22   0   0
## 17352   0   0   0
## 17353   0  16   0
## 17354  42  37   0
## 17355   0   0 358
## 17356   0  16 511
## 17357  32 149 100
## 17358   0  33 149
## 17359   0   0 133
## 17360   0  31 317
## 17361  52   0  63
## 17362 171   0 259
## 17363  60   0   0
## 17364   0  11 187
## 17365   0  51   0
## 17366   0   0  78
## 17367  39  60 479
## 17368 619  74 112
## 17369   0   0   6
## 17370 446  43   0
## 17371   0   0   0
## 17372   0   0 143
## 17373   0   0   0
## 17374  13   0   0
## 17375 187   0   0
## 17376   8   0   0
## 17377 134 635   0
## 17378   8  26   0
## 17379  18   0   0
## 17380  31 720   0
## 17381 266   0 237
## 17382 400  15   0
## 17383  33 475   0
## 17384  19 103   0
## 17385  35   0  34
## 17386  42  15   0
## 17387 450 131   0
## 17388  78  52   0
## 17389   0 734   0
## 17390 105   0   0
## 17391   0   0   0
## 17392 389   0   0
## 17393   0   0   0
## 17394   0   0  10
## 17395   0   0   0
## 17396  88   0  21
## 17397  38 175  71
## 17398   0   0  39
## 17399   0   0   0
## 17400   0   0  38
## 17401   0   0   0
## 17402   0   0   0
## 17403   0   0   0
## 17404   0  55   0
## 17405  35   0   0
## 17406   7   0   0
## 17407   0   0 137
## 17408   0   0   0
## 17409   0   0   0
## 17410   0  41   0
## 17411 527   0   0
## 17412 729   0 207
## 17413   0   0 159
## 17414   0   0   0
## 17415   0   7   0
## 17416   0   0  12
## 17417  12  72   0
## 17418 139   0  83
## 17419   0   0  82
## 17420  63   0  15
## 17421  86   0   0
## 17422   0   0   0
## 17423   0   0 419
## 17424  50   0  24
## 17425   0   0   0
## 17426   0  40   0
## 17427   0   0  33
## 17428   0   6  20
## 17429   0  22   0
## 17430 204   0   0
## 17431   0   0   0
## 17432   0   9   0
## 17433   0  46   0
## 17434   0   0   0
## 17435   0   0   0
## 17436   0   0   0
## 17437 210   0   0
## 17438   0  28   0
## 17439   0   0   0
## 17440   0   0   0
## 17441  95  11  19
## 17442   0   0   7
## 17443   0   0 281
## 17444  30   0   0
## 17445 477   0  12
## 17446 150  16  25
## 17447   0  79  46
## 17448   0  95 109
## 17449  10   0   0
## 17450  82   0   0
## 17451 120   0   0
## 17452   0   0  33
## 17453   0   0  46
## 17454   0   0  93
## 17455 298   0  20
## 17456   0   0   0
## 17457   0   0 136
## 17458   0  15  14
## 17459   0   0  33
## 17460   0  44 504
## 17461  34   0  62
## 17462   0   7   0
## 17463   0   0 443
## 17464 154   0   0
## 17465   0  52   0
## 17466  19  51   0
## 17467   0   0   0
## 17468   0  13   0
## 17469   0   0   0
## 17470   0   0  85
## 17471   0   0   0
## 17472   0   0   0
## 17473   0   0   0
## 17474   0  15   0
## 17475   0   0   0
## 17476  16 504  34
## 17477   0   0  52
## 17478   0 148   0
## 17479   0  20   0
## 17480   0  30   0
## 17481   0 122   0
## 17482  79  92   0
## 17483   0 137   0
## 17484   0   0   0
## 17485   0   0   0
## 17486  31  63   0
## 17487  27 432 174
## 17488 391  39   0
## 17489   0  80   0
## 17490  39   0   0
## 17491  62  41   0
## 17492   0  24   0
## 17493  65  85   0
## 17494   0   0   0
## 17495  60  46   0
## 17496  48  71   0
## 17497 482 377   0
## 17498  16 107   0
## 17499   0  42   0
## 17500  43   0   0
## 17501   0 190   0
## 17502  53  24   0
## 17503   0  51   0
## 17504  62  60   0
## 17505   0  75   0
## 17506   0  20   0
## 17507   0  60   0
## 17508   0  21   0
## 17509 170   0   0
## 17510   0  16   0
## 17511   0 425   0
## 17512  85  43   0
## 17513   0   0   0
## 17514   0  14   0
## 17515   0   0   0
## 17516   0   0   0
## 17517   0   0   0
## 17518  35   0   0
## 17519   0  47   0
## 17520   0 171   0
## 17521   0   0   0
## 17522   0  51   0
## 17523  38   0   0
## 17524   0   0   0
## 17525  34   0   0
## 17526   0  40   0
## 17527   0   0   0
## 17528   0   0   0
## 17529   0   0   0
## 17530   0  29   0
## 17531   0   0   0
## 17532   0   0   0
## 17533   0   0   0
## 17534   0   0   0
## 17535   0   0   0
## 17536   0   0   0
## 17537   0   0   0
## 17538   8   0   0
## 17539   0  61   0
## 17540   0  64   0
## 17541   0   0   0
## 17542   0   0   0
## 17543   0   0   0
## 17544   0   0   0
## 17545   0   0   0
## 17546   0   0   0
## 17547   0   0   0
## 17548   0   0   0
## 17549   0   0   0
## 17550   0   0   0
## 17551   0   0   0
## 17552  34   0   0
## 17553   0   0   0
## 17554   0   0   0
## 17555   0   0   0
## 17556 100   0   0
## 17557   0   0   0
## 17558   0   0   0
## 17559   0   0   0
## 17560   0   0   0
## 17561   0   0   0
## 17562   0   0   0
## 17563   0   0   0
## 17564   0   0   0
## 17565   0   9   0
## 17566   0   0   0
## 17567   0   0   0
## 17568   0   0   0
```
Picking a model from m = 3 to impute the data.  


```r
complete_Maindf1 <- complete(imputed_maindf1, 1)
summary(complete_Maindf1)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.07   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The 

```r
CompleteMeanTotal <- complete_Maindf1 %>% group_by(date) %>%  summarize(Steps = sum(steps)) %>% mutate(m=month(date))

g4<-ggplot(data = CompleteMeanTotal, aes(x=Steps,fill=factor(m)))
gg4<-g4+geom_histogram(bins = 9, alpha=.5)+geom_vline(xintercept = mean(CompleteMeanTotal$Steps))+
        labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Total Steps Each Day (Imputed dataset)",
         x="STEPS",
         y="FREQUENCY",
         fill="MONTH") 


library(cowplot)

plot_grid(gg1, gg4, nrow = 2, labels = "AUTO")
```

![](PAMD_files/figure-html/Comparison from missing data to imputed data-1.png)<!-- -->
The comparison shows the new dataset mean decrease -203.1231, median decrease -326, the steps increase 69042, observation increase by 8 because we imputed the datasets. 

Calculating the steps mean and median.  


```r
CompleteTotal <-
  complete_Maindf1 %>% drop_na() %>% group_by(date) %>%  summarize(Steps = sum(steps)) %>% mutate(m =
                                                                                                    month(date))

CompleteMeanMed <-
  CompleteTotal %>% summarize(
    Obs = length(date),
    SumofSteps = sum(Steps),
    StepsMean = mean(Steps),
    StepsMedian = median(Steps)
  )

GTCompleteMeanMed <- CompleteMeanMed %>% gt::gt() %>% data_color(
  columns = vars(Obs, SumofSteps, StepsMean, StepsMedian),
  colors = c("yellow")
) %>%
  tab_header(title = "Step Mean and Median Summary (No missing data",
             subtitle = glue("{start_date} to {end_date}"))


GTCompleteMeanMed
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jnuwfitjfz .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jnuwfitjfz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jnuwfitjfz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jnuwfitjfz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jnuwfitjfz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jnuwfitjfz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jnuwfitjfz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jnuwfitjfz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jnuwfitjfz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jnuwfitjfz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jnuwfitjfz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jnuwfitjfz .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jnuwfitjfz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jnuwfitjfz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jnuwfitjfz .gt_from_md > :first-child {
  margin-top: 0;
}

#jnuwfitjfz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jnuwfitjfz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jnuwfitjfz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jnuwfitjfz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jnuwfitjfz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jnuwfitjfz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jnuwfitjfz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jnuwfitjfz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jnuwfitjfz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jnuwfitjfz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jnuwfitjfz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jnuwfitjfz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jnuwfitjfz .gt_left {
  text-align: left;
}

#jnuwfitjfz .gt_center {
  text-align: center;
}

#jnuwfitjfz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jnuwfitjfz .gt_font_normal {
  font-weight: normal;
}

#jnuwfitjfz .gt_font_bold {
  font-weight: bold;
}

#jnuwfitjfz .gt_font_italic {
  font-style: italic;
}

#jnuwfitjfz .gt_super {
  font-size: 65%;
}

#jnuwfitjfz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="jnuwfitjfz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Step Mean and Median Summary (No missing data</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SumofSteps</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMedian</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">61</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">651215</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10675.66</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10571</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->



```r
GTMeanTotal
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lubhynfhqr .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lubhynfhqr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lubhynfhqr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lubhynfhqr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lubhynfhqr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lubhynfhqr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lubhynfhqr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lubhynfhqr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lubhynfhqr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lubhynfhqr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lubhynfhqr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lubhynfhqr .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#lubhynfhqr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lubhynfhqr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lubhynfhqr .gt_from_md > :first-child {
  margin-top: 0;
}

#lubhynfhqr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lubhynfhqr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lubhynfhqr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#lubhynfhqr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lubhynfhqr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#lubhynfhqr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lubhynfhqr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lubhynfhqr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lubhynfhqr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lubhynfhqr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#lubhynfhqr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lubhynfhqr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#lubhynfhqr .gt_left {
  text-align: left;
}

#lubhynfhqr .gt_center {
  text-align: center;
}

#lubhynfhqr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lubhynfhqr .gt_font_normal {
  font-weight: normal;
}

#lubhynfhqr .gt_font_bold {
  font-weight: bold;
}

#lubhynfhqr .gt_font_italic {
  font-style: italic;
}

#lubhynfhqr .gt_super {
  font-size: 65%;
}

#lubhynfhqr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="lubhynfhqr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="4" class="gt_heading gt_title gt_font_normal" style>Step Mean and Median Summary</th>
    </tr>
    <tr>
      <th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2012-10-01 to 2012-11-30</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">Obs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">SumofSteps</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">StepsMedian</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">53</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">570608</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10766.19</td>
      <td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">10765</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
formula for the total steps, average steps and median steps respectively (see below chunk code):


```r
NewSteps <- CompleteMeanMed$SumofSteps - StepsMeanMed$SumofSteps
NewMean <- CompleteMeanMed$StepsMean - StepsMeanMed$StepsMean
NewMed <- CompleteMeanMed$StepsMedian - StepsMeanMed$StepsMedian
NewSteps
```

```
## [1] 80607
```

```r
NewMean
```

```
## [1] -90.53294
```

```r
NewMed
```

```
## [1] -194
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
MonFri <- c(2,3,4,5,6)
SatSun <- c(1,7)
```

New datasets with imputed


```r
maindf2 <- complete_Maindf1 %>% mutate(DayNumber = day(date))
MonFriDf <- maindf2 %>% filter(DayNumber %in% MonFri)
SatSunDf <- maindf2 %>% filter(DayNumber %in% SatSun)
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
TSMonFri <- MonFriDf %>% drop_na() %>% group_by(TimeInterval=interval)%>% 
        summarize(Total_Obs = length(interval), 
        SumOfSteps = sum(steps),
        MeanOfSteps = mean(steps)) 

g5 <-ggplot(data = TSMonFri, aes(y=MeanOfSteps, x=TimeInterval))+geom_line(color="red")+
   labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Monday through Friday (imputed data)",
         x="STEPS",
         y="FREQUENCY"
      ) 

TSSatSun <- SatSunDf %>% drop_na() %>% group_by(TimeInterval=interval)%>% 
        summarize(Total_Obs = length(interval), 
        SumOfSteps = sum(steps),
        MeanOfSteps = mean(steps)) 

g6 <-ggplot(data = TSSatSun, aes(y=MeanOfSteps, x=TimeInterval))+geom_line(color="green")+
   labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Saturday-Sunday (imputed data)",
         x="STEPS",
         y="FREQUENCY") 

plot_grid(g5, g6, nrow = 2)
```

![](PAMD_files/figure-html/Timeseries difference Weekdays vs Weekends-1.png)<!-- -->









