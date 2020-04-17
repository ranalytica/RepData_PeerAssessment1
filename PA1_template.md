Reproducible Research: Peer Assessment 1
================

## Environment Settings

Not all packages are in one chunk code due to conflicts with lubridate
either with VIM, mice or misforest package.

``` r
library(tidyverse)
library(reshape2)
library(lubridate)
library(gt)
library(glue)
library(paletteer)
library(scales)
```

``` r
sessionInfo()
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

## Load Datasets

``` r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fname <- c("./data/pamd.zip")

source("LoadUnzip.R")
dateDownloaded
# LoadUnzip.R = if(!file.exists("data")){
#         dir.create("data")
# }
# 
# download.file(fileUrl,destfile="./data/pamd.zip",method="curl")
# 
# dateDownloaded <- date()
# 
# maindf <- read_delim(fname, delim = ",", col_names = TRUE)
```

Dataset download date Thu Apr 16 19:29:07 2020.

Range of Date

``` r
start_date <- min(maindf$date)
end_date <- max(maindf$date)
range(maindf$date)
```

    ## [1] "2012-10-01" "2012-11-30"

## What is **mean** total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1.  Calculate the total number of steps taken per day? Answer: MeanTotal
    gives us the breakdown of steps each day.

2.  If you do not understand the difference between a histogram and a
    barplot, research the difference between them. Make a histogram of
    the total number of steps taken each day

3.  Calculate and report the mean and median of the total number of
    steps taken per day

GTMeanTotal is using the (gt) grammar for tables, scale, glue, and
paletteer package. The yellow hightlighted portion gives as the Total
steps - mean and median for 53 observations (I used *drop\_na* from
dplyr package to remove the NA).

``` r
MeanTotal <- maindf %>% drop_na() %>% group_by(date) %>%  summarize(Steps = sum(steps)) %>% mutate(m=month(date))
head(MeanTotal)
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

Mean and Median

``` r
StepsMeanMed <- MeanTotal %>% summarize(Obs = length(date), SumofSteps = sum(Steps), StepsMean = mean(Steps), StepsMedian = median(Steps) ) 

GTMeanTotal <- StepsMeanMed %>% gt::gt() %>% data_color(
        columns = vars(Obs,SumofSteps, StepsMean,StepsMedian),
        colors = c("yellow")) %>% 
        tab_header(title = "Step Mean and Median Summary",
        subtitle = glue("{start_date} to {end_date}"))

GTMeanTotal
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#pjbvjgczzo .gt_table {
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

#pjbvjgczzo .gt_heading {
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

#pjbvjgczzo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pjbvjgczzo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pjbvjgczzo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pjbvjgczzo .gt_col_headings {
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

#pjbvjgczzo .gt_col_heading {
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

#pjbvjgczzo .gt_column_spanner_outer {
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

#pjbvjgczzo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pjbvjgczzo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pjbvjgczzo .gt_column_spanner {
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

#pjbvjgczzo .gt_group_heading {
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

#pjbvjgczzo .gt_empty_group_heading {
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

#pjbvjgczzo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pjbvjgczzo .gt_from_md > :first-child {
  margin-top: 0;
}

#pjbvjgczzo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pjbvjgczzo .gt_row {
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

#pjbvjgczzo .gt_stub {
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

#pjbvjgczzo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjbvjgczzo .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#pjbvjgczzo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pjbvjgczzo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pjbvjgczzo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pjbvjgczzo .gt_footnotes {
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

#pjbvjgczzo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#pjbvjgczzo .gt_sourcenotes {
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

#pjbvjgczzo .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#pjbvjgczzo .gt_left {
  text-align: left;
}

#pjbvjgczzo .gt_center {
  text-align: center;
}

#pjbvjgczzo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pjbvjgczzo .gt_font_normal {
  font-weight: normal;
}

#pjbvjgczzo .gt_font_bold {
  font-weight: bold;
}

#pjbvjgczzo .gt_font_italic {
  font-style: italic;
}

#pjbvjgczzo .gt_super {
  font-size: 65%;
}

#pjbvjgczzo .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="pjbvjgczzo" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Step Mean and Median Summary

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SumofSteps

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMean

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMedian

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

570608

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10766.19

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10765

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

### Histogram of Steps taken Each Day

``` r
g1<-ggplot(data = MeanTotal, aes(x=Steps,fill=factor(m)))
gg1 <- g1+geom_histogram(bins = 9, alpha=.5)+geom_vline(xintercept = mean(MeanTotal$Steps))+
  labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Total Steps Each Day, Observation = 53 days",
         x="STEPS",
         y="FREQUENCY",
         fill="MONTH")
gg1
```

![](GHTemp_files/figure-gfm/Mean%20Steps%20histogram-1.png)<!-- -->

## What is the average daily activity pattern?

I’m using the gt (grammar for tables) package to analyze the average of
steps in 5 minute interval. This table has 288 observations - see
below:.

``` r
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#omgbrxjjpu .gt_table {
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

#omgbrxjjpu .gt_heading {
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

#omgbrxjjpu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#omgbrxjjpu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#omgbrxjjpu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#omgbrxjjpu .gt_col_headings {
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

#omgbrxjjpu .gt_col_heading {
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

#omgbrxjjpu .gt_column_spanner_outer {
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

#omgbrxjjpu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#omgbrxjjpu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#omgbrxjjpu .gt_column_spanner {
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

#omgbrxjjpu .gt_group_heading {
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

#omgbrxjjpu .gt_empty_group_heading {
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

#omgbrxjjpu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#omgbrxjjpu .gt_from_md > :first-child {
  margin-top: 0;
}

#omgbrxjjpu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#omgbrxjjpu .gt_row {
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

#omgbrxjjpu .gt_stub {
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

#omgbrxjjpu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#omgbrxjjpu .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#omgbrxjjpu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#omgbrxjjpu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#omgbrxjjpu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#omgbrxjjpu .gt_footnotes {
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

#omgbrxjjpu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#omgbrxjjpu .gt_sourcenotes {
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

#omgbrxjjpu .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#omgbrxjjpu .gt_left {
  text-align: left;
}

#omgbrxjjpu .gt_center {
  text-align: center;
}

#omgbrxjjpu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#omgbrxjjpu .gt_font_normal {
  font-weight: normal;
}

#omgbrxjjpu .gt_font_bold {
  font-weight: bold;
}

#omgbrxjjpu .gt_font_italic {
  font-style: italic;
}

#omgbrxjjpu .gt_super {
  font-size: 65%;
}

#omgbrxjjpu .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="omgbrxjjpu" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Personal Movement Activity Monitoring Device

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

TimeInterval

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Total\_Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SumOfSteps

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

MeanOfSteps

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_right">

0

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

91

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.7169811

</td>

</tr>

<tr>

<td class="gt_row gt_right">

5

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

18

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

10

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

7

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

15

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

8

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

20

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

4

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

25

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

111

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.0943396

</td>

</tr>

<tr>

<td class="gt_row gt_right">

30

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

28

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.5283019

</td>

</tr>

<tr>

<td class="gt_row gt_right">

35

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

46

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.8679245

</td>

</tr>

<tr>

<td class="gt_row gt_right">

40

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

45

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

78

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.4716981

</td>

</tr>

<tr>

<td class="gt_row gt_right">

50

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

16

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

55

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

7

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

100

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

17

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

105

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

36

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

110

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

8

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

115

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

18

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

120

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

125

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

59

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

1.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

130

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

97

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.8301887

</td>

</tr>

<tr>

<td class="gt_row gt_right">

135

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

9

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

140

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

9

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

145

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

20

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3773585

</td>

</tr>

<tr>

<td class="gt_row gt_right">

150

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

14

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.2641509

</td>

</tr>

<tr>

<td class="gt_row gt_right">

155

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

200

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

205

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

210

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

60

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

1.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

215

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

220

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

225

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

7

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

230

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

235

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

12

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.2264151

</td>

</tr>

<tr>

<td class="gt_row gt_right">

240

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

245

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

250

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

82

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

255

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

50

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

300

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

305

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

310

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

315

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

320

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

11

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

325

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

33

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

330

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

86

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

335

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

31

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.5849057

</td>

</tr>

<tr>

<td class="gt_row gt_right">

340

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

26

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.4905660

</td>

</tr>

<tr>

<td class="gt_row gt_right">

345

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

4

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

350

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

355

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

400

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9ED; color: #000000;">

63

</td>

<td class="gt_row gt_right" style="background-color: #FFE9ED; color: #000000;">

1.1886792

</td>

</tr>

<tr>

<td class="gt_row gt_right">

405

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

50

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

410

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

136

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.5660377

</td>

</tr>

<tr>

<td class="gt_row gt_right">

415

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

420

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

18

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

425

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

19

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3584906

</td>

</tr>

<tr>

<td class="gt_row gt_right">

430

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">

218

</td>

<td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">

4.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

435

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

35

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

440

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE6EA; color: #000000;">

185

</td>

<td class="gt_row gt_right" style="background-color: #FFE6EA; color: #000000;">

3.4905660

</td>

</tr>

<tr>

<td class="gt_row gt_right">

445

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

44

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.8301887

</td>

</tr>

<tr>

<td class="gt_row gt_right">

450

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

165

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

3.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

455

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

59

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

1.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

500

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

505

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

83

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.5660377

</td>

</tr>

<tr>

<td class="gt_row gt_right">

510

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

159

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

3.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

515

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

119

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.2452830

</td>

</tr>

<tr>

<td class="gt_row gt_right">

520

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

176

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

3.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

525

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

157

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

2.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

530

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

111

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.0943396

</td>

</tr>

<tr>

<td class="gt_row gt_right">

535

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE3E7; color: #000000;">

321

</td>

<td class="gt_row gt_right" style="background-color: #FFE3E7; color: #000000;">

6.0566038

</td>

</tr>

<tr>

<td class="gt_row gt_right">

540

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

849

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

16.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

545

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD3D8; color: #000000;">

972

</td>

<td class="gt_row gt_right" style="background-color: #FFD3D8; color: #000000;">

18.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

550

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A8A9; color: #000000;">

2091

</td>

<td class="gt_row gt_right" style="background-color: #F4A8A9; color: #000000;">

39.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

555

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

2358

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

44.4905660

</td>

</tr>

<tr>

<td class="gt_row gt_right">

600

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">

1669

</td>

<td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">

31.4905660

</td>

</tr>

<tr>

<td class="gt_row gt_right">

605

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9494; color: #000000;">

2611

</td>

<td class="gt_row gt_right" style="background-color: #EE9494; color: #000000;">

49.2641509

</td>

</tr>

<tr>

<td class="gt_row gt_right">

610

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8C; color: #000000;">

2850

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8C; color: #000000;">

53.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

615

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">

3363

</td>

<td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">

63.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

620

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

2648

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

49.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

625

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9898; color: #000000;">

2495

</td>

<td class="gt_row gt_right" style="background-color: #EF9898; color: #000000;">

47.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

630

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">

2764

</td>

<td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">

52.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

635

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A8AA; color: #000000;">

2085

</td>

<td class="gt_row gt_right" style="background-color: #F4A8AA; color: #000000;">

39.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

640

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">

2333

</td>

<td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">

44.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

645

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">

2341

</td>

<td class="gt_row gt_right" style="background-color: #F09E9E; color: #000000;">

44.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

650

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">

1980

</td>

<td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">

37.3584906

</td>

</tr>

<tr>

<td class="gt_row gt_right">

655

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9594; color: #000000;">

2599

</td>

<td class="gt_row gt_right" style="background-color: #EE9594; color: #000000;">

49.0377358

</td>

</tr>

<tr>

<td class="gt_row gt_right">

700

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">

2322

</td>

<td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">

43.8113208

</td>

</tr>

<tr>

<td class="gt_row gt_right">

705

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

2352

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

44.3773585

</td>

</tr>

<tr>

<td class="gt_row gt_right">

710

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9292; color: #000000;">

2677

</td>

<td class="gt_row gt_right" style="background-color: #ED9292; color: #000000;">

50.5094340

</td>

</tr>

<tr>

<td class="gt_row gt_right">

715

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">

2889

</td>

<td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">

54.5094340

</td>

</tr>

<tr>

<td class="gt_row gt_right">

720

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

2646

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

49.9245283

</td>

</tr>

<tr>

<td class="gt_row gt_right">

725

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9191; color: #000000;">

2702

</td>

<td class="gt_row gt_right" style="background-color: #ED9191; color: #000000;">

50.9811321

</td>

</tr>

<tr>

<td class="gt_row gt_right">

730

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">

2951

</td>

<td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">

55.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

735

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">

2349

</td>

<td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">

44.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

740

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">

2770

</td>

<td class="gt_row gt_right" style="background-color: #ED8F8F; color: #000000;">

52.2641509

</td>

</tr>

<tr>

<td class="gt_row gt_right">

745

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E57272; color: #000000;">

3686

</td>

<td class="gt_row gt_right" style="background-color: #E57272; color: #000000;">

69.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

750

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA8685; color: #000000;">

3066

</td>

<td class="gt_row gt_right" style="background-color: #EA8685; color: #000000;">

57.8490566

</td>

</tr>

<tr>

<td class="gt_row gt_right">

755

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8988; color: #000000;">

2976

</td>

<td class="gt_row gt_right" style="background-color: #EB8988; color: #000000;">

56.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

800

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E76D6C; color: #000000;">

3889

</td>

<td class="gt_row gt_right" style="background-color: #E76D6C; color: #000000;">

73.3773585

</td>

</tr>

<tr>

<td class="gt_row gt_right">

805

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E57474; color: #000000;">

3615

</td>

<td class="gt_row gt_right" style="background-color: #E57474; color: #000000;">

68.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

810

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA3D35; color: #FFFFFF;">

6860

</td>

<td class="gt_row gt_right" style="background-color: #EA3D35; color: #FFFFFF;">

129.4339623

</td>

</tr>

<tr>

<td class="gt_row gt_right">

815

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #D53030; color: #FFFFFF;">

8349

</td>

<td class="gt_row gt_right" style="background-color: #D53030; color: #FFFFFF;">

157.5283019

</td>

</tr>

<tr>

<td class="gt_row gt_right">

820

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #CD2C2C; color: #FFFFFF;">

9071

</td>

<td class="gt_row gt_right" style="background-color: #CD2C2C; color: #FFFFFF;">

171.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

825

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #D73130; color: #FFFFFF;">

8236

</td>

<td class="gt_row gt_right" style="background-color: #D73130; color: #FFFFFF;">

155.3962264

</td>

</tr>

<tr>

<td class="gt_row gt_right">

830

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #C92A2A; color: #FFFFFF;">

9397

</td>

<td class="gt_row gt_right" style="background-color: #C92A2A; color: #FFFFFF;">

177.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

835

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #B71C1C; color: #FFFFFF;">

10927

</td>

<td class="gt_row gt_right" style="background-color: #B71C1C; color: #FFFFFF;">

206.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

840

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #BE2221; color: #FFFFFF;">

10384

</td>

<td class="gt_row gt_right" style="background-color: #BE2221; color: #FFFFFF;">

195.9245283

</td>

</tr>

<tr>

<td class="gt_row gt_right">

845

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #C82929; color: #FFFFFF;">

9517

</td>

<td class="gt_row gt_right" style="background-color: #C82929; color: #FFFFFF;">

179.5660377

</td>

</tr>

<tr>

<td class="gt_row gt_right">

850

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #C62828; color: #FFFFFF;">

9720

</td>

<td class="gt_row gt_right" style="background-color: #C62828; color: #FFFFFF;">

183.3962264

</td>

</tr>

<tr>

<td class="gt_row gt_right">

855

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #CF2D2D; color: #FFFFFF;">

8852

</td>

<td class="gt_row gt_right" style="background-color: #CF2D2D; color: #FFFFFF;">

167.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

900

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E03633; color: #FFFFFF;">

7603

</td>

<td class="gt_row gt_right" style="background-color: #E03633; color: #FFFFFF;">

143.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

905

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE3F36; color: #FFFFFF;">

6574

</td>

<td class="gt_row gt_right" style="background-color: #EE3F36; color: #FFFFFF;">

124.0377358

</td>

</tr>

<tr>

<td class="gt_row gt_right">

910

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F3473C; color: #FFFFFF;">

5783

</td>

<td class="gt_row gt_right" style="background-color: #F3473C; color: #FFFFFF;">

109.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

915

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F3483D; color: #FFFFFF;">

5730

</td>

<td class="gt_row gt_right" style="background-color: #F3483D; color: #FFFFFF;">

108.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

920

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F24B42; color: #FFFFFF;">

5497

</td>

<td class="gt_row gt_right" style="background-color: #F24B42; color: #FFFFFF;">

103.7169811

</td>

</tr>

<tr>

<td class="gt_row gt_right">

925

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F0504B; color: #000000;">

5086

</td>

<td class="gt_row gt_right" style="background-color: #F0504B; color: #000000;">

95.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

930

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E67777; color: #000000;">

3509

</td>

<td class="gt_row gt_right" style="background-color: #E67777; color: #000000;">

66.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

935

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">

2397

</td>

<td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">

45.2264151

</td>

</tr>

<tr>

<td class="gt_row gt_right">

940

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FEC9CD; color: #000000;">

1314

</td>

<td class="gt_row gt_right" style="background-color: #FEC9CD; color: #000000;">

24.7924528

</td>

</tr>

<tr>

<td class="gt_row gt_right">

945

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">

2054

</td>

<td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">

38.7547170

</td>

</tr>

<tr>

<td class="gt_row gt_right">

950

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F7B2B4; color: #000000;">

1854

</td>

<td class="gt_row gt_right" style="background-color: #F7B2B4; color: #000000;">

34.9811321

</td>

</tr>

<tr>

<td class="gt_row gt_right">

955

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

1116

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

21.0566038

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1000

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F3A6A7; color: #000000;">

2150

</td>

<td class="gt_row gt_right" style="background-color: #F3A6A7; color: #000000;">

40.5660377

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1005

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">

1430

</td>

<td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">

26.9811321

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1010

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">

2248

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">

42.4150943

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1015

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8F8E; color: #000000;">

2791

</td>

<td class="gt_row gt_right" style="background-color: #EC8F8E; color: #000000;">

52.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1020

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">

2063

</td>

<td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">

38.9245283

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1025

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">

2692

</td>

<td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">

50.7924528

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1030

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">

2347

</td>

<td class="gt_row gt_right" style="background-color: #F09D9E; color: #000000;">

44.2830189

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1035

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">

1983

</td>

<td class="gt_row gt_right" style="background-color: #F6ADAE; color: #000000;">

37.4150943

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1040

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">

1839

</td>

<td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">

34.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1045

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">

1502

</td>

<td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">

28.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1050

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FEC8CD; color: #000000;">

1330

</td>

<td class="gt_row gt_right" style="background-color: #FEC8CD; color: #000000;">

25.0943396

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1055

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F9B9BC; color: #000000;">

1693

</td>

<td class="gt_row gt_right" style="background-color: #F9B9BC; color: #000000;">

31.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1100

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">

1662

</td>

<td class="gt_row gt_right" style="background-color: #FABABD; color: #000000;">

31.3584906

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1105

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FBBEC1; color: #000000;">

1573

</td>

<td class="gt_row gt_right" style="background-color: #FBBEC1; color: #000000;">

29.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1110

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

1130

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

21.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1115

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">

1354

</td>

<td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">

25.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1120

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">

1504

</td>

<td class="gt_row gt_right" style="background-color: #FCC1C4; color: #000000;">

28.3773585

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1125

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">

1403

</td>

<td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">

26.4716981

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1130

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">

1772

</td>

<td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">

33.4339623

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1135

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

2649

</td>

<td class="gt_row gt_right" style="background-color: #ED9393; color: #000000;">

49.9811321

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1140

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A3; color: #000000;">

2228

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A3; color: #000000;">

42.0377358

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1145

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

2364

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

44.6037736

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1150

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

2440

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

46.0377358

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1155

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">

3137

</td>

<td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">

59.1886792

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1200

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">

3385

</td>

<td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">

63.8679245

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1205

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE5956; color: #000000;">

4648

</td>

<td class="gt_row gt_right" style="background-color: #EE5956; color: #000000;">

87.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1210

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F0514C; color: #000000;">

5027

</td>

<td class="gt_row gt_right" style="background-color: #F0514C; color: #000000;">

94.8490566

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1215

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF524F; color: #000000;">

4917

</td>

<td class="gt_row gt_right" style="background-color: #EF524F; color: #000000;">

92.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1220

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">

3360

</td>

<td class="gt_row gt_right" style="background-color: #E87C7C; color: #000000;">

63.3962264

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1225

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9392; color: #000000;">

2659

</td>

<td class="gt_row gt_right" style="background-color: #ED9392; color: #000000;">

50.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1230

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">

2887

</td>

<td class="gt_row gt_right" style="background-color: #EC8B8B; color: #000000;">

54.4716981

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1235

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F9B8BA; color: #000000;">

1718

</td>

<td class="gt_row gt_right" style="background-color: #F9B8BA; color: #000000;">

32.4150943

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1240

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">

1406

</td>

<td class="gt_row gt_right" style="background-color: #FDC5C9; color: #000000;">

26.5283019

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1245

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F5ACAD; color: #000000;">

2000

</td>

<td class="gt_row gt_right" style="background-color: #F5ACAD; color: #000000;">

37.7358491

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1250

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09C9C; color: #000000;">

2388

</td>

<td class="gt_row gt_right" style="background-color: #F09C9C; color: #000000;">

45.0566038

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1255

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E67675; color: #000000;">

3566

</td>

<td class="gt_row gt_right" style="background-color: #E67675; color: #000000;">

67.2830189

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1300

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">

2244

</td>

<td class="gt_row gt_right" style="background-color: #F2A2A2; color: #000000;">

42.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1305

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

2114

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

39.8867925

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1310

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F1A0A0; color: #000000;">

2293

</td>

<td class="gt_row gt_right" style="background-color: #F1A0A0; color: #000000;">

43.2641509

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1315

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">

2172

</td>

<td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">

40.9811321

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1320

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

2451

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

46.2452830

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1325

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

2991

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

56.4339623

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1330

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F1A1A1; color: #000000;">

2266

</td>

<td class="gt_row gt_right" style="background-color: #F1A1A1; color: #000000;">

42.7547170

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1335

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FEC8CC; color: #000000;">

1332

</td>

<td class="gt_row gt_right" style="background-color: #FEC8CC; color: #000000;">

25.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1340

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

2118

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

39.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1345

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">

2838

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">

53.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1350

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">

2508

</td>

<td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">

47.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1355

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E98180; color: #000000;">

3223

</td>

<td class="gt_row gt_right" style="background-color: #E98180; color: #000000;">

60.8113208

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1400

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">

2955

</td>

<td class="gt_row gt_right" style="background-color: #EB8989; color: #000000;">

55.7547170

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1405

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED908F; color: #000000;">

2754

</td>

<td class="gt_row gt_right" style="background-color: #ED908F; color: #000000;">

51.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1410

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

2310

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

43.5849057

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1415

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9595; color: #000000;">

2581

</td>

<td class="gt_row gt_right" style="background-color: #EE9595; color: #000000;">

48.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1420

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">

1880

</td>

<td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">

35.4716981

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1425

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F5ACAE; color: #000000;">

1990

</td>

<td class="gt_row gt_right" style="background-color: #F5ACAE; color: #000000;">

37.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1430

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F2A3A3; color: #000000;">

2218

</td>

<td class="gt_row gt_right" style="background-color: #F2A3A3; color: #000000;">

41.8490566

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1435

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

1458

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

27.5094340

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1440

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD5D9; color: #000000;">

907

</td>

<td class="gt_row gt_right" style="background-color: #FFD5D9; color: #000000;">

17.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1445

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC6CA; color: #000000;">

1382

</td>

<td class="gt_row gt_right" style="background-color: #FDC6CA; color: #000000;">

26.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1450

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

2312

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

43.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1455

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

2320

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

43.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1500

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">

1591

</td>

<td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">

30.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1505

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F6B0B1; color: #000000;">

1912

</td>

<td class="gt_row gt_right" style="background-color: #F6B0B1; color: #000000;">

36.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1510

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">

1881

</td>

<td class="gt_row gt_right" style="background-color: #F7B1B3; color: #000000;">

35.4905660

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1515

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">

2059

</td>

<td class="gt_row gt_right" style="background-color: #F4A9AB; color: #000000;">

38.8490566

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1520

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

2436

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

45.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1525

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">

2531

</td>

<td class="gt_row gt_right" style="background-color: #EE9797; color: #000000;">

47.7547170

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1530

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EE9696; color: #000000;">

2551

</td>

<td class="gt_row gt_right" style="background-color: #EE9696; color: #000000;">

48.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1535

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E77979; color: #000000;">

3462

</td>

<td class="gt_row gt_right" style="background-color: #E77979; color: #000000;">

65.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1540

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC605D; color: #000000;">

4394

</td>

<td class="gt_row gt_right" style="background-color: #EC605D; color: #000000;">

82.9056604

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1545

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F14E48; color: #000000;">

5229

</td>

<td class="gt_row gt_right" style="background-color: #F14E48; color: #000000;">

98.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1550

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F14C44; color: #FFFFFF;">

5412

</td>

<td class="gt_row gt_right" style="background-color: #F14C44; color: #FFFFFF;">

102.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1555

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC5F5C; color: #000000;">

4450

</td>

<td class="gt_row gt_right" style="background-color: #EC5F5C; color: #000000;">

83.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1600

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E87E7E; color: #000000;">

3293

</td>

<td class="gt_row gt_right" style="background-color: #E87E7E; color: #000000;">

62.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1605

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">

3399

</td>

<td class="gt_row gt_right" style="background-color: #E77B7B; color: #000000;">

64.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1610

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">

3951

</td>

<td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">

74.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1615

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E87D7C; color: #000000;">

3348

</td>

<td class="gt_row gt_right" style="background-color: #E87D7C; color: #000000;">

63.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1620

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8787; color: #000000;">

3016

</td>

<td class="gt_row gt_right" style="background-color: #EB8787; color: #000000;">

56.9056604

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1625

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E98282; color: #000000;">

3168

</td>

<td class="gt_row gt_right" style="background-color: #E98282; color: #000000;">

59.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1630

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">

2325

</td>

<td class="gt_row gt_right" style="background-color: #F19E9F; color: #000000;">

43.8679245

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1635

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">

2044

</td>

<td class="gt_row gt_right" style="background-color: #F5AAAB; color: #000000;">

38.5660377

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1640

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

2367

</td>

<td class="gt_row gt_right" style="background-color: #F09D9D; color: #000000;">

44.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1645

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">

2409

</td>

<td class="gt_row gt_right" style="background-color: #EF9B9B; color: #000000;">

45.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1650

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

2449

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

46.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1655

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

2315

</td>

<td class="gt_row gt_right" style="background-color: #F19F9F; color: #000000;">

43.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1700

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

2471

</td>

<td class="gt_row gt_right" style="background-color: #EF9999; color: #000000;">

46.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1705

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

2984

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

56.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1710

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">

2688

</td>

<td class="gt_row gt_right" style="background-color: #ED9291; color: #000000;">

50.7169811

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1715

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E98080; color: #000000;">

3245

</td>

<td class="gt_row gt_right" style="background-color: #E98080; color: #000000;">

61.2264151

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1720

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E76E6D; color: #000000;">

3854

</td>

<td class="gt_row gt_right" style="background-color: #E76E6D; color: #000000;">

72.7169811

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1725

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA6663; color: #000000;">

4184

</td>

<td class="gt_row gt_right" style="background-color: #EA6663; color: #000000;">

78.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1730

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E57373; color: #000000;">

3654

</td>

<td class="gt_row gt_right" style="background-color: #E57373; color: #000000;">

68.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1735

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E98382; color: #000000;">

3162

</td>

<td class="gt_row gt_right" style="background-color: #E98382; color: #000000;">

59.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1740

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E86B69; color: #000000;">

3980

</td>

<td class="gt_row gt_right" style="background-color: #E86B69; color: #000000;">

75.0943396

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1745

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

2995

</td>

<td class="gt_row gt_right" style="background-color: #EB8888; color: #000000;">

56.5094340

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1750

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">

1843

</td>

<td class="gt_row gt_right" style="background-color: #F7B3B5; color: #000000;">

34.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1755

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F5ADAE; color: #000000;">

1985

</td>

<td class="gt_row gt_right" style="background-color: #F5ADAE; color: #000000;">

37.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1800

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">

2156

</td>

<td class="gt_row gt_right" style="background-color: #F3A5A6; color: #000000;">

40.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1805

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">

3075

</td>

<td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">

58.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1810

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E86B6A; color: #000000;">

3959

</td>

<td class="gt_row gt_right" style="background-color: #E86B6A; color: #000000;">

74.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1815

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">

4522

</td>

<td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">

85.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1820

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">

3141

</td>

<td class="gt_row gt_right" style="background-color: #EA8383; color: #000000;">

59.2641509

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1825

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E57575; color: #000000;">

3592

</td>

<td class="gt_row gt_right" style="background-color: #E57575; color: #000000;">

67.7735849

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1830

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">

4118

</td>

<td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">

77.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1835

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">

3935

</td>

<td class="gt_row gt_right" style="background-color: #E86C6A; color: #000000;">

74.2452830

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1840

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">

4523

</td>

<td class="gt_row gt_right" style="background-color: #ED5D5A; color: #000000;">

85.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1845

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F14E47; color: #000000;">

5271

</td>

<td class="gt_row gt_right" style="background-color: #F14E47; color: #000000;">

99.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1850

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED5B58; color: #000000;">

4589

</td>

<td class="gt_row gt_right" style="background-color: #ED5B58; color: #000000;">

86.5849057

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1855

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #ED5C59; color: #000000;">

4537

</td>

<td class="gt_row gt_right" style="background-color: #ED5C59; color: #000000;">

85.6037736

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1900

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC5D5A; color: #000000;">

4498

</td>

<td class="gt_row gt_right" style="background-color: #EC5D5A; color: #000000;">

84.8679245

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1905

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">

4125

</td>

<td class="gt_row gt_right" style="background-color: #E96765; color: #000000;">

77.8301887

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1910

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">

3076

</td>

<td class="gt_row gt_right" style="background-color: #EA8585; color: #000000;">

58.0377358

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1915

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">

2828

</td>

<td class="gt_row gt_right" style="background-color: #EC8D8D; color: #000000;">

53.3584906

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1920

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F6AFB1; color: #000000;">

1925

</td>

<td class="gt_row gt_right" style="background-color: #F6AFB1; color: #000000;">

36.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1925

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD0D5; color: #000000;">

1098

</td>

<td class="gt_row gt_right" style="background-color: #FFD0D5; color: #000000;">

20.7169811

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1930

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

1452

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

27.3962264

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1935

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

2121

</td>

<td class="gt_row gt_right" style="background-color: #F4A7A8; color: #000000;">

40.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1940

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">

1601

</td>

<td class="gt_row gt_right" style="background-color: #FBBDC0; color: #000000;">

30.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1945

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">

1354

</td>

<td class="gt_row gt_right" style="background-color: #FDC7CB; color: #000000;">

25.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1950

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

2420

</td>

<td class="gt_row gt_right" style="background-color: #EF9A9A; color: #000000;">

45.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

1955

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F8B5B8; color: #000000;">

1777

</td>

<td class="gt_row gt_right" style="background-color: #F8B5B8; color: #000000;">

33.5283019

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2000

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">

1040

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">

19.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2005

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D7; color: #000000;">

1008

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D7; color: #000000;">

19.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2010

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">

1025

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">

19.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2015

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">

1767

</td>

<td class="gt_row gt_right" style="background-color: #F8B6B8; color: #000000;">

33.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2020

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">

1421

</td>

<td class="gt_row gt_right" style="background-color: #FDC4C8; color: #000000;">

26.8113208

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2025

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

1122

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

21.1698113

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2030

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

1447

</td>

<td class="gt_row gt_right" style="background-color: #FCC3C7; color: #000000;">

27.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2035

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

1131

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

21.3396226

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2040

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">

1036

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D6; color: #000000;">

19.5471698

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2045

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

1130

</td>

<td class="gt_row gt_right" style="background-color: #FFCFD4; color: #000000;">

21.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2050

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #F9B8BB; color: #000000;">

1712

</td>

<td class="gt_row gt_right" style="background-color: #F9B8BB; color: #000000;">

32.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2055

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D5; color: #000000;">

1068

</td>

<td class="gt_row gt_right" style="background-color: #FFD1D5; color: #000000;">

20.1509434

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2100

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

845

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

15.9433962

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2105

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD4D9; color: #000000;">

913

</td>

<td class="gt_row gt_right" style="background-color: #FFD4D9; color: #000000;">

17.2264151

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2110

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFCCD1; color: #000000;">

1243

</td>

<td class="gt_row gt_right" style="background-color: #FFCCD1; color: #000000;">

23.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2115

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">

1020

</td>

<td class="gt_row gt_right" style="background-color: #FFD2D6; color: #000000;">

19.2452830

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2120

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFDBDF; color: #000000;">

660

</td>

<td class="gt_row gt_right" style="background-color: #FFDBDF; color: #000000;">

12.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2125

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">

425

</td>

<td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">

8.0188679

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2130

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD8DC; color: #000000;">

777

</td>

<td class="gt_row gt_right" style="background-color: #FFD8DC; color: #000000;">

14.6603774

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2135

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

864

</td>

<td class="gt_row gt_right" style="background-color: #FFD6DA; color: #000000;">

16.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2140

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">

460

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">

8.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2145

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">

413

</td>

<td class="gt_row gt_right" style="background-color: #FFE1E4; color: #000000;">

7.7924528

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2150

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">

431

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">

8.1320755

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2155

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

139

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.6226415

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2200

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

77

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.4528302

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2205

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">

195

</td>

<td class="gt_row gt_right" style="background-color: #FFE6E9; color: #000000;">

3.6792453

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2210

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

255

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

4.8113208

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2215

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">

451

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E4; color: #000000;">

8.5094340

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2220

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE2E5; color: #000000;">

375

</td>

<td class="gt_row gt_right" style="background-color: #FFE2E5; color: #000000;">

7.0754717

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2225

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">

461

</td>

<td class="gt_row gt_right" style="background-color: #FFE0E3; color: #000000;">

8.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2230

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFDEE2; color: #000000;">

517

</td>

<td class="gt_row gt_right" style="background-color: #FFDEE2; color: #000000;">

9.7547170

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2235

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

117

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.2075472

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2240

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

17

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.3207547

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2245

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

6

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.1132075

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2250

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

85

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.6037736

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2255

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

244

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

4.6037736

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2300

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

175

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

3.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2305

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

151

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

2.8490566

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2310

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2315

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

44

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.8301887

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2320

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

51

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.9622642

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2325

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

84

</td>

<td class="gt_row gt_right" style="background-color: #FFE9EC; color: #000000;">

1.5849057

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2330

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

138

</td>

<td class="gt_row gt_right" style="background-color: #FFE8EB; color: #000000;">

2.6037736

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2335

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

249

</td>

<td class="gt_row gt_right" style="background-color: #FFE5E8; color: #000000;">

4.6981132

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2340

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

175

</td>

<td class="gt_row gt_right" style="background-color: #FFE7EA; color: #000000;">

3.3018868

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2345

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

34

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

0.6415094

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2350

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

12

</td>

<td class="gt_row gt_right" style="background-color: #FFEBEE; color: #000000;">

0.2264151

</td>

</tr>

<tr>

<td class="gt_row gt_right">

2355

</td>

<td class="gt_row gt_center">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

57

</td>

<td class="gt_row gt_right" style="background-color: #FFEAED; color: #000000;">

1.0754717

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

\#\#\#Timeseries 1. Make a time series plot (i.e. type=“l”) of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all days (y-axis)

``` r
g2<-ggplot(data = SummaryIntervalSteps, aes(y=MeanOfSteps, x=TimeInterval))
g2+geom_line(color="red")+
        labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Average Steps Each Day",
         x="Time Interval ",
         y="Average Steps",
         fill="MONTH") 
```

![](GHTemp_files/figure-gfm/Time%20Series-1.png)<!-- -->

2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps? 8:35 a.m.

<!-- end list -->

``` r
SummaryIntervalSteps %>%  filter(MeanOfSteps==max(MeanOfSteps)) %>% select(TimeInterval, MeanOfSteps)
```

    ## # A tibble: 1 x 2
    ##   TimeInterval MeanOfSteps
    ##          <dbl>       <dbl>
    ## 1          835        206.

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)
2.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

<!-- end list -->

``` r
Yeardf <- maindf %>% group_by(`Year`=year(date)) %>%  
        summarize(ISNA_Count = sum(is.na(steps)), Mean_ISNA=mean(is.na(steps)), 
        Total_Obs = length(maindf$date)) %>% ungroup() %>% gt() %>% 
        tab_header(title = "Year 'IS.NA' Summary",
        subtitle = glue("{start_date} to {end_date}"))
Yeardf
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gxpeffnycg .gt_table {
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

#gxpeffnycg .gt_heading {
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

#gxpeffnycg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gxpeffnycg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gxpeffnycg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gxpeffnycg .gt_col_headings {
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

#gxpeffnycg .gt_col_heading {
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

#gxpeffnycg .gt_column_spanner_outer {
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

#gxpeffnycg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gxpeffnycg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gxpeffnycg .gt_column_spanner {
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

#gxpeffnycg .gt_group_heading {
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

#gxpeffnycg .gt_empty_group_heading {
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

#gxpeffnycg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gxpeffnycg .gt_from_md > :first-child {
  margin-top: 0;
}

#gxpeffnycg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gxpeffnycg .gt_row {
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

#gxpeffnycg .gt_stub {
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

#gxpeffnycg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gxpeffnycg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gxpeffnycg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gxpeffnycg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gxpeffnycg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gxpeffnycg .gt_footnotes {
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

#gxpeffnycg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gxpeffnycg .gt_sourcenotes {
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

#gxpeffnycg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gxpeffnycg .gt_left {
  text-align: left;
}

#gxpeffnycg .gt_center {
  text-align: center;
}

#gxpeffnycg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gxpeffnycg .gt_font_normal {
  font-weight: normal;
}

#gxpeffnycg .gt_font_bold {
  font-weight: bold;
}

#gxpeffnycg .gt_font_italic {
  font-style: italic;
}

#gxpeffnycg .gt_super {
  font-size: 65%;
}

#gxpeffnycg .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="gxpeffnycg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Year ‘IS.NA’ Summary

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Year

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

ISNA\_Count

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean\_ISNA

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Total\_Obs

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_right">

2012

</td>

<td class="gt_row gt_center">

2304

</td>

<td class="gt_row gt_right">

0.1311475

</td>

<td class="gt_row gt_center">

17568

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

## Monthly Missing Data

``` r
Monthdf <- maindf %>% group_by(M=month(date,label=TRUE, abbr=TRUE)) %>% 
  summarize(Total_Obs = length(date), ISNA_Count = sum(is.na(steps)),
            Mean_ISNA=mean(is.na(steps))) 

MonthdfGT <- Monthdf %>% gt() %>% 
  tab_header(
    title = "Monthly 'IS.NA' Summary",
    subtitle = glue("{start_date} to {end_date}"))
MonthdfGT
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hdmhdjvyyw .gt_table {
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

#hdmhdjvyyw .gt_heading {
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

#hdmhdjvyyw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hdmhdjvyyw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hdmhdjvyyw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hdmhdjvyyw .gt_col_headings {
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

#hdmhdjvyyw .gt_col_heading {
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

#hdmhdjvyyw .gt_column_spanner_outer {
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

#hdmhdjvyyw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hdmhdjvyyw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hdmhdjvyyw .gt_column_spanner {
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

#hdmhdjvyyw .gt_group_heading {
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

#hdmhdjvyyw .gt_empty_group_heading {
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

#hdmhdjvyyw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hdmhdjvyyw .gt_from_md > :first-child {
  margin-top: 0;
}

#hdmhdjvyyw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hdmhdjvyyw .gt_row {
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

#hdmhdjvyyw .gt_stub {
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

#hdmhdjvyyw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hdmhdjvyyw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hdmhdjvyyw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hdmhdjvyyw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hdmhdjvyyw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hdmhdjvyyw .gt_footnotes {
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

#hdmhdjvyyw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hdmhdjvyyw .gt_sourcenotes {
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

#hdmhdjvyyw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hdmhdjvyyw .gt_left {
  text-align: left;
}

#hdmhdjvyyw .gt_center {
  text-align: center;
}

#hdmhdjvyyw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hdmhdjvyyw .gt_font_normal {
  font-weight: normal;
}

#hdmhdjvyyw .gt_font_bold {
  font-weight: bold;
}

#hdmhdjvyyw .gt_font_italic {
  font-style: italic;
}

#hdmhdjvyyw .gt_super {
  font-size: 65%;
}

#hdmhdjvyyw .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="hdmhdjvyyw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Monthly ‘IS.NA’ Summary

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

M

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Total\_Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

ISNA\_Count

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean\_ISNA

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center">

Oct

</td>

<td class="gt_row gt_center">

8928

</td>

<td class="gt_row gt_center">

576

</td>

<td class="gt_row gt_right">

0.06451613

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Nov

</td>

<td class="gt_row gt_center">

8640

</td>

<td class="gt_row gt_center">

1728

</td>

<td class="gt_row gt_right">

0.20000000

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

``` r
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

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dcrqlgfyjr .gt_table {
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

#dcrqlgfyjr .gt_heading {
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

#dcrqlgfyjr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dcrqlgfyjr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dcrqlgfyjr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dcrqlgfyjr .gt_col_headings {
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

#dcrqlgfyjr .gt_col_heading {
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

#dcrqlgfyjr .gt_column_spanner_outer {
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

#dcrqlgfyjr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dcrqlgfyjr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dcrqlgfyjr .gt_column_spanner {
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

#dcrqlgfyjr .gt_group_heading {
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

#dcrqlgfyjr .gt_empty_group_heading {
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

#dcrqlgfyjr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dcrqlgfyjr .gt_from_md > :first-child {
  margin-top: 0;
}

#dcrqlgfyjr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dcrqlgfyjr .gt_row {
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

#dcrqlgfyjr .gt_stub {
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

#dcrqlgfyjr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcrqlgfyjr .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dcrqlgfyjr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcrqlgfyjr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dcrqlgfyjr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dcrqlgfyjr .gt_footnotes {
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

#dcrqlgfyjr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dcrqlgfyjr .gt_sourcenotes {
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

#dcrqlgfyjr .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dcrqlgfyjr .gt_left {
  text-align: left;
}

#dcrqlgfyjr .gt_center {
  text-align: center;
}

#dcrqlgfyjr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dcrqlgfyjr .gt_font_normal {
  font-weight: normal;
}

#dcrqlgfyjr .gt_font_bold {
  font-weight: bold;
}

#dcrqlgfyjr .gt_font_italic {
  font-style: italic;
}

#dcrqlgfyjr .gt_super {
  font-size: 65%;
}

#dcrqlgfyjr .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="dcrqlgfyjr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

By day ‘IS.NA’ Summary

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Daily

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Total\_Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

ISNA\_Count

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean\_ISNA

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center">

Sun

</td>

<td class="gt_row gt_center">

2304

</td>

<td class="gt_row gt_center">

288

</td>

<td class="gt_row gt_right">

0.1250000

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Mon

</td>

<td class="gt_row gt_center">

2592

</td>

<td class="gt_row gt_center">

576

</td>

<td class="gt_row gt_right">

0.2222222

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Tue

</td>

<td class="gt_row gt_center">

2592

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_right">

0.0000000

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Wed

</td>

<td class="gt_row gt_center">

2592

</td>

<td class="gt_row gt_center">

288

</td>

<td class="gt_row gt_right">

0.1111111

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Thu

</td>

<td class="gt_row gt_center">

2592

</td>

<td class="gt_row gt_center">

288

</td>

<td class="gt_row gt_right">

0.1111111

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Fri

</td>

<td class="gt_row gt_center">

2592

</td>

<td class="gt_row gt_center">

576

</td>

<td class="gt_row gt_right">

0.2222222

</td>

</tr>

<tr>

<td class="gt_row gt_center">

Sat

</td>

<td class="gt_row gt_center">

2304

</td>

<td class="gt_row gt_center">

288

</td>

<td class="gt_row gt_right">

0.1250000

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

``` r
g3 <- ggplot(data = dailydf, aes(Daily))

g3 + geom_bar(aes(weight = ISNA_Count))+
        labs(title="Personal Movement Activity Monitoring Device", 
         subtitle="Missing data in Week Days",
         x="Daily",
         y="Frequency") 
```

![](GHTemp_files/figure-gfm/histogram%20by%20days-1.png)<!-- -->

3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- end list -->

``` r
# load these library at these section because it affects lubridate labels
# and abbr
library(mice)
library(missForest)
library(VIM)
```

Using aggr function to plot NA.

``` r
aggr(maindf, col=c('navyblue', 'red'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(maindf), cex.axis=.8,
     gap=5, ylab=c("Missing Data", "Pattern"))
```

![](GHTemp_files/figure-gfm/aggr%20vim-1.png)<!-- -->

    ## 
    ##  Variables sorted by number of missings: 
    ##  Variable     Count
    ##     steps 0.1311475
    ##      date 0.0000000
    ##  interval 0.0000000

I’m using Predictive Mean Matching (PMM) model to impute the missing
data. 1. m = number of multiple imputations. Default is 5 and I picked
3.  
2\. maxit = A scalar giving the number of imputations. the default is 5
and I chose 50 3. Method = pmm 4. seed = set.seed

``` r
imputed_maindf1 <- mice(maindf, m=3, maxit = 50, 
                        method = 'pmm', seed = 420)
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

Summary of the Imputed Data.

``` r
summary(imputed_maindf1)
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

Multiple imputations (m=3)

``` r
head(imputed_maindf1$imp$steps)
```

    ##   1  2  3
    ## 1 0 47 47
    ## 2 0  0  0
    ## 3 0  0  0
    ## 4 0  0  0
    ## 5 0  0  0
    ## 6 0  0  0

Picking a model from m = 3 to impute the data.

``` r
complete_Maindf1 <- complete(imputed_maindf1, 1)

summary(complete_Maindf1)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.07   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0

4.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- end list -->

``` r
CompleteMeanTotal <- complete_Maindf1 %>% group_by(date) %>%  summarize(Steps = sum(steps)) %>% mutate(m=month(date))

g4<-ggplot(data = CompleteMeanTotal, aes(x=Steps,fill=factor(m)))
        
gg4<-g4+geom_histogram(bins = 9, alpha=.5)+geom_vline(
                xintercept = mean(CompleteMeanTotal$Steps))+
        labs(title="Personal Movement Activity Monitoring Device", 
                subtitle="Total Steps Each Day (Imputed dataset), Observation = 61 days",
                x="STEPS",
                y="FREQUENCY",
                fill="MONTH") 

library(cowplot)

plot_grid(gg1, gg4, nrow = 2, labels = "AUTO")
```

![](GHTemp_files/figure-gfm/Comparison%20from%20missing%20data%20to%20imputed%20data-1.png)<!-- -->
The above histogram shows the imputed dataset’s mean decrease -203.1231,
median decrease -326, the steps increase 69042, observation increase by
8 because we imputed the datasets.

Calculating the steps mean and median.

``` r
CompleteTotal <-
    complete_Maindf1 %>% drop_na() %>% group_by(date) %>%  
    summarize(Steps = sum(steps)) %>% mutate(m =month(date))

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

<!--html_preserve-->

<style>html {
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

<div id="jnuwfitjfz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Step Mean and Median Summary (No missing data

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SumofSteps

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMean

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMedian

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">

61

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

651215

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10675.66

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10571

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

Original data set with NA remove.

``` r
GTMeanTotal
```

<!--html_preserve-->

<style>html {
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

<div id="lubhynfhqr" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="4" class="gt_heading gt_title gt_font_normal" style>

Step Mean and Median Summary

</th>

</tr>

<tr>

<th colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

2012-10-01 to 2012-11-30

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Obs

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SumofSteps

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMean

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

StepsMedian

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_center" style="background-color: #FFFF00; color: #000000;">

53

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

570608

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10766.19

</td>

<td class="gt_row gt_right" style="background-color: #FFFF00; color: #000000;">

10765

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

Comparison formula:

``` r
NewSteps <- CompleteMeanMed$SumofSteps - StepsMeanMed$SumofSteps
NewMean <- CompleteMeanMed$StepsMean - StepsMeanMed$StepsMean
NewMed <- CompleteMeanMed$StepsMedian - StepsMeanMed$StepsMedian
NewSteps
```

    ## [1] 80607

``` r
NewMean
```

    ## [1] -90.53294

``` r
NewMed
```

    ## [1] -194

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a
    weekday or weekend day.

<!-- end list -->

``` r
MonFri <- c(2,3,4,5,6)

SatSun <- c(1,7)
```

New datasets with imputed missing data.

``` r
maindf2 <- complete_Maindf1 %>% mutate(DayNumber = day(date))

MonFriDf <- maindf2 %>% filter(DayNumber %in% MonFri)

SatSunDf <- maindf2 %>% filter(DayNumber %in% SatSun)
```

2.  Make a panel plot containing a time series plot (i.e. type=“l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- end list -->

``` r
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

![](GHTemp_files/figure-gfm/Timeseries%20difference%20Weekdays%20vs%20Weekends-1.png)<!-- -->
