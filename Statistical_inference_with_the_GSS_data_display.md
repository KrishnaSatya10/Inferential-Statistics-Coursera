---
title: "Statistical inference with the GSS data"
knit: (function(input_file, encoding) {
  rmarkdown::render(input_file,
  encoding=encoding,
  output_file=file.path(dirname(input_file), 'Statistical_inference_with_the_GSS_data_display.html'))})
output: 
  html_document:
   keep_md: true 
  fig_height: 4
highlight: pygments
theme: united
---





  
## Setup
  
### Load packages

Let's load the packages required for analysis before we proceed further:


```r
suppressMessages(library(ggplot2, quietly = TRUE))
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```
## Warning: As of rlang 0.4.0, dplyr must be at least version 0.8.0.
## x dplyr 0.7.8 is too old for rlang 0.4.2.
## i Please update dplyr to the latest version.
## i Updating packages on Windows requires precautions:
##   <https://github.com/jennybc/what-they-forgot/issues/62>
```

```r
suppressMessages(library(dplyr, quietly = TRUE))
```

```
## Warning: package 'dplyr' was built under R version 3.5.2
```

```r
suppressMessages(library(statsr, quietly = TRUE))
```

```
## Warning: package 'statsr' was built under R version 3.5.3
```

```
## Warning: package 'BayesFactor' was built under R version 3.5.3
```

```
## Warning: package 'coda' was built under R version 3.5.3
```

```r
suppressMessages(library(mosaic, quietly = TRUE))
```

```
## Warning: package 'mosaic' was built under R version 3.5.3
```

```
## Warning: package 'ggformula' was built under R version 3.5.3
```

```
## Warning: package 'ggstance' was built under R version 3.5.3
```

```
## Warning: package 'mosaicData' was built under R version 3.5.3
```
We will explore the data using the `dplyr` package and visualize it using the `ggplot2` package for data visualization. In addition, we will also import `statsr`package to utilize some of the statistical modeling functionalities offered by it. 





### Load data
The dataset used for this research analysis has been obtained from the General Social Survey (GSS) which contains cumulative data collected between 1972 - 2012. There are a total of 57061 observations with 114 fields, where each field corresponds to a specific question posed to the respondent. 


```r
load("gss.Rdata")
```

* * *
  
## Part 1: Data
 
### About General Social Survey

According to Wikipedia, General Social Survey (GSS) is a sociological survey which collects information and keeps a historical record of the concerns, experiences, attitudes, and practices of residents of the United States in order to:

1) monitor and explain trends and constants in attitudes, behaviors, and attributes

2) examine the structure and functioning of society in general as well as the role played by relevant subgroups

3) compare the United States to other societies in order to place American society in comparative perspective and develop cross-national models of human society

4) make high-quality data easily accessible to scholars, students, policy makers, and others

###Methodology

+ The GSS an observational study conducted on target population of adults over the age of 18. 
+ It randomly selects respondents in households from a mix of urban, suburban, and rural geographic areas of United States to take part in the survey. 
+ The survey data has been gathered through a face-to-face with respondents by NORC at the University of Chicago.

**Note: **

Since this is an observational study based on random assignment, and not an experiment that actively seeks to establish causal relationship between the variables, care must be taken while interpreting association between variables. Association doesn't imply a causal relationship between associated variables in an observational study. 

Also, because the participation in the study is strictly voluntary and the study is based on adults over an age of 18 years some bias may make its way into this sample which is unavoidable. 

However, as the data has been collected randomly (simple random sample) from over thousand respondents any bias introduced may have a minimal impact when generalizaing the study findings to all adult American citizens. 
  
  * * *
  
## Part 2: Research question

The following analysis performed on GSS data serves to answer the following question:

Is there an associaion between race and perception towards military spending?


This analysis aims to establish if there is a signficant evidence of discrepancy in perception towards military spending across two races: 'Black' and 'White'. The variables chosen for the same are:

    race - Race of the respondent (categorical variable with three levels: Black, White, Other)
    
    natarms - Perception towards military spending(categorical variable with three levels: Too Little, Too Much, About Right)

  
  * * *
  
## Part 3: Exploratory data analysis 

Filtering required columns from the gss data to perform our analysis:

```r
arms_race <- gss[,c('race','natarms','year')]
arms_race <- na.omit(arms_race)
```
We focus our analysis primarily on Black and White races. Hence for ease of analysis let's exclude other races. 

```r
arms_race <- arms_race %>% filter(arms_race['race'] != "Other")
```

```
## Warning: package 'bindrcpp' was built under R version 3.5.2
```

Computing proportions of black and white respondents:

```r
summary(arms_race)
```

```
##     race              natarms           year     
##  White:26067   Too Little : 7226   Min.   :1973  
##  Black: 4112   About Right:13209   1st Qu.:1978  
##  Other:    0   Too Much   : 9744   Median :1987  
##                                    Mean   :1989  
##                                    3rd Qu.:2000  
##                                    Max.   :2012
```

```r
addmargins(tally(natarms~race,data = arms_race))
```

```
##              race
## natarms       White Black Other   Sum
##   Too Little   6467   759     0  7226
##   About Right 11501  1708     0 13209
##   Too Much     8099  1645     0  9744
##   Sum         26067  4112     0 30179
```
The summary table shows that the 26067 members of White race have participated in the survey, against 4112 Black race, thereby accounting to 86.38% of total participants(calculations exclude 'other' races).
Of these, on an average, 32.29% believe that spending is too much, 43.76% believe that it is about right, and 23.94% believe that it is too less. 

The aim of this analysis is to compare if the perception of individual races differs significantly from average perception. 

The mosaic plot helps us visualize data better. The sizes of blocks in this plot are in proportion to the values seen in the summary table. 


```r
plot(arms_race$race,arms_race$natarms,xlab = "Race", ylab = "Perception towards military spending", col = c("lightblue","lightgreen","pink"))
```

![](F:\Alohomora\STUDY\Coursera Certificates\Inferential Statistics Duke\Statistical_inference_with_the_GSS_data_display_files/figure-html/plot-data-a-1.png)<!-- -->
From the plot we can see that the proportion of black respondents who bellieve the military spend is too much is higher than those observed in white population. 

 * * *

##Inference

###Stating the hypotheses

**_Null Hypothesis:_** Race and perception towards military spending are independent. Perception towards military spending does not vary with race.

**_Alternative Hypothesis:_** Race and perception towards miitary spending are associated. Perception towards military spending varies with race.

###Checking conditions

+ Respondents have been randomly sampled, and at 30179 datapoints, consitite less than 10% of the population. 

+ Each respondent's response accounts to only one cell in the table, as observed from continency table. 

+ Each cell has atleast 5 expected cases.

###Method used: Chi-square Goodness of Fit test

Chi-square Goodness of Fit test helps us determine if the observed counts are very different from the expected counts,in other words the deviations are large from what would be expected based on sampling variation or simply chance alone.

In the context of our analysis, Chi-square GOF will help us evaluate how
well the observed data fit the expected distribution.If in fact the perception towards military spending is independent of race, then we would expect the observed count to follow the expected percentage distribution in the population. 

The p-value obtained from Chi-Square GOF test will help us decide if the difference is significant enough that it cannot be attributed to just sampling bias. 

###Result interpretation


```r
chisq.test(arms_race$natarms, arms_race$race)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  arms_race$natarms and arms_race$race
## X-squared = 153.07, df = 2, p-value < 2.2e-16
```

+   df = (r-1) x (c-1)
where

+   r = no. of rows in contingency table 

+   c = no. of columns in contingency table

We've observed that the chi-square statisic is 153.07 and the corresponding p-value < 0.05, the significance level. 

We fail to reject the null hypothesis; the data provides a convincing evidence that race of an individual and in his/her perception towards military spending are associated.

**Note:** Since this analysis is based on Chi-square GOF test for categorical variables containing multiple levels, confidence interval for difference of proportions cannot be estimated for the same across multiple levels. 

* * *
