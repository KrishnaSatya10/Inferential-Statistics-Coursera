library(statsr)
library(dplyr)
library(ggplot2)
data("atheism")
atheism <- data.frame(atheism)

#Filter for US data:
us <-filter(atheism, nationality == "United States")

#Filter out for us 2012 data
us12 <-filter(atheism, nationality == "United States", year == 2012)

#A) SINGLE PROPORTION PROBLEM:

#1) Confidence Interval Calculation:

#Proportion of atheists in US 2012 (sample stats)
p_us12 <- nrow(us12[us12$response == "atheist",])/nrow(us12) 


#SE = SD / sqrt(n)
se_us12 <- sqrt(p_us12*(1-p_us12)/nrow(us12))

#CI:
CI <- c((p_us12 - 1.96*se_us12), (p_us12 + 1.96*se_us12))

#Computing using inference formula:
inference(y = response, data = us12, type = 'ci',method = 'theoretical', statistic = 'proportion',success = 'atheist')
#Gives the same confidence interval as computed earlier. 

#2)Hypothesis Test Evaluation:
#Now check if US has significant deviation from global average to call it an atheist nation

global12 <- atheism[atheism$year== 2012,]
p_global12 <- nrow(global12[global12$response== 'atheist',])/ nrow(global12)

#SE for HT
se_global12 <- sqrt(p_global12*(1-p_global12)/nrow(us12))

#z-score calculation:
z <- (p_us12 - p_global12)/se_global12
#p value: 0.03155 from lookup

#use pnorm to calculate p value:
#p_value <- 2* pnorm(z)
p_value <- 2*(pnorm(p_us12, mean = p_global, sd = se_global))
#There is a convincing evidence that they are varying


#B)TWO PROPORTION PROBLEM: Difference in means for US 2005 and 2012 atheism index.

#1) Confidence Interval Calculation:

#Filtering out for 2005 US data:
us05 <- filter(atheism, nationality == "United States", year == 2005)

#Proportion of atheists in US 2005 (sample stats)
p_us05 <- nrow(us05[us05$response == "atheist",])/nrow(us05) 


#SE = SD / sqrt(n) #this is strictly used for calculating CI
se_pooled_us_ci <- sqrt((p_us12*(1-p_us12)/nrow(us12)) + (p_us05*(1-p_us05)/nrow(us05)))

#Calcuate CI of difference
CI_us <- c(((p_us12 - p_us05) - 1.96*se_pooled_us_ci),((p_us12 - p_us05) + 1.96*se_pooled_us_ci))

#Using inference formula:
inference(y = response,x=year, data = us, type = 'ci',method = 'theoretical', statistic = 'proportion',success = 'atheist')
#Gives the same confidence interval as computed earlier. 

#2)Hypothesis Test Evaluation:
#To evaluate f there is a significant devition between the two years' atheist index:
p_pooled_us <- (nrow(us05[us05$response == "atheist",]) + nrow(us12[us12$response == "atheist",]))/ (nrow(us05) + nrow(us12))
  
  
#SE for HT
se_us_ht <- sqrt(p_pooled_us*(1-p_pooled_us) * ((1/nrow(us12)) + (1/nrow(us05))))

#z-score calculation:
z_diff <- (p_us12 - p_us05)/se_us_ht
#z score is 5.24. We reject null. There is a significant difference


#use pnorm to calculate p value:
p_value <- 2*(1-pnorm((p_us12-p_us05), mean = 0, sd = se_us_ht))
#There is a convincing evidence that they are varying

#Using inference formula:
inference(y=response, x= year, data = us, statistic = 'proportion',success = 'atheist',type = 'ht',method = 'theoretical',alternative = 'twosided')
