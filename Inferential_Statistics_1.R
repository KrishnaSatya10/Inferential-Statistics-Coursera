library(dplyr)
library(ggplot2)
library(statsr)
library(tidyr)

set.seed(9102015)  
data(ames)



#Only two are being considered: area and sale price??
ggplot(data = ames , aes(x = area)) + geom_histogram()

ames %>% summarize(mu = mean(area),sigma = sd(area), med = median(area), maximum = max(area), minimum = min(area),q1 = quantile(area, 0.25),q3 = quantile(area, 0.75), IQR_pop = IQR(area))

help(sample_n) #also check for rep_sample_n
#This command selects a random sample of rows specified in the bracket argument:
samp <- sample_n(ames, n) #original formula. below is for dplyr 
sample1 <- ames %>% sample_n( size = 50) #This yields one sample whose mean we know. 
sample1 %>% summarise(lower =  - z_star_95 * sd(area) / sqrt(n), upper = mean(area) + z_star_95 * sd(area) / sqrt(n))
#The code gives the confidence interval of a single sample. In such a way we can get confidence interval of all samples (15000 samples)

#If we wish to generate 15000 such smaples of size 50 and plot sampling distribution we can do it in the following manner:
#We use rep_sample_n command instead of sample_n command. because we want to repeatedly sample; ths is passed as an argument to rep_sample_n function
sampling_50_area <- ames %>% rep_sample_n(size = 50,  reps =15000 ,replace = TRUE) %>% summarise ( x_bar = mean(area), standard_dev = sd(area))
ggplot(data = sampling_50_area, aes(x = x_bar)) +geom_histogram()

z_star_95 <- qnorm(0.975) #qnorm function is used to compute the z score. 
n = 50

CI_1000_samples <- ames %>% rep_sample_n(size = 50, reps = 1000, replace = TRUE) %>% summarize(lower= mean(area) - z_star_95*sd(area)/sqrt(n),upper= mean(area) + z_star_95*sd(area)/sqrt(n)) #Similarly we calculate the CI for all 1000 samples
pop_mean <- mean(ames$area)

CI_1000_samples <- CI_1000_samples %>% mutate(decision = ifelse((pop_mean < upper & pop_mean > lower),'YES','NO'))
nrow(CI_1000_samples[CI_1000_samples$decision == 'NO',]) #There are 65 cases in which the population mean fell outside the CI. 
#This very aptly reflects the real world scenario. And hence we can say with a ___ CI that the true popultion will lie between a sepcific interval. 
#Now we are going to use the gather function which will combine two or more columns and put them in a single column. 

CI_data <- gather(CI_1000_samples,key= 'type', value = 'bound',lower:upper)
CI_data <- CI_data[order(CI_data$replicate),]
#We create that horizontal line plot from this which depicts how many cases we got our mu outside the CI in a different color:
ggplot(CI_data[1:50,], aes(x = bound, y = replicate, group = replicate, color = decision)) +geom_point() + geom_line() +geom_vline(xintercept = pop_mean, color = "pink")
