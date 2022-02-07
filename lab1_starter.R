# Name: Evangelos Kassos
# Economics 50, Harvard University

############ LAB 1 CODING EXERCISE

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)
library("scales")


# Set your working directory to the folder where you downloaded the .dta file
# Note - this can also be done via the drop down menu
# Go to session -> set working directory -> choose working directory
setwd("~/Documents/EC50/Lab 1")

## Read data 
nlsy <- read_dta("nlsy97.dta")

#Q1: Create Histogram of kid_income (hint: sample code is found in table 3 of the assignment page)
ggplot(nlsy) + geom_histogram(aes(x=kid_income, y=..density..), bins = 50)
ggsave("histogram_kid_income.png")

#Q2: Mean of kid_income

kid_income_mean = mean(nlsy$kid_income, na.rm=TRUE)

  # ANSWER: The sample mean of kid_income is $70499.94

#Q3: Fraction below mean

nlsy$below_mean <- 0
nlsy$below_mean <- ifelse(nlsy$kid_income < mean(nlsy$kid_income, na.rm=TRUE), 1, 0)
sum(nlsy$below_mean)/nrow(nlsy)
nObservations = nrow(nlsy)

  # ANSWER: The fraction of observations that have kid_income below its mean is 59.6%. 
  # As we can see from the histogram we drew in question 1, there are some significant outliers on the top end of the
  # kid_income range, which substantially increase the arithmetic mean. 

#Q4: Median of kid income reported by summary()

kid_income_median = median(nlsy$kid_income, na.rm=TRUE)
       
  # ANSWER: The sample median of kid_income is $58750.


#Q5: Standard deviation

kid_income_sd = sd(nlsy$kid_income, na.rm=TRUE)

  # ANSWER: The sample standard deviation of kid_income is $59552.02.


#Q6: Fraction within 1 and 2 standard deviations of mean

fractionOneDeviation = sum(nlsy$kid_income < kid_income_mean + kid_income_sd & nlsy$kid_income > kid_income_mean - kid_income_sd, ma.rm = TRUE) / nObservations
fractionTwoDeviations = sum(nlsy$kid_income < kid_income_mean + 2 * kid_income_sd & nlsy$kid_income > kid_income_mean - 2 * kid_income_sd, ma.rm = TRUE) / nObservations

  # ANSWER: The fraction of observations that are within one standard deviation of the mean of kid_income is 78.69%.
  # The fraction of observations that are within one standard deviation of the mean of kid_income is 94.91%.

#Q7: Generate percentile ranks

percentile_rank <- function(x){
  rank <- ifelse(is.na(x), NA, rank(x, ties.method = "average"))
  100*rank/max(rank, na.rm=T)
}

nlsy$kid_inc_rank <- percentile_rank(nlsy$kid_income)

ggplot(nlsy) + geom_histogram(aes(x=kid_inc_rank, y=..density..), bins = 50)
ggsave("histogram_kid_inc_rank.png")

#Compare mean and median in percentile ranks

kid_inc_rank_mean = mean(nlsy$kid_inc_rank, na.rm=TRUE)           #Mean rank = 50.09
kid_inc_rank_median = median(nlsy$kid_inc_rank, na.rm=TRUE)       #Median rank = 50.11

#Q8: Bin scatters to find variables related to kid_income

ggplot(nlsy, aes(x = child_education , y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

ggplot(nlsy, aes(x = child_sat , y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

ggplot(nlsy, aes(x = parent_inc , y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

ggplot(nlsy, aes(x =  age2015, y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

ggplot(nlsy, aes(x =  mother_education, y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

ggplot(nlsy, aes(x =  father_education, y = kid_income)) +
  stat_binmean(n = 20, geom = "line") + 
  stat_binmean(n = 20, geom = "point")

# Which of the variables you chose seem to be non-linearly related to kid_income?

  # ANSWER: We can see that child_education (besides the last few years of education after college) 
  # seems to have a positive linear relationship with an increased kid_income. The same is true for
  # an increasing SAT score. However, we cannot say the same for the years the kid's mother and father
  # spent in education, along with the kid's age, as they do not appear to be linearly correlated with
  # the kid's income level, specifically for low variable values.

#Q9: Random assignment simulation

  #a) Set seed so that simulations are replicable
  set.seed(21423503)

  #Generate uniformly distributed random number between 0 and 1
  
  nlsy$random_number <- runif(length(nlsy$kid_income))
  
  #b) (i) Generate new variable: treatment_group

  nlsy$treatment_group <- ifelse(nlsy$random_number < 0.5, 0, 1)

  #b) (ii) How many observations in treatment group? How many in control group?
  
  nControl = sum(nlsy$treatment_group == 0)     # 2751 observations in control group
  nTreatment = sum(nlsy$treatment_group == 1)   # 2735 observations in treatment group
  
  #c) Compute sample mean and sample sd for all variables listed in table 1 
  
  by(nlsy$parent_inc, list(nlsy$treatment_group), mean) 
  by(nlsy$parent_inc, list(nlsy$treatment_group), sd)
  
  options(dplyr.width = Inf)
  nlsy %>% group_by(treatment_group) %>% summarise_all("mean") 
  nlsy %>% group_by(treatment_group) %>% summarise_all("sd")  
    
    # ANSWER: 
  
    # Table with sample means for all variables
  
    # treatment_group   kid_income  incarcerated    child_education   child_college child_sat parent_inc  mother_education
    # control group       69606.       0.0963            13.8             0.292        NA       46345.          12.7
    # treatment group     71399.       0.103             13.8             0.298        NA       46481.          12.7
    
    # treatment_group   father_education    female  black   hispanic  white   region  age2015   cohort  kid_inc_rank random_number
    # control group           12.7          0.501   0.261    0.194    0.606    2.64    33.0      1982.      49.8        0.248
    # treatment group         12.7          0.501   0.269    0.203    0.593    2.67    33.0      1982.      50.4        0.744
  
    # Table with sample standard deviation for all variables
    
    # treatment_group   kid_income  incarcerated    child_education   child_college child_sat parent_inc  mother_education
    # control group       58171.       0.295             2.99             0.455        NA       46842.          2.51
    # treatment group     60907.       0.304             3.01             0.457        NA       45328.          2.47
    
    # treatment_group   father_education    female  black   hispanic  white   region  age2015   cohort  kid_inc_rank random_number
    # control group           2.43          0.500   0.439    0.396    0.489    0.990   1.40      1.40       28.9        0.146
    # treatment group         2.30          0.500   0.443    0.402    0.491    0.983   1.39      1.39       28.9        0.146