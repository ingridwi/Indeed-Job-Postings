############################################################   
# 140 Final project 
# Ingrid Wijaya, YueLong Zhang, Arman Bazak, DongHyun Chun
############################################################   
#install.packages('readr')
#install.packages('dplyr')
#install.packages('readxl')
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('car') 
#install.packages('DescTools') 

library(readr)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(car)
library(DescTools)

#################### 
# DATA CLEANING 
#################### 
###  Data cleaning code in Python
# removed jobs listings outside U.S
# replaced NA values of numerical variables with its mean 
# replaced NA values of categorical variables with its mode 
data_US <- read_csv("no_na.csv")
data_US <- data_US[,-1]
head(data_US)
dim(data_US)

#################### 
# EDA
#################### 
### EDA code in Python

#################### 
# PART 1: 
# Top 3 important predictors in predicting estimated salary using random forest
#################### 
###  Code in Python

#################### 
# PART 2:
# Hypothesis testing for normTitleCategory
####################
# Percentage of NAs - 0
sum(is.na(data_US$normTitleCategory)) / length(data_US$normTitleCategory) * 100 

# Unique values 
table(data_US$normTitleCategory)

# boxplot: normTitleCategory
p = ggplot(data_US, aes(y=normTitleCategory, x=estimatedSalary, fill=normTitleCategory)) +
  geom_boxplot(show.legend = FALSE) + 
  labs(title='Estimated salary by job category', y='Job category', x='Estimated salary (dollars)')
p + theme_classic() 

# Hypothesis test
mod.aov = aov(estimatedSalary ~ factor(normTitleCategory), data = data_US)
## null hyp: mean of education requirements is all the same
summary(mod.aov) #reject null.

#################### 
# PART 3:
# Hypothesis testing for experienceRequired
#################### 

group_1 <- data_US %>% filter(experienceRequired >=0 & experienceRequired <= 5)
group_2 <- data_US %>% filter(experienceRequired >5 & experienceRequired <= 10)
group_3 <- data_US %>% filter(experienceRequired >10 & experienceRequired <= 15)
group_4 <- data_US %>% filter(experienceRequired >15 & experienceRequired <= 20) 
data_US<- data_US %>% mutate(group = case_when(experienceRequired >= 0 & experienceRequired <= 5 ~ "1", 
                                                         experienceRequired > 5 & experienceRequired <= 10 ~ "2",  
                                                         experienceRequired > 10 & experienceRequired <= 15 ~ "3",
                                                         experienceRequired > 15 & experienceRequired <= 20 ~ "4" ))
simplified_datafest <-data_US %>% summarise(group, estimatedSalary)

# boxplot: experienceRequired
p = ggplot(simplified_datafest, aes(x=group, y=estimatedSalary, fill=group)) +
  geom_boxplot(show.legend = FALSE) + 
  scale_fill_brewer(palette="BrBG") + 
  scale_x_discrete(labels=c('0 - 5', '5 - 10', '10 - 15', '15 - 20')) +
  labs(title='Estimated salary by minimum experience requirements', x='Minimum experience requirements (years)', y='Estimated salary (dollars)')

p + theme_classic() 


# test of mean pairwise 

a2 <- aov(estimatedSalary ~ group, data= simplified_datafest)
summary(a2)

TukeyHSD(a2, "group")

# based on the pairwise comparison of mean, it is shown that each group of 
# income has different means that are statistically signficant. It is shown that upto group 3,
# as number of required work experience increased, the salary for the job icnreased. 
# however, it changes for group4, the one with the highest requirement of salary 
# and has the lowest income. This is not arguable with the single varaible and there must be 
# other factors that are affecting such as jobs with 15 years or more work experience requirement 
# is the low paying job 


# test of variance pairwise 
leveneTest(estimatedSalary ~ group, data = data_US)

var(group_1$estimatedSalary)
var(group_2$estimatedSalary)
var(group_3$estimatedSalary)
var(group_4$estimatedSalary)
# pairwise test of variance 
var.test(group_1$estimatedSalary, group_2$estimatedSalary
         ,alternative = "two.sided")
var.test(group_1$estimatedSalary, group_3$estimatedSalary
         ,alternative = "two.sided")
var.test(group_1$estimatedSalary, group_4$estimatedSalary
         ,alternative = "two.sided")
var.test(group_2$estimatedSalary, group_3$estimatedSalary
         ,alternative = "two.sided")
var.test(group_2$estimatedSalary, group_4$estimatedSalary
         ,alternative = "two.sided")
var.test(group_3$estimatedSalary, group_4$estimatedSalary
         ,alternative = "two.sided")

# pairwise, the variance difference is signficant for each group
# As the number of experience required increases, variance increases 
# this shows that salary is more volatile as the number of years of experience required increases 

# Closer look into experience required
# Percentage of NAs - 0
sum(is.na(data_US$experienceRequired)) / length(data_US$experienceRequired) * 100

#industry
sum(is.na(data_US$industry)) / length(data_US$industry) * 100
data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(industry)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(mean) %>%
  print(n=26)

data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(industry)   %>% 
  count() %>%
  arrange(n) %>%
  print(n=26)

length(unique(data_US$industry))

#normTitle
sum(is.na(data_US$normTitle)) / length(data_US$normTitle) * 100
data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(normTitle)   %>% 
  summarise(mean(estimatedSalary)) 

normTitle_15 = data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(normTitle)   %>% 
  count() %>%
  arrange(n) %>%
  print(n=26)

ggplot(normTitle_15, aes(x=normTitle, y=n, group =1)) + 
  geom_line()

length(unique(data_US$normTitle))

#normTitleCategory
sum(is.na(data_US$normTitleCategory)) / length(data_US$normTitleCategory) * 100
data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(normTitleCategory)   %>% 
  count() %>%
  arrange(n) %>%
  print(n=51)

length(unique(data_US$normTitleCategory))

#companyId
sum(is.na(data_US$companyId)) / length(data_US$companyId) * 100
data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(companyId)   %>% 
  summarise(mean(estimatedSalary)) 
length(unique(data_US$companyId))


data_US %>% filter(experienceRequired > 15)  %>% 
  group_by(supervisingJob)   %>% 
  summarise(mean(estimatedSalary)) 

#################### 
# PART 4:
# Hypothesis testing for educationRequirements
#################### 
# Percentage of NAs - 0
sum(is.na(data_US$educationRequirements)) / length(data_US$educationRequirements) * 100 

# Unique values 
table(data_US$educationRequirements)

# boxplot: education requirement
p = ggplot(data_US, aes(x=educationRequirements, y=estimatedSalary, fill=educationRequirements)) +
  geom_boxplot(show.legend = FALSE) + 
  scale_fill_brewer(palette="OrRd") + 
  scale_x_discrete(labels=c('High School', 'Higher Education', 'No Education')) +
  labs(title='Estimated salary by education requirements', x='Education requirements', y='Estimated salary (dollars)')

p + theme_classic() 

# Hypothesis test
mod.aov = aov(estimatedSalary ~ factor(normTitleCategory) + factor(experienceRequired) + factor(educationRequirements), data = data_US)
## null hyp: mean of education requirements is all the same
summary(mod.aov) #reject null.

# Post Hoc Analysis: Tukey's test 
mod = aov(estimatedSalary ~ factor(educationRequirements), data = data_US)
PostHocTest(mod, conf.level = 0.95) 

food_type = data_US$food_type
elite= factor(xMT_data_tall$elite, labels=c("Non-Elite", "Elite"))
rating = xMT_data_tall$rating

interaction.plot(food_type, elite, rating, 
                 main = "Interaction plot between type of food businesses and ratings",
                 xlab = "Type of businesses",
                 ylab = "Average rating",
                 col = c("red", "blue"))

# Closer look into those each edudcation requirements
data_US %>% filter(educationRequirements == 'High School')  %>% 
  group_by(industry)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'High School')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'High School')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  tail(5)

# Closer look into those with NO diploma
data_US %>% filter(educationRequirements == 'None')  %>% 
  group_by(industry)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'None')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'None')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  tail(5)

# look into those with higher ed
data_US %>% filter(educationRequirements == 'Higher Education')  %>% 
  group_by(industry)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'Higher Education')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  print(n=26)

data_US %>% filter(educationRequirements == 'Higher Education')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean = mean(estimatedSalary)) %>%
  arrange(desc(mean)) %>%
  tail(5)

data_US %>% group_by(educationRequirements) %>% 
  count()

compare_highEd= data_US %>% filter(educationRequirements == 'Higher Education')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean_experience_highEd = mean(experienceRequired),
            mean_salary_highEd = mean(estimatedSalary),
            mean_lic_highEd = mean(licenseRequiredJob)) %>%
  arrange(desc(mean_salary_highEd)) %>%
  print(n=26)

compare_none= data_US %>% filter(educationRequirements == 'None')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean_experience_none = mean(experienceRequired),
            mean_salary_none = mean(estimatedSalary),
            mean_lic_none = mean(licenseRequiredJob)) %>%
  arrange(desc(mean_salary_none)) %>%
  print(n=26)

compare_highSch= data_US %>% filter(educationRequirements == 'High School')  %>% 
  group_by(normTitleCategory)   %>% 
  summarise(mean_experience_highSch = mean(experienceRequired),
            mean_salary_highSch = mean(estimatedSalary),
            mean_lic_highSch = mean(licenseRequiredJob)) %>%
  arrange(desc(mean_salary_highSch)) %>%
  print(n=26)

full_join(compare_highSch, compare_none)
full_join(compare_highEd, compare_none)




