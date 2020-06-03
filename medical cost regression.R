# Part 1: Prepare before analysis
# set random seed
set.seed(597257)

# pakages
library(ggplot2)
library(broom)
# library(plotly) 

# Read in the heart dataset under same dictionary
insurance <- read.csv('insurance.csv')


# Part 2: Exploratory Data Analysis

# 1）get a general idea of dataset:
# view dataset
View(insurance) 
head(insurance)
# dimension of dataset
dim(insurance) 

# 2) check for missing values:
table(is.na(insurance))
# there is no missing value

# 3) EDA of charges:
# look at the summary and distributions:
# summary table includes range, quantiles, mean
summary(insurance$charges)
# histogram of charges
hist(insurance$charges)
# boxplot of charges
boxplot(insurance$charges) 
title(main='Boxplot of charges')
# conclusion: the two plots shows The original values of charges are right-skewed, not following the normal distribution.

# log (log-e/ln) transformations:
# histogram of log charges
hist(log(insurance$charges))
# density plot of log charges
plot(density(log(insurance$charges))) 
# violin plot of log charges
ggplot(data=insurance,aes(y=log(charges),x=1))+
  geom_violin()+
  labs(title='violin plot of log charges')+
  theme(plot.title = element_text(hjust = 0.5)) #set theme to center the title  
# boxplot of log charges
boxplot(log(insurance$charges)) 
title(main='Boxplot of log charges')
# conclusion: we will consider the log-transformed data because it look more normal
# log-transform charges to new variable log_charges
insurance$log_charges=log(insurance$charges) 



# 4) EDA of age:   
# look at the summary and distributions, check for outliers:
# summary table includes range, quantiles, mean
summary(insurance$age)
# histogram of age
hist(insurance$age)
# density plot of age
plot(density(insurance$age))
# violin plot of age
ggplot(data=insurance,aes(y=age,x=1))+
  geom_violin()+
  labs(title='violin plot of age')+
  theme(plot.title = element_text(hjust = 0.5))
# box plot of age
boxplot(insurance$age)
title(main='Boxplot of age')
# conclusion: No extremely large or extremely small points. Age doesn‘t have any outlier.

# try to find relationship between age and log charges:
# scatter plot of age vs log charges
ggplot(data=insurance, aes(x=age,y=log_charges)) + 
  geom_point(size=1.2, color = "blue")+
  labs(title="age vs.log-charges ")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))
#plot_ly(data = insurance, x = ~age, y = ~log_charges,type='scatter') # it can also draw the scatter plot, but need to install and library 'plotly' package firstly.
# Pearson correlation of age vs log charges
cor(insurance$age,insurance$log_charges)
# conclusion: Age and log-charges might have a relationship; older people spend more.

# 5) EDA of BMI:  
# look at the summary and distributions, check for outliers:
# summary table includes range, quantiles, mean
summary(insurance$bmi)
# histogram of BMI
hist(insurance$bmi)
# density plot of BMI
plot(density(insurance$bmi))
# violin plot of BMI
ggplot(data=insurance,aes(y=bmi,x=1))+
  geom_violin()+
  labs(title='violin plot of BMI')+
  theme(plot.title = element_text(hjust = 0.5))
# box plot of BMI
boxplot(insurance$bmi)
title(main='boxplot of BMI')
# conclusion: BMI has some extremely large points, but are biological possible, so they could be useful.

# try to find relationship between BMI and log charges:
# scatter plot of BMI vs log charges
ggplot(data=insurance, aes(x=bmi,y=log_charges)) + 
  geom_point(size=1.2, color = "blue")+
  labs(title="age vs.log-charges ")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))
#plot_ly(data = insurance, x = ~age, y = ~log_charges,type='scatter') # it can also draw the scatter plot
# Pearson correlation of BMI vs log charges
cor(insurance$bmi,insurance$log_charges)
# conclusion: BMI and log-charges don’t seem like to have obvious relationship, but when BMI are over 30 or so，there are some much higher values.

# transformed BMI to Obesity:
# construct obesity variable by divide BMI into non-obesity  (BMI < 30) and obesity (BMI ≥ 30)
insurance$obesity=factor(ifelse(insurance$bmi >= 30, 'yes', 'no'),levels=c('no','yes')) # set non-obesity as the "lowest level" so it will be the baseline in lm
# frequency table of obesity
table(insurance$obesity)
# density plots of log charges in non-obesity and obesity
ggplot(data=insurance,aes(x=log_charges))+
  geom_density()+
  facet_grid(rows=vars(obesity))+
  labs(title="Density plots of log charges in non-obesity and obesity")+
  theme(plot.title = element_text(hjust = 0.5))
# boxplots of log charges in non-obesity and obesity
ggplot(data=insurance,aes(y=log_charges,x=obesity))+
  geom_boxplot()+
  stat_boxplot(col="blue", geom="errorbar", width=0.5)+
  labs(title="Boxplots of log charges in non-obesity and obesity")+
  theme(plot.title = element_text(hjust = 0.5))
# conclusion: the Obesity variable may influence log-charges

# 6) EDA of smoker:  
# frequency table of smoker
table(insurance$smoker)
# try to find relationship between smoker and log charges:
# boxplots of log charges in non-smoker and smoker
ggplot(data=insurance,aes(y=log_charges,x=smoker))+
  geom_boxplot()+
  stat_boxplot(col="blue", geom="errorbar", width=0.5)+ 
  labs(title="Boxplots of log charges in non-smoker and smoker")+
  theme(plot.title = element_text(hjust = 0.5))
#plot_ly(data = insurance, x = ~smoker, y = ~log_charges,type='box') # it can also draw the box plots, but need to install and library 'plotly' package firstly.
# histogram of log charges in non-smoker and smoker
ggplot(data=insurance,aes(x=log_charges))+
  geom_histogram(binwidth =.5,fill='white',colour="black")+
  facet_grid(rows=vars(smoker))+
  labs(title="Histogram of log charges in non-smoker and smoker")+
  theme(plot.title = element_text(hjust = 0.5))
# conclusion:Smoking might has an influence on log-charges. Smokers has higher medical charges than non-smokers. 

# 7) EDA of sex:  
# frequency table of sex
table(insurance$sex)
# try to find relationship between sex and log charges:
# boxplots of log charges in female and male
ggplot(data=insurance,aes(y=log_charges,x=sex))+
  geom_boxplot()+
  stat_boxplot(col="blue", geom="errorbar", width=0.5)+ 
  labs(title="Boxplots of log charges in female and male")+
  theme(plot.title = element_text(hjust = 0.5))
# plot_ly(data = insurance, x = ~sex, y = ~log_charges,type='box')# it can also draw the box plots
# histogram of log charges in non-smoker and smoker
ggplot(data=insurance,aes(x=log_charges))+
  geom_histogram(binwidth =.5,fill='white',colour="black")+
  facet_grid(rows=vars(sex))+
  labs(title="Histogram of log charges in female and male")+
  theme(plot.title = element_text(hjust = 0.5))
# conclusion: Sex does not seem to have an influence on log-charges. 


# 8) EDA of region:  
# frequency table of region
table(insurance$region)
# try to find relationship between region and log charges:
# boxplots of log charges in four regions
ggplot(data=insurance,aes(y=log_charges,x=region))+
  geom_boxplot()+
  stat_boxplot(col="blue", geom="errorbar", width=0.5)+ 
  labs(title="Boxplots of log charges in four regions")+
  theme(plot.title = element_text(hjust = 0.5))
# plot_ly(data = insurance, x = ~region, y = ~log_charges,type='box')# it can also draw the box plots
# histogram of log charges in four regions
ggplot(data=insurance,aes(x=log_charges))+
  geom_histogram(binwidth =.4,fill='white',colour="black")+
  facet_grid(rows=vars(region))+
  labs(title="Histogram of log charges in four regions")+
  theme(plot.title = element_text(hjust = 0.5))
# conclusion: Region does not seem to have an influence on log-charges. 

# 9) EDA of children:  
# frequency table of children
table(insurance$children)
# histogram of children
hist(insurance$children)
# try to find relationship between region and log charges:
# boxplots of log charges in six groups
ggplot(data=insurance,aes(y=log_charges,x=factor(children)))+
  geom_boxplot()+
  stat_boxplot(col="blue", geom="errorbar", width=0.5)+ 
  labs(title="Boxplots of log charges in six groups")+
  theme(plot.title = element_text(hjust = 0.5))
# conclusion: The number of people with 4 or 5 children are very small, so it hard to see if there’s any relationship between children and log charges; Plus, we didn’t find obvious relationship between. 

#conclusion for 4-9: we found 3 key variables: Age, Smoking, Obesity (Transformed BMI). They most likely to have influence on log-charges and taht obey common sense in life, so we are intrested in them.

# 10) Confounding variable:
# using the scatter plot of age vs log-charges with color-coding by smoker to find the relationship between age, log charges and smoker
ggplot(data=insurance, aes(x=age,y=log_charges,colour = smoker)) + 
  geom_point(size=1.2)+
  labs(title="scatter of age vs log-charges (color-coding by smoker)")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))
#plot_ly(data = insurance, x = ~age, y = ~log_charges, color = ~smoker,type='scatter') # it can also draw the scatter plot

# using the scatter plot of age vs log-charges with color-coding by obesity to find the relationship between age, log charges and obesity
ggplot(data=insurance, aes(x=age,y=log_charges,colour = obesity)) + 
  geom_point(size=1.2)+
  labs(title="scatter of age vs log-charges (color-coding by obesity)")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))
#plot_ly(data = insurance, x = ~age, y = ~log_charges, color = ~obesity,type='scatter') # it can also draw the scatter plot

# using the contingency table to find the relationship between smoker and obesity
table(insurance$smoker,insurance$obesity)

# conclusion: the 3 variables seem don‘t have relationship with each other,  so we won’t have to worry about confounding variables.


# Part 3: Hypothesis Testing
# two sample t-test for H_0: there is no difference in log-charges of smokers and non-smokers
test_smoker=t.test(insurance$log_charges[insurance$smoker == 'yes'], insurance$log_charges[insurance$smoker == 'no'])
test_smoker
tidy(test_smoker)
# two sample t-test for H_0: there is no difference in log-charges of obesity and non-obesity
test_obesity=t.test(insurance$log_charges[insurance$obesity == 'yes'], insurance$log_charges[insurance$obesity == 'no'])
test_obesity
tidy(test_obesity)
# two sample t-test for H_0: there is no difference in log-charges of older (age≥median:39) and younger (age<39) 
test_age=t.test(insurance$log_charges[insurance$age>=median(insurance$age)], insurance$log_charges[insurance$age <median(insurance$age)])
test_age
tidy(test_age)

# conclusion: all of the 3 p-values are smaller than 0.05, and all 95% Confidence interval does not include 0, so the t-tests further shows all 3 key variables have influence on the log-charges at 95% level.
# Moreover, by compare the mean-value, we can find smokers, older people, obesity ones tend to have more medical charges.


# Part 4: Linear Regression
# simple linear regression between age and log charges
slr=lm(log_charges~age , data=insurance)
summary(slr)
tidy(slr)
# conclusion:
# 1. P-value is significant at  95% level, so we can conclude there is association between age and log charges at this level
# 2. Average log_e charges to go up by 0.04 (log_e dollars) when increasing age by 1 year
# 3. Average log_e charges to be 7.74 for a person aged 0 year
# check our regression line in the scatter plot between age vs log_charges
ggplot(data=insurance, aes(x=age,y=log_charges,colour = smoker)) + 
  geom_point(size=1.2)+
  geom_smooth(method = "lm", se = FALSE, color = "orange", size=1.4)+
  # geom_abline(slope=summary(slr)$coefficients[2,1],intercept =summary(slr)$coefficients[1,1] ,color='orange',size=1.4)+
  annotate("text", x=27, y=9, label="log-charges=7.74+0.035*age",size=4)+ 
  labs(title="age vs.log-charges ")+
  theme_bw()+
  theme(panel.grid.major =element_blank(),plot.title = element_text(hjust = 0.5))
# we can find the line doesn't fit well from the plot

# multivariable Linear Regression
mlr=lm(log_charges~age+obesity+smoker , data=insurance)
summary(mlr)
tidy(mlr)
# baseline of obesity variable is non-obesity and of smoker variable is non-smoker
# conclusion:
# 1. All p-values are significant and all three independent variables has association with log-charges when adjusting for other variables at 95% level 
# 2. We estimate the average log_e charges to go up by 0.04 (log_e dollars) when increasing age by 1 year and keeping obesity and smoker constant
# 3. We estimate the average log_e charges to go up by 0.14 (log_e dollars) when comparing obesity to non-obesity and keeping age and smoker constant
# 4. We estimate the average log_e charges to go up by 1.55 (log_e dollars) when comparing smoker to non-smoker and keeping age and obesity constant
# 5. We estimate the average log_e charges to be 7.33 for a non-obesity, non-smoker person aged 0 year 




