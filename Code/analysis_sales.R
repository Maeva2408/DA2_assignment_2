########################
##                    ##
##  Sales Anlaysis    ##
##     DA2 & C1       ##
##    Assignment  2   ##
##                    ##
########################



## During this pandemic, restaurant had to submit to new strict regulations. I chose to take all the observations (42) of
## that new period and see how the sales are affected by the productive hours (and other variable).Then I will be 
## able to give new staffing guidelines to managers of the restaurant in order to stay profitable. 
## I will also do a prediction of the sales.

## Data description
## There is a lot of variables that could affect the sales.
## I used the Sales in EURO (HT) per day (y).
## I chose the variable in a way that either the manager has  control on the variables or that he can know 
## it in advance to take it into consideration when he is doing the schedule.
##  Manager's decision : Working hours and hours of downtime :
## the productive hours (hours of kitchen's employee and waiters)
## the leadership hours (hours of the managers working either in the kitchen or as a waiter)
## The restaurant is using an external delivery platform. When the team feels swamped by the 
## number of clients on site ordering in the restaurant, the team can chose to pause the delivery 
## platform so that no client can order on the platform during a given period.
## Weather : sunny, cloudy, rainy
## Day of the week : Monday, tuesday, etc

##########
# 1) Research question:
#     - IDEA: Want to understand the pattern (possible connection) between the sales and the new environment
#     - Question: What is the pattern between Productive hours and Sales?
#     - Intention: Close to causality: interpretable coefficient and prediction

# 2) What are the possible problems with the data - data quality
#   - Representative sample?
#       - This sample is not a representative sample, it is only one restaurant, so I can't compare
#         to the other one of the same brand because even before the covid, the sales and the tendency were very different
#         And the period is very special so as soon as regulations change, the environment will be different
#   - Measurement errors in the variables?
#       - The data is the official data so the measurement errors on the variable is very smal.
#
#  I will chose a significative level of 0.2 because it is enough for guidelines.
#
#
####
# Clear memory and call packages
rm(list=ls())
library(tidyverse)
library(lubridate)
#installed.packages("kableExtra")
library(knitr)
#install.packages("aTSA")
library(aTSA)
library(cowplot)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(geosphere)
library(moments)
library(dplyr)
library(pander)
library(jtools)
library(huxtable)


### Read the file
## Sales data
My_path <- "https://raw.githubusercontent.com/Maeva2408/DA2_assignment_2/main/Data/Raw/CA_cimdix_20oct_30nov.csv"
ca <- read_csv(paste0(My_path))

### Cleaning
##I will put the Downtime in hours
ca <- ca %>%  mutate(hours_of_downtime = Minutes_of_downtime/60)
ca$Minutes_of_downtime = NULL

##Reading time-format/converting to date_time
ca <- ca %>% mutate(Date = dmy(Date))

#The data is clean enough to start the analysis

### Data

## Countinous Data


# Sales summary
summary_sales <- summarise(ca,
                                    variable = "Sales",
                                    n= n(),
                                    mean = mean(x = ca$Sales),
                                    median = median(x = ca$Sales),
                                    min= min(ca$Sales),
                                    max = max(ca$Sales),
                                    sd = sd(ca$Sales),
                                    skew = skewness(ca$Sales))
# Product hours summary
summary_prod <- summarise(ca,
                           variable = "Productive_hours",
                           n= n(),
                           mean = mean(x = ca$Productive_hours),
                           median = median(x = ca$Productive_hours),
                           min= min(ca$Productive_hours),
                           max = max(ca$Productive_hours),
                           sd = sd(ca$Productive_hours),
                           skew = skewness(ca$Productive_hours))

# leadership hours summary
summary_lead <- summarise(ca,
                          variable = "Leadership_hours",
                          n= n(),
                          mean = mean(x = ca$Leadership_hours),
                          median = median(x = ca$Leadership_hours),
                          min= min(ca$Leadership_hours),
                          max = max(ca$Leadership_hours),
                          sd = sd(ca$Leadership_hours),
                          skew = skewness(ca$Leadership_hours))

# Hours of downtime summary
summary_down <- summarise(ca,
                          variable = "hours_of_downtime",
                          n= n(),
                          mean = mean(x = ca$hours_of_downtime),
                          median = median(x = ca$hours_of_downtime),
                          min= min(ca$hours_of_downtime),
                          max = max(ca$hours_of_downtime),
                          sd = sd(ca$hours_of_downtime),
                          skew = skewness(ca$hours_of_downtime))


table_summary <- add_row(summary_sales,summary_prod)
table_summary <- table_summary %>% rbind(summary_lead, summary_down)
kable(table_summary, caption = "Summary Statistic")

## I observe that for every variable the median and the mean are fairly close for each other. So the distribution
## is close to symmetric. I chose not to transform any of ma variable.
## statistical reason : the distribution is close to symmetric
## Substansive reason : this analysis is aim at helping manager, so it is essential to keep it the most simple as possible

## I suspect that it might be one or two extreme values but I will see better in the visualization

### Visualization of the data:

# NO 1: check the time-series in different graps
ca_aux <- gather(ca, key = measure, value = Rate, 
                 c("Sales", "Productive_hours", "Leadership_hours", "hours_of_downtime"))

ggplot( ca_aux, aes(x = Date, y = Rate , color = measure ) ) + 
  geom_line() +
  facet_wrap( ~ measure , scales = "free",
              labeller = labeller( measure = c("Sales"="Sales (euro)",
             "Productive_hours"="Total of productive hours","Leadership_hours"="Total hours of leadership",
             "hours_of_downtime" = "Total hours od downtime" ) ) ) +
  labs( x = "Date" , y = '') +
  guides(color = FALSE ) +
  scale_y_continuous()+
  theme_bw()

## No trend but maybe seasonality


# There is two extrem values, the 31/10/2020 there is a high total of hours of downtime and the 25/10/202
# I have a fery few of observations and we are in a special period so drop or control some
# variable will loose the sense this analysis so I chose to keep all my data but keep that in mind during the analysis

# NO 2: standardization - good to compare the (co)-movement

stdd <- function( x ){ ( x - mean( x , rm.na = T ) ) / sd( x , na.rm = T ) }

ggplot( ca , aes( x = Date ) ) +
  geom_line( aes( y = stdd( Sales )    , color = "Sales" ) ) +
  geom_line( aes( y = stdd( Productive_hours ) , color = "Productive_hours" ) ) +
  geom_line( aes( y = stdd( hours_of_downtime )  , color = "hours_of_downtime" ) ) +
  geom_line( aes( y = stdd( Leadership_hours )    , color = "Leadership_hours") ) +
  scale_color_manual(name = "Variable",
                     values = c( "Sales" = "red", "Productive_hours" = "blue",
                                 "hours_of_downtime" = "orange", "Leadership_hours" = "green"),
                     labels = c("Sales" = "Sales (euro)", "Productive_hours"="Total of productive hours",
                                "hours_of_downtime"="Total hours of downtime", "Leadership_hours"="Total hours of leadership")) +
  labs(x="Years",y="Standardized values", title = "Visualization in standardize scale")


## no interpretation yet but good for visualization

### I will check if some variable are high correlated together
## Create a general function to check the pattern
chck_sp_ca <- function(x_var){
  ggplot( ca , aes(x = x_var, y = Sales)) +
    geom_point(color ='palevioletred3') +
    geom_smooth(method="loess" , formula = y ~ x, color = "lightgoldenrod2" )+
    labs(y = "Averaged Sales", title = "Visualization of the correlation between variables")+
    theme_bw()}


# Productive hours 
chck_sp_ca(ca$Productive_hours)
# Leadership hours
chck_sp_ca(ca$Leadership_hours)
# Hours of downtime
chck_sp_ca(ca$hours_of_downtime)

numeric_ca <- keep( ca , is.numeric )
ca_ct <- cor(numeric_ca , use = "complete.obs")

# Check for highly correlated values:
sum( ca_ct >= 0.8 & ca_ct != 1 ) / 2

## 0 : Good the variable are not correlated between each other

### I will do UNIT-ROOT TESTS to check the seasonality and trend

# Philips-Perron test for unit-root:
# Type 1: y_t = rho * y_t-1
# Type 2: y_t = alpha + rho * y_t-1
# Type 3: Delta y_t = alpha + delta * t + rho * Delta y_t-1 (Neglect this output!)
#   Reason to neglect: the power of this test is low (against e.g. seasonality)

## Sales
pp.test( ca$Sales , lag.short = F)

# interpretation : the p value for type 1 and 2 are higher than 0,05 therefore we can not reject the null, so there
# might be a trend or random walk

## Productive hours
pp.test( ca$Productive_hours, lag.short = F)
# interpretation : the p value for type 1 is higher than 0,05 therefore we can not reject the null, so there
# might be random walk

## Leadership hours
pp.test( ca$Leadership_hours , lag.short = F)
# P value is smaller than 0,05 so we can reject the null so there is no trend or random walk

## Hours of downtime
pp.test( ca$hours_of_downtime , lag.short = F)

# P value is smaller than 0,05 so we can reject the null that there is no trend or random walk


### I will try to visualize the ACF PACF Serial-correlation (a.k.a. Auto-correlation)

url_git <- "https://raw.githubusercontent.com/Maeva2408/DA2_assignment_2/main/"

source( paste0( url_git , "Code/ggplot.acorr.R" ) )

# ACF - gives the correlation between y_t and lags: Corr(y_t,y_t-lag)
# PACF - (Partial Autocorrelation Fnc)
#     shows the correlation between Corr(y_t,y_t-lag) | Corr( y_t , y_t-lag-1 )
#     thus what is the correlation between y_t and y_t-lag if 
#       I have controlled for the previous lags already!
#
# In both graph the dashed lines gives 95% CI for statistical value = 0
#   In ACF it means if bars within the line we have a White-Noise: Corr = 0

# Sales
ggplot.acorr( ca$Sales , lag.max = 24, ci= 0.95, 
              large.sample.size = F, horizontal = TRUE)

# productives hours
ggplot.acorr( ca$Productive_hours , lag.max = 24, ci= 0.95, 
              large.sample.size = F, horizontal = TRUE)

# Leadership
ggplot.acorr( ca$Leadership_hours , lag.max = 24, ci= 0.95, 
              large.sample.size = F, horizontal = TRUE)

# Hours of downtime
ggplot.acorr( ca$hours_of_downtime , lag.max = 24, ci= 0.95, 
              large.sample.size = F, horizontal = TRUE)


## In all of the graph I can see that nearly all the bars are inside the two lines.
## Only very few values are outside the lines (less that 5%), so the difference
## between a model with or without seasonality may not be significant and considering the
## few observation I have plus the fact that the interpretation are for the manager of the restaurant
## and need to be as simple as possible, I choose keep level and use my data as stationary.
## However, as soon as I add new observations to the data, it will be imperative to check again and use lags
## if necessary.


### Some scatter-plots:

## Sales and Leadership hours
ggplot( ca , aes( x = ca$Leadership_hours , y = ca$Sales ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='Total hours of Leadership (un-adjusted)',y='Sales (euro)', 
        title = "Scatterplot Sales - Leadership hours per day of the week") +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_text( aes(label = as.factor(ca$Day) ) , nudge_y = -0.15, size = 3 ) +
  theme_bw()

#  Low chances that leadership hours will have an impact on the sales, I will exclude this variable

## Sales and Productive hours
ggplot( ca , aes(x = ca$Productive_hours , y = ca$Sales ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='Total of Productive hours (un-adjusted)',y='Sales (euro) (%)', title = "Scatterplot Sales - productive hours") +
  scale_y_continuous()+
  scale_x_continuous()+
  theme_bw()

# I observe that there are two cutting points. I can see that before 57 hours there is a linear regression,
# There is a plateau betwwen 57 and 60.5 where the sales stay pretty much the same
# after 60.5 there is again a linear regression. I will try a piecewise linear spline regression

# Piecewise linear spline regression
cutoff <- c(2,57,60.5)
ggplot( data = ca, aes( x = ca$Productive_hours  , y = ca$Sales ) ) + 
  geom_point( color='palevioletred3') +
  theme_bw()+
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = 'lightgoldenrod2' )+
  labs( x='Total of Productive hours (un-adjusted)',y='Sales (euro) (%)', title = "PLS : Sales and Productive hours")

# Let's compare to Linear

ggplot( data = ca, aes( x = ca$Productive_hours  , y = ca$Sales ) ) + 
  geom_point( color='palevioletred3') +
  theme_bw()+
  geom_smooth( method = lm , color = 'lightgoldenrod2' )+
  labs( x='Total of Productive hours (un-adjusted)',y='Sales (euro) (%)', 
        title = "Linear regression : Sales and Productive hours")

# I observe that the graph of the Piecewise linear spline regression is a bit overfitted 
# However, thet linear one is also ok so for the simplicity of the interpretation I will choose linear model


## Sales and Downtime hours
ggplot( ca , aes( x = ca$hours_of_downtime , y = ca$Sales ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='Total of downtime hours (un-adjusted)',y=' Sales (euro)', 
        title = "Scatterplot : Sales and downtime hours per day of the week") +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_text( aes(label = as.factor(ca$Day) ) , nudge_y = -0.15, size = 3 ) +
  theme_bw()

# By knowing the process of shut down of the delivery platform and my personal experience,
# I can say that the relation between sales and downtime hours is mediated by the number of client during the day 
# and the time of the day, both variable that I did not include in my data.
# From example, on a busy Friday evening, the restaurant needs to close the delivery platform for a few hours while
# they deal with the amount of customers on site.
# The number of orders and the time of the downtime could be found so, for the next analysis, it would be useful to include
#them, but for now I exclude the downtime hours of the analysis because the result will not be pertinent.

## Considering all the observations, I will model only three of my variables with linear regression.

### Models

## reg1 : Linear : Productive hours + Weather + Day of the week
## reg2 : Linear : Productive hours + Weather 
## reg3 : Linear : Productive hours + day of the week
## reg4 : Linear : Day of the week and weather

reg1 <- lm_robust( ca$Sales ~ ca$Productive_hours + as.factor( ca$Weather )+ as.factor(ca$Day) , data = ca )
summary(reg1)
reg2 <- lm_robust( ca$Sales ~ ca$Productive_hours + as.factor(ca$Weather) , data = ca )
summary(reg2)
reg3 <- lm_robust( ca$Sales ~ as.factor(ca$Day)  + ca$Productive_hours, data = ca )
summary(reg3)
reg4 <- lm_robust( ca$Sales ~ as.factor(ca$Day)+ as.factor(ca$Weather) ,  data = ca )
summary(reg4)

#####
# Creating model summary with texreg
data_out <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_assignment_2/out/"
htmlreg( list(reg1 , reg2 , reg3, reg4),
         type = 'html',
         custom.model.names = c("All 3","Prod-weather", "Prod-Day",
                                "Day-Weather"),
         caption = "Modelling Sales analysis",
         file = paste0( data_out ,'model_comparison_sales.html'), include.ci = FALSE)

######
# Based on model comparison my chosen model is reg2 and reg4 
# Reg4   
# Substantive: - The information on the day related to the weather are valuable by looking at the
# weather and the day of the week, the manager will have an average idea of the sales.
#   Statistical: - simple model, easy to interpret
#                - R2 is the highest
# Reg2
# Substantive: - The productive hours is a variable that the manager has a direct control over
# So, its was important for me to give guidelines on this variable.
#  Statistical: - simple model, easy to interpret

#######################

### Predicted value and residuals

#Model 1
ca$reg2_y_pred <- reg2$fitted.values
# Model 2
ca$reg4_y_pred <- reg4$fitted.values

# I Calculate the errors of the models
ca$reg2_res <- ca$Sales - ca$reg2_y_pred

ca$reg4_res <- ca$Sales - ca$reg4_y_pred

# Find date with largest negative errors
ca_res_neg2 <- ca %>% top_n( -5 , reg2_res ) %>% 
  select( Date , Sales , reg2_y_pred , reg2_res )

ca_res_neg4 <- ca %>% top_n( -5 , reg4_res ) %>% 
  select( Date , Sales , reg4_y_pred , reg4_res )

# Find date with largest positive errors
ca_res_pos2 <- ca %>% top_n( 5 , reg2_res ) %>% 
  select( Date , Sales , reg2_y_pred , reg2_res )

ca_res_pos4 <- ca %>% top_n( 5 , reg4_res ) %>% 
  select( Date , Sales , reg4_y_pred , reg4_res )


kable(ca_res_neg2,caption = "Modele 1 : List of the 5 sales with the largest negative errors")
kable(ca_res_neg4, caption = "Modele 2 : List of the 5 sales with the largest negative errors")
kable(ca_res_pos2, caption = "Modele 1 : List of the 5 sales with the largest positive errors")
kable(ca_res_pos4, caption = "Modele 2 : List of the 5 sales with the largest positive errors")
# Create: y_hat-y plot
## Modele 1
ggplot( data = ca ) +
  geom_point (aes( x = reg2_y_pred , y = Sales ) ,  color="palevioletred3")+
  geom_line( aes( x = Sales , y = Sales ) , color = "lightgoldenrod2" , size = 1.5 )+
  labs( x = "Predicted sales (euro)", y = "Actual Sales (euro)", 
        title = "Model 1 : Scatterplot : Actual value - Predicted value")

## Model 2
# Create: y_hat-y plot
ggplot( data = ca ) +
  geom_point (aes( x = reg4_y_pred , y = Sales ) ,  color="palevioletred3")+
  geom_line( aes( x = Sales , y = Sales ) , color = "lightgoldenrod2" , size = 1.5 )+
  labs( x = "Predicted sales (euro)", y = "Actual Sales (euro)", 
        title = "Model 2 : Scatterplot : Actual value - Predicted value")


## BIC and AIC


reg2_lm <- lm( ca$Sales ~ ca$Productive_hours + as.factor(ca$Weather) , data = ca )
summary(reg2)
reg4_lm <- lm(ca$Sales ~ as.factor(ca$Day)+ as.factor(ca$Weather) ,  data = ca )

BIC(reg2_lm,reg4_lm)
AIC(reg2_lm,reg4_lm)

## I can observe that in the BIC comparaison, the model 1 is smaller so better, but in the AIC comparaison, the modele 
## is smaller so better. However the difference are sigificatly small.


#################################
## Prediction uncertainty
#

# CI of predicted value/regression line is implemented in ggplot

# You can get them by predict function
#   interval can be any of c("none", "confidence", "prediction")
#   alpha = 0.05 (default) is the significance level
###
# CI of regression line
# Model 1
pred2_CI <- predict( reg2, newdata = ca , interval ="confidence" , alpha = 0.2 )
pred2_CI
# Model 2
pred4_CI <- predict( reg4, newdata = ca , interval ="confidence" , alpha = 0.2 )
pred4_CI

# Hand made CI for regression line
# 1) Add to datatset:
ca <- ca %>% mutate( CI_reg2_lower = pred2_CI$fit[,2],
                     CI_reg2_upper = pred2_CI$fit[,3] )
ca <- ca %>% mutate( CI_reg4_lower = pred4_CI$fit[,2],
                     CI_reg4_upper = pred4_CI$fit[,3] )
# 2) Plot

ggplot(  ) + 
  geom_point( data = ca, aes( x = Date, y = Sales ) , color='blue') +
  geom_line( data = ca, aes( x = Date, y = reg2_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = ca, aes( x = Date, y = CI_reg2_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = ca, aes( x = Date, y = CI_reg2_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "Dates",y = "Sales (euro)") 


ggplot(  ) + 
  geom_point( data = ca, aes( x = Date, y = Sales ) , color='blue') +
  geom_line( data = ca, aes( x = Date, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = ca, aes( x = Date, y = CI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = ca, aes( x = Date, y = CI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "Dates",y = "Sales (euro)") 

## From my personal experience, a shift could be very difficult if the projection of sales was off by 500 (euro).
## Between -500 and 500 some solutionS are provided : during the shift when there is less client than expected,
## we let people go home early, and deduct the work hours. If there are more clients than expected, employees 
## know they are likely to work a bit longer. The step I would do is investigate those residuals and see if 
##there were any external problems, if not,I will wait to gather more data before giving those guidelines to the manager.