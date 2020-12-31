########################
##                    ##
##                    ##
##                    ##
##                    ##
##                    ##
########################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)
library(lubridate)
#installed.packages("kableExtra")
library(knitr)
#install.packages('fastDummies')
library('fastDummies')
#install.packages("aTSA")
library(aTSA)
install.packages("cowplot")
library(cowplot)
library(moments)


# Read the raw files
# Sales data
my_path_ca <- "https://raw.githubusercontent.com/Maeva2408/DA2_assignment_2/main/Data/Raw/CA_cimdix_20oct_30nov.csv"
ca <- read_csv(paste0(my_path_ca))

### Cleaning
#I will put the Downtime in hours
ca <- ca %>%  mutate(hours_of_downtime = Minutes_of_downtime/60)
ca$Minutes_of_downtime = NULL
### Reading time-format/converting to date_time
ca <- ca %>% mutate(Date = dmy(Date))
### Putting the day in number
ca <- ca %>% mutate( Day= wday(Date))

#The data is clean enough to start the analysis

## Summary
summary <- summary(ca)
kable(summary)

summary_sales <- summarise(ca,
                                    variable = "Sales",
                                    n= n(),
                                    mean = mean(x = ca$Sales),
                                    median = median(x = ca$Sales),
                                    min= min(ca$Sales),
                                    max = max(ca$Sales),
                                    sd = sd(ca$Sales),
                                    skew = skewness(ca$Sales))
summary_prod <- summarise(ca,
                           variable = "Productive_hours",
                           n= n(),
                           mean = mean(x = ca$Productive_hours),
                           median = median(x = ca$Productive_hours),
                           min= min(ca$Productive_hours),
                           max = max(ca$Productive_hours),
                           sd = sd(ca$Productive_hours),
                           skew = skewness(ca$Productive_hours))

summary_lead <- summarise(ca,
                          variable = "Leadership_hours",
                          n= n(),
                          mean = mean(x = ca$Leadership_hours),
                          median = median(x = ca$Leadership_hours),
                          min= min(ca$Leadership_hours),
                          max = max(ca$Leadership_hours),
                          sd = sd(ca$Leadership_hours),
                          skew = skewness(ca$Leadership_hours))

table_summary <- add_row(summary_sales,summary_prod)
table_summary <- table_summary %>% rbind(summary_lead)
kable(table_summary)

ggplot( ca , aes( x = ca$Date , y = ca$Sales ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='GDP percent changes (un-adjusted)',y='Inflation changes (%)') +
  geom_text( aes(label = ca$Day ) , nudge_y = -0.15, size = 4 ) +
  theme_bw()

# Visualization of the data:

# NO 1: check the time-series in different graps
ca_aux <- gather(ca, key = measure, value = Rate, 
                 c("Sales", "Productive_hours", "Leadership_hours"))

ggplot( ca_aux, aes(x = Date, y = Rate , color = measure ) ) + 
  geom_line() +
  facet_wrap( ~ measure , scales = "free",
              labeller = labeller( measure = c("Sales"="Sales (???)",
             "Productive_hours"="Total of productive hours","Leadership_hours"="Total hours of leadership") ) ) +
  labs( x = "Date" , y = '' ) +
  guides(color = FALSE ) +
  scale_y_continuous()+
  theme_bw()

# NO 2: standardization - good to compare the (co)-movement

stdd <- function( x ){ ( x - mean( x , rm.na = T ) ) / sd( x , na.rm = T ) }

ggplot( ca , aes( x = Date ) ) +
  geom_line( aes( y = stdd( ca$Leadership_hours ) , color = "Leadership_hours" ) ) +
  geom_line( aes( y = stdd( ca$Sales )  , color = "Sales" ) ) +
  scale_color_manual(name = "Variable",
                     values = c("Leadership_hours" = "midnightblue",
                                 "Sales" = "goldenrod2"),
                     labels = c("Leadership_hours"="Total hours of leadership",
                                "Sales"="Sales (???)")) +
  labs(x="days",y="Standardized values")+
  scale_y_continuous()+
  theme_bw()


stdd <- function( x ){ ( x - mean( x , rm.na = T ) ) / sd( x , na.rm = T ) }

ggplot( ca , aes( x = Date ) ) +
  geom_line( aes( y = stdd( ca$Productive_hours )    , color = "Productive_hours" ) ) +
  geom_line( aes( y = stdd( ca$Leadership_hours ) , color = "Leadership_hours" ) ) +
  geom_line( aes( y = stdd( ca$Sales )  , color = "Sales" ) ) +
  scale_color_manual(name = "Variable",
                     values = c( "Productive_hours" = "palevioletred3", "Leadership_hours" = "midnightblue",
                                 "Sales" = "goldenrod2"),
                     labels = c("Productive_hours" = "Total of productives hours", "Leadership_hours"="Total hours of leadership",
                                "Sales"="Sales (???)")) +
  labs(x="days",y="Standardized values")+
  scale_y_continuous()+
  theme_bw()

## not interpretable but good for visualization


# UNIT-ROOT TESTS

# Philips-Perron test for unit-root:
# Type 1: y_t = rho * y_t-1
# Type 2: y_t = alpha + rho * y_t-1
# Type 3: Delta y_t = alpha + delta * t + rho * Delta y_t-1 (Neglect this output!)
#   Reason to neglect: the power of this test is low (agianst e.g. seasonality)
pp.test( ca$Sales , lag.short = F)

# interpretation : the p value for type 1 and 2 are higher than 0,05 therefore we can not reject the null, so there
# is a trend or random walk

pp.test( ca$Productive_hours, lag.short = F)
# interpretation : the p value for type 1 is higher than 0,05 therefore we can not reject the null, so there
# is random walk
pp.test( ca$Leadership_hours , lag.short = F)
# P value is smaller than 0,05 so we can reject the null that there is a trend or random walk

pp.test( ca$hours_of_downtime , lag.short = F)

pp.test( ca$Day , lag.short = F)

# P value is smaller than 0,05 so we can reject the null that there is a trend or random walk

# Detour: Serial-correlation (a.k.a. Auto-correlation)
source( paste0( my_path_ca , 'codes/ggplot.acorr.R' ) )

# ACF - gives the correlation between y_t and lags: Corr(y_t,y_t-lag)
# PACF - (Partial Autocorrelation Fnc)
#     shows the correlation between Corr(y_t,y_t-lag) | Corr( y_t , y_t-lag-1 )
#     thus what is the correlation between y_t and y_t-lag if 
#       we have controlled for the previous lags already!
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


# we are good to continue

# Some scatter-plots:

# Sales and Leadership hours
ggplot( ca , aes( x = ca$Sales , y = ca$Leadership_hours ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='GDP percent changes (un-adjusted)',y='Inflation changes (%)') +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_text( aes(label = as.factor(ca$Day) ) , nudge_y = -0.15, size = 4 ) +
  theme_bw()

# Sales and Productive hours
ggplot( ca , aes( x = ca$Sales , y = ca$Productive_hours ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='GDP percent changes (un-adjusted)',y='Inflation changes (%)') +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_text( aes(label = as.factor(ca$Weather) ) , nudge_y = -0.15, size = 4 ) +
  theme_bw()


# Sales and downtime hours
ggplot( ca , aes( x = ca$Sales , y = ca$hours_of_downtime ) ) +
  geom_point( color = 'palevioletred3' ) +
  geom_smooth(method="loess", color="lightgoldenrod2",se=F) +
  labs( x='GDP percent changes (un-adjusted)',y='Inflation changes (%)') +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_text( aes(label = as.factor(ca$Weather) ) , nudge_y = -0.15, size = 4 ) +
  theme_bw()


# reg1: d_inflat ~ d_unemp + dp_eur + dp_gdp + seasonal dummies
reg1 <- lm( ca$Sales ~ ca$Productive_hours + ca$Leadership_hours + ca$hours_of_downtime + as.factor( ca$Weather )+ ca$Day , data = ca )
summary(reg1)
reg1b <- coeftest(reg1, vcov.=NeweyWest( reg1 , prewhite=FALSE, lag=0, verbose=TRUE)) 
reg2 <- lm( ca$Sales ~ ca$Productive_hours + ca$Leadership_hours + ca$hours_of_downtime + ca$Day , data = ca )
reg3 <- lm( ca$Sales ~ ca$Productive_hours + ca$Leadership_hours + ca$hours_of_downtime, data = ca )
summary(reg1b)
summary(reg2)
summary(reg3)
reg4 <- lm( ca$Sales ~ ca$Productive_hours + ca$Leadership_hours, data = ca )
summary(reg4)
reg5 <- lm( ca$Sales ~ ca$Productive_hours, data = ca )
summary(reg5)
reg6 <- lm( ca$Sales ~ ca$Leadership_hours, data = ca )
summary(reg6)
reg7 <- lm( ca$Sales ~ ca$Day, data = ca )
summary(reg7)
reg8 <- lm( ca$Sales ~ ca$hours_of_downtime, data = ca )
summary(reg8)
reg9 <- lm( ca$Sales ~ ca$Weather, data = ca )
reg10 <- lm(ca$Sales ~ ca$Weather + ca$Productive_hours, data = ca )
summary(10)