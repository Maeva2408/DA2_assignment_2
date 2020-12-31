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

#The data is clean enough to start the analysis

## Summary
summary <- summary(ca)
kable(summary)



# Visualization of the data:

# NO 1: check the time-series in different graps
ca_aux <- gather(ca, key = measure, value = Rate, 
                 c("Sales","hours_of_downtime", "Productive_hours", "Leadership_hours"))
 
ggplot( ca_aux, aes(x = Date, y = Rate , color = measure ) ) + 
  geom_line() +
  facet_wrap( ~ measure , scales = "free") +
  labs( x = "Date" , y = '' ) +
  guides(color = FALSE ) +
  scale_y_continuous()+
  theme_bw()

# NO 2: standardization - good to compare the (co)-movement

stdd <- function( x ){ ( x - mean( x , rm.na = T ) ) / sd( x , na.rm = T ) }

ggplot( ca , aes( x = Date ) ) +
  geom_line( aes( y = stdd( ca$Productive_hours )    , color = "gdp" ) ) +
  geom_line( aes( y = stdd( ca$Leadership_hours ) , color = "inflat" ) ) +
  geom_line( aes( y = stdd( ca$hours_of_downtime )  , color = "unemp" ) ) +
  geom_line( aes( y = stdd( ca$Sales )  , color = "unemp" ) ) +
  scale_color_manual(name = "Variable",
                     values = c( "gdp" = "red", "inflat" = "blue",
                                 "unemp" = "orange", "EUR" = "green"),
                     labels = c("gdp" = "GDP", "inflat"="Inflation",
                                "unemp"="Unemployment", "EUR"="EUR")) +
  labs(x="Years",y="Standardized values")

## not interpretable but good for visualization

# "hours_of_downtime"
ggplot( ca , aes(x = hours_of_downtime )) +
  geom_histogram(binwidth = 0.3,fill='palevioletred3', col= "black") +
  labs(x = "hours of downtime")+
  theme_bw()

# Leadership hours
ggplot( ca , aes(x = ca$Leadership_hours )) +
  geom_histogram(binwidth = 0.5,fill='palevioletred3', col= "black") +
  labs(x = "hours of leadership")+
  theme_bw()

# Productives hours
ggplot( ca , aes(x = ca$Productive_hours )) +
  geom_histogram(binwidth = 2,fill='palevioletred3', col= "black") +
  labs(x = "hours of productivity")+
  theme_bw()

# weather
ggplot( ca , aes(x = ca$Sales, fill=Weather )) +
  geom_histogram(binwidth = 500, col= "black") +
  labs(x = "Sales")+
  scale_fill_brewer(palette = "PuRd")+
  theme_bw()

# Day of the week
ggplot( ca , aes(x = ca$Sales, fill=Day )) +
  geom_histogram(binwidth = 300, col= "black") +
  labs(x = "hours of productivity")+
  scale_fill_brewer(palette = "PuRd")+
  theme_bw()


library(sjmisc)
library(magrittr)


# Checking some scatter-plots:
# Create a general function to check the pattern
chck_sp_ca <- function(x_var){
  ggplot( ca , aes(x = x_var, y = Sales)) +
    geom_point(color ='palevioletred3') +
    geom_smooth(method="loess" , formula = y ~ x, color = "lightgoldenrod2" )+
    labs(y = "Averaged Sales")+
    theme_bw()
}

# Productive hours 
chck_sp_ca(ca$Productive_hours)
# Leadership hours
chck_sp_ca(ca$Leadership_hours)
# Hours of downtime
chck_sp_ca(ca$hours_of_downtime)
#Day of the week
chck_sp_ca(as.factor(ca$Day))
# Weather
chck_sp_ca(as.factor(ca$Weather) )

####
# 4) Comparing explanatory variables 
#
numeric_ca <- keep( ca , is.numeric )
ca_ct <- cor(numeric_ca , use = "complete.obs")

# Check for highly correlated values:
sum( ca_ct >= 0.8 & ca_ct != 1 ) / 2

## Good no multicorrenality

# UNIT-ROOT TESTS
install.packages("aTSA")
library(aTSA)
# Philips-Perron test for unit-root:
# Type 1: y_t = rho * y_t-1
# Type 2: y_t = alpha + rho * y_t-1
# Type 3: Delta y_t = alpha + delta * t + rho * Delta y_t-1 (Neglect this output!)
#   Reason to neglect: the power of this test is low (agianst e.g. seasonality)
pp.test( ca$Productive_hours, lag.short = F)
pp.test( ca$Leadership_hours , lag.short = F)
pp.test( ca$hours_of_downtime , lag.short = F)
pp.test( ca$Sales , lag.short = F)








