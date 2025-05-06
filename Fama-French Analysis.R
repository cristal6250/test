## necessary libraries
library(readxl)        # for reading Excel files
library(conflicted)    # to resolve function conflicts
library(tidyverse)     # includes dplyr, ggplot2, etc.
library(readr)
library(quantmod)      # for financial data and charting
library(janitor)       # for cleaning column names
library(ggthemes)      # for nice plot themes
library(broom)         # for tidying model outputs
library(vtable)        # for summary statistics
library(xts)           # for time series objects
library(lubridate)     # for working with dates

#### conflict preferences for tidyverse
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")




# Load and clean data
alphabet_data <- read_excel("C:/Users/cr   stal/Desktop/FAMA FRENCH/BloombergDataCris.xlsx")
alphabet_data <- janitor::clean_names(alphabet_data)   # all lowercase and underscores

# Fix date format
library(lubridate) # for easy date manipulation (e.g., ymd(), month(), etc.)
alphabet_data$date <- ymd(alphabet_data$date)
library(tidyverse)
lubridate::ymd(alphabet_data$date)




# Create return and excess return columns (two new variables)
alphabet_data$abc_return <- alphabet_data$chg_pct_1d
alphabet_data$abc_excess_return <- alphabet_data$abc_return - alphabet_data$rf

## im just gonna delete them for rn
alphabet_data$abc_return <- NULL
alphabet_data$abc_excess_return <- NULL
## need them to move forward tho



# View descriptive statistics
library(vtable)
st(alphabet_data)
## brings up a summary stat of my data

# Convert to xts for time-series plotting (new vars go into globe environ)
abc_returns_xts <- xts(alphabet_data$abc_return, order.by = alphabet_data$date)
abc_excess_returns_xts <- xts(alphabet_data$abc_excess_return, order.by = alphabet_data$date)


# Plot returns
library(quantmod) ##to use chart_series
chart_Series(abc_returns_xts, name = "Alphabet Daily Returns")
chart_Series(abc_excess_returns_xts, name = "Alphabet Excess Returns")


# Run regressions
## CAPM Model: ABC Excess Return = B0 + market excess return + u
CAPM <- lm(abc_excess_return ~ mkt_rf, data = alphabet_data)
summary(CAPM)

## Fama-French 3-Factor Model: ABC Excess Return = B0 + market excess return +
## Small Minus Big + High Minus Low + u
FF3 <- lm(abc_excess_return ~ mkt_rf + smb + hml, data = alphabet_data)
summary(FF3)

## Fama-French 5-Factor Model:  ABC Excess Return = B0 + market excess return +
## Small Minus Big + High Minus Low + Robust Minus Weak 
## + Conservative Minus Aggressive + u
FF5 <- lm(abc_excess_return ~ mkt_rf + smb + hml + rmw + cma, data = alphabet_data)
summary(FF5)

# Optional: remove variables (if needed later)
# alphabet_data$abc_return <- NULL
# alphabet_data$abc_excess_return <- NULL


### now we have our regressions
library(stargazer)
??stargazer
## to create clean, readable summary tables of regression models
stargazer(FF3, FF5, title = "Fama-French Three & Five Factor Regression OLS", type = "text")
## populates table in console

plot(FF5, which = 1:6) ## 7 is so cool lol and u can keep going up



#Test multiple linear hypothesis for Three factor model vs. 5 factor model

## creates restricted models
library(carData)
library(car) #need to do hypothesis test
linearHypothesis(FF5, c("smb=0", "hml=0", "rmw=0", "cma=0"))
linearHypothesis(FF5, c("rmw=0", "cma=0"))


library(olsrr)
#MultiCollinearity Diagnostics
ols_vif_tol(FF5)


 #Breusch Pagan test is used to test for herteroskedasticity (non-constant error variance)
ols_test_breusch_pagan(FF5)


library(nortest)
ad.test(FF5$residuals)
qqnorm(FF5$residuals)

library(usethis)
use_git_config(user.name = "cristal6250", user.email = "cristal6250@gmail.com")
use_git()


