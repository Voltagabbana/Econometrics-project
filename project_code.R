# libraries

library(foreign) 
library(xts)
library(ggplot2)
library(pastecs)
require(psych)
library(MASS)
library(stargazer)
library(BatchGetSymbols)
library(RColorBrewer)
library(tseries)
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(car)
library(nlWaldTest)
library(forecast)
library(ARDL)
library(corrplot)

###############################################################################





###############################################################################

load("/Users/pippomonti99/Desktop/Documents/INSURANCE & ECONOMETRICS/project/indicisistemati.RData")

# we selected a start and a finish date in order to have the same 
# number of observations (they are already included in the dataset provided)

first.date <- "2008-06-03"  
last.date <- "2022-04-25"

###############################################################################

# using the command xts we create our time series and then we plot them 

par(mfrow=c(2,1))

oil <- xts(CL_Fnew[,2],CL_Fnew[,1])
plot(oil, col = "green", main = "Oil prices")

oil_vol <- xts(OVX[,2],OVX[,1])
plot(oil_vol, main = "Oil Volatility index (OVX)", col = "blue")

par(mfrow = c(2,1))

gold <- xts(GC_Fnew[,2],GC_Fnew[,1])
plot(gold, col = "orange", main = "Gold prices")

gold_vol <- xts(GVZ[,2],GVZ[,1])
plot(gold_vol, col = "purple", main = "Gold Volatility index (GVZ)")

par(mfrow = c(1,1))

SP <- xts(GSPC[,2],GSPC[,1])
plot(SP , col = "red", main = "Market index - S&P 500")

# general stats regarding our time series

dati = cbind(CL_Fnew[,2], GC_Fnew[,2], GSPC[,2], GVZ[,2], OVX[,2])
colnames(dati) <- c("oil", "gold", "SP", "gold_vol", "oil_vol")
head(dati)
S <- round(stat.desc(dati),2)
S

# removing some statistics

S <- S[-2,]   # to remove number of null observations
S <- S[-6,]   # to remove total sum
S <- S[-12,]  # to remove coef. var
S <- S[-9,]   # to remove CI.mean.0.95
S <- S[-8,]   # to remove SE.mean
S

# correlation analysis

pairs(dati)

par(mfrow=c(1,1))

cor(dati)
M <-cor(dati)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

################################################################################

##### VARIABLES ANALYSIS: are they stationary? #####
# Using augmented Dickey-Fuller test we check the stationarity of our time series

## GOLD ##

par(mfrow = c(3,1))

adf.test(gold)  # HIGH P-VALUE (0.6948) ----> NOT stationary

gold_log = log(gold)  # trying with logarithm transformation
plot(gold_log, main = "Log(Gold)", col = "orange")
adf.test(gold_log) # HIGH P-VALUE AGAIN (0.5993) ----> NOT stationary 

d_gold = diff(gold) # trying again with the differences
plot(d_gold, main = "Gold differences", col = "orange") 
adf.test(na.omit(d_gold)) # low p-value --> stationary

d_gold_log = diff(gold_log)
plot(d_gold_log, main = "Log(Gold) differences", col = "orange")
adf.test(na.omit(d_gold_log))  # as before, low p-value ---> stationary


# GOLD VOLATILITY (GVZ) ##

par(mfrow = c(1,1))

adf.test(gold_vol) # P-VALUE = 0.011 ---> STATIONARY

gold_vol_log = log(gold_vol)  # we tried with logarithm as well
plot(gold_vol_log, main = "Log(Gold_volatility)", col = "purple")
adf.test(gold_vol_log)  # P-VALUE is still very low ---> Series is STATIONARY


## OIL ##

par(mfrow = c(3,1))

adf.test(oil)  # P-VALUE = 0.3471 ---> NOT STATIONARY

# On 20/04/2020 oil price was negative (row 2990)
# to avoid problem with negative values in logarithms we decided to change it 
# by interpolating, using prices from the day before and the day after

oil[2990] = 14.14

oil_log = log(oil)  # applying log transformation
plot(oil_log, main = "Log(Oil)", col = "green")
adf.test(na.omit(oil_log)) # P-value still high (0.2756)  ---> NOT stationary

d_oil = diff(oil)  # differences
plot(na.omit(d_oil), main = "Oil price differences", col = "green")
adf.test(na.omit(d_oil)) # low p-value ---> series becomes stationary

d_oil_log = diff(oil_log)
plot(d_oil_log, main = "Log(Oil) differences", col = "green")
adf.test(na.omit(d_oil_log)) # as expected p-value remains very low --> still stationary


## OIL VOLATILITY (OVX) ##

par(mfrow = c(1,1))

adf.test(oil_vol) # P-value < 0.01  ----> STATIONARY SERIES

oil_vol_log = log(oil_vol) 
plot(oil_vol_log, main = "Log(Oil volatility)", col = "blue")
adf.test(oil_vol_log) # P-value remains low ---> SERIES REMAINS STATIONARY


## S&P 500 ##
par(mfrow = c(3,1))

adf.test(SP)  # P-VALUE = 0.2976 ---> NO STATIONARITY

SP_log = log(SP)
plot(SP_log, main = "Log(S&P500)", col = "red")
adf.test(SP_log) # P-value becomes very low --> STATIONARY

d_SP = diff(SP)
plot(d_SP, main = "S&P500 differences", col = "red")
adf.test(na.omit(d_SP)) # still stationary

d_SP_log <- diff(SP_log)
plot(d_SP_log, main = "Log(S&P500) differences", col = "red")
adf.test(na.omit(d_SP_log)) # again, still stationary

################################################################################

## PLOT OF THE STATIONARY SERIES WE USED IN THE FINAL MODEL ##

layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,3,3,3,3,3,0,4,4,4,4,4,0,0,0,5,5,5,5,5,0,0,0),3,11,byrow = TRUE))

plot(d_gold_log, main = "Log(Gold) differences", col = "orange")
plot(gold_vol_log, main = "Log(Gold_volatility)", col = "purple")
plot(d_oil_log, main = "Log(Oil) differences", col = "green")
plot(oil_vol_log, main = "Log(Oil volatility)", col = "blue")
plot(d_SP_log, main = "Log(S&P500) differences", col = "red")

################################################################################

### TO CREATE THE MODEL WE NEED TO USE THE COMMAND "ts" 

oil <- ts(CL_Fnew[,2],CL_Fnew[,1])
gold <- ts(GC_Fnew[,2],GC_Fnew[,1])
oil_vol <- ts(OVX[,2],OVX[,1])
gold_vol <- ts(GVZ[,2],GVZ[,1])
SP <- ts(GSPC[,2],GSPC[,1])

oil[2990] = 14.14  # updating the negative value again for the same reason as before

# checking that all the series have the same dimension
length(oil)
length(gold)
length(oil_vol)
length(gold_vol)
length(SP)


d_gold = diff(gold)
gold_log = log(gold)
d_gold_log = diff(gold_log)

oil_log = log(oil)
d_oil = diff(oil)
d_oil_log = diff(oil_log)

oil_vol_log = log(oil_vol)

gold_vol_log = log(gold_vol)

SP_log = log(SP)
d_SP = diff(SP)
d_SP_log = diff(SP_log)

###############################################################################

# Since SP,oil and gold "original" series are NOT stationary we use the series with 
# differences instead

# Due to that, we remove the first observation from the volatility series in order 
# to have the same dimension


oil_vol_log <- oil_vol_log[-1] 
gold_vol_log <- gold_vol_log[-1]

oil_vol <- oil_vol[-1]
gold_vol <- gold_vol[-1]



################################################################################

## FIRST MODEL ##
# Simplest model possible, using all stationary series 
# Response: Log(S&P500)
# Regressors: oil and gold log differences,Log(oil volatility) and Log(gold volatility)
# to do so, we need to update SP_log dimensionality
SP_log <- SP_log[-1]

fit <- lm(SP_log  ~ d_oil_log + d_gold_log + oil_vol_log + gold_vol_log)
summary(fit)

# R^2 is quite good but both d_oil_log and d_gold_log are not significative

dwtest(fit) # --> autocorrelation

bptest(fit) # there is heteroskedasticity


## SECOND MODEL ##
# We try to improve the first model by using the differences of the last response
# Response: differences of Log(S&P500)
# Covariates: Log(Oil) and Log(Gold) differences, Log(OVX) and Log(GVZ)

fit_log <- lm(d_SP_log  ~ d_oil_log + d_gold_log + oil_vol_log + gold_vol_log)
summary(fit_log)

# R^2 = 0.037 (really low) and only 2 variables are significative

dwtest(fit_log) # -> There is no autocorrelation!

bptest(fit_log) # Low p-value --> Heteroskedasticity


## THIRD MODEL ##
# We now try to use an ARDL (AutoRegressive Distributed Lag) model
# To do so, we used the function "auto_ardl" to get the best model possibile
# with the regressors lagged 5 times at most.

dataset <- cbind(SP_log, d_oil_log, d_gold_log, oil_vol_log, gold_vol_log)


best = auto_ardl(SP_log ~ d_oil_log + d_gold_log + oil_vol_log + gold_vol_log, data = dataset,
                 max_order=c(1,5,5,5,5))

best$top_orders  # to see the lag orders of the best models

summary(best$best_model) # --> R^2 really high ---> overfitting 
# and also oil_vol_log at t-2 is not significative

dwtest(best$best_model) # high p-value ---> autocorrelation problems solved

bptest(best$best_model) # Heteroskedasticity is still there

modellosololagy=dynlm(SP_log~L(SP_log,1))

summary(modellosololagy)

## FOURTH AND FINAL MODEL ##

# We tried ARDL also with the differences of Log(S&P500) as a response 
# to see if we get better results

dataset2 <- cbind(d_SP_log, d_oil_log, d_gold_log, oil_vol_log, gold_vol_log)

## first of all, we use corrplot to look at the correlation between our series

par(mfrow = c(1,1))
cor(dataset2)
M_2 <-cor(dataset2)
corrplot(M_2, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


best_diff = auto_ardl(d_SP_log ~ d_oil_log + d_gold_log + oil_vol_log + gold_vol_log, data = dataset2,
                 max_order=c(1,3,3,3,3))


best_diff$top_orders  # best orders

summary(best_diff$best_model)  # R^2 = 0.21, all variables are significative


dwtest(best_diff$best_model)  # p-value = 0.6 --> no autocorrelation!

bptest(best_diff$best_model) # heteroscedasticity again

# We plot some residuals

plot(best_diff$best_model$residuals , col = "purple", main = "Residuals plot") 

plot(best_diff$best_model$fitted.values , best_diff$best_model$residuals, col = "purple", 
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")

# Even from the Residuals vs Fitted plot we note that we still have heteroscedasticity














