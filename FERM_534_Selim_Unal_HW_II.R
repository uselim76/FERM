# Load necessary libraries
library(quantmod)
library(ggplot2)
library(fpp2)
library(urca)
library(fUnitRoots)
library(lmtest)
library(tsoutliers)
library(tseries)

# Get data and omit NA values
getSymbols("EURTRY=X", from="2016-06-30", to="2020-06-30", periodicity="weekly")
EURTRY=Cl(`EURTRY=X`)
EURTRY<-na.omit(log(EURTRY))
rEURTRY<-na.omit(diff(EURTRY))

check <- na.omit(log(as.numeric(`EURTRY=X`[,6])))
checkd <- na.omit(diff(check))

# Part I - Preliminary Analysis of Data
# Plot data
autoplot(EURTRY) + xlab("Year") + ylab("EURTRY Rate in Log Scale") +
  ggtitle("EUR TRY Rate in Logarithmic Scale")

autoplot(rEURTRY) + xlab("Year") + ylab("Changes in EURTRY Rate in Log Scale") +
  ggtitle("Differenced Returns on EUR TRY Rate in Logarithmic Scale")

# EURTRY has clearly a trend is non-stationary,
# on the other hand rEUTRY seems stationary from the graph

# Let's check auto-correlations  and partial auto-correlations
# of each series

ggtsdisplay(EURTRY)
# EURTRY is highly auto-correlated and we have partial auto-correlation at lag 1
# This implies EURTRY is AR(1) process. We have to apply ADF test
# to check whether it is a random walk or not. 

ggtsdisplay(rEURTRY)
# rEURTRY does not have any spikes except for lags 3 and 12 in both
# auto-correlation and partial auto-correlation.

# Part II
# Let's check ADF test results of both series
adf.test(EURTRY)
# As p value is high above 5% threshold,
# we cannot reject the null hypothesis that
# data is non-stationary and that data fits 
# into a random walk model. This shows that
# differencing of data will help eliminate auto-correlation.

adf.test(rEURTRY)
# As p value is way below 5% threshold,
# we can reject the null hypothesis that
# data is non-stationary and that data fits 
# into a random walk model

# Part III
# Let's find the model with ar function
ar(as.numeric(rEURTRY),method='mle')

# ar function suggests an AR(3) model on rEURTRY
# Let's create an AR(3) model on rEURTRY
m1=arima(as.numeric(rEURTRY),order=c(3,0,0))
# Let's check the significance level of coefficients
m1$coef/sqrt(diag(m1$var.coef)) 

# Part IV
# we see that second coefficient of the model m1 is
# insignificant. (I take level above 1.5 to be significant)
# Let's create a model without it.

m2=arima(as.numeric(rEURTRY),order=c(3,0,0), fixed=c(NA,0,NA,NA))
m2
# new model first coefficient is 0.1054, third coefficient is 0.1743
# and intercept is 0.0042

# Part V
# Check Box-Ljung test 
# since we eliminate one coefficient dof is 2
Box.test(m2$residuals,lag=10,type='Ljung', fitdf=2) 

# p-value of the test is high above 5% threshold
# therefore we cannot reject null hypothesis that there is no linear dependence 
# in the series up to the given lag.  
# Hence, we presume auto-correlation has been eliminated in the series by m2 model

# Part VI
# we know from the first part lag 3 and 12 are 
# significant in auto-correlation of rEURTRY
# Let's create a model of MA(12)
m3=arima(as.numeric(rEURTRY),order=c(0,0,12))

# Part VII

# Let's check the significance level of coefficients
m3$coef/sqrt(diag(m3$var.coef)) 

# we see that only coefficients 3,11 and 12 are significant
# (I take level above 1.5 to be significant)
# Let's create a model with them.

m4=arima(as.numeric(rEURTRY),order=c(0,0,12), fixed=c(0,0,NA,0,0,0,0,0,0,0,NA,NA,NA))
m4

# Part VIII
# Check Box-Ljung test 
# since we eliminate 9 coefficients dof is 3
Box.test(m4$residuals,lag=10,type='Ljung', fitdf=3) 

# Again, p-value of the test is high above 5% threshold
# therefore we cannot reject null hypothesis that there is no linear dependence 
# in the series up to the given lag.  
# Hence, we presume auto-correlation has been eliminated in the series by m4 model

# Part IX
# Selection of model

# Based on the AIC scores, m4 model is better
# as its score is lower than m2 model score.

