
####### #######  #######  BA-BHAAI1098U ####### #######  

# Student ID: 165658
# Student name and surname: Diego Fonti
# Student e-mail: difo23ac@student.cbs.dk

#######  ####### #######  ####### #######  ####### ####


### PART 1: Part 1 – Data description and preparation ###

rm(list=ls())

## install packages 
require(Quandl);require(forecast);require(lmtest); library(broom)
require(texreg);require(stargazer);require(urca)
## visualize data from FRED
setwd("~/Desktop/CBS/Forecasting/DATA/Final exam")
csvdata<-data.frame(read.csv("MRTSSM448USN.csv"))
data <- csvdata$MRTSSM448USN
plot(ts(data),main=" Retail Sales for Clothing and Clothing Accessory Stores ")

## Data cleaning and forecasting horizon
data<- head(data, -4)# removing last 4 period, from January to April 2023
T <- length(data) #set up the length of your data
s <- 12 #frequency of the data - monthly
years <- T/s # Check for an integer number of years

startyear <- 1992
startperiod <- 1
fstart <- c(2023,1) # start year of the forecats
h <- 12 # forecast 1 years ahead
y <- log(data) # log transform to smoothe seasonality 

## Transform to data in TS 
y <- ts(y, start=c(startyear,startperiod),frequency=s)#Transform to data in TS
data <- ts(data, start=c(startyear,startperiod),frequency=s) 
plot(y, main=" Retail Sales for Clothing and Clothing Accessory Stores ")

#-----------------End of Part 1------------------
#------------------------------------------------

### PART 2: Modelling Deterministic trend, deterministic seasonality and cyclical component ###

## Deterministic trend 

t <- (1:T)
t2 <- t^2

model_q <- lm(y~t+t2) #run a quadratic regression
model_e <- lm(y~t) #run a linear regression

# summary(model_q)
# summary(model_e)
screenreg(list(model_e,model_q))
# Based on R^2 results,the best model is the quadratic one.

e_trend <- y-model_e$residuals
q_trend <- y-model_q$residuals

# e_trend <- exp(e_trend) #Exponentiate the forecast if used log-transformation
# q_trend <- exp(q_trend) ##### DA DECIDERE

plot(y,xlim=c(startyear,2028),ylim=c(min(y),max(y)),main="Deterministic model comparison")
lines(e_trend,col="green")
lines(q_trend,col="blue")
text(2026, e_trend[372], "Exponential", col="green")
text(2026, q_trend[372], "Quadratic", col="blue")

#Compute the growth rate
growth <- exp(model_e$coefficients[2])
#Thus, the growth rate is:
growth <- (growth-1)*s*100
growth #per year


# DW Test

library("lmtest")
#We consider testing:
#H0: The error terms are not autocorrelated.
dwtest(model_e)
dwtest(model_q)

# Both models have DW statistics less than 2, indicating the presence of positive 
# autocorrelation in their residuals. Additionally, the p-values for both models 
# are very small, showing strong evidence against the assumption of 
# no autocorrelation. In both cases, the null hypothesis of no autocorrelation 
# is rejected in favor of the alternative hypothesis that there is 
# autocorrelation in the residuals.

#------------------------------------------------
## TREND+SEASONALITY 
#Create a seasonal dummy M1,M2,...,M11
M <- seasonaldummy(y) #December is the omitted season
modeETS <- tslm(y ~ trend + M)
#summary(modeETS)
screenreg(list(model_e,modeETS))

fitTS<-y-modeETS$residuals
plot(y,main="Deterministic Trend and Seasonality")
lines(fitTS,col="green")

dwtest(modeETS) 
# DW=0.61 suggest that there is positive autocorrelation in the residuals
# the p-value<2.2e-16 is extremely below the 0.05 of significance, 
# this means that we can reject the null hipotesys of of no autocorrelation, 
# and accept the alternative HP. In summary, based on the given results, 
# there is strong evidence of positive autocorrelation in the residuals of the regression model.

trend<-y-modeETS$residuals
residuals<-y-trend

plot(residuals, ylim = c(min(residuals), 3 * max(residuals)), main = 'Residuals')
abline(h = 0, col = 'green')
mtext("Residuals", side = 4, line = 2.5, at = 0)

# plot residuals, PACF and ACF together to explore autocorrelation. 
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
# First plot for ACF
acf(residuals)
# Second plot for PACF
pacf(residuals)

dev.off()

#------------------------------------------------
## Cyclical component: MA, AR, ARMA model

################## MA model ################

model.ma<-arima(y,order=c(0,0,12))
plot(model.ma$residuals)
acf(model.ma$residuals)  # signs of autocorrelations

################## AR models ################

model.ar3<-arima(y,order=c(3,0,0)) 
plot(model.ar3$residuals)
acf(model.ar3$residuals) # signs of autocorrelations

model.ar5<-arima(y,order=c(6,0,0)) 
plot(model.ar5$residuals)
acf(model.ar5$residuals) # signs of autocorrelations

################### ARMA models #################

require(forecast)
model.arma <- auto.arima(y, d=0, seasonal = FALSE, ic = "aic", 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)

# AR (5) is the best model

TrSeasE <- model.matrix(~ t+M)

model.ar5 <- Arima(y,order = c(5, 0, 0),include.mean = FALSE, xreg=TrSeasE)
model.ar6 <- Arima(y,order = c(6, 0, 0),include.mean = FALSE, xreg=TrSeasE)
model.ar3 <- Arima(y,order = c(3, 0, 0),include.mean = FALSE, xreg=TrSeasE)
model.ma12 <- Arima(y,order = c(0, 0, 12),include.mean = FALSE, xreg=TrSeasE)
screenreg(list(modeETS,model.ar3,model.ar5,model.ma12,model.ar6))

# Let's extract only the relevant information to choose between the models
# Load the required packages
library(broom)
# Function to extract model summary
extract_model_summary <- function(model, label) {
  model_summary <- data.frame(
    Model = label,
    AIC = AIC(model),
    BIC = BIC(model)
  )
  return(model_summary)
}


# Extract the model summaries with labels
model_summary_ar6 <- extract_model_summary(model.ar6, "AR(6,0,0)")
model_summary_ar5 <- extract_model_summary(model.ar5, "AR(5,0,0)")
model_summary_ar3 <- extract_model_summary(model.ar3, "AR(3,0,0)")
model_summary_ma12 <- extract_model_summary(model.ma12, "MA(0,0,12)") 

# Combine the model summaries into a single data frame
model_summaries <- rbind(model_summary_ar6,model_summary_ar5, model_summary_ar3, model_summary_ma12)

# Print the combined model summaries 
print(model_summaries) 

# From the analysis, even if AIC are similar, we prefer to stick to smaller
# models AR (3), AR (6) and AR (5). Let's see the residuals.

par(mfrow = c(3, 1)) # Create a 1x2 grid for two plots side by side
acf(model.ar3$residuals)
acf(model.ar5$residuals)
acf(model.ar6$residuals)
dev.off()

# All models show no correlation between the residuals, se let's test them
# for in-sample forecast and see how they differentiate.

## In-sample forecast
# I take 60 data points out of sample, which I will forecast later.
h <- s*5
test.set <- subset(data, end=length(data)-h); z <- subset(data, start=length(data)-h+1)

T <- length(test.set) #set up the length of your data
t <- (1:T)
t2 <- t^2
M <- seasonaldummy(test.set)

TrSeasL <- model.matrix(~ t+M)
Tr.model.ar3L <- Arima(test.set,order = c(3,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.ar3L

Tr.model.ar5L <- Arima(test.set,order = c(5,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.ar5L

Tr.model.ar6L <- Arima(test.set,order = c(6,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.ar6L

# Compute the in-sample forecast accuracy measures 
t <- ((T+1):(T+h))
M <- seasonaldummy(z)

TrSeasL <- model.matrix(~ t+M)
forecast.ar3L<-forecast(Tr.model.ar3L, h=h, xreg=TrSeasL)
plot(forecast.ar3L,ylab="",main="AR(3) with linear trend")
lines(data)

forecast.ar5L<-forecast(Tr.model.ar5L, h=h, xreg=TrSeasL)
plot(forecast.ar5L,ylab="",main="AR(5) with linear trend")
lines(data)


forecast.ar6L<-forecast(Tr.model.ar6L, h=h, xreg=TrSeasL)
plot(forecast.ar6L,ylab="",main="AR(6) with linear trend")
lines(data)

accuracy(forecast.ar3L,z)
accuracy(forecast.ar5L,z)
accuracy(forecast.ar6L,z)

# the AR (6) and AR (5) models perform better on the in-sample test. The ME, MAE and 
# RMSE are  better compared to AR (3) model. By parsimony principle,
# we choose so as a final model we will choose AR (5), with linear trend.

#-----------------End of Part 2------------------
#------------------------------------------------

### PART 3: Modelling stochastic trend and deterministic seasonality 

# In order to better visualize our assumption, since the seasonal pattern 
# In the data is very rigid, I will de-seasonalise de data. 

# Remove seasonality:
endperiod <- startperiod-1; if(endperiod==0){endperiod <- s};
detrended <- y-ma(y,order = s) 
detrended <- window(detrended, start=c(startyear+1,startperiod), end=c(startyear+years-2,endperiod)) 
plot(detrended)
seasonal.matrix <- matrix(detrended,ncol=s,byrow=years-2) 
seasonalFactor <- apply(seasonal.matrix,2,mean) 
seasonality <- rep(seasonalFactor,years)
y <- y-seasonality
plot(y,main= "De-seasonalised data")

# let's try to see if the have a stochastic trend and deterministic seasonality
plot(diff(y), main= "First differece")

# First differences look roughly stationary and but they don't seem white noise
# Let's inspect ACF and PACF of the series and its differences:
# 2 figures arranged in 2 rows and 1 column
par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(y)
pacf(y)
# The sample ACF seems like it fail to dump
# The first partial autocorrelation is not close to 1 and shows significant spikes

acf(diff(y))
pacf(diff(y))
# There are significant differences in first order ACF and PACF.

#Perform a formal test on Unit roots: Dickey-Fuller Test
require(urca)
test <- ur.df(y,type=c("trend"),lags=10,selectlags="AIC")
summary(test) 
dev.off()
# Reject HP of stochastic trend

# fit ARIMA model
# Model the series in first differences,assuming unit root presence
model.arima <- auto.arima(y, d=1, seasonal = FALSE, ic = "aic", 
                          stepwise = FALSE, approximation = FALSE, 
                          trace = TRUE)
model.arima # Good AIC value
e <- model.arima$residuals

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(e); pacf(e)
dev.off()

# No sign of autocorrelation in the residuals

#-----------------End of Part 3------------------
#------------------------------------------------

### PART 4: Modelling Stochastic trend and stochastic seasonality ###

# Let's now assume that also seasonality is stochastic
x <- log(ts(data, start=c(startyear,startperiod),frequency=s)) #use original data
# not adjusted for seasonality

model.sarima <- auto.arima(x, d=1, seasonal = TRUE, ic = "aic", 
                           stepwise = FALSE, approximation = FALSE, 
                           trace = TRUE)
model.sarima # Good AIC value

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(model.sarima$residuals); pacf(model.sarima$residuals)
dev.off() # No sign of autocorrelation in the residuals

#-----------------End of Part 4------------------
#------------------------------------------------

### PART 5: Smoothing techniques ###

# In this case data is large enough for exponential smoothing.
# in addition to this, the forecast horizon of 12 months is not voluminous 
# and smoothing technique might lead do a  good result

# In this case, simple exponential smoothing is not the best model since the mean
# of the series change over time. Most recent growth in this case is more informative
# than past growth. In other words, they are not equally important.
# In this case, since the data is trending and shows a constant additive seasonal variation, 
# Holt's-Winter method is more appropriate.

sales.fit <- HoltWinters(x)
sales.fit
plot(sales.fit)

# We can see from the picture that the model agrees pretty well 
# with the observed values.

#-----------------End of Part 5------------------
#------------------------------------------------

### PART 6: Comparing the models and testing ###

# Let's compare each model and the relative AIC 

screenreg(list(model.ar5,model.arima,model.sarima))

# In sample testing
#----------------------------------------------
# In this code we explore stochastic trend vs deterministic trend, 
# for that we will use data cleaned from the seasonality (y) to focus on trends.
# Then we explore stochastic seasonality vs deterministic seasonality,
# for that we will work with the original seasonal data. (x)
#----------------------------------------------
h <- s*5
yy <- subset(y, end=length(y)-h); zz <- subset(y, start=length(y)-h+1)

T <- length(yy) #set up the length of your data
t <- (1:T)
M <- seasonaldummy(yy)
TrSeasL <- model.matrix(~ t+M)
Tr.model.ar5 <- Arima(yy,  order = c(5,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.arimaDS <- Arima(yy,order = c(3,1,1),include.mean = TRUE, xreg=M)

t <- ((T+1):(T+h))
M <- seasonaldummy(zz)
TrSeasL <- model.matrix(~ t+M)

Tr.forecast.ar5<-forecast(Tr.model.ar5, h=h, xreg=TrSeasL)
Tr.forecast.arimaDS<-forecast(Tr.model.arimaDS, h=h, xreg=M)

accuracy(Tr.forecast.ar5,zz)
accuracy(Tr.forecast.arimaDS,zz)

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
plot(Tr.forecast.ar5,xlim=c(startyear+years-1,startyear+years+1));lines(y)
plot(Tr.forecast.arimaDS,xlim=c(startyear+years-1,startyear+years+1));lines(y)
dev.off()

par(mfrow=c(2,1), mar=c(3,2,2,3)) 
plot(Tr.forecast.ar5,main="Forecast based on AR(5,0) with deterministic seasonality and trend"); lines(y)
plot(Tr.forecast.arimaDS,main="Forecast based on ARIMA(3,1,3) with deterministic seasonality"); lines(y)
dev.off()

#------------ stochastic seasonality vs deterministic seasonality--------------
h <- s*5
yy <- subset(x, end=length(x)-h); zz <- subset(x, start=length(x)-h+1)

T <- length(yy) #set up the length of your data
t <- (1:T)
M <- seasonaldummy(yy)
TrSeasL <- model.matrix(~ t+M)
Tr.model.ar5 <- Arima(yy,  order = c(5,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.sarima <- Arima(yy,order = c(3, 1, 1),seasonal = c(0, 1, 1))

t <- ((T+1):(T+h))
M <- seasonaldummy(zz)
TrSeasL <- model.matrix(~ t+M)

Tr.forecast.ar5<-forecast(Tr.model.ar5, h=h, xreg=TrSeasL)
Tr.forecast.sarima<-forecast(Tr.model.sarima, h=h)
HW.model <- HoltWinters(yy)
HW.forecast <- forecast(HW.model, h=h,level=95)

accuracy(Tr.forecast.ar5,zz)
accuracy(Tr.forecast.sarima,zz)
accuracy(HW.forecast,zz)

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
plot(Tr.forecast.ar5,xlim=c(startyear+years-1,startyear+years+1));lines(y)
plot(HW.forecast,xlim=c(startyear+years-1,startyear+years+1));lines(y)
dev.off()

par(mfrow=c(3,1), mar=c(3,2,2,3)) 
plot(Tr.forecast.ar5,main="Forecast based on AR(5,0) with deterministic seasonality and trend"); lines(x)
plot(Tr.forecast.sarima,main="Forecast based on SARIMA(3,1,1)(0,1,1)"); lines(x)
plot(HW.forecast);lines(x)
dev.off()

par(mfrow=c(2,1), mar=c(3,2,2,3)) 
plot(Tr.forecast.sarima,main="Forecast based on SARIMA(3,1,1)(0,1,1)"); lines(x)
plot(HW.forecast);lines(x)
dev.off()

#-----------------End of Part 6------------------
#------------------------------------------------

### PART 7: Model forecast. ###

#Let's make a forecast
salesforecast <- forecast(sales.fit, h=12,level=95)
salesforecast
plot(salesforecast)

plot(residuals(sales.fit))
forecast.point <- exp(salesforecast$mean)
forecast.point

plot(data, ylim = c(min(data), 1.2 * max(data)), main="Holt-Winters model reforecast")
lines(forecast.point, col="blue", lwd= 2)







