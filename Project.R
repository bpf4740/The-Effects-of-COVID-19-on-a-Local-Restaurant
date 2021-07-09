getwd()
library(car)
library(forecast)
library(lubridate)
library(orcutt)

future <- read.csv('Future Dates.csv')
future$Date <- as.Date(future$Date, '%m/%d/%Y')
all <- read.csv('All Dates.csv')
all$Date <- as.Date(all$Date, '%m/%d/%Y')
project <- read.csv('Project.csv')
project$Date <- as.Date(project$Date, '%m/%d/%Y')
str(project)

plot(project$Date, project$NumberofPeople, type = 'l', xaxt = 'n',
     xlab = 'Date', ylab = 'Number of People Served Per Day', 
     main = 'Tracking the Effect of the Coronavirus
     On the Number of People Being Served Per Day
     In a Local Restaurant')
axis(1, project$Date, format(project$Date, "%b %d"), cex.axis = .7)
abline(v=as.Date("2020-03-13"), col = 'red')


#Potential Dummy Variable 
lm1 <- lm(NumberofPeople~0+Date+dummy, data=project)
summary(lm1)

par(mfrow = c(2,2))
hist(lm1$residuals, xlab = 'Residuals', main = "Histogram of Residuals")
plot(lm1$residuals, ylab = 'residuals', main = 'Fitted Values')
abline(h=0)
qqnorm(lm1$residuals)
qqline(lm1$residuals)
plot(lm1$residuals, type = 'l', ylab = 'Residuals', main = 'Observation Order')
abline(h=0)
par(mfrow = c(1,1))

dwt(lm1, alternative = 'positive')
acf(lm1$residuals)

#Potential Holts Method

plot(diff(project$NumberofPeople, lag = 1), type = 'l')
subholt <- HoltWinters(subts, beta = F, gamma = F)
subholt
holtpred <- predict(subholt, n.ahead = 30, prediction.interval = T)
holtpred

#Potential ARMA Model 

par(mfrow=c(2,1))
acf(project$NumberofPeople, main = 'ACF of Number of People Served')
pacf(project$NumberofPeople, main='PACF of Number of People Served')
par(mfrow=c(1,1))

AR11 <- arima(project$NumberofPeople, order = c(1,0,1))
AR11

AR <- auto.arima(project$NumberofPeople, max.P = 3, max.q = 3, max.d = 0)
AR

par(mfrow = c(2,2))
hist(AR11$residuals, xlab = 'Residuals', main = "Histogram of Residuals")
plot(AR11$residuals, ylab = 'residuals', main = 'Fitted Values')
abline(h=0)
qqnorm(AR11$residuals)
qqline(AR11$residuals)
plot(AR11$residuals, type = 'l', ylab = 'Residuals', main = 'Observation Order')
abline(h=0)
par(mfrow = c(1,1))

subforecast <- forecast(AR11, h=57, level = 95)
subforecast

subforecast2 <- cbind(future, subforecast)

plot(project$Date, project$NumberofPeople, type = 'l', xaxt = 'n',
     xlab = 'Date', ylab = 'Number of People Served Per Day', 
     main = 'Tracking the Effect of the Coronavirus
     On the Number of People Being Served Per Day
     In a Local Restaurant',
     xlim = as.Date(c("2020-01-20", "2020-05-31")),
     ylim = c(16,120))
axis(1, all$Date, format(all$Date, "%b %d"), cex.axis = .7)
abline(v=as.Date("2020-03-13"), col = 'red')
lines(subforecast2$Date,subforecast2$`Point Forecast`, col = 'red')
lines(subforecast2$Date, subforecast2$`Lo 95`, col = 'red', lty = 'dashed')
lines(subforecast2$Date, subforecast2$`Hi 95`, col = 'red', lty = 'dashed')

plot(acf(AR11$residuals), main = 'ACF of ARMA(1,1) Residuals')
