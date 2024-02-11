# Importing the Data Set
mortality <- read.csv("F://salford study material//mortality analysis_2.csv")
mortality_cvd <- read.csv("F://salford study material//CVD.csv")


#Finding mean,meadian,mode,sd
mean(mortality$traffic_injury)
mean(mortality$CVD)
mean(mortality$unintentional_poisoning)
mean(mortality$Suicide_mortality_rate)


median(mortality$traffic_injury)
median(mortality$CVD)
median(mortality$unintentional_poisoning)
median(mortality$Suicide_mortality_rate)

#mode is occurrence of value
mode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode(mortality$traffic_injury)
mode(mortality$CVD)
mode(mortality$unintentional_poisoning)
mode(mortality$Suicide_mortality_rate)

sd(mortality$traffic_injury)
sd(mortality$CVD)
sd(mortality$unintentional_poisoning)
sd(mortality$Suicide_mortality_rate)



install.packages("moments") # for skewness n kurtosis
library(moments)

skewness(mortality$traffic_injury)
skewness(mortality$CVD)
skewness(mortality$unintentional_poisoning)
skewness(mortality$Suicide_mortality_rate)

kurtosis(mortality$traffic_injury)
kurtosis(mortality$CVD)
kurtosis(mortality$unintentional_poisoning)
kurtosis(mortality$Suicide_mortality_rate)


install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("psych")

library(PerformanceAnalytics)
library(corrplot)
library(psych)


#Correlation Between Two Variables
cor(mortality$total_mortality, mortality$traffic_injury)# pearson # quantitative


#correlation between all variable

mortality_corr <- cor(mortality[, c(3,4,5,6,7,8,9,10)])

# Displaying Correlation
head(mortality_corr)

#  Correlation Matrix 1
corrplot(mortality_corr, 
         method = "pie",
         type = "upper")

#  Correlation Matrix 2
corrplot(mortality_corr, 
         method = "number",
         type = "upper")


#regression

model_1 <-lm(total_mortality ~ traffic_injury, mortality)
summary.lm(model_1)

# Single Linear Regression
plot(total_mortality ~ traffic_injury, mortality, col = "blue", main = "Regression: Total mortality & Traffic injury", xlab = "Total mortality", ylab = "Traffic injury")

abline(model_1, col="red")
plot(model_1, 1)
plot(model_1, 2)
plot(model_1, 3)

# Multiple Linear Regression

model_2 <-lm(total_mortality ~ traffic_injury + unintentional_poisoning + Suicide_mortality_rate + Inflation_GDP_deflator + CVD , mortality)
summary.lm(model_2)

pairs(mortality[,c(7,3,5,6)], lower.panel = NULL, pch = 19, cex = 0.2)

plot(model_2, 1)
plot(model_2, 2)
plot(model_2, 3)





#time series analysis

install.packages("TTR")
install.packages("forecast")

library("TTR")
library("forecast")

#traffic injury analysis


mortalTS <- read.csv("F://salford study material//bd_data_1.csv")

mortalseries <- ts(mortalTS,start=c(2000))
plot.ts(mortalseries)



mortalseries_forecasts <- HoltWinters(mortalseries, gamma=FALSE)
mortalseries_forecasts 
plot(mortalseries_forecasts)



HoltWinters(mortalseries, gamma=FALSE, l.start=11, b.start=7)
mortalseries_forecasts2 <- forecast(mortalseries_forecasts, h=15)
plot(mortalseries_forecasts2)

acf(mortalseries_forecasts2$residuals, lag.max=15 , na.action = na.pass)
Box.test(mortalseries_forecasts2$residuals, lag=15, type="Ljung-Box")


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

mortalseries_forecasts2$residuals <-mortalseries_forecasts2$residuals[!is.na(mortalseries_forecasts2$residuals)]
plotForecastErrors(mortalseries_forecasts2$residuals)

#suicide rate analysis
mortalTS <- read.csv("F://salford study material//suicide.csv")

mortalseries <- ts(mortalTS,start=c(2000))
plot.ts(mortalseries)



mortalseries_forecasts <- HoltWinters(mortalseries, gamma=FALSE)
mortalseries_forecasts 
plot(mortalseries_forecasts)



HoltWinters(mortalseries, gamma=FALSE, l.start=6, b.start=2)
mortalseries_forecasts2 <- forecast(mortalseries_forecasts, h=15)
plot(mortalseries_forecasts2)

acf(mortalseries_forecasts2$residuals, lag.max=15 , na.action = na.pass)
Box.test(mortalseries_forecasts2$residuals, lag=15, type="Ljung-Box")




mortalseries_forecasts2$residuals <-mortalseries_forecasts2$residuals[!is.na(mortalseries_forecasts2$residuals)]
plotForecastErrors(mortalseries_forecasts2$residuals)

#hypothesis testing
# one sample t-test

# Plot the distribution   
hist(mortality$CVD)

t.test(mortality$CVD, mu=17, alternative="less")

mean(mortality_cvd$CVD)
t.test(mortality_cvd$CVD, mu=20, alternative="greater")



#paired sample test
t.test(mortality$traffic_injury , mortality$Suicide_mortality_rate, paired=TRUE)

