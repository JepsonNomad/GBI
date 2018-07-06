library(RCurl)
library(ggplot2)
library(forecast)

# Download GL Blocking Index data
myfile <- getURL("https://www.esrl.noaa.gov/psd/data/correlation/gbi.ncep.day")
myfile
# Format it
GBI <- read.csv(textConnection(myfile), header=T, sep=" ")
head(GBI)
str(GBI)
names(GBI) <- c("Year","Month","Day","GBI")
GBI$GBI <- as.numeric(levels(GBI$GBI))[GBI$GBI]
GBI$GBI
# Add a formatted date layer
GBI$Date <- as.POSIXct(paste(GBI$Year,
                             paste(stringr::str_pad(GBI$Month, pad="0",width=2),
                                   stringr::str_pad(GBI$Day, pad="0",width=2),
                                   sep="-"),
                             sep="-"), 
                       format="%Y-%m-%d")
GBI$Date
# Remove commented rows which don't produce a meaningful date
GBI <- GBI[!is.na(GBI$Date),] 
# Aggregate by year
GBI.year <- aggregate(GBI, by=list(GBI$Year), FUN = mean, na.rm=T)
GBI.year <- GBI.year[,c("GBI","Date")]
GBI.year
str(GBI.year)
# Aggregate by month-year
GBI$YrMo <- paste(GBI$Year,
                  stringr::str_pad(GBI$Month, pad="0",width=2), 
                  sep="-")
GBI.yrmo <- aggregate(GBI, by=list(GBI$YrMo), FUN = mean, na.rm=T)
GBI.yrmo <- GBI.yrmo[,c("Date","GBI")]
str(GBI.yrmo)
# Look at GBI during months leading up to green-up
GBI.sprng <- GBI[as.numeric(levels(GBI$Month)[GBI$Month]) < 7,]
GBI.sprng
GBI.annsprngs <- aggregate(GBI.sprng,
                           by=list(GBI.sprng$Year),
                           FUN = mean,
                           na.rm = T)
GBI.annsprngs <- GBI.annsprngs[,c("GBI","Date")]
# Plot the results
# Here, daily, monthly, and yearly GBI values
ggplot() +
  geom_line(data=GBI,aes(x=Date,y=GBI), 
            colour="black") +
  geom_line(data=GBI.moyr, aes(x=Date, y = GBI), 
            colour="red3", lwd=0.5) +
  geom_line(data=GBI.year, aes(x=Date, y = GBI), 
            colour="blue3", lwd=2) +
  geom_line(data=GBI.annsprngs, aes(x=Date, y = GBI), 
            colour="green3", lwd=2)

## Time series analysis
# Based on Ryan Womack's time series youtube videos
# https://www.youtube.com/watch?v=QHsmAM6nktY
GBI.ts <- ts(GBI$GBI,
             start = c(as.numeric(format(min(GBI$Date), "%Y")),
                       as.numeric(format(min(GBI$Date), "%j"))),
             end = c(as.numeric(format(max(GBI$Date), "%Y")), 
                     as.numeric(format(max(GBI$Date), "%j"))),
             frequency = 365)
class(GBI.ts)
time(GBI.ts) # decimal representation of timepoint of each observation
start(GBI.ts) # check start day
end(GBI.ts) # check end day
frequency(GBI.ts) # check frequency
plot(GBI.ts) # look at the data
cycle(GBI.ts)

# Aggregate across years to look at non-seasonal trends
GBI.ann.ts <- aggregate(GBI.ts, 
                        FUN = mean)
plot(GBI.ann.ts)

# Look at seasonal variation
boxplot(GBI.ts ~ cycle(GBI.ts),
        xlab="Day of year")
# GBI tends to be highest in the late summer

# Slice time out of the GBI data - this is less meaningful for daily data but could be interesting with monthly data
GBI.window <- window(GBI.ts, start = c(1948,196), frequency=365)
plot(GBI.window)

# Decompose the timeseries
GBI.decomp <- decompose(GBI.ts)
plot(GBI.decomp)
GBI.decomp

# Examine autocorrelation:
acf(GBI.ts)

# Look at the partial autocorrelation plot to examine the correlation of n>1 periods after accounting for the effect of the periods [0,n-1]
pacf(GBI.ts)

## Forecasting
# Holt-Winters fit
GBI.hw <- HoltWinters(GBI.ts, alpha = 0.05)
plot(GBI.hw)
summary(GBI.hw)
GBI.predict <- predict(GBI.hw, n.ahead=7200)
ts.plot(GBI.ts, GBI.predict, col=c("blue","red"))
# Doesn't look very realistic. Captured seasonality but predicts diminishing GBI. Let's try the tbats method:

# TBATS forecasting
GBI.ts.tbats <- tbats(GBI.ts)
plot(GBI.ts.tbats)
GBI.ts.tbats
# Forecast the data for ~5 years ahead
GBI.ts.fore <- forecast(GBI.ts.tbats, h=7200)
plot(GBI.ts.fore)
GBI.ts.fore
# This one looks a bit more realistic

diff(GBI.ts)
acf(diff(GBI.ts)) # Test for randomness: 
# If random walk: Anything besides lag-0 is unrelated to x

## AR Model
# Autoregression only:
GBI.ar <- ar(GBI.ts, method="mle") # Convergence issues
GBI.ar
acf(GBI.ar$res[-c(1:13)])
# ACF of residuals does not have a strong pattern, so we can conclude that the autoregressive model explains a great deal of the seasonal variation.
GBI.ar

summary(lm(GBI$GBI ~ GBI$Date))
acf(resid(lm(GBI$GBI ~ GBI$Date)))
# The ar model explained away a lot more variation than a simple linear model of GBI vs time

# Moving averages only:
GBI.ma <- arima(GBI.ts, order=c(0,0,1))
plot(GBI.ma)
GBI.ma
# Autoregression AND moving average
GBI.arma <- arima(GBI.ts, order=c(1,0,1))
plot(GBI.arma)
GBI.arma
# ARIMA model - autoregression, integrated, AND moving average:
GBI.arima <- arima(GBI.ts, order=c(2,1,2))
plot(GBI.arima)
GBI.arima

GBI.predict <- predict(GBI.arima, n.ahead = 7200)
ts.plot(GBI.ts, GBI.predict$pred, col=c("blue","red"))
# This prediction seems to deal with the trend well but not seasonality

AIC(GBI.ma)
AIC(GBI.arma)
AIC(GBI.arima) # The arima model is most parsimonious

# For more info on time series analysis with R, check out the Task Views
# https://cran.r-project.org/web/views/TimeSeries.html