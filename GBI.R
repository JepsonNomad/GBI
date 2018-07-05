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

# Time series analysis
GBI.ts <- ts(GBI$GBI,
             start = as.numeric(format(min(GBI$Date), "%Y")),
             end = as.numeric(format(max(GBI$Date), "%Y")),
             frequency = 365)
GBI.ts
acf(GBI.ts)
# Forecasting
GBI.ts.tbats <- tbats(GBI.ts)
plot(x.ts.tbats)
x.ts.tbats
# Forecast the data for ~5 years ahead
x.ts.fore <- forecast(x.ts.tbats, h=7200)
plot(x.ts.fore)
x.ts.fore


