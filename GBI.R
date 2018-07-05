library(RCurl)
library(ggplot2)

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
GBI.year
str(GBI.year)
# Aggregate by month-year
GBI$YrMo <- paste(GBI$Year,
                  stringr::str_pad(GBI$Month, pad="0",width=2), 
                  sep="-")
GBI.yrmo <- aggregate(GBI, by=list(GBI$YrMo), FUN = mean, na.rm=T)
str(GBI.yrmo)

# Plot the results
# Here, daily, monthly, and yearly GBI values
ggplot() +
  geom_line(data=GBI,aes(x=Date,y=GBI), 
            colour="black") +
  geom_line(data=GBI.moyr, aes(x=Date, y = GBI), 
            colour="red", lwd=0.5) +
  geom_line(data=GBI.year, aes(x=Date, y = GBI), 
            colour="red", lwd=2)
