# GBI

## Intent
This is a short block of R code to download and investigate the Greenland Blocking Index (GBI). Data is extracted from https://www.esrl.noaa.gov/psd/data/correlation/gbi.ncep.day. The GBI is a measure of high-pressure blocking expressed as meters above sea level, reflecting the mean geopotential height of 500 hPa over the region bounded by 60-80 deg N and 20-80 deg W. For further information see Hanna et al 2016 Int J Climatology.

## Dependencies
Code uses RCurl and ggplot2 to to download and visualise the data.

## Returns
Returns daily, monthly, and annual GBI values as individual data.frames and visualizes all three in one plot.