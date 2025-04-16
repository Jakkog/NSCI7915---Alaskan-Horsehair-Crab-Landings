#---------- Alaskan Horsehair Crab Landings Analysis - NOAA ----------#

# Lachlan Gerdes
# NSCI7915



#----- setwd -----#
setwd("C:/Users/ltger/OneDrive/Desktop/datasets/Kaggle/Alaskan_HorsehairCrabLandings")


#---------- Install Packages/ Load Libraries ----------#

install.packages("forecast")
install.packages("usethis")
library(forecast)
library(usethis)


#---------- Connect RScript to github repo ----------#

use_git(path = "C:/Users/ltger/OneDrive/Desktop/datasets/Kaggle/Alaskan_HorsehairCrabLandings")





#----- Import dataset -----#

crabData <- read.csv("Alaskan_HorsehairCrabLandings.csv")

#---- Remove tibble -----#

crabData <- as.data.frame(crabData)


# male female split

maleData <- crabData[crabData[,"maturity"]=="Total male",]
femaleData <- crabData[crabData[,"maturity"]=="Total female",]

#

plot(maleData[,"latitude"],maleData[,"haul"])
plot(maleData[,"longitude"],maleData[,"haul"])
hist(maleData[,"haul"])


# time and male data

plot(maleData[,"year"],maleData[,"haul"])

#depht vs catch
plot(maleData[,"bottom_depth"],maleData[,"haul"])


# surface temp vs haul
plot(maleData[,"surface_temperature"],maleData[,"haul"])



#bubble plot

plot(maleData[,"longitude"], maleData[,"latitude"],cex=log(maleData[,"haul"])/5,col=hsv(h=pnorm(scale(log(maleData[,"haul"])))))


# bivariate regression on lat long catch

summary(lm(log(maleData[,"haul"]) ~ maleData[,"latitude"] + maleData[,"longitude"]))
crabResid <- resid(lm(log(maleData[,"haul"]) ~ maleData[,"latitude"] + maleData[,"longitude"]))
plot(crabResid,maleData[,"year"])
hist(crabResid, breaks = 30) # not normal dist

plot(crabResid, maleData[,"cpue"], log="y") # not normal, not a consistant pattern

# need data in a form that time series can be done

#yearbycatch <- table(maleData[,"year"], log(maleData[,"haul"]))

#

avgCatchYear <- array()
for(i in unique(maleData[,"year"]))
    avgCatchYear[i] = mean(log(maleData[maleData[,"year"]==i,"haul"]))
    avgCatchYear = na.omit(avgCatchYear)
    
# time series plot
crabYears <- 1980:2018


plot(crabYears,avgCatchYear)

# testing autocorrelation -> allows you to compare each point in year with the next years point and then again for each consecutive year.

acf(avgCatchYear)

plot(1980:2017, diff(avgCatchYear))
abline(0,0)
acf(diff(avgCatchYear)) # negative correlation, increases followed by decreases

# need residulas of trend


auto.arima(avgCatchYear) # used to model future values

#

plot(resid(auto.arima(avgCatchYear)))
abline(0,0)

# can now cross correlate with each year for total catch etc


lotcddsto.arima(avgCatchYear))
