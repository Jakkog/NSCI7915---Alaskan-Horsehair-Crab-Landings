#---------- Alaskan Horsehair Crab Landings Analysis - NOAA ----------#

# Lachlan Gerdes
# NSCI7915



#----- setwd -----#

setwd("C:/Users/ltger/OneDrive/Desktop/datasets/Kaggle/Alaskan_HorsehairCrabLandings")


#---------- Install Packages/ Load Libraries ----------#

install.packages("forecast")
install.packages("usethis")
install.packages("dplyr")
install.packages("maps")
install.packages("mapproj")
install.packages('lmerTest')
library(lmerTest)
install.packages('MuMIn')
library(MuMIn)
library(forecast)
library(usethis)
library(dplyr)
library(maps)
library(mapproj)
library(lme4)




#----- Import dataset -----#

crabData <- read.csv("Alaskan_HorsehairCrabLandings.csv")

#---- Remove tibble -----#

crabData <- as.data.frame(crabData)

#---------- Splitting the dataset by sex ----------#

maleData <- crabData[crabData[,"maturity"]=="Total male",]
femaleData <- crabData[crabData[,"maturity"]=="Total female",]


#---------- Summary Statistics ----------#

summary(maleData[7:11])
summary(femaleData[7:11])

#---------- crabData byYear ----------#

byYear <- crabData %>%
  group_by(year) %>%
  summarise(
    haulMean <- mean(haul, na.rm = TRUE),
    haulSD <- sd(haul, na.rm = TRUE),
    cpueMean <- mean(cpue, na.rm = TRUE), 
    cpueSD <- sd(cpue, na.rm = TRUE),
    depthMean <- mean(bottom_depth, na.rm = TRUE),
    surfacetempMean <- mean(surface_temperature, na.rm = TRUE),
    bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)
  )

byYear

plot(byYear$year, byYear$`haulMean <- mean(haul, na.rm = TRUE)`)
plot(byYear$year, byYear$`haulSD <- sd(haul, na.rm = TRUE)`)
plot(byYear$year, byYear$`cpueMean <- mean(cpue, na.rm = TRUE)`) # 642 cpue = >40k
plot(byYear$year, byYear$`cpueSD <- sd(cpue, na.rm = TRUE)`, log = "y") # 642 cpue = >40k
plot(byYear$year, byYear$`depthMean <- mean(bottom_depth, na.rm = TRUE)`)
plot(byYear$year, byYear$`surfacetempMean <- mean(surface_temperature, na.rm = TRUE)`)
plot(byYear$year, byYear$`bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)`)


#---------- maleData byYear ----------#

malebyYear <- maleData %>%
  group_by(year) %>%
  summarise(
    m.haulMean <- mean(haul, na.rm = TRUE),
    m.haulSD <- sd(haul, na.rm = TRUE),
    m.cpueMean <- mean(cpue, na.rm = TRUE), 
    m.cpueSD <- sd(cpue, na.rm = TRUE),
    m.depthMean <- mean(bottom_depth, na.rm = TRUE),
    m.surfacetempMean <- mean(surface_temperature, na.rm = TRUE),
    m.bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)
  )

malebyYear

plot(malebyYear$year, malebyYear$`m.haulMean <- mean(haul, na.rm = TRUE)`)
plot(malebyYear$year, malebyYear$`m.haulSD <- sd(haul, na.rm = TRUE)`)
plot(malebyYear$year, malebyYear$`m.cpueMean <- mean(cpue, na.rm = TRUE)`) # 642 cpue = >40k
plot(malebyYear$year, malebyYear$`m.cpueSD <- sd(cpue, na.rm = TRUE)`) # 642 cpue = >40k
plot(malebyYear$year, malebyYear$`m.depthMean <- mean(bottom_depth, na.rm = TRUE)`)
plot(malebyYear$year, malebyYear$`m.surfacetempMean <- mean(surface_temperature, na.rm = TRUE)`)
plot(malebyYear$year, malebyYear$`m.bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)`)

#---------- femaleData byYear ----------#

femalebyYear <- femaleData %>%
  group_by(year) %>%
  summarise(
    f.haulMean <- mean(haul, na.rm = TRUE),
    f.haulSD <- sd(haul, na.rm = TRUE),
    f.cpueMean <- mean(cpue, na.rm = TRUE), 
    f.cpueSD <- sd(cpue, na.rm = TRUE),
    f.depthMean <- mean(bottom_depth, na.rm = TRUE),
    f.surfacetempMean <- mean(surface_temperature, na.rm = TRUE),
    f.bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)
  )

malebyYear

plot(femalebyYear$year, femalebyYear$`f.haulMean <- mean(haul, na.rm = TRUE)`)
plot(femalebyYear$year, femalebyYear$`f.haulSD <- sd(haul, na.rm = TRUE)`)
plot(femalebyYear$year, femalebyYear$`f.cpueMean <- mean(cpue, na.rm = TRUE)`) # 642 cpue = >40k
plot(femalebyYear$year, femalebyYear$`f.cpueSD <- sd(cpue, na.rm = TRUE)`) # 642 cpue = >40k
plot(femalebyYear$year, femalebyYear$`f.depthMean <- mean(bottom_depth, na.rm = TRUE)`)
plot(femalebyYear$year, femalebyYear$`f.surfacetempMean <- mean(surface_temperature, na.rm = TRUE)`)
plot(femalebyYear$year, femalebyYear$`f.bottomtempMean <- mean(bottom_temperature, na.rm = TRUE)`)




#---------- MALE location of landings ----------#

plot(maleData[,"latitude"],maleData[,"haul"])
plot(maleData[,"longitude"],maleData[,"haul"])
hist(maleData[,"haul"])

#---------- FEMALE locations of landings ----------#

plot(femaleData[,"latitude"],femaleData[,"haul"])
plot(femaleData[,"longitude"],femaleData[,"haul"])
hist(femaleData[,"haul"])


#---------- Haul by Year ----------#

plot(maleData[,"year"],maleData[,"haul"])
plot(femaleData[,"year"],femaleData[,"haul"])

#---------- Depth by Haul ----------#

plot(maleData[,"bottom_depth"],maleData[,"haul"])
plot(femaleData[,"bottom_depth"],femaleData[,"haul"])


#---------- Surface temp by Haul ----------#

plot(maleData[,"surface_temperature"],maleData[,"haul"])
plot(femaleData[,"surface_temperature"],femaleData[,"haul"])

#---------- Temperature by Year ----------#

crabData$year <- as.integer(crabData$year)

surfacetempAvgPerYear <- crabData %>%
  group_by(year) %>%
  summarise(avgSTempPerYear = mean(surface_temperature, na.rm = TRUE))

surfacetempAvgPerYear
plot(surfacetempAvgPerYear)
plot(scale(surfacetempAvgPerYear))
abline(0,0)


#---------- Bubble plot for landing locations----------#

plot(maleData[,"longitude"], maleData[,"latitude"],cex=log(maleData[,"haul"])/5,col=hsv(h=pnorm(scale(log(maleData[,"haul"])))))
plot(femaleData[,"longitude"],femaleData[,"latitude"],cex=log(femaleData[,"haul"])/5,col=hsv(h=pnorm(scale(log(femaleData[,"haul"])))))

#---------- MALE bivariate regression on lat long catch ----------#

summary(lm(log(maleData[,"haul"]) ~ maleData[,"latitude"] + maleData[,"longitude"]))
crabResid <- resid(lm(log(maleData[,"haul"]) ~ maleData[,"latitude"] + maleData[,"longitude"]))
plot(crabResid,maleData[,"year"])
hist(crabResid, breaks = 30) # not normal dist

plot(crabResid, maleData[,"cpue"], log="y") # not normal, not a consistant pattern


#---------- FEMALE bivariate regression on lat long catch ----------#

summary(lm(log(femaleData[,"haul"]) ~ femaleData[,"latitude"] + femaleData[,"longitude"]))
f.crabResid <- resid(lm(log(femaleData[,"haul"]) ~ femaleData[,"latitude"] + femaleData[,"longitude"]))
plot(f.crabResid,femaleData[,"year"])
hist(f.crabResid, breaks = 30, add = T, col = hsv(0.1, alpha = 0.3), border = 'deeppink') # not normal? much closer than male

plot(f.crabResid, femaleData[,"cpue"],log = "y")



# need data in a form that time series can be done

#yearbycatch <- table(maleData[,"year"], log(maleData[,"haul"]))

#

avgCatchYear <- array()
for(i in unique(maleData[,"year"]))
    avgCatchYear[i] = mean(log(maleData[maleData[,"year"]==i,"haul"]))
    avgCatchYear = na.omit(avgCatchYear)
    
f.avgCatchYear <- array()
for(i in unique(femaleData[,"year"]))
    f.avgCatchYear[i] = mean(log(femaleData[femaleData[,"year"]==i,"haul"]))
    f.avgCatchYear = na.omit(f.avgCatchYear)
    
    
# time series plot
crabYears <- 1980:2018


plot(crabYears,avgCatchYear)
plot(crabYears,f.avgCatchYear)

# testing autocorrelation -> allows you to compare each point in year with the next years point and then again for each consecutive year.

acf(avgCatchYear)
acf(f.avgCatchYear)

plot(1980:2017, diff(avgCatchYear))
abline(0,0)
acf(diff(avgCatchYear)) # negative correlation, increases followed by decreases


plot(1980:2017, diff(f.avgCatchYear))
abline(0,0)
acf(diff(f.avgCatchYear))

# need residulas of trend


auto.arima(avgCatchYear) # used to model future values
auto.arima(f.avgCatchYear)

#

plot(resid(auto.arima(avgCatchYear)))
abline(0,0)

plot(resid(auto.arima(f.avgCatchYear)))
abline(0,0)

# can now cross correlate with each year for total catch etc

avgCatchYear %>% forecast(h=10) %>% autoplot()
f.avgCatchYear %>% forecast(h=10) %>% autoplot()

arima.avgCY <- auto.arima(avgCatchYear) # used to model future values
arima.f.avgCY <- auto.arima(f.avgCatchYear)

arima.avgCY %>% forecast(h=10) %>% autoplot()
arima.f.avgCY %>% forecast(h=10) %>% autoplot()







#---------- crabMap ----------#

map(database = "world", regions = c("USA","Canada", "Mexico","Guatemala", "Belize", "Honduras","El Salvador", "Nicaragua","Costa Rica","Panama","Colombia"), projection = 'rectangular',fill = T,col = 'grey90',parameters = 90)

points(crabData$latitude,crabData$longitude,pch=21,col='grey50',bg='gold',cex=0.7)


mapsnames <- map("world", plot = FALSE)




#---------- mix regression analysis ----------#


m <- lmer(log(haul) ~ year + latitude +longitude + bottom_depth + surface_temperature + bottom_temperature + log(cpue) + (1 | maturity), data = crabData)
summary(m) # lat long bottom depth correlated could be problem signal -> overinterpretation 
ranef(m)
rand(m)
# R2m (marginal r2) is for fixed effects
# R2c (conditional r2) is everything combined
r.squaredGLMM(m)

P <- lmer(log(cpue) ~ year + latitude +longitude + bottom_depth + surface_temperature + bottom_temperature + log(haul) + (1 | maturity), data = crabData)
summary(P) # lat long bottom depth correlated could be problem signal -> overinterpretation 
ranef(P)
rand(P)
# R2m (marginal r2) is for fixed effects
# R2c (conditional r2) is everything combined
r.squaredGLMM(P)



hist(log(maleData[,"cpue"])



