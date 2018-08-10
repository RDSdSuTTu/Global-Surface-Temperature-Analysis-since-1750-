#import data from the dataset and assign it to a variable called 'cityData'
cityData <- read.csv("GlobalLandTemperaturesByCity.csv")

#investigate the data set to take inventory of its characteristics including
#variable names
str(cityData)
head(cityData)

#bring the dplyr package into the project
library(dplyr)

#filter out data for the city of Lubbock and assign it to 'LubbockData'
LubbockData <- filter(cityData, City == "Lubbock")

#create a plot that shows increasing certainty over time for the
#Lubbock data set

#to limit the number of data points in the proposed plot, we'll average
#values of a year and plot that while omitting years containing missing data
LBBuncertaintyByYear <- na.omit(group_by((LubbockData %>% mutate(year =
                                                                   as.numeric(format(as.POSIXct(dt), format = "%Y")))),
                                         year) %>% summarize(avUncertainty=mean(AverageTemperatureUncertainty)))

#look at the first 15 rows of LBBuncertaintyByYear to make sure the
#data looks good and reasonable
head(LBBuncertaintyByYear, 15)

#----------------LUBBOCK TEMPERATURE UNCERTAINTY PLOT-------------------------#
library(ggplot2)
#make the plot
uncertaintyPlot <- ggplot(LBBuncertaintyByYear, aes(x=year, y=avUncertainty,
                                                    color=avUncertainty)) + geom_point() + scale_color_gradient(low="dark green",
                                                                                                                high="red") + ggtitle("Yearly Temperature Uncertainty in Lubbock") +
  ylab("Temperature Uncertainty in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)); uncertaintyPlot


#create 2 new variables (columns) that represent the upper and lower bound
#temperature values for the 95% confidence interval for a particular time period
#and store this new data set in LBB
LBB <- mutate(LubbockData, MaxTemp = AverageTemperature + (AverageTemperatureUncertainty/2),
              MinTemp = AverageTemperature - (AverageTemperatureUncertainty / 2))

#add a column for only year to the LBB data set for the sake of performing a
#group_by operation later
LBB <- mutate(LBB, year = as.numeric(format(as.POSIXct(dt), format = "%Y")))

#create 3 plots of temperature vs. year in Lubbock

#The first plot will be the most straight-forward plot, taking
#the average temperature for each year and plotting it against that year

#The second plot seeks to maximize warming over time within the 95% confidence
#interval by assuming a temperature at the lower bound for the first year in the
#data series and increasing the location of the theoretical temperature of each
#year linerally over the time frame of the data set such that the theoretical
#temperature taken for the last year is at the upper bound of the 95%
#confidence interval for that year.

#The third plot seeks to minimize the warming (maximize the cooling if there is
#cooling) over time by taking the process employed for plot 2, but taking the
#reverse of it such that we start at the upper bound of the 95% confidence
#interval for the first year and end up at the lower bound of the 95% confidence
#interval for the last year

#add two columns to the LBB data set that will be used for the second and
#third plots as described above

#the following is added ensure row_number works as intended for subsequent functions
detach("package:ggplot2", unload="TRUE")
detach("package:plyr", unload="TRUE")
LBB <- filter(LBB, year >= 1900 & year <= 2012)
LBB <- mutate(LBB, fastWarmT = MinTemp + AverageTemperatureUncertainty *
                (row_number() - 1)/(nrow(LBB) - 1), slowWarmT = MaxTemp -
                (AverageTemperatureUncertainty * (row_number() - 1)/(nrow(LBB) - 1)))

#check the data set for reasonableness
head(LBB); tail(LBB)

#group the LBB data set by year and produce three different data sets
# 1. mean average temperature by year
# 2. mean fastWarmT by year
# 3. mean slowWarmT by year

meanWarm <- na.omit(group_by(LBB, year) %>% summarize(avTemp =
                                                        mean(AverageTemperature)))
fastWarm <- na.omit(group_by(LBB, year) %>% summarize(avTemp = mean(fastWarmT)))
slowWarm <- na.omit(group_by(LBB, year) %>% summarize(avTemp = mean(slowWarmT)))
#check to see if the data looks good/reasonable
head(meanWarm); tail(meanWarm)
head(fastWarm); tail(fastWarm)
head(slowWarm); tail(slowWarm)

#------------------------LUBBOCK AVERAGE TEMP PLOTS----------------------#
#source code for completing the first plot
library(ggplot2)
#plot with linear regression line
meanWarmingPlot1 <- ggplot(meanWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock") + ylab("Average Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(method=lm)
#draw the plot
meanWarmingPlot1

#plot with loess smoothed fit line
meanWarmingPlot2 <- ggplot(meanWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock") + ylab("Average Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(color="yellow")
#draw the plot
meanWarmingPlot2

#print the slope of maxWarming (deg C delta/year)
coef(lm(meanWarm$avTemp ~ meanWarm$year))[2]
#Value: +0.729 degC/100 years

#-----------------------LUBBOCK MAXIMIZED WARMING RATE PLOTS------------------------#
#source code for completing the first plot
library(ggplot2)
#plot with linear regression line
fastWarmingPlot1 <- ggplot(fastWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock with Maximized Warming") +
  ylab("Average Theotetical Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(method=lm)
#draw the plot
fastWarmingPlot1

#plot with loess smoothed fit line
fastWarmingPlot2 <- ggplot(fastWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock with Maximized Warming") +
  ylab("Average Theotetical Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(color="yellow")
#draw the plot
fastWarmingPlot2

#print the slope of maxWarming (deg C delta/year)
coef(lm(fastWarm$avTemp ~ fastWarm$year))[2]
#Value: +1.039 degC/100 years

#------------------LUBBOCK MINIMIZED WARMING RATE PLOTS------------------#
#source code for completing the second plot
#plot with linear regression line
slowWarmingPlot1 <- ggplot(slowWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock with Minimized Warming") +
  ylab("Average Theotetical Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(method=lm)
#draw the plot
slowWarmingPlot1

#plot with loess smoothed fit line
slowWarmingPlot2 <- ggplot(slowWarm, aes(x=year, y=avTemp, color = avTemp)) +
  geom_point() + scale_color_gradient(low="blue", high="red") +
  ggtitle("Yearly Average Temperatures In Lubbock with Minimized Warming") +
  ylab("Average Theotetical Temperature in °C") +
  xlab("Year") + theme(plot.title = element_text(hjust=0.5)) + geom_smooth(color="yellow")
#draw the plot
slowWarmingPlot2

#print the slope of maxWarming (deg C delta/year)
coef(lm(slowWarm$avTemp ~ slowWarm$year))[2]
#value: +0.420 degC/100 years

library(dplyr)


#-------------WARMING BY COUNTRY WORLD MAP PLOT----------------#
#Working on plotting temperature change rates and the year we'll eclipse 2 degrees of warming
#in Celsius using googleVis package and graphics

#import country data
countryData <- read.csv("GlobalLandTemperaturesByCountry.csv")

#add a year variable to the data
countryData <- mutate(countryData, year=as.numeric(format(as.POSIXct(dt), format = "%Y")))

#group the data by country and year, find the average temperature per year, and omit yearly data
#that contain missing information
countryTemps <- na.omit(countryData %>% group_by(Country, year) %>%
                          summarize(avT = mean(AverageTemperature)))

#filter out years later than 1900
countryTemps <- filter(countryTemps, year > 1899)

#get a data frame with average 100 year temperature changes by country
country100yrTdeltaRate <- group_by(countryTemps, Country) %>%
  summarize(Warming_Rate_Per_100_Years_By_degC =
              round(100 *coef(lm(avT ~ year))[2], digits = 2))

#bring in the googleVis package
library(googleVis)

#plot the map
Global100MeanTchange <- gvisGeoMap(country100yrTdeltaRate,
                                   locationvar = "Country",
                                   numvar= "Warming_Rate_Per_100_Years_By_degC",
                                   options = list(colors='[0xF9FF33, 0xFF3333, 0xB22828]'))
plot(Global100MeanTchange)

#-----YEAR TO REACH 2 DEGREE THRESHOLD WORLD MAP PLOT--------#

#determine year when country's average yearly temp will exceed 2 degrees
#above pre-industrialized levels (or avereage yearly temp will be about
#2 deg C higher than average yearly temp around 1900)

twoDegreeEclipseYear <- mutate(country100yrTdeltaRate,
                               Year_to_Eclipse_2_Degree_Warming = ceiling(1900 + 200/
                                                                            Warming_Rate_Per_100_Years_By_degC))
#mean value of the Year_to_Eclipse_2_Degree_Warming variable, rounded up
ceiling(mean(twoDegreeEclipseYear$Year_to_Eclipse_2_Degree_Warming))
#Average Year to 2 deg threshold: 2104

#plot the map
Global2DegEclipse <- gvisGeoMap(twoDegreeEclipseYear,
                                locationvar = "Country",
                                numvar= "Year_to_Eclipse_2_Degree_Warming",
                                options = list(colors='[0xB22828, 0xFF3333, 0xF9FF33]'))
plot(Global2DegEclipse)


#----CLEANING & EXPORTING DATA FOR SEASONAL LUBBOCK PLOT IN PYTHON----#
#filter out seasonal data for lubbock and group into 1820-1900, 1900-1940, 1940-1980, 1980-2013 clusters
#temperatures will then be averaged within the seasonal, periodic clusters

#bring the stringi package in to manipulate the string in the dt variable of the LBB data set
library(stingi)

#create a new column of data for the month in LBB and name this data set LBBmonth
LBBmonths <- mutate(LBB, month = as.numeric(substr(dt, 6, 7)))

#get the pre-1901 winter data
LBB_DJF1900 <- na.omit(LBBmonths %>% filter((month == 12 | month == 1 | month == 2) & year <= 1900) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_DJF_1900_T <- mean(LBB_DJF1900$avT)
#get the 1900-1940 winter data
LBB_DJF1940 <- na.omit(LBBmonths %>% filter((month == 12 | month == 1 | month == 2) & year >= 1900 &
                                              year <= 1940) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_DJF_1940_T <- mean(LBB_DJF1940$avT)
#get the 1940-1980 winter data
LBB_DJF1980 <- na.omit(LBBmonths %>% filter((month == 12 | month == 1 | month == 2) & year >= 1940 &
                                              year <= 1980) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_DJF_1980_T <- mean(LBB_DJF1980$avT)
#get the 1980-2013 winter data
LBB_DJF2013 <- na.omit(LBBmonths %>% filter((month == 12 | month == 1 | month == 2) & year >= 1980) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_DJF_2013_T <- mean(LBB_DJF2013$avT)
#construct winter T vector
winterTs <- c(LBB_DJF_1900_T, LBB_DJF_1940_T, LBB_DJF_1980_T, LBB_DJF_2013_T)

#get the pre-1901 spring data
LBB_MAM1900 <- na.omit(LBBmonths %>% filter((month == 3 | month == 4 | month == 5) & year <= 1900) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_MAM_1900_T <- mean(LBB_MAM1900$avT)
#get the 1900-1940 spring data
LBB_MAM1940 <- na.omit(LBBmonths %>% filter((month == 3 | month == 4 | month == 5) & year >= 1900 &
                                              year <= 1940) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_MAM_1940_T <- mean(LBB_MAM1940$avT)
#get the 1940-1980 spring data
LBB_MAM1980 <- na.omit(LBBmonths %>% filter((month == 3 | month == 4 | month == 5) & year >= 1940 &
                                              year <= 1980) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_MAM_1980_T <- mean(LBB_MAM1980$avT)
#get the 1980-2013 spring data
LBB_MAM2013 <- na.omit(LBBmonths %>% filter((month == 3 | month == 4 | month == 5) & year >= 1980) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_MAM_2013_T <- mean(LBB_MAM2013$avT)
#construct spring T vector
springTs <- c(LBB_MAM_1900_T, LBB_MAM_1940_T, LBB_MAM_1980_T, LBB_MAM_2013_T)

#get the pre-1901 summer data
LBB_JJA1900 <- na.omit(LBBmonths %>% filter((month == 6 | month == 7 | month == 8) & year <= 1900) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_JJA_1900_T <- mean(LBB_JJA1900$avT)
#get the 1900-1940 summer data
LBB_JJA1940 <- na.omit(LBBmonths %>% filter((month == 6 | month == 7 | month == 8) & year >= 1900 &
                                              year <= 1940) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_JJA_1940_T <- mean(LBB_JJA1940$avT)
#get the 1940-1980 summer data
LBB_JJA1980 <- na.omit(LBBmonths %>% filter((month == 6 | month == 7 | month == 8) & year >= 1940 &
                                              year <= 1980) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_JJA_1980_T <- mean(LBB_JJA1980$avT)
#get the 1980-2013 summer data
LBB_JJA2013 <- na.omit(LBBmonths %>% filter((month == 6 | month == 7 | month == 8) & year >= 1980) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_JJA_2013_T <- mean(LBB_JJA2013$avT)
#construct summer T vector
summerTs <- c(LBB_JJA_1900_T, LBB_JJA_1940_T, LBB_JJA_1980_T, LBB_JJA_2013_T)

#get the pre-1901 autumn data
LBB_SON1900 <- na.omit(LBBmonths %>% filter((month == 9 | month == 10 | month == 11) & year <= 1900) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_SON_1900_T <- mean(LBB_SON1900$avT)
#get the 1900-1940 autumn data
LBB_SON1940 <- na.omit(LBBmonths %>% filter((month == 9 | month == 10 | month == 11) & year >= 1900 &
                                              year <= 1940) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_SON_1940_T <- mean(LBB_SON1940$avT)
#get the 1940-1980 autumn data
LBB_SON1980 <- na.omit(LBBmonths %>% filter((month == 9 | month == 10 | month == 11) & year >= 1940 &
                                              year <= 1980) %>% group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_SON_1980_T <- mean(LBB_SON1980$avT)
#get the 1980-2013 autumn data
LBB_SON2013 <- na.omit(LBBmonths %>% filter((month == 9 | month == 10 | month == 11) & year >= 1980) %>%
                         group_by(dt) %>% summarize(avT = AverageTemperature))
LBB_SON_2013_T <- mean(LBB_SON2013$avT)
#construct autumn T vector
autumnTs <- c(LBB_SON_1900_T, LBB_SON_1940_T, LBB_SON_1980_T, LBB_SON_2013_T)

#column combine the periodic seasonal temperatures
LBBseasonalTs <- cbind(winterTs, springTs, summerTs, autumnTs)

#move the Lubbock seasonal temperatures to the parent folder as a csv file
#named "LBBseasonalTemps.csv"
write.csv(LBBseasonalTs, "LBBseasonalTemps.csv")

#calculate the rate of warming for each season since 1900

#winter analysis
season <- na.omit(LBBmonths %>% filter((month == 12 | month == 1 | month == 2) & year >= 1900))
season <- group_by(season, year) %>% summarize(avT = mean(AverageTemperature))
seasonRate <- 100 * coef(lm(season$avT ~ season$year))[2]
seasonRate
#warming rate per 100 years during the winter since 1900: 0.41 deg C

#spring analysis
season <- na.omit(LBBmonths %>% filter((month == 3 | month == 4 | month == 5) & year >= 1900))
season <- group_by(season, year) %>% summarize(avT = mean(AverageTemperature))
seasonRate <- 100 * coef(lm(season$avT ~ season$year))[2]
seasonRate
#warming rate per 100 years during the spring since 1900: 1.07 deg C

#summer analysis
season <- na.omit(LBBmonths %>% filter((month == 6 | month == 7 | month == 8) & year >= 1900))
season <- group_by(season, year) %>% summarize(avT = mean(AverageTemperature))
seasonRate <- 100 * coef(lm(season$avT ~ season$year))[2]
seasonRate
#warming rate per 100 years during the summer since 1900: 0.98 deg C

#autumn analysis
season <- na.omit(LBBmonths %>% filter((month == 9 | month == 10 | month == 11) & year >= 1900))
season <- group_by(season, year) %>% summarize(avT = mean(AverageTemperature))
seasonRate <- 100 * coef(lm(season$avT ~ season$year))[2]
seasonRate
#warming rate per 100 years during the autumn since 1900: 0.79 deg C

For Lubbock Analysis (in Python):
  #looking to import csv data that was manipulated and
  #formatted in R
  
  #the following code was used based on a tutorial on
  #YouTube: https://www.youtube.com/watch?v=Tq6rCWPdXoQ
  
  import numpy as np
import matplotlib.pyplot as plt
import csv

data = np.genfromtxt('LBBseasonalTemps.csv', delimiter=',')
to1900 = data[1, 1:5]
from1900to1940 = data[2, 1:5]
from1940to1980 = data[3, 1:5]
from1980to2013 = data[4, 1:5]

#the plotting code was based on code from the following website
#URL: https://pythonspot.com/en/matplotlib-bar-chart/

# data to plot
n_groups = 4

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2
opacity = 0.8

rects1 = plt.bar(index, to1900, bar_width,
                 alpha=opacity,
                 color='y',
                 label='1820-1900')

rects2 = plt.bar(index + bar_width, from1900to1940, bar_width,
                 alpha=opacity,
                 color='orange',
                 label='1900-1940')

rects3 = plt.bar(index + 2*bar_width, from1940to1980, bar_width,
                 alpha=opacity,
                 color='r',
                 label='1940-1980')

rects4 = plt.bar(index + 3*bar_width, from1980to2013, bar_width,
                 alpha=opacity,
                 color='maroon',
                 label='1980-2013')

plt.xlabel('Season')
plt.ylabel('Temperatures in \N{DEGREE SIGN}C')
plt.title('Lubbock Temperatures by Season')
plt.xticks(index + bar_width, ('winter', 'spring', 'summer', 'autumn'))
plt.legend()

plt.tight_layout()
plt.show()

