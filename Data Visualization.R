## FINAL PROJECT SCRIPT PART 2: DATA VISUALIZATION AND ANALYSIS
## This script must be run after the Data Cleaning and Preprocessing script
## Last Updated: Dec 3rd 2018

#loading all packages neccessary
library(dplyr)
library(ggplot2)
library(scales)
library(choroplethr)
library(choroplethrMaps)

# remove anything in environment
rm(list = ls())

# load data from working directory
dfAll = read.csv("Merged Dataframe from all data.csv", stringsAsFactors = FALSE)

#####################################################################################
########################## DESCRIPTIVE STATISTICS ###################################
#####################################################################################
summary(dfAll)

#####################################################################################
################################## PLOTTING #########################################
#####################################################################################

############## Plot the difference in Turnout Rate based on Election Type ###########

# Remove puncutation in specific column to convert the characters to numeric
dfAll$GDP = gsub(",","",dfAll$GDP)
dfAll$Turnout.Rate = gsub("%","", dfAll$Turnout.Rate)
dfAll$Population.VEP = gsub(",", "", dfAll$Population.VEP)
dfAll$Population.VAP = gsub(",", "", dfAll$Population.VAP)

# Change the column to the right data type
dfAll$GeoName = as.character(dfAll$GeoName)
dfAll$GDP = as.numeric(dfAll$GDP)
dfAll$Election.Type = as.character(dfAll$Election.Type)
dfAll$Turnout.Rate = as.numeric(dfAll$Turnout.Rate)
dfAll$Population.VEP = as.numeric(dfAll$Population.VEP)
dfAll$Population.VAP = as.numeric(dfAll$Population.VAP)

#Change Caucus - R or D to just Caucus and make sure every data is correct based on 
# Election type
dfAll[grep("Primary",dfAll$Election.Type), "Election.Type"] = "Primary"
dfAll[grep("Caucus",dfAll$Election.Type), "Election.Type"] = "Caucus"

# group data by election type then by date
byElectionType = group_by(dfAll, Election.Type, Date)
# create a table of the average turnout rate by election type and year
avgElectionType = summarize(byElectionType,
                            averageTurnoutRate = mean(Turnout.Rate, na.rm = TRUE))
avgElectionType = as.data.frame(avgElectionType)

# remove the NA in the table
avgElectionType = avgElectionType[-5,]

# plot a graph to compare the Average Turnout by Election Type and year
avgTurnoutplot = qplot(x = Election.Type, 
                       y = averageTurnoutRate, 
                       data = avgElectionType,
                       geom = "point", 
                       facets = . ~ Date, 
                       color = Election.Type, 
                       size = 20)
#Add x axis and y axis label
avgTurnoutplot = avgTurnoutplot + xlab("Election Type") + ylab("Average Turnout Rate") + 
                ggtitle("Average Turnout Rate by Election Type")
avgTurnoutplot = avgTurnoutplot + theme(axis.title = element_text(size = 14),
                                        axis.text = element_text(size = 14),
                                        legend.position = "none") 
avgTurnoutplot

#save the plot
ggsave("Average Turnout Rate by Election Type.png", plot = avgTurnoutplot)

############## Plot the GDP per Capita vs Turnout Rate to see correlation ########### 

# plot a scatter plot with x as GDP Per Capita and y as Turnout Rate 
corGDPTR = qplot(x = GDP/Population.VEP, y = Turnout.Rate, 
                 data = dfAll, geom = "point")
# add x label, y label, and the title
corGDPTR = corGDPTR + xlab("GDP Per Capita") + ylab("Turnout Rate") + 
           ggtitle("GDP Per Capita VS Turnnout Rate")
# change the font size 
corGDPTR = corGDPTR + theme(axis.title = element_text(size = 12),
                            axis.text = element_text(size = 10))
corGDPTR

#save the plot
ggsave("GDP Per Capita vs Turnout Rate.png", plot = corGDPTR)

############## Plot the GDP per Capita vs Turnout Rate to see correlation ########### 

# plot a scatter plot with x as Average Unemployment and y as Turnout Rate 
corTRUemp = qplot(x = AverageUnemployment, y = Turnout.Rate, data = dfAll, geom = "point")
# add x label, y label, and the title 
corTRUemp = corTRUemp + xlab("Average Unemployment") + ylab("Turnout Rate") + 
  ggtitle("Average Unemployment VS Turnnout Rate")
# change the font size 
corTRUemp = corTRUemp + theme(axis.title = element_text(size = 12),
                              axis.text = element_text(size = 10))
corTRUemp

#save the plot
ggsave("Average Unemployment VS Turnnout Rate.png", plot = corTRUemp)

############## Plot the GDP per Capita vs Turnout Rate to see correlation ########### 

# plot a scatter plot with x as Average Unemployment and y as GDP Per Capita 
corGDPUnemp = qplot(x = GDP/Population.VEP, y = AverageUnemployment,
                    data = dfAll, geom = "point")
# add x label, y label, and the title 
corGDPUnemp = corGDPUnemp + ylab("Average Unemployment") + xlab("GDP Per Capita") + 
  ggtitle("GDP Per Capita VS Average Unemployment")
# change the font size 
corGDPUnemp = corGDPUnemp + theme(axis.title = element_text(size = 12),
                                  axis.text = element_text(size = 10))
corGDPUnemp

#save the plot
ggsave("GDP Per Capita VS Average Unemployment.png", plot = corGDPUnemp)

#####################################################################################
################################## MAPPING ##########################################
#####################################################################################
# set all missing values to 0
dfAll[is.na(dfAll)] = "0"

################### AVERAGE UNEMPLOYMENT RATE 2012 ########################### 
# grab the average unemployment rate in 2012
mapAvgUemp12 = dfAll[dfAll$Date == 2012, c("GeoName", "AverageUnemployment")]
# since there are two Minnesota values, remove one of them
mapAvgUemp12 = mapAvgUemp12[-24,]

#change the Average Unemployment column name to value for mapping
colnames(mapAvgUemp12)[2] = "value"

#upload state region data for mapping
data("state.regions")
# assign to a shorter variable name
sr = state.regions

# order the average unemployment data by state name in ascending order
mapAvgUemp12 = mapAvgUemp12[order(mapAvgUemp12$GeoName), ]
# order the state names in sr in ascending order 
sr = sr[order(sr$region),]
# lowercase the state names for comparing later on
mapAvgUemp12$GeoName = tolower(mapAvgUemp12$GeoName)
# compare the state names in both dataset to make sure we can map it
identical(mapAvgUemp12$GeoName, sr$region)

# change the column name from GeoName to region for mapping
colnames(mapAvgUemp12)[1] = "region"
# MAP THE AVERAGE UNEMPLOYMENT RATE!
mapAvgUemp12 = state_choropleth(mapAvgUemp12)
# Add title
mapAvgUemp12 = mapAvgUemp12 + ggtitle("Average Unemployment for each state in 2012")
mapAvgUemp12
#save the map
ggsave("Average Unemployment for each state in 2012.png", plot = mapAvgUemp12)

################### AVERAGE UNEMPLOYMENT RATE 2016 ########################### 
# Set NA data for Date for Colorado and Wyoming to 2016
dfAll[c(59,105), "Date"] = 2016
# grad average unemployment rate for year 2016
mapAvgUemp16 = dfAll[dfAll$Date == 2016, c("GeoName", "AverageUnemployment")]

# change the Average Unemployment column name to value for mapping
names(mapAvgUemp16)[2] = "value"

# order the average unemployment data by state name in ascending order
mapAvgUemp16 = mapAvgUemp16[order(mapAvgUemp16$GeoName), ]
# order the state names in sr in ascending order 
sr = sr[order(sr$region),]
# lowercase the state names for comparing later on
mapAvgUemp16$GeoName = tolower(mapAvgUemp16$GeoName)
# compare the state names in both dataset to make sure we can map it
identical(mapAvgUemp16$GeoName, sr$region)

# change the column name from GeoName to region for mapping
colnames(mapAvgUemp16)[1] = "region"
# MAP THE AVERAGE UNEMPLOYMENT RATE!
mapAvgUemp16 = state_choropleth(mapAvgUemp16)
# Add title
mapAvgUemp16 = mapAvgUemp16 + ggtitle("Average Unemployment for each state in 2016")
mapAvgUemp16
ggsave("Average Unemployment for each state in 2016.png", plot = mapAvgUemp16)

################### CHANGES IN AVERAGE UNEMPLOYMENT RATE ######################### 
# grab average unemployment data from year 2012
diffUemp = dfAll[dfAll$Date == 2012, c("GeoName", "AverageUnemployment")]
# change the column name to AvgUnemployment12 to indicate year 2012
colnames(diffUemp)[2] = "AvgUnemployment12"
# grab average unemployment data from year 2012
temp = dfAll[dfAll$Date == 2016, c("GeoName", "AverageUnemployment")]
# change the column name to AvgUnemployment16 to indicate year 2016
colnames(temp)[2] = "AvgUnemployment16"
# merge the data 
diffUemp = merge(diffUemp, temp, by = "GeoName", all=TRUE)
# remove one Minnesota data
diffUemp = diffUemp[-24,]

# create a new column in track whether the average unemployment rate increases or
# decreases
# initialize to 0
diffUemp$IncOrDec = 0
# Filter the rows if average unemployment rate in 2016 is greater than 2012. If so,
# set to 1
diffUemp[diffUemp$AvgUnemployment16 > diffUemp$AvgUnemployment12, "IncOrDec"] = 1
# Filter the rows if average unemployment rate in 2016 is less than or equal to 2012. 
# If so, set to - 1
diffUemp[diffUemp$AvgUnemployment16 <= diffUemp$AvgUnemployment12, "IncOrDec"] = -1

# lowercase the state name to compare for mapping
diffUemp$GeoName =tolower(diffUemp$GeoName)
# change the column name for mapping
colnames(diffUemp)[1] = "region"
colnames(diffUemp)[4] = "value"

# order the state in ascending order to compare to sr regions for mapping
diffUemp = diffUemp[order(diffUemp$region), ]
# compare the state names 
identical(sr$region, diffUemp$region)

# Map whether the state's average unemployment increases or decreases
diffUemp = state_choropleth(diffUemp, 
                            title = "Average Unemployment Increase or Decrease",
                            legend = "Increase or Decrease",
                            num_colors = 2) +
           scale_fill_brewer(palette = "Set1", labels =  c("Decrease", "Increase"))
diffUemp
ggsave("Average Unemployment Increase or Decrease.png", plot = diffUemp)

#################### DIFFERENCE IN 2012 AND 2016 GDP #########################
# grab the GDP and Popuplation data for 2012
diffGDP = dfAll[dfAll$Date == 2012, c("GeoName", "GDP", "Population.VEP")]
# grab the GDP and Popuplation data for 2016
temp = dfAll[dfAll$Date == 2016, c("GeoName", "GDP", "Population.VEP")]
# merge the data
diffGDP = merge(diffGDP, temp, by = "GeoName", all = TRUE)
#change the column names appropriately 
colnames(diffGDP)[2] = "GDP2012"
colnames(diffGDP)[3] = "Population12"
colnames(diffGDP)[4] = "GDP2016"
colnames(diffGDP)[5] = "Population16"

#change the data type in each column appropriately
diffGDP$GDP2012 = as.numeric(diffGDP$GDP2012)
diffGDP$Population12 = as.numeric(diffGDP$Population12)
diffGDP$GDP2016 = as.numeric(diffGDP$GDP2016)
diffGDP$Population16 = as.numeric(diffGDP$Population16)

# create a new column of GDP Per Capita by taking the GDP for each year and divide it
# by Population data
diffGDP$GDPerCapita12 = diffGDP$GDP2012/diffGDP$Population12
diffGDP$GDPerCapita16 = diffGDP$GDP2016/diffGDP$Population16

# Subtract the GDP per Capita in 2016 to GDP Per Capita in 2012
diffGDP$difference = diffGDP$GDPerCapita16 - diffGDP$GDPerCapita12

# remove an extra Minnesota data
diffGDP = diffGDP[-24,]

# lower case the state names for mapping and comparison
diffGDP$GeoName =tolower(diffGDP$GeoName)
# change the column names for mapping
colnames(diffGDP)[1] = "region"
colnames(diffGDP)[8] = "value"

# order 
diffGDP = diffGDP[order(diffGDP$region), ]
# compare the state names to ensure it's the same
identical(sr$region, diffGDP$region)
# map the GDP difference
diffGDP = state_choropleth(diffGDP,
                           title = "GDP Difference")
diffGDP
# save the map
ggsave("GDP Difference.png", diffGDP)

#################### INCREASE OR DECREASE OF TURNOUT RATE #########################
# grab the turnout rate data for 2012
diffTR = dfAll[dfAll$Date == 2012, c("GeoName", "Turnout.Rate")]
# grab the turnout data for 2016
temp = dfAll[dfAll$Date == 2016, c("GeoName", "Turnout.Rate")]
# merge the turnout data
diffTR = merge(diffTR, temp, by = "GeoName", all = TRUE)

# change the column name appropriately
colnames(diffTR)[2] = "TurnoutRate12"
colnames(diffTR)[3] = "TurnoutRate16"

# change the columns to the correct data type
diffTR$TurnoutRate12 = as.numeric(diffTR$TurnoutRate12)
diffTR$TurnoutRate16 = as.numeric(diffTR$TurnoutRate16)

# group the data by state 
bystate = group_by(diffTR, GeoName)
# add the turnout data by state
summ = summarize(bystate,
                 TurnoutRate12 = sum(TurnoutRate12),
                 TurnoutRate16 = sum(TurnoutRate16))
diffTR = summ

# Create a column to indicate whether the turnout rate increase or decrease
diffTR$IncOrDec = 0
# Filter the rows if turnout rate in 2016 is greater than 2012. If so,
# set to Increase
diffTR[diffTR$TurnoutRate16>diffTR$TurnoutRate12, "IncOrDec"] = "Increase"
# Filter the rows if turnout rate in 2016 is less than or equal to 2012. If so,
# set to Decrease
diffTR[diffTR$TurnoutRate16<=diffTR$TurnoutRate12, "IncOrDec"] = "Decrease"

# lower case the state to compare and map
diffTR$GeoName =tolower(diffTR$GeoName)

# change the column names to region and value for mapping
names(diffTR)[1] = "region"
names(diffTR)[4] = "value"

# order the data by state in ascending order for comparing
diffTR = diffTR[order(diffTR$region), ]
# compare the state names to ensure it can be mapped later on
identical(sr$region, diffTR$region)

# map the increase or decrease of turnout rate for each states
diffTR = state_choropleth(diffTR,
                          title = "Increase or Decrease of Turnout Rate",
                          legend = "Legend") +
         scale_fill_brewer(palette = "Set1", labels =  c("Decrease", "Increase"))
diffTR
# save the map
ggsave("Increase or Decrease of Turnout Rate.png", diffTR)



