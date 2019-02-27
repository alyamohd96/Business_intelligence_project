### FINAL PROJECT SCRIPT PART 1: DATA CLEANING AND PREPROCESSING
### This script is for Data Cleaning and Preprocessing only 
### Date  updated: Dec 1, 2018

rm(list=ls())

## Final Project

## Creat 'cols' variable to use for naming the columns in each Data Frame. 
## This is important because the data comes with two levels of column headers so
## using header = T only returns the top line of headers which are not useful
cols = c('Date', 'State', 'Election Type', 'Party', 'Turnout Rate', 'Population VEP', 
         'Population VAP','Democrat', 'Republican', 'Minor', 
         'Total Ballots Counted','Notes' ,'Additional Notes' )

cols2 = c('Date', 'State', 'Election Type', 'Turnout Rate', 'Population VEP', 
         'Population VAP','Democrat', 'Republican', 'Minor', 
         'Total Ballots Counted')

## Read the data and use the column names established
df16=read.csv('2016Election.csv', col.names = cols,na.strings = c("NA", ""))
df12=read.csv('2012Election.csv', col.names = cols2,na.strings = c("NA", ""))

# Delete the unnecessary columns that won't be used
df16$Notes = NULL
df16$Additional.Notes = NULL
df16$Party = NULL
df16$Democrat = NULL
df16$Republican = NULL
df16$Minor = NULL
df16$Total.Ballots.Counted = NULL
df12$Democrat = NULL
df12$Republican = NULL
df12$Minor = NULL
df12$Total.Ballots.Counted = NULL


## Delete the first row of each column since they have some headers included
df16 = df16[-1,]
df12 = df12[-1,]

## Some of the rows have dates and some do not
## We need to know each rows year and year only so that once me merge the two df
## We will know which rows belong to which year
df16$Date = '2016'
df12$Date = '2012'

## This gets rid of all the NA's. The only NA's we had were in Turnout Data. 
df16 = na.omit(df16) 
df12 = na.omit(df12)

#Convert factor type to character type for column State
df12$State = as.character(df12$State)
df16$State = as.character(df16$State)

# Change Missouri (a) to just Missouri and South Carolina (b) to just South Carolina
df12[df12$State == "Missouri (b)", "State"] = "Missouri"
df12[df12$State == "South Carolina (a)", "State"] = "South Carolina"

#View(df16)

## Unemployment Data
## The Data came in a pdf format so I copied it onto MS Word then into MS Excel and saved
##  it as a .CSV

## Create 'colsu' variable to list the names of each column
colsu = c('State', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 
          'Sept', 'Oct', 'Nov', 'Dec')

## Read the .csv files and use the 'colsu' variable to name each column
dfu16=read.csv('2016Unem.csv', col.names = colsu, stringsAsFactors = F)
dfu12=read.csv('2012Unem.csv', col.names = colsu, stringsAsFactors = F)

## Create a 'rows' variable to list which rows are unnecessary
rows = c(-1,-2,-55,-56)

## Use the 'rows' variable to delete the unnecessary rows
dfu16 = dfu16[rows,]
dfu12 = dfu12[rows,]

## Create 'cols.num' as a variable listing which columns I want to be numeric
cols.num <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 
              'Sept', 'Oct', 'Nov', 'Dec')

## Uses 'cols.num' variable to make the unemployment rates into numeric
dfu16[cols.num] <- sapply(dfu16[cols.num],as.numeric)
dfu12[cols.num] <- sapply(dfu12[cols.num],as.numeric)

## Test to see if dfu12$Mar is actually numeric now
class(dfu12$Mar)

## Creates a new column that calculates the Average Unemployment for each State throughout
## the State
dfu12$AverageUnemployment = rowSums(dfu12[,c(2:13)])/12
dfu16$AverageUnemployment = rowSums(dfu16[,c(2:13)])/12

## Change D.C. to District of Columbia for uniformity
dfu12[dfu12$State == "D.C. " , "State"] = "District of Columbia"
dfu16[dfu16$State == "D.C. " , "State"] = "District of Columbia"

#GDP data

#2016 GDP by state
url= "https://apps.bea.gov/itable/drilldown.cfm?reqid=70&stepnum=40&Major_Area=3&State=00000&Area=XX&TableId=505&Statistic=1&Year=2016&YearBegin=-1&Year_End=-1&Unit_Of_Measure=Levels&Rank=0&Drill=1"
library(rvest) 
pg= read_html(url)
tb= html_table(pg, fill= TRUE)
tb

#2012 GDP by state
url2= "https://apps.bea.gov/itable/drilldown.cfm?reqid=70&stepnum=40&Major_Area=3&State=00000&Area=XX&TableId=505&Statistic=1&Year=2012&YearBegin=-1&Year_End=-1&Unit_Of_Measure=Levels&Rank=0&Drill=1"
pg2= read_html(url2)
tb2= html_table(pg2, fill= TRUE)
tb2

#Turn 2012 and 2016 tables into data frames, take out unecessary first columns
dfGDP2016= tb[[1]]
#View(dfGDP2016)
dfGDP2012= tb2[[1]]
#View(dfGDP2012)

dfGDP2016[1]= NULL
dfGDP2012[1]= NULL

names(dfGDP2012)[2]= "GDP"
names(dfGDP2016)[2]= "GDP"

#Merge GDP12 with election data for 2012, and GDP16 with election data for 2016
dfGDPelection12 = merge(dfGDP2012, df12, by.x =  'GeoName', by.y = 'State')
dfGDPelection16 = merge(dfGDP2016, df16, by.x =  'GeoName', by.y = 'State')

#Merge GDPelection12 with dfu12, and GDPelection16 with dfu16 seperately based on state
#have to trim any leading and trailing whitespace so the state match
dfu12$State = trimws(dfu12$State)
dfGDPelection12$GeoName = trimws(dfGDPelection12$GeoName)
dfAll12 = merge(dfGDPelection12, dfu12, by.x = "GeoName", by.y = "State", all = TRUE)

dfu16$State = trimws(dfu16$State)
dfGDPelection16$GeoName = trimws(dfGDPelection16$GeoName)
dfAll16 = merge(dfGDPelection16, dfu16, by.x = "GeoName", by.y = "State", all = TRUE)

#vertically merge dfAll12 and dfAll16 to get dfAll
dfAll = rbind(dfAll12, dfAll16)

#write a csv file for all data that have been merged
write.csv(dfAll, "Merged Dataframe from all data.csv")
