library(readxl)
load("C:/Users/lperi/OneDrive/UMD iSchool/DCIC/Manumissions_Original.xlsx")
library(readxl)
Manumissions_Original <- read_excel("C:/Users/lperi/OneDrive/UMD iSchool/DCIC/Manumissions_Original.xlsx")
View(Manumissions_Original)
data<-Manumissions_Original
head (data)
str(data)
library (tidyverse)
#library (dplyr)
library (magrittr)
#Subset the data set to excerpt basic columns for analysis
#Columns retained are DataItem, Age, County, Date, DateManumitted, DateRecorded, DateRecorded2,
#Owner_LastName, Owner_FirstName, Owner_MiddleName, Slave_LastName, Slave_FirstName, Slave_MiddleName, Alias,
#Sex, Employment
basic <- data%>%
select(DataItem, Age, County, Date, DateManumitted, DateRecorded, DateRecorded2, Owner_LastName, Owner_FirstName, Owner_MiddleName, Slave_LastName, Slave_FirstName, Slave_MiddleName, Alias,Sex, Employment)
str (basic)
basic$DataItem<- as.numeric(basic$DataItem)
#Clean up the data.  First phase, change all the variables from default character.
#Change all "NULL" to NA
basic <- basic %>% replace(.=="NULL", NA) # replace with NA
str (basic)
#Now that all of the "NULL" have been replaced, continue changing type of variables
basic$Age <- as.numeric(basic$Age)
basic$Date <- as.Date(basic$Date)
basic$Date <- as.Date.character(basic$Date)
#The format for Date is not standardized, so will need to extract the year from the last four characters
#in a future step.
basic$DateManumitted <- as.Date(basic$DateManumitted)
#Similar problem with DateManumitted.  Assume this is a problem for all date fields.
basic$Sex <- as.factor(basic$Sex)
str (basic)
# Check Age field, which had some unusual values
summarize (basic$Age)
summary (basic$Age)
which (basic$Age > 99)
which (basic$Age >90)
which (basic$Age >85)
which (basic$Age >80)
max (basic$Age[basic$Age< 99])
max (basic$Age)
max(basic$Age, na.rm = TRUE)
max(basic$Age[basic$Age != 237], na.rm = TRUE)
#The maximum age value 237 appears to be a mistake.  Replace it with NA.
basic$Age[710] <- NA
basic$Age[710]
summary (basic$Age)
basic$Age
basic$Age <- as.numeric (basic$Age)
summary (basic$Age)
summary (basic)
basic$County <- as.factor (basic$County)
summary (basic$County)
summary (basic)
boxplot (basic$Age)
par (mar = c(2,2,2,2))
boxplot (basic$Age)
#Subset the date columns to begin extracting a valid year for each record.
#
yearcols <- basic%>%
select(Date, DateManumitted, DateRecorded, DateRecorded2)
#Do this again, but keep the identifier so that can rejoin the new column with the basic data base
yearcols <- basic%>%
select(DataItem, Date, DateManumitted, DateRecorded, DateRecorded2)
view (yearcols)
#
#Clean white space from date columns

#basic$Date <- trimws(basic$Date)
#basic$DateManumitted <- trimws(basic$DateManumitted)
#basic$DateRecorded <- trimws(basic$DateRecorded)
#basic$DateRecorded2 <- trimws(basic$DateRecorded2)
#basic$Date <- trimws(basic$Date)
yearcols$DateManumitted <- trimws(yearcols$DateManumitted)
yearcols$DateRecorded <- trimws(yearcols$DateRecorded)
yearcols$DateRecorded2 <- trimws(yearcols$DateRecorded2)
#
x <- yearcols$Date
#test of substr count
substr(x, nchar(x)-3, nchar(x)-3)
#Use -2 to get the third character from the end
testyear <- ifelse (substr(x, nchar(x)-2, nchar(x)-2)==7|substr(x, nchar(x)-2, nchar(x)-2==8), basic$Date,
testyear <- ifelse (substr(x, nchar(x)-2, nchar(x)-2)==7|substr(x, nchar(x)-2, nchar(x)-2==8), basic$Date,b
))
testyear <- ifelse(
substr(x, nchar(x)-2, nchar(x)-2)==7|substr(x, nchar(x)-2, nchar(x)-2)== 8,
substr (x, nchar(x)-3, nchar(x)),
ifelse(
substr(x,2,2) ==7 | substr(x,2,2) ==8, substr(x, 1, 4),
NA))
#Recreate this as a function and apply it to each year column.
#Create the function extractyr
extractyr <- function(x){
  ifelse(
      substr(x, nchar(x)-2, nchar(x)-2)==7|substr(x, nchar(x)-2, nchar(x)-2)== 8,
      substr (x, nchar(x)-3, nchar(x)),
           ifelse(
            substr(x,2,2) ==7 | substr(x,2,2) ==8, substr(x, 1, 4),
             NA))
}
#
#Now apply this to the Date columns to create new variables in the dataset basic
x <- basic$Date
basic$Year1 <- extractyr(x)
x <- basic$DateManumitted
basic$Year2 <- extractyr(x)
x<- basic$DateRecorded
basic$Year3 <- extractyr(x)
x <- basic$DateRecorded2
basic$Year4 <- extractyr(x)
# 
#Check to see if the new variables are in the dataset
head (basic [, 17:20])
#
#Now create new Year variable that is the earliest of the values for the years
#Year columns are in 17 to 20
basic$Year <- apply (basic[, 17:20], 1, min, na.rm = TRUE)
names(basic)
head (basic[, 17:21])
#Change the Year columns from charactor to numeric
basic[, 17:21]%<>% mutate_if(is.character, as.numeric)
#
#basic$Year <- as.numeric (basic$Year)
#basic$Year1 <- as.numeric (basic$Year1)
#basic$Year2 <- as.numeric (basic$Year2)
#basic$Year3 <- as.numeric (basic$Year3)
#basic$Year4 <- as.numeric (basic$Year4)
#Save with new name for transfer
ManuClean<- basic
#
#Export data to .csv format for later use
write.csv(ManuClean, file = "ManuClean2.csv", row.names = FALSE)






