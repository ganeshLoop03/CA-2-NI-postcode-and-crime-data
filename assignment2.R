# Section 1 : Tasks undertaken for NIPostcode dataset


# Task A
# Displaying structure, total number of rows and first 10 rows from dataframe 


# Reading csv file and converting all blanks to NA 
NiPostCode <- read.csv("NIPostcodes.csv",na.strings="",header=FALSE)


NiPostCode
# to show structure of data set
str(NiPostCode)


# to show total number of rows in dataset
nrow(NiPostCode)


# to display first 10 rows of the data
NiPostCode[1:10,]



# Task B
# adding headers/title 
colnames(NiPostCode) <- column_names
names(NiPostCode)[1] <-"OrganisationName"
names(NiPostCode)[2] <-"Sub-buildingName"
names(NiPostCode)[3] <-"BuildingName"
names(NiPostCode)[4] <-"Number"
names(NiPostCode)[5] <-"Location"
names(NiPostCode)[6] <-"Alt Thorfare"
names(NiPostCode)[7] <-"Secondary Thorfare"
names(NiPostCode)[8] <-"Locality"
names(NiPostCode)[9] <-"Townland"
names(NiPostCode)[10] <-"Town"
names(NiPostCode)[11] <-"County"
names(NiPostCode)[12] <-"Postcode"
names(NiPostCode)[13] <-"x-coordinates"
names(NiPostCode)[14] <-"y-coordinates"
names(NiPostCode)[15] <-"Primary Key"


NiPostCode[1:10,]
str(NiPostCode)

NiPostcodes

# Task C : Removing missing entities


# delete columns with more than 50% missing values
NiPostcodes <- NiPostCode[, -which(colMeans(is.na(NiPostCode)) > 0.5)]
(NiPostcodes)



# Task D 
# count of all missing values
table(is.na (NiPostCode))


# output of all NA's in Individual columns 
na_count <-sapply(NiPostCode, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


# to find mean of all null values
mean(is.na(NiPostCode))


# output of mean of NA's in Individual columns 
na_mean <-sapply(NiPostCode, function(y) sum(mean(which(is.na(y)))))
na_mean <- data.frame(na_mean)
na_mean


# Task E


# Modifing the County attribute to be a categorising factor
NiPostcodes$County <- as.factor(NiPostcodes$County)
str(NiPostcodes)
NiPostcodes

# Task F


# to move the primary key identifier to the start of the dataset.
NiPostcodes<-NiPostcodes[,c(9, 1:8)]
NiPostcodes[1:10,]


# Task G


#Creating new dataset and storing Locality, Townland and Town information that has town name Limavady
library(dplyr)

Limavady_data <- subset (NiPostCode, Town == "LIMAVADY", 
                          select=c(Locality, Townland, Town))

# writing data to csv file 
str(Limavady_data)


write.csv(Limavady_data, 
          file = "Limavady.csv")


# Task H

# saving modified NIPostcode dataset

write.csv(NiPostcodes, 
          file ="CleanNIPostcodeData.csv")


#working with NI crime dataset 
############
############
###########
############

# Task A: amalgamate all of the crime data from each csv file into one dataset
# for this step all the data in stored in temp working directory folder


# Creating folder named temp and storing all crime data files
folder <- "C:/Users/shoeb/Desktop/CA-2-NI-postcode-and-crime-data/temp/"

# Selecting all files with patter .csv and naming the file as file_list
file_list <- list.files(path=folder, 
                        pattern="*.csv")


# performing rbind on all the files present in file_list
AllNIcrime <- do.call("rbind", lapply(file_list, 
                 function(x) 
                  read.csv(paste(folder, x, sep=''), 
                   stringsAsFactors = FALSE)))

str(AllNIcrime)


#reading total number of rows and head of the ALLNIcrimeData
nrow(AllNIcrime)

head(AllNIcrime)



#saving dataset AllNICrimeData
write.csv(AllNIcrime, "AllNICrimeData.csv", row.names=FALSE)



# Task B: Modify the structure of the AllNICrimeData file 

# using subset function to remove columns

df <- subset(AllNIcrime, select = -c(Crime.ID, Falls.within, Reported.by, LSOA.code, LSOA.name, Last.outcome.category, Context))


head(df)

# structure of the modified file
str(df)


# Task C

# Modifing the Crimetype attribute to be a categorising factor
df$Crime.type <- as.factor(df$Crime.type)

# displaying structure of the modified file
str(df)


df
# Task D: 


# Filterng the column Location by removing On or near
df[4] <- lapply(df[4], gsub, pattern = "On or near", replacement = "", fixed = TRUE)

# Filterng the cloumn Location by removing "No Location" which was found to be avaiable
# 
df[4] <- lapply(df[4], gsub, pattern = "No Location" , replacement = "", fixed = TRUE)



# Marking empty rows as NA  
library(dplyr)
df <- df %>% mutate_all(na_if, " ")
(df)


head(df,15)


# Task E

# Selecting 1000 random samples of crime data from the AllNICrimeData

library(dplyr)

# complete.cases function to get the non-NA location value
completeFun <- function(df, desiredCols) {
  completeVec <- complete.cases(df[, desiredCols])
  return(df[completeVec, ])
}

df <- completeFun(df, "Location")

# selecting 1000 randon sample in which location in not NA from complete case function
sample <- df[1:999,]

sample



#writing the sample file as random_crime_sample
write.csv(sample, " random_crime_sample.csv", row.names=FALSE)

#tidyverse library to import and manipulate the data and the ggmap library to do the actual geocoding and mapping.

library(tidyverse)
library(ggmap)


# reducing columns for easy merge 

locations 
locations <- distinct(sample, Month, Location, Crime.type, Latitude, Longitude)

NiPostCode1 <- distinct(NiPostcodes, Postcode, Location)
NiPostCode1


# converting both columns to factor for match while merging

locations$Location <- as.factor(locations$Location)
str(locations)

NiPostCode1$Location <- as.factor(NiPostCode1$Location)
str(NiPostCode1)



# to lower case in order to match primary atribute "Location"


locations$Location <- tolower(locations$Location)
locations

NiPostCode1$Location <- tolower(NiPostCode1$Location)
NiPostCode1



# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

locations$Location <- trim(locations$Location)

NiPostCode1$Location <- trim(NiPostCode1$Location)



#Delete Duplicates and Merging to find suitable postcode value from the postcode dataset
find_a_postcode  <- merge(x = locations, y = NiPostCode1[!duplicated(NiPostCode1$Location),], by = "Location", all.x = TRUE)

find_a_postcode 


# removing NA values from random_crime_sample data
random_crime_sample <- na.omit(find_a_postcode)

random_crime_sample


# storing the postcode output on _updated_random_crime_sample
write.csv(random_crime_sample, "random_crime_sample.csv", row.names=FALSE)

#structure of modified crime data sample
str(random_crime_sample)

# total number of rows 
nrow(random_crime_sample)


###################################################


# Task G

random_crime_sample <- updated_random_sample

#sorting postcode starting by BT1 and crime type
library(stringr)
chart_data <- random_crime_sample %>%
  select(Crime.type, Postcode) %>%   
  filter(str_detect(Postcode, "BT1"))

chart_data

write.csv(updated_random_sample, "chart_data.csv", row.names=FALSE)

# Summary statistics 

summary(chart_data)


# Task H
# plotting

#table() uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.
table(chart_data$Postcode)

barplot(table(chart_data$Postcode), col = rainbow(20), main="Crime and postcode",
        xlab="Poscodes",
        ylab="Total number of Anti-socialbehaviour", space = 10)


