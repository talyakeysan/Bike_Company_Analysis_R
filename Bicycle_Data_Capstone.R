library(tidyverse)  #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

colnames(Divvy_Trips_2020_Q1)

# Inspect the dataframes and look for incongruencies

str(Divvy_Trips_2020_Q1)
all_trips <- Divvy_Trips_2020_Q1

# Remove lat, long, fields as this data was dropped beginning in 2020

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

all_trips <- all_trips %>% 
  select(-c(X, X.1, X.2))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


#changed the chr format of "started_at" to date and removed the time with H:M format to turn date only
all_trips$started_at <- sub("(?<=^| )([0-9])\\.", "0\\1.", all_trips$started_at, perl = TRUE)  # Pad single-digit days
all_trips$started_at <- sub("\\.([0-9])\\.", ".0\\1.", all_trips$started_at, perl = TRUE)  # Pad single-digit months

# Then convert to date
all_trips$date <- as.Date(as.POSIXct(all_trips$started_at, format = "%d.%m.%Y %H:%M"))

str(all_trips$date)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
# Convert 'started_at' and 'ended_at' to POSIXct
all_trips$started_at <- as.POSIXct(all_trips$started_at, format = "%d.%m.%Y %H:%M")
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format = "%d.%m.%Y %H:%M")

#ride length as minutes
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")
is.character(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)
str(all_trips_v2)
# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member.casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member.casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member.casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member.casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member.casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member.casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member.casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member.casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member.casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')




