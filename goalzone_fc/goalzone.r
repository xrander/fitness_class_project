# Load libraries
library(tidyverse)

# Set working directories
setwd("~/Documents/Data Science/Personal Project/fitness_class_project")

# Import data
goalzone_fc <- read_csv("fitness_class_2212.csv")

# Investigate the data frame

str(goalzone_fc) # structure of the data
# All columns except 'weight', and 'month_as_member' columns doesn't follow the data description
#The other columns are better mutated as factors.

# Checking for unique values in columns
goalzone_col <- colnames(goalzone_fc)[5:8] # Extract column names of interest

lapply(goalzone_fc[,goalzone_col], unique) # Check for unique values


# Mutate columns that do not follow data description to write data type
goalzone_fc <- goalzone_fc %>%
  mutate(booking_id = factor(booking_id),
         days_before = factor(days_before),
         day_of_week = factor(ifelse(day_of_week == "Wednesday", "Wed",
                              ifelse(day_of_week == "Fri.", "Fri",
                                     ifelse(day_of_week == "Monday", "Mon", day_of_week)))),
         time = factor(time, levels = c("AM", "PM"),ordered = T),
         category = factor(ifelse(category == "-", "Unknown", category)),
         attended = factor(attended, levels = c(1,0)))

summary(goalzone_fc) # quick summary statistics of the data
# weight is having 20 missing values

#replacing na in weight
goalzone_fc <- goalzone_fc %>%
  mutate(weight = replace_na(weight, mean(weight, na.rm = T)))

