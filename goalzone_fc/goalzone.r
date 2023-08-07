# Load libraries
library(tidyverse)
library(ggthemes)

# Set working directories
setwd("~/Documents/Data Science/Personal Project/fitness_class_project")

# Import data
goalzone_fc <- read_csv("https://raw.githubusercontent.com/xrander/fitness_class_project/master/fitness_class_2212.csv")

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
         attended = factor(attended, levels = c(1,0), labels = c("yes", "no")))

summary(goalzone_fc) # quick summary statistics of the data
# weight is having 20 missing values

#replacing na in weight
goalzone_fc <- goalzone_fc %>%
  mutate(weight = replace_na(weight, mean(weight, na.rm = T)),
         weight = round(weight, 2))


goalzone_fc %>%
  ggplot(aes(category, fill = time))+
  geom_bar(position = "dodge")+
  facet_wrap(~attended)

goalzone_fc %>%
  ggplot(aes(time, fill = day_of_week))+
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = 2,
                    direction = -1)


ggplot(goalzone_fc, aes(time, fill = day_of_week))+
  geom_bar(position = "dodge")+
  facet_wrap(~attended)+
  scale_fill_viridis_d()+
  theme_clean()


## Number of months as a member
ggplot(goalzone_fc, aes(months_as_member))+
  geom_histogram(binwidth = 2)

max(goalzone_fc$months_as_member)
