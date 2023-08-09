# Load libraries
library(tidyverse)
library(ggthemes)
library(e1071)

# Set working directories
setwd("~/Documents/Data Science/Personal Project/fitness_class_project")



# Import data
goalzone_fc <- read_csv("https://raw.githubusercontent.com/xrander/fitness_class_project/master/fitness_class_2212.csv")

# Investigate the data frame

# Data Validation
str(goalzone_fc) # structure of the data
# All columns except 'weight', and 'month_as_member' columns doesn't follow the data description
#The other columns are better mutated as factors.

# Checking for duplicate data
unique(duplicated(goalzone_fc[,])) # There are no duplicate data

summary(goalzone_fc)
# The data type should be numeric for better optimization, but it does not really matter
# Data range for this data type doesn't matter in this case

## Weight is having 20 missing data and the missing data will be replaced using the mean, passed through an ifelse statement
## days before is also having the wrong format. It was imported as a character data type and will be coarsed into numeric data
## day of the week, time, category, and attended should be in the factor data format


# Checking for unique values in columns
goalzone_col <- colnames(goalzone_fc)[4:8] # Extract column names of interest

lapply(goalzone_fc[,goalzone_col], unique) # Check for unique values


# Mutate columns that do not follow data description to write data type
goalzone_fc <- goalzone_fc %>%
  mutate(booking_id = as.numeric(booking_id),
         #trim strings, remove all days which is a recurring character and coarse the figures into numeric data type
         days_before = as.numeric(str_remove_all(str_trim(days_before), "days")),
         # rename the observations to aid consistency and coarse to factor data type
         day_of_week = factor(ifelse(day_of_week == "Wednesday", "Wed",
                              ifelse(day_of_week == "Fri.", "Fri",
                                     ifelse(day_of_week == "Monday", "Mon", day_of_week)))),
         # replace missing data with mean of weights and round to 2 decimal place
         weight = round(replace_na(weight, mean(weight, na.rm = T)), 2),
         time = factor(time, levels = c("AM", "PM"),ordered = T),
         category = factor(ifelse(category == "-", "Unknown", category)),
         attended = factor(attended, levels = c(1,0), labels = c("yes", "no")))


summary(goalzone_fc) # quick summary statistics of the data

# Single variable 
goalzone_fc %>% ggplot(aes(days_before))+
  geom_histogram()

goalzone_fc %>%
  ggplot(aes(weight)) +
  geom_histogram(binwidth = 3)

goalzone_fc %>% ggplot(aes(months_as_member))+
  geom_density()+
  labs(y = "density",
       x = "months as member") #a

# Multiple variable visualization
ggplot(goalzone_fc, aes(attended))+
  geom_bar() # no has more observation

ggplot(goalzone_fc, aes(attended, fill = category))+
  geom_bar(position = "dodge")+
  theme_igray()

goalzone_fc %>%
  ggplot(aes(category, fill = time))+
  geom_bar(position = "dodge")+
  facet_wrap(~attended)+
  scale_fill_brewer(palette = 18,
                    direction = -1)
  theme_bw

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

