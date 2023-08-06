# Load libraries
library(tidyverse)

# Set working directories
setwd("~/Documents/Data Science/Personal Project/fitness_class_project")

# Import data
goalzone_fc <- read_csv("fitness_class_2212.csv")

# Investigate the data frame

str(goalzone_fc) # structure of the data

summary(goalzone_fc)

# Checking for column with missing value
missing_values <- is.na(goalzone_fc)
col_with_missing <- colSums(missing_values)
col_with_missing
goalzone_fc[is.na(goalzone_fc$weight),]
