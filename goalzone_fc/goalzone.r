# Load libraries
library(tidyverse)
library(ggthemes)
library(e1071)
library(rsample)
library(pROC)
library(rpart)

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
         category = factor(ifelse(category == "-", "Unknown", category), ordered = F),
         attended = as.numeric(attended))


summary(goalzone_fc) # quick summary statistics of the data

# Single variable 
goalzone_fc %>% ggplot(aes(days_before))+
  geom_histogram(binwidth = 3)

goalzone_fc %>% ggplot(aes(day_of_week))+
  geom_bar()

goalzone_fc %>%
  ggplot(aes(weight)) +
  geom_histogram(binwidth = 3)

# Multiple variable visualization
### Create count data of gym exercise category according to attendance
goalzone_fc_count <- goalzone_fc %>%
  mutate(attended = as.factor(attended)) %>%
  group_by(attended, category) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 

### mutate category levels, adjusted according to their count data

ggplot(goalzone_fc, aes(factor(attended), fill = category))+
  geom_bar(position = "dodge")+
  geom_text(data = goalzone_fc_count,
            aes(y = count, label = count),
            position = position_dodge(1),
            vjust = 0.02)+
  theme_igray()+
  labs(x = "Attended",
       y = "Count")+
  ggtitle("Attendance according to Gym Class Category")

# Months as member distribution
goalzone_fc %>% ggplot(aes(months_as_member))+
  geom_density()+
  labs(y = "density",
       x = "months as member")
"months as member is positively skewed or right-skewed"

goalzone_fc %>% ggplot(aes(factor(attended), months_as_member, fill = category))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.3,
   #           col = "springgreen3")+
  labs(y = "Months as member",
       x = "Attended")+
  theme_igray()


# Type of Machine Learning Problem
#### To predict the outcome of attendance, a logistic regression or Decision Tree can be used
goalzone_fc_md_data <- goalzone_fc %>%
  select(-1) # preparing the data

# Set seed
set.seed(50)

# Split the data to train and test data
split <- initial_split(goalzone_fc_md_data, prop = 0.7)
goalzone_train <- training(split) #training data
goalzone_test <- testing(split) #testing data

# Train the model
goalzone_model <- glm(attended ~ ., data = goalzone_train, family = "binomial")

summary(goalzone_model)

goalzone_test$pred <-predict(goalzone_model, goalzone_test, type = "response")

goalzone_test <- goalzone_test %>%
  mutate(pred = ifelse(pred < 0.2981, 0, 1)) # The average of the attneded here is 0.2981, hence the reason it is used

mean(goalzone_test$attended)
mean(goalzone_test$pred)

# Model Performance
mean(goalzone_test$attended == goalzone_test$pred)

# Visual model performance using ROC(Receiver Operating Characteristics Cost)
roc_curve <- roc(goalzone_test$attended, goalzone_test$pred)

plot(roc_curve, col = "red")
auc(roc_curve) #area under the curve is 0.7372