# Load libraries
library(tidyverse)
library(ggthemes)
library(rsample)
library(pROC)
library(rpart)
library(rpart.plot)

# Set working directories
setwd("~/Documents/Data Science/Personal Project/fitness_class_project")



# Import data
goalzone_fc <- read_csv("https://raw.githubusercontent.com/xrander/fitness_class_project/master/fitness_class_2212.csv")

# Investigate the data frame

# Data Validation
str(goalzone_fc) # structure of the data
# All columns columns do not follow the data description except 'weight', and 'month_as_member'
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


#Relationship between Attendance and Number of Months as Member

goalzone_fc %>% ggplot(aes(months_as_member, attended))+
  geom_point()+
  geom_smooth(se = F,
              method = "glm",
              method.args = list(family = "binomial")) +
  labs(x = "Months as member",
       y = "Attended",
       title = "Relationship between Attendance and Number of Months as Member")+
  theme_igray()

goalzone_fc %>% ggplot(aes(factor(attended, labels = c("Absent", "Present")), months_as_member))+
  geom_boxplot()+
  labs(x = "Attendance",
       y = "Months as Members")+
  theme_igray()

# Type of Machine Learning Problem
#### To predict the outcome of attendance, a logistic regression or Decision Tree can be used
goalzone_fc_md_data <- goalzone_fc %>%
  select(-1) # preparing the data

# Model 1

# Set seed
set.seed(50)

# Split the data to train and test data
split <- initial_split(goalzone_fc_md_data, prop = 0.7)
goalzone_train <- training(split) #training data
goalzone_test <- testing(split) #testing data

# Train the model
goalzone_model <- glm(attended ~ .,
                      data = goalzone_train,
                      family = "binomial")

summary(goalzone_model)

# Make predictions with the model
goalzone_test$pred <-predict(goalzone_model, goalzone_test, type = "response")

goalzone_test <- goalzone_test %>%
  mutate(pred = ifelse(pred < 0.5, 0, 1)) 

mean(goalzone_test$attended)
mean(goalzone_test$pred)

# Build the confusion matrix
table(goalzone_test$attended, goalzone_test$pred)

# Model (logistic regression) Prediction performance
mean(goalzone_test$attended == goalzone_test$pred)

# Calculate the ROC
roc_curve <- roc(goalzone_test$attended, goalzone_test$pred)

# Visual model performance using ROC(Receiver Operating Characteristics Cost)
plot(roc_curve,
     col = "red",
     main = "Logistic Regression ROC Curve")
text(2, 2,
     "AUC = 0.75",
     adj=c(1,0.5),
     font = 1,
     cex = 1.0,
     col = "red")

# Estimate the Area under the curve of the ROC
auc(roc_curve) #area under the curve is 0.7517

# Model 2
## Decision trees (rpart) will be used as the second model
goalzone_model2 <-  rpart(attended ~ .,
                          goalzone_train,
                          method = "class")

# Visualize the decision tree
rpart.plot(goalzone_model2,
           type = 4,
           box.palette = c("red", "green"),
           fallen.leaves = TRUE)

# Predict the outcome using model 2
goalzone_test <- goalzone_test %>%
  mutate(pred2 = as.numeric(as.character(predict(goalzone_model2, goalzone_test, type = "class"))))

# Compare model with actual outcome
mean(goalzone_test$attended)
mean(goalzone_test$pred2)

# Build the confusion matrix
table(goalzone_test$attended, goalzone_test$pred2)

# Model2 (Decision trees) prediction performance 
mean(goalzone_test$attended == goalzone_test$pred2)

roc_curve2<- roc(goalzone_test$attended, goalzone_test$pred2)
plot(roc_curve2,
     col = "blue",
     main = "Decision Tree ROC Curve")
text(2, 3, "AUC = 0.66",
     adj = c(1, 0.5),
     cex = 1.0,
     font = 1,
     col = "blue")

# Estimate the Area Under the Curve for the Decision Model ROC 
auc(roc_curve2)