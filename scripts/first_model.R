library(dplyr)
library(tidyverse)
library(pastecs)
library(caret)
library(lubridate)
library(rpart)
library(party)
library(partykit)
library(rattle)
library(skimr)


# turn off scientific notation
options(scipen=999)
options(tibble.print_max = Inf)

# load the unprocessed data
df <- read.csv("data/raw/teaching_training_data.csv")


# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)


# setting working to be a factor
df <- df %>% mutate(working = as.factor(working))

train <- filter(df, survey_num == 1) %>% # look only at the first survey
  distinct(unid, .keep_all = TRUE) %>% # remove the duplicates for each unique identifier
  mutate(age_at_0 = interval(dob, as.Date(survey_date_month) %m-% months(4))/years(1)) %>% # calculating the baseline age
  select(-company_size, -survey_date_month, -job_start_date, -job_leave_date, -monthly_pay) %>% # dropping columns about job specifics - we only care about whether or not they were employed
  select(-financial_situation_5years, -financial_situation_now, -X, -survey_num, -dob, -fin_situ_change) %>% # dropping redundant columns
  select(-unid) %>%  #We should not need the unique identifier - this is only one survey, so every individual should be unique already
  select(-peoplelive_15plus, -numearnincome, -province) # dropping columns with the most NA's to reduce the potential bias of imputation

names(train)

# checking size of training data
nrow(train)

# dropping all of the rows where every entry is NA - these rows cannot be imputated because they do not have any prior information
train <- filter(train, !is.na(gender), !is.na(working), !is.na(volunteer), !is.na(leadershiprole), !is.na(peoplelive), !is.na(numchildren), !is.na(anygrant), !is.na(anyhhincome), !is.na(givemoney_yes), !is.na(fin_situ_now), !is.na(fin_situ_future), !is.na(age_at_0))


# checking how many training points are left
nrow(train)




# IMPUTING THE DATA

preProcValues <- preProcess(train, method = c("knnImpute"))
train_imp <- predict(preProcValues, train)


skim_to_wide(train)

set.seed(13)
partition <- createDataPartition(train$working, p = 0.8, list = FALSE)
# sorry the naming is very poor here
train.train <- train[partition, ]
train.test <- train[partition, ]

train.train <- train.train %>% filter(!is.na(gender), !is.na(numchildren), !is.na(age_at_0))
train.test <- train.test %>% filter(!is.na(gender), !is.na(numchildren), !is.na(age_at_0))


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_tree <- train(working ~ ., train.train, method = "rpart", trControl = trControl, metric = "Kappa",
                   tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.00025)))



predictions <- predict(model_tree, train.test)

predictions


hold <- table(predictions, train.test$working)
confusionMatrix(hold)

# VERY BAD SPECIFICITY - the model predicts FALSE too much, even when "working" is TRUE


# I would not recommend running this line - the resulting tree has around 2 billion nodes. Will have to fix later.
# fancyRpartPlot(model_tree$finalModel)

summary(model_tree$finalModel)

