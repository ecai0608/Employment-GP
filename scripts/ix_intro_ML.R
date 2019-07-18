# METADATA ====
# Description: Intro to ML on youth data
# Created: 2019-06-24 (Neil Rankin)
# Updated: 2019-07-05 (Neil Rankin)
# Reviewed: NA

# SUMMARY: EDA and regex

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
options(tibble.print_max = 15)

df <- read.csv("data/raw/teaching_training_data.csv")

# some wrangling (discuss how to structure project workflow)

# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)

df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

#df <- df %>% mutate(working = as.factor(working))


df <- df %>% mutate(working = as.factor(working))


train <- filter(df, survey_num == 1) %>% mutate(age_at_0 = interval(dob, as.Date(survey_date_month) %m-% months(4))/years(1)) %>% 
  select(-company_size, -survey_date_month, -job_start_date, -job_leave_date, -monthly_pay) %>% 
  select(-financial_situation_5years, -financial_situation_now, -X, -survey_num) %>%
  select(-unid) %>%  #We should not need the unique identifier - this is only one survey, so every individual should be unique already
  select(-peoplelive_15plus, -numearnincome, -province)


nrow(train)
names(train)

train <- filter(train, !is.na(gender), !is.na(working), !is.na(volunteer), !is.na(leadershiprole), !is.na(peoplelive), !is.na(numchildren), !is.na(anygrant), !is.na(anyhhincome), !is.na(givemoney_yes), !is.na(fin_situ_now), !is.na(fin_situ_future), !is.na(fin_situ_change), !is.na(age_at_0))

nrow(train)

# IMPUTING THE DATA

preProcValues <- preProcess(train, method = c("knnImpute"))
train_imp <- predict(preProcValues, train)




skim_to_wide(train)






partition <- createDataPartition(train$working, p = 0.8, list = FALSE)
train.train <- train[partition, ]
train.test <- train[partition, ]

train.train <- train.train %>% filter(!is.na(gender), !is.na(numchildren), !is.na(age_at_0))
train.test <- train.test %>% filter(!is.na(gender), !is.na(numchildren), !is.na(age_at_0))


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_glm <- train(working ~ ., train.train, method = "rpart", trControl = trControl, metric = "Kappa")



predictions <- predict(model_glm, train.test)

predictions


hold <- table(predictions, train.test$working)
confusionMatrix(hold)

fancyRpartPlot(model_glm$finalModel)



ggplot(data = df) + 
  geom_bar(mapping = aes(x = fin_situ_now))


ggplot(data = df) + 
  geom_point(mapping = aes(x = fin_situ_now, y = fin_situ_change))

# hmm doesn't look right
ggplot(data = df) + 
  geom_jitter(mapping = aes(x = fin_situ_now, y = fin_situ_change))

# can also 'facet'


ggplot(data = df) + 
  geom_bar(mapping = aes(x = fin_situ_now)) + 
  facet_wrap(~fin_situ_future)



# think about the 'contraints' you have placed on the variable through its construction




# Now split into testing and training



# create training data


set.seed(1234)


df_train_index <- df %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)


# opportunity to discuss joins
# http://stat545.com/bit001_dplyr-cheatsheet.html

df_train <- left_join(df_train_index, df)

df_test <- anti_join(df, df_train_index)


# Modelling time


# run a regression

reg1 <- lm(working ~ gender, data = df_train)

summary(reg1)

predict(reg1, df_test)



reg2 <- lm(working ~ gender + fin_situ_now + anyhhincome, data = df_train)

summary(reg2)

# how does below differ?
reg3 <- lm(working ~ gender + as.factor(fin_situ_now) + anyhhincome, data = df_train)

summary(reg3)

# discussion of what this means

# predict

df_pred3 <- as.data.frame(predict.lm(reg3, df_test)) %>% 
  rename(pred3 = "predict.lm(reg3, df_test)")


# then bind together
df_pred3 <- bind_cols(df_test, df_pred3)


# now manually classify
stat.desc(df_pred3$pred3)
quantile(df_pred3$pred3, na.rm = TRUE)

ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3))


ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3, colour = gender))


# pick say 30%

df_pred3 <- df_pred3 %>% 
  mutate(binary_pred3 = case_when(pred3 >= 0.3 ~ TRUE, 
                                  pred3 < 0.3 ~ FALSE))


hold2 <- table(df_pred3$binary_pred3, df_pred3$working)
confusionMatrix(hold2)


# Might be easier to group_by

confusion_matrix <- df_pred3 %>% 
  filter(!is.na(binary_pred3)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred3) %>% 
  dplyr::summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()

# ggplot?
ggplot(confusion_matrix) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred3), stat = 'identity')

# proportions
confusion_matrix <- confusion_matrix %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)

# Is this model good or bad?
# Why?
