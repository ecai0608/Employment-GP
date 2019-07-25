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
library(corrplot)


# turn off scientific notation
options(scipen=999)
options(tibble.print_max = 30)


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
  filter(unid != 17) # removed one entry where a woman entered survey date for her date of birth


# calculating the baseline age and truncating to a whole number
train <- train %>% mutate(age = interval(dob, as.Date(survey_date_month) %m-% months(4))/years(1)) %>% 
  mutate(age = floor(age))


# dropping irrelevant/redundant columns
train <- train %>% select(-company_size, -survey_date_month, -job_start_date, -job_leave_date, -monthly_pay) %>% 
  select(-financial_situation_5years, -financial_situation_now, -X, -survey_num, -dob, -fin_situ_change, -unid) %>% 
  select(-peoplelive_15plus) # SHOW ANALYSIS OF THIS COLUMN - NO CORRELATION TO LIKELIHOOD OF WORKING

# parsing numeric values out of responses for numearnincome, numchildren, peoplelive
train <- train %>% mutate(numearnincome = parse_number(as.character(numearnincome)), 
                          numchildren = parse_number(as.character(numchildren)),
                          peoplelive = parse_number(as.character(peoplelive)))



# ---------- RUN THIS BLOCK TO EXAMINE NA TRENDS IN ALL OF THE FEATURES
# total_na <- rowSums(is.na(train))
# train_na <- cbind(train, total_na)

# names(train_na)

# examineNA <- mutate(train_na, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
#   group_by(total_na) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
#   mutate(ratio = total_true/(total_false + total_true))
# examineNA
# ---------------



# INSERT ANALYSIS OF NA VALUES - IN PARTICULAR, INDIVIDUALS THAT DO NOT RESPOND TO SURVEY QUESTIONS SEEM TO BE MORE LIKELY
# TO NOT WORK
# FEATURES: province, volunteer, leadershiprole, peoplelive, numchildren, numearnincome, givemoney_yes, fin_situ_'s
train <- train %>% mutate(province = as.factor(ifelse(is.na(province), "None", as.character(province)))) %>% 
  mutate(volunteer = as.factor(ifelse(is.na(volunteer), "None", as.character(volunteer)))) %>% 
  mutate(leadershiprole = as.factor(ifelse(is.na(leadershiprole), "None", as.character(leadershiprole)))) %>% ###CUT OFF HERE
  mutate(peoplelive = as.factor(ifelse(is.na(peoplelive), "None", as.character(peoplelive)))) %>%
  mutate(numchildren = as.factor(ifelse(is.na(numchildren), "None", as.character(numchildren)))) %>%
  mutate(numearnincome = as.factor(ifelse(is.na(numearnincome), "None", as.character(numearnincome)))) %>%
  mutate(givemoney_yes = as.factor(ifelse(is.na(givemoney_yes), "None", as.character(givemoney_yes)))) %>%
  mutate(fin_situ_now = as.factor(ifelse(is.na(fin_situ_now), "None", as.character(fin_situ_now)))) %>%
  mutate(fin_situ_future = as.factor(ifelse(is.na(fin_situ_future), "None", as.character(fin_situ_future))))

# ---- DOWNSIDE OF THIS: ALL OF THE FACTORS MUST BE TURNED INTO FACTORS ----
# NUMERIC FEATURES THAT SHOULD BE CONSIDERED: peoplelive, numchildren, numearnincome, fin_situ's
# CLUSTER/CATEGORIZE PEOPLELIVE, numearnincome


# still need to use OPT, CPT, COM, etc.


as_tibble(train)
skim_to_wide(train)

names(train)


set.seed(13)
partition <- createDataPartition(train$working, p = 0.8, list = FALSE)
# sorry the naming is very poor here
train.train <- train[partition, ]
train.test <- train[-partition, ]



# still need to try upsampling...I don't think ensemble methods do anything for us here


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_tree <- train(working ~ ., train.train, method = "rpart", na.action = na.pass, 
                    trControl = trControl, metric = "Kappa")



predictions <- predict(model_tree, train.test, na.action = na.rpart)
predictions

hold <- table(predictions, train.test$working)
confusionMatrix(hold)

# VERY BAD SPECIFICITY - the model predicts FALSE too much, even when "working" is TRUE


# I would not recommend running this line - the resulting tree has around 2 billion nodes. Will have to fix later.
# fancyRpartPlot(model_tree$finalModel)

# ok fuck imputation it wasn't even used in the original model lol

summary(model_tree$finalModel)
modelLookup("rpart")
