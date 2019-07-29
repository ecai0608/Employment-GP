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
library(gridExtra)


# turn off scientific notation
options(scipen=999)
options(tibble.print_max = 30)


# load the unprocessed data
df <- read.csv("data/raw/teaching_training_data.csv")


barplot(table(df$numearnincome))


# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(numearnincome = parse_number(as.character(numearnincome)), 
         numchildren = parse_number(as.character(numchildren)),
         peoplelive = as.numeric(parse_number(as.character(peoplelive))))


# setting working to be a factor
df <- df %>% mutate(working = as.factor(working))


# importing all of the test scores
CFT <- read.csv("data/raw/teaching_training_data_cft.csv")
COM <- read.csv("data/raw/teaching_training_data_com.csv")
GRIT <- read.csv("data/raw/teaching_training_data_grit.csv")
NUM <- read.csv("data/raw/teaching_training_data_num.csv")
OPT <- read.csv("data/raw/teaching_training_data_opt.csv")


# removing all duplicate entries based off of unique identifiers
CFT <- distinct(CFT, unid, .keep_all = TRUE)
COM <- distinct(COM, unid, .keep_all = TRUE)
GRIT <- distinct(GRIT, unid, .keep_all = TRUE)
NUM <- distinct(NUM, unid, .keep_all = TRUE)
OPT <- distinct(OPT, unid, .keep_all = TRUE)


df <- df %>% left_join(CFT, by = "unid") %>% select(-X.x, -X.y) %>% 
  left_join(COM, by = "unid") %>% select(-X) %>% 
  left_join(GRIT, by = "unid") %>% select(-X) %>% 
  left_join(NUM, by = "unid") %>% select(-X) %>% 
  left_join(OPT, by = "unid") %>% select(-X)



df <- df %>% mutate(cft_score = as.factor(cft_score)) %>% 
  mutate(com_score = as.factor(com_score)) %>% 
  mutate(grit_score = as.factor(grit_score)) %>% 
  mutate(num_score = as.factor(num_score)) %>% 
  mutate(opt_score = as.factor(opt_score))



#####
data <- filter(df, survey_num == 1) %>% # look only at the first survey
  distinct(unid, .keep_all = TRUE) %>% # remove the duplicates for each unique identifier
  filter(unid != 17) # removed one entry where a woman entered survey date for her date of birth


# calculating the baseline age and truncating to a whole number
data <- data %>% mutate(age = interval(dob, as.Date(survey_date_month) %m-% months(4))/years(1)) %>% 
  mutate(age = floor(age))


# dropping irrelevant/redundant columns
data <- data %>% select(-company_size, -survey_date_month, -job_start_date, -job_leave_date, -monthly_pay) %>% 
  select(-financial_situation_5years, -financial_situation_now, -survey_num, -dob, -unid, -peoplelive_15plus)



# ---------- RUN THIS BLOCK TO EXAMINE NA TRENDS IN ALL OF THE FEATURES
examineNA_givemoney <- mutate(data, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
  group_by(givemoney_yes) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
  mutate(probability = total_true/(total_false + total_true))
examineNA_givemoney <- mutate(examineNA_givemoney, givemoney_yes = ifelse(is.na(givemoney_yes), "None", givemoney_yes))

givemoney_plot <- ggplot(examineNA_givemoney) + 
  aes(x = givemoney_yes, y = probability) + 
  geom_point()
givemoney_plot


examineNA_numchildren <- mutate(data, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
  group_by(numchildren) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
  mutate(probability = total_true/(total_false + total_true))
examineNA_numchildren <- mutate(examineNA_numchildren, numchildren = ifelse(is.na(numchildren), "None", numchildren))


examineNA_com_score <- mutate(data, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
  group_by(com_score) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
  mutate(probability = total_true/(total_false + total_true))
examineNA_com_score <- mutate(examineNA_com_score, com_score = ifelse(is.na(com_score), "None", com_score))



examineNA_province <- mutate(data, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
  group_by(province) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
  mutate(probability = total_true/(total_false + total_true))
examineNA_province <- mutate(examineNA_province, province = ifelse(is.na(province), "None", province))



numchildren_plot <- ggplot(examineNA_numchildren) + 
  aes(x = numchildren, y = probability) + 
  geom_point()

province_plot <- ggplot(examineNA_province) + 
  aes(x = province, y = probability) + 
  geom_point()

com_score_plot <- ggplot(examineNA_com_score) + 
  aes(x = com_score, y = probability) + 
  geom_point()

grid.arrange(givemoney_plot, numchildren_plot, province_plot, com_score_plot, ncol = 2)





total_na <- rowSums(is.na(data))
# data <- data %>% cbind(total_na)
data <- data %>% cbind(total_na) %>% mutate(total_na = as.factor(ifelse(total_na == 0, 0, 1)))

test <- glm(working ~ total_na, data, family = "binomial")
summary(test)


# ---------------


# INSERT ANALYSIS OF NA VALUES - IN PARTICULAR, INDIVIDUALS THAT DO NOT RESPOND TO SURVEY QUESTIONS SEEM TO BE MORE LIKELY
# TO NOT WORK
# FEATURES: province, volunteer, leadershiprole, peoplelive, numchildren, numearnincome, givemoney_yes, fin_situ_'s
data <- data %>% mutate(province = as.factor(ifelse(is.na(province), "None", as.character(province)))) %>% 
  mutate(volunteer = as.factor(ifelse(is.na(volunteer), "None", as.character(volunteer)))) %>% 
  mutate(leadershiprole = as.factor(ifelse(is.na(leadershiprole), "None", as.character(leadershiprole)))) %>%
  # mutate(peoplelive = as.factor(ifelse(is.na(peoplelive), "None", as.character(peoplelive)))) %>%
  mutate(numchildren = as.factor(ifelse(is.na(numchildren), "None", as.character(numchildren)))) %>%
  # mutate(numearnincome = as.factor(ifelse(is.na(numearnincome), "None", as.character(numearnincome)))) %>%
  mutate(givemoney_yes = as.factor(ifelse(is.na(givemoney_yes), "None", as.character(givemoney_yes)))) %>%
  mutate(fin_situ_now = as.factor(ifelse(is.na(fin_situ_now), "None", as.character(fin_situ_now)))) %>%
  mutate(fin_situ_future = as.factor(ifelse(is.na(fin_situ_future), "None", as.character(fin_situ_future)))) %>% 
  mutate(cft_score = as.factor(ifelse(is.na(cft_score), "None", as.character(cft_score)))) %>% 
  mutate(com_score = as.factor(ifelse(is.na(com_score), "None", as.character(com_score)))) %>% 
  mutate(num_score = as.factor(ifelse(is.na(num_score), "None", as.character(num_score)))) %>% 
  mutate(opt_score = as.factor(ifelse(is.na(opt_score), "None", as.character(opt_score))))


data <- filter(data, !is.na(gender), !is.na(age))



# We do the test-train split here, because we need to center and scale the data to perform imputation. Leaving the test
# data in would allow information leakage from the test set to the train set.
set.seed(13)
partition <- createDataPartition(data$working, p = 0.8, list = FALSE)
train <- data[partition, ]
test <- data[-partition, ]



# UPSAMPLING TO BALANCE WORKING CLASSES
train_employed = filter(train, working == TRUE)
train_unemployed = filter(train, working == FALSE)

nrow(train_employed)
nrow(train_unemployed)
difference <- nrow(train_unemployed) - nrow(train_employed)
difference

upsamples <- train_employed[sample(nrow(train_employed), nrow(train_employed), replace = TRUE),]

train_employed <- rbind(train_employed, upsamples)
train_upsampled <- rbind(train, train_employed)



# storing the means and variances of numearnincome and peoplelive to reverse the scaling done by knnImpute
nei_mean <- mean(na.omit(train$numearnincome))
nei_var <- var(na.omit(train$numearnincome))

pl_mean <- mean(na.omit(train$peoplelive))
pl_var <- var(na.omit(train$peoplelive))

# imputing the data
impute_train <- preProcess(select(train, -working), "knnImpute")
train <- predict(impute_train, train)

# rescaling the data
train <- train %>% mutate(numearnincome = (numearnincome*sqrt(nei_var)) + nei_mean,
                       peoplelive = (peoplelive*sqrt(pl_var)) + pl_mean)

# rounding the data to integer values
train <- train %>% mutate(numearnincome = round(numearnincome),
                          peoplelive = round(peoplelive))



# ---- DOWNSIDE OF THIS: ALL OF THE FACTORS MUST BE TURNED INTO FACTORS ----
# NUMERIC FEATURES THAT SHOULD BE CONSIDERED: peoplelive, numchildren, numearnincome, fin_situ's


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# without upsampling
model_tree <- train(working ~ ., train, method = "rpart", na.action = na.pass, 
                    trControl = trControl, metric = "Kappa")

# with upsampling
model_tree_upsampled <- train(working ~ ., train, method = "rpart", na.action = na.pass, 
                    trControl = trControl, metric = "Kappa")


# predicting using both models
predictions <- predict(model_tree, test, na.action = na.rpart)
predictions_upsampled <- predict(model_tree, test, na.action = na.rpart)


# comparing performance of both models
hold <- table(predictions, test$working)
confusionMatrix(hold)

hold_upsampled <- table(predictions, test$working)
confusionMatrix(hold_upsampled)


# in general, VERY BAD SPECIFICITY - the model predicts FALSE too much, even when "working" is TRUE


# I would not recommend running this line - the resulting tree has around 2 billion nodes. Will have to fix later.
fancyRpartPlot(model_tree$finalModel)

