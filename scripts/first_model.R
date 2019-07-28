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


barplot(table(df$numearnincome))


# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)


# setting working to be a factor
df <- df %>% mutate(working = as.factor(working))


# importing all of the test scores
CFT <- read.csv("data/raw/teaching_training_data_cft.csv")
COM <- read.csv("data/raw/teaching_training_data_com.csv")
GRIT <- read.csv("data/raw/teaching_training_data_grit.csv")
NUM <- read.csv("data/raw/teaching_training_data_num.csv")
OPT <- read.csv("data/raw/teaching_training_data_opt.csv")


# removing all NAs from the test scores
CFT <- na.omit(CFT)
COM <- na.omit(COM)
GRIT <- na.omit(GRIT)
NUM <- na.omit(NUM)
OPT <- na.omit(OPT)

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
  select(-financial_situation_5years, -financial_situation_now, -survey_num, -dob, -fin_situ_change, -unid)

# parsing numeric values out of responses for numearnincome, numchildren, peoplelive
data <- data %>% mutate(numearnincome = parse_number(as.character(numearnincome)), 
                          numchildren = parse_number(as.character(numchildren)),
                          peoplelive = as.numeric(parse_number(as.character(peoplelive))),
                        peoplelive_15plus = as.numeric(parse_number(as.character(peoplelive_15plus))))



# ---------- RUN THIS BLOCK TO EXAMINE NA TRENDS IN ALL OF THE FEATURES
total_na <- rowSums(is.na(data))
# data <- data %>% cbind(total_na)
data <- data %>% cbind(total_na) %>% mutate(total_na = as.factor(ifelse(total_na == 0, 0, 1)))

names(data)

examineNA <- mutate(data, work_true = ifelse(as.logical(working), 1, 0), work_false = ifelse(as.logical(working), 0, 1)) %>% 
 group_by(numearnincome) %>% summarise(total_true = sum(work_true), total_false = sum(work_false)) %>% 
 mutate(ratio = total_true/(total_false + total_true))
examineNA



ggplot(data) +
  aes(x = numearnincome) + 
  geom_bar()


ggplot(examineNA) + 
  aes(x = numearnincome, y = ratio) + 
  geom_point()
# ---------------


# INSERT ANALYSIS OF NA VALUES - IN PARTICULAR, INDIVIDUALS THAT DO NOT RESPOND TO SURVEY QUESTIONS SEEM TO BE MORE LIKELY
# TO NOT WORK
# FEATURES: province, volunteer, leadershiprole, peoplelive, numchildren, numearnincome, givemoney_yes, fin_situ_'s
data <- data %>% mutate(province = as.factor(ifelse(is.na(province), "None", as.character(province)))) %>% 
  mutate(volunteer = as.factor(ifelse(is.na(volunteer), "None", as.character(volunteer)))) %>% 
  mutate(leadershiprole = as.factor(ifelse(is.na(leadershiprole), "None", as.character(leadershiprole)))) %>%
  #mutate(peoplelive = as.factor(ifelse(is.na(peoplelive), "None", as.character(peoplelive)))) %>%
  #mutate(peoplelive_15plus = as.factor(ifelse(is.na(peoplelive_15plus), "None", as.character(peoplelive_15plus)))) %>% 
  #mutate(numchildren = as.factor(ifelse(is.na(numchildren), "None", as.character(numchildren)))) %>%
  #mutate(numearnincome = as.factor(ifelse(is.na(numearnincome), "None", as.character(numearnincome)))) %>%
  mutate(givemoney_yes = as.factor(ifelse(is.na(givemoney_yes), "None", as.character(givemoney_yes)))) %>%
  mutate(fin_situ_now = as.factor(ifelse(is.na(fin_situ_now), "None", as.character(fin_situ_now)))) %>%
  mutate(fin_situ_future = as.factor(ifelse(is.na(fin_situ_future), "None", as.character(fin_situ_future)))) %>% 
  mutate(cft_score = as.factor(ifelse(is.na(cft_score), "None", as.character(cft_score)))) %>% 
  mutate(com_score = as.factor(ifelse(is.na(com_score), "None", as.character(com_score)))) %>% 
  mutate(num_score = as.factor(ifelse(is.na(num_score), "None", as.character(num_score)))) %>% 
  mutate(opt_score = as.factor(ifelse(is.na(opt_score), "None", as.character(opt_score))))


data <- select(data, -grit_score)

# ---- DOWNSIDE OF THIS: ALL OF THE FACTORS MUST BE TURNED INTO FACTORS ----
# NUMERIC FEATURES THAT SHOULD BE CONSIDERED: peoplelive, numchildren, numearnincome, fin_situ's
# CLUSTER/CATEGORIZE PEOPLELIVE, numearnincome


# still need to analyze OPT, CPT, COM, etc.






skim_to_wide(data)





set.seed(13)
partition <- createDataPartition(data$working, p = 0.8, list = FALSE)
# sorry the naming is very poor here
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
train <- rbind(train, train_employed)


trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_tree <- train(working ~ ., train, method = "rpart", na.action = na.pass, 
                    trControl = trControl, metric = "Kappa")




test_employed = filter(test, working == TRUE)
test_unemployed = filter(test, working == FALSE)
nrow(test_employed)
nrow(test_unemployed)
difference <- nrow(test_unemployed) - nrow(test_employed)
difference





predictions <- predict(model_tree, test, na.action = na.rpart)
predictions

hold <- table(predictions, test$working)
confusionMatrix(hold)

# VERY BAD SPECIFICITY - the model predicts FALSE too much, even when "working" is TRUE


# I would not recommend running this line - the resulting tree has around 2 billion nodes. Will have to fix later.
# fancyRpartPlot(model_tree$finalModel)

# ok fuck imputation it wasn't even used in the original model lol

summary(model_tree$finalModel)
modelLookup("rpart")


names(data)
skim_to_wide(data)
