# CLUSTERING HARAMBEE DATA BASED ON 5-TEST SCORES


library(lubridate)
library(tidyverse)
library(ggplot2)
library(cluster)
library(pastecs)
library(corrplot)
library(skimr)

# importing all of the test scores
CFT <- read.csv("data/raw/teaching_training_data_cft.csv")
COM <- read.csv("data/raw/teaching_training_data_com.csv")
GRIT <- read.csv("data/raw/teaching_training_data_grit.csv")
NUM <- read.csv("data/raw/teaching_training_data_num.csv")
OPT <- read.csv("data/raw/teaching_training_data_opt.csv")


skim_to_wide(OPT)

# removing all NAs from the test scores
CFT <- na.omit(CFT)
COM <- na.omit(COM)
GRIT <- na.omit(GRIT)
NUM <- na.omit(NUM)
OPT <- na.omit(OPT)

# removing all duplicate entries based off of unique identifiers
CFT.dist <- distinct(CFT, unid, .keep_all = TRUE)
COM.dist <- distinct(COM, unid, .keep_all = TRUE)
GRIT.dist <- distinct(GRIT, unid, .keep_all = TRUE)
NUM.dist <- distinct(NUM, unid, .keep_all = TRUE)
OPT.dist <- distinct(OPT, unid, .keep_all = TRUE)



# merging data sets into 1 dataframe



combined_scores <- merge(CFT.dist, COM.dist, by = "unid") %>% select(-X.x, -X.y) %>% 
  merge(GRIT.dist, by = "unid") %>% select(-X) %>% 
  merge(NUM.dist, by = "unid") %>% select(-X) %>% 
  merge(OPT.dist, by = "unid") %>% select(-X)

# centering and scaling the data to allow better performance with k-means
unid <- combined_scores$unid

scores <- select(combined_scores, -unid) %>% mutate(cft_score = scale(cft_score, center = TRUE, scale = TRUE),
                                                    com_score = scale(com_score, center = TRUE, scale = TRUE),
                                                    grit_score = scale(grit_score, center = TRUE, scale = TRUE),
                                                    num_score = scale(num_score, center = TRUE, scale = TRUE),
                                                    opt_score = scale(opt_score, center = TRUE, scale = TRUE))

corrplot(cor(scores), method = "square")

scores <- select(scores, -grit_score, -opt_score)
corrplot(cor(scores), method = "square")

ggplot(scores) + 
  aes(x = com_score, y = cft_score) + 
  geom_point()

set.seed(123)
trials <- 1:50
tot_within_ss <- sapply(trials, function(k) {
  print(k)
  cl <- kmeans(scores, k, nstart = 20)
  cl$tot.withinss
})

plot(trials, tot_within_ss, type = "b")
