# METADATA ====
# Description: Data wrangling and visualisation of youth data
# Created: 2019-06-24 (Neil Rankin)
# Updated: 2019-06-24 (Neil Rankin)
# Reviewed: NA

# SUMMARY: EDA and regex

library(pastecs)
library(tidyverse)


# turn off scientific notation
options(scipen=999)

# Do some EDA
# compare outcomes (introduce ols)
# do regex



# look at variable

# do for financial perceptions

# clean

# feature engineer

# visualise and summarise

# check whether it varies
# by gender, age
# visualise
# regression analysis


# LOAD DATA ====

df <- read.csv("data/raw/teaching_training_data.csv")

# Financial situation

table(df$financial_situation_now)

# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)


# visualise

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


# use pastecs::stat.desc to get some descriptive statistics

stat.desc(df$fin_situ_now)



# Now test whether males and females have different financial situations now and perceptions for the future


reg1 <- lm(fin_situ_now ~ gender, data = df)
summary(reg1)


reg2 <- lm(fin_situ_future ~ gender, data = df)
summary(reg2)


reg3 <- lm(fin_situ_future ~ gender*fin_situ_now, data = df)
summary(reg3)


reg4 <- lm(fin_situ_change ~ gender, data = df)
summary(reg4)

reg5 <- lm(fin_situ_change ~ gender*fin_situ_now, data = df)
summary(reg5)

saveRDS(df, file = "data/processed/processed_data.RDS")
