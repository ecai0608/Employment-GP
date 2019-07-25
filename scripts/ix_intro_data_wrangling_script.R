# METADATA ====
# Description: Data wrangling and visualisation of youth data
# Created: 2019-06-30 (Neil Rankin)
# Updated: 2019-07-04 (Neil Rankin)
# Reviewed: NA

# SUMMARY: Script for wrangling and visualisation exercises on Employment Challenge data



# RESOURCES

# dplyr (tidyverse) https://dplyr.tidyverse.org/articles/dplyr.html
# ggplot (also tidyverse) https://ggplot2.tidyverse.org/
# lubridate https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html



# INITIALISE ====

#> Script-secific libraries ----

# Here is where we load libraries
library(lubridate)
library(tidyverse)


# LOAD DATA ====

df <- read.csv("data/raw/teaching_training_data.csv")

filter(df, as.Date(survey_date_month) == as.Date(dob))
class(df$survey_date_month)
class(df$dob)

# Notice the path (it is machine independent)

# Look at the data - what do we see?

table(df$gender)

unique_individuals <- df %>% 
  distinct(unid, gender)


# WRANGLING & FEATURE ENGINEERING ====

# Often an iterative process:
#   - Look at data
#   - Create feature
#   - Look again

# View data

view(df)


# Start with communication score

table(df$com_score)

# ggplot

ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score))

# explanation of what each component is


# How does this differ by gender


ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score, fill = gender))


ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score, fill = gender), position = "dodge")

# what can we learn from this figure?


# Now age

# How would we calculate this?

# introducing `mutate` and some lubridate functions

df <- df %>% filter(survey_num == 1) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1))
  #mutate(age = floor(age_at_survey))



summary(df$age_at_survey)

# see lubridate link (https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)


ggplot(data = df) + 
  geom_bar(mapping = aes(x = age))

# How does this differ by gender

ggplot(data = df) + 
  geom_bar(mapping = aes(x = age, fill = gender), position = "dodge")

# what does `dodge` do?


# A different approach

ggplot(data = df) + 
  geom_density(mapping = aes(x = age_at_survey, fill = gender), alpha = 0.3)

# NAs and older people cloud our view

# tidyverse::filter
df_without_gender_NAs <- df %>% 
  filter(!is.na(gender))

ggplot(data = df_without_gender_NAs) + 
  geom_density(mapping = aes(x = age, colour = gender), alpha = 0.3) + 
  xlim(15, 35)


# group_by
# we might want to count the number of surveys people have done

df <- df %>% 
  group_by(unid) %>% 
  mutate(total_surveys = max(survey_num)) %>% 
  ungroup()

df_unid <- df %>% 
  group_by(unid) %>% 
  summarise(total_surveys = max(survey_num)) %>% 
  ungroup()


# Always good practise to ungroup() since later you might forget that you have grouped and operations willbe affected by it



# create other data frames to create descriptive stats (and other things)

df_gender_cft <- df_without_gender_NAs %>% 
  group_by(gender) %>% 
  filter(!is.na(cft_score)) %>% 
  summarise(cft_mean = mean(cft_score), nobs = n())




# save your 'featured engineered' data

saveRDS(df, file = "data/processed/processed_data.RDS")


