rm(list = ls())

setwd("~/mit/95/project/15.095_project/")

train_results <- read.csv("analysis/results/train_results.csv")
train_results <- train_results %>% 
  mutate(results_norm = (results - mean(results)) / sd(results))
train_results2 <- read.csv("analysis/results/train_results2.csv")
train_results2 <- train_results2 %>% 
  mutate(results_norm2 = (results2 - mean(results2)) / sd(results2))

test <- cbind(train_results,train_results2$results_norm2)
test %>% colnames()
test %>% 
  mutate(diff = results_norm - `train_results2$results_norm2`) %>%
  mutate(pos_diff = diff > 0) %>% 
  group_by(minority) %>% 
  summarize(mean(pos_diff))

test %>% 
  mutate(diff = results_norm - `train_results2$results_norm2`) %>% 
  summary()


#flipped
train_results2_flip <- read.csv("analysis/results2/train_results2.csv")
train_results2_flip <- train_results2_flip %>% 
  mutate(results_norm2 = (results2 - mean(results2)) / sd(results2))

test <- cbind(train_results,train_results2_flip$results_norm2)
test %>% colnames()
test %>% 
  mutate(diff = results_norm - `train_results2_flip$results_norm2`) %>% 
  mutate(pos_diff = diff > 0) %>% 
  group_by(minority) %>%
  summarize(mean(pos_diff))



train_results %>% 
  group_by(minority) %>% 
  summarize(mean(results_norm))


test_results <- read.csv("analysis/results/test_results.csv")

test_results2 <- read.csv("analysis/results2/test_results2.csv")

train_results3 <- read.csv("analysis/results2/train_results3.csv")
test_results3 <- read.csv("analysis/results2/test_results3.csv")




