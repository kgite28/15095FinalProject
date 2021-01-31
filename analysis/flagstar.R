#rm(list = ls())
rm(list = setdiff(ls(),"keep_copy"))

setwd("~/mit/95/15.095_project/")
source("analysis/get_results_function.R")

# flag <- read.csv("data/flag.csv")
# keep_copy <- flag
flag <- keep_copy

#ignoring gender and age for now.
flag <- flag %>%
  select(-matches("gender*"),-matches("senior*"))
#trying just black as minority
flag <- flag %>% 
  mutate(minority = black.TRUE,
         minority = ifelse(minority >= 1,1,0))
flag <- flag %>%
  select(-matches("*TRUE"),-matches("*FALSE"))

#drop units149
flag <- flag %>% 
  select(-units..149)
#drop balloon1 and baloon2
flag <- flag %>% 
  select(-c(balloon.1,balloon.2))

#look at 2019 only
flag <- flag %>% 
  filter(year == 2019) %>% 
  select(-year)

#standardize
flag <- flag %>% 
  mutate(across(-c(ID,denied,minority),~ (.x - mean(.x)) / sd(.x)))

#train - test split
mean(ifelse(flag$denied == -1,0,1))
mean(flag$minority)

#80-20
#set.seed(1444)
set.seed(1595)
smp_size <- floor(0.80 * nrow(flag))
train_ind <- sample(seq_len(nrow(flag)), size = smp_size)
train <- flag[train_ind, ]
test <- flag[-train_ind, ]

#checking percentages
mean(ifelse(train$denied == -1,0,1))
mean(train$minority)
mean(ifelse(test$denied == -1,0,1))
mean(test$minority)
#they are fine

#balancing the dataset. eventually keep all denied but only half of the approvals
#for now keep 1/2 of denied and quarter of approvals so it trians faster
# num_to_keep_den <- sum(train$denied == 1)*.3
# num_to_keep_app <- sum(train$denied == -1)*.07
#keep all denials and get same number of approvals
num_to_keep_den <- sum(train$denied == 1)
num_to_keep_app <- sum(train$denied == 1)
train %>% write.csv("analysis/all_train.csv")
train <- train %>% 
  filter(denied == 1) %>% 
  sample_n(size = num_to_keep_den) %>% 
  rbind(train %>% 
          filter(denied == -1) %>% 
          sample_n(size = num_to_keep_app)
  )

mean(ifelse(train$denied == -1,0,1))
mean(train$minority)

train %>% write.csv("analysis/train.csv",row.names = FALSE)
test %>% write.csv("analysis/test.csv",row.names = FALSE)

#########
#results#
#########

train_results <- read.csv("analysis/train_results.csv")
test_results <- read.csv("analysis/test_results.csv")
train_results2 <- read.csv("analysis/train_results2.csv")
test_results2 <- read.csv("analysis/test_results2.csv")
train_results3 <- read.csv("analysis/train_results3.csv")
test_results3 <- read.csv("analysis/test_results3.csv")

rbind(get_results(train_results$denied,train_results$results,train_results$minority,"svm_train"),
      get_results(train_results2$denied,train_results2$results,train_results2$minority,"svm_fair_train"),
      get_results(train_results3$denied,train_results3$results,train_results3$minority,"svm_fair_train2"),
      get_results(test_results$denied,test_results$results,test_results$minority,"svm_test"),
      get_results(test_results2$denied,test_results2$results,test_results2$minority,"svm_fair_test"),
      get_results(test_results3$denied,test_results3$results,test_results3$minority,"svm_fair_test2")
) %>% view()

train_results %>% 
  mutate(loss = 1 - results*denied) %>% 
  mutate(loss = ifelse(loss < 0,0,loss)) %>% 
  mutate(total_loss = sum(loss),
         total_obs = sum(n())) %>% 
  group_by(minority) %>% 
  summarize(loss = sum(loss),
            obs = n(),
            total_loss = first(total_loss),
            total_obs = first(total_obs)) %>% 
  mutate(percent_loss = loss / total_loss,
         percent_obs = obs / total_obs)
