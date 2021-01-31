rm(list = ls())

setwd("~/mit/95/project/15.095_project/")

source("analysis/get_results_function.R")

#########
#results#
#########

train_results <- read.csv("analysis/results/train_results.csv")
test_results <- read.csv("analysis/results/test_results.csv")

train_results2 <- read.csv("analysis/results/train_results2.csv")
test_results2 <- read.csv("analysis/results/test_results2.csv")

train_results3 <- read.csv("analysis/results/train_results3.csv")
test_results3 <- read.csv("analysis/results/test_results3.csv")

train_results2_125 <- read.csv("analysis/results/train_results2_125.csv")
test_results2_125 <- read.csv("analysis/results/test_results2_125.csv")

train_results2_2 <- read.csv("analysis/results/train_results2_2.csv")
test_results2_2 <- read.csv("analysis/results/test_results2_2.csv")

train_results3_125 <- read.csv("analysis/results/train_results3_125.csv")
test_results3_125 <- read.csv("analysis/results/test_results3_125.csv")

train_results3_2 <- read.csv("analysis/results/train_results3_2.csv")
test_results3_2 <- read.csv("analysis/results/test_results3_2.csv")

train_results2_5 <- read.csv("analysis/results/train_results2_5.csv")
test_results2_5 <- read.csv("analysis/results/test_results2_5.csv")

train_results3_5 <- read.csv("analysis/results/train_results3_5.csv")
test_results3_5 <- read.csv("analysis/results/test_results3_5.csv")


train_results_racist <- read.csv("analysis/results/train_results_racist.csv")
test_results_racist <- read.csv("analysis/results/test_results_racist.csv")

train_results_racist_all_white <- read.csv("analysis/results/train_results_racist_allwhite.csv")
test_results_racist_all_white <- read.csv("analysis/results/test_results_racist_allwhite.csv")

#logit results (already aggregated)
log1 <- read.csv("analysis/logregwithminorities_insample.csv")
log2 <- read.csv("analysis/logregwithminorities.csv")
log3 <- read.csv("analysis/logregwithoutminorities_insample.csv")
log4 <- read.csv("analysis/logregwithoutminorities.csv")
log5 <- read.csv("analysis/logregwithminorities_insample.csv")
log6 <- read.csv("analysis/logreg_testignoringminorities.csv")

results <- rbind(get_results(train_results$denied,train_results$results,train_results$minority,"svm_train"),
      get_results(train_results2$denied,train_results2$results,train_results2$minority,"svm_fair_train"),
      get_results(train_results3$denied,train_results3$results,train_results3$minority,"svm_fair_train2"),
      get_results(test_results$denied,test_results$results,test_results$minority,"svm_test"),
      get_results(test_results2$denied,test_results2$results,test_results2$minority,"svm_fair_test"),
      get_results(test_results3$denied,test_results3$results,test_results3$minority,"svm_fair_test2"),
      get_results(test_results2_125$denied,test_results2_125$results,test_results2_125$minority,"svm_fair_test 1.25"),
      get_results(test_results3_125$denied,test_results3_125$results,test_results3_125$minority,"svm_fair_test2 1.25"),
      get_results(test_results2_2$denied,test_results2_2$results,test_results2_2$minority,"svm_fair_test 2"),
      get_results(test_results3_2$denied,test_results3_2$results,test_results3_2$minority,"svm_fair_test2 2"),
      get_results(train_results2_125$denied,train_results2_125$results,train_results2_125$minority,"svm_fair_train 1.25"),
      get_results(train_results3_125$denied,train_results3_125$results,train_results3_125$minority,"svm_fair_train2 1.25"),
      get_results(train_results2_2$denied,train_results2_2$results,train_results2_2$minority,"svm_fair_train 2"),
      get_results(train_results3_2$denied,train_results3_2$results,train_results3_2$minority,"svm_fair_train2 2"),
      get_results(train_results_racist$denied,train_results_racist$results,train_results_racist$minority,"svm_racist_train"),
      get_results(test_results_racist$denied,test_results_racist$results,test_results_racist$minority,"svm_racist_test"),
      get_results(train_results_racist_all_white$denied,train_results_racist_all_white$results,train_results_racist_all_white$minority,"svm_racist_train_all_white"),
      get_results(test_results_racist_all_white$denied,test_results_racist_all_white$results,test_results_racist_all_white$minority,"svm_racist_test_all_white"),
      get_results(train_results2_5$denied,train_results2_5$results,train_results2_5$minority,"svm_fair_train 5"),
      get_results(test_results2_5$denied,test_results2_5$results,test_results2_5$minority,"svm_fair_test 5"),
      get_results(train_results3_5$denied,train_results3_5$results,train_results3_5$minority,"svm_fair_train2 5"),
      get_results(test_results3_5$denied,test_results3_5$results,test_results3_5$minority,"svm_fair_test2 5"),
      log1,log2,log3,log4,log5,log6
)


results <- results %>% 
  #add TNR
  mutate(overall_TNR = (tn_min + tn_maj) / (tn_min + tn_maj + fn_min + fn_maj),
         #add minority denial rate
         min_den_rate = (tp_min + fp_min) / (tn_min + fn_min + tp_min + fp_min),
         #add false negative rate
         FNR_min = fn_min / (fn_min + tp_min),
         FNR_maj = fn_maj / (fn_maj + tp_maj),
         FNR_diff = FNR_min - FNR_maj
  ) %>% 
  #multiply all rates by 100
  mutate(across(c(overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate),
         ~ .x * 100)) %>% 
  select(method,auc,overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate,
         avg_score_min,avg_score_maj) %>% 
  relocate(method,auc,overall_TNR,FPR_min,FPR_maj,FPR_diff,FNR_min,FNR_maj,FNR_diff,min_den_rate,
           avg_score_min,avg_score_maj)

results %>% write.csv("analysis/combined_results.csv")

#resolve sensativity stuff
look1 <- c("svm_train","svm_fair_train","svm_fair_train 1.25", "svm_fair_train 2","svm_fair_train 5")
look2 <- c("svm_test","svm_fair_test","svm_fair_test 1.25", "svm_fair_test 2","svm_fair_test 5")
look3 <- c("svm_train","svm_fair_train2","svm_fair_train2 1.25", "svm_fair_train2 2","svm_fair_train2 5")
look4 <- c("svm_test","svm_fair_test2","svm_fair_test2 1.25", "svm_fair_test2 2","svm_fair_test2 5")

results %>% 
  filter(method %in% look1) %>% 
  view()

#slightly more loans to minorities, slightly smaller FPR difference, slightly worse accuracy

results %>% 
  filter(method %in% look2) %>% 
  view()

#small difference between 1.25 version and others. others besides 1.25 all the same.

results %>% 
  filter(method %in% look3) %>% 
  view()

#2 and 5 are pretty much the same. 

results %>% 
  filter(method %in% look4) %>% 
  view()


#get rid of sensativity
drop <- c("svm_fair_train 1.25", "svm_fair_train 2","svm_fair_train 5",
          "svm_fair_test 1.25", "svm_fair_test 2","svm_fair_test 5",
          "svm_fair_train2 1.25", "svm_fair_train2 2","svm_fair_train2 5",
          "svm_fair_test2 1.25", "svm_fair_test2 2","svm_fair_test2 5")

results_subset <- results %>% 
  filter(!(method %in% drop)) %>% 
  mutate(method = ifelse(method == "svm_train","SVM No Minority Feature - Training Set",method),
         method = ifelse(method == "svm_fair_train","SVM Fair V1 - Training Set",method),
         method = ifelse(method == "svm_fair_train2","SVM Fair V2 - Training Set",method),
         method = ifelse(method == "svm_test","SVM No Minority Feature - Test Set",method),
         method = ifelse(method == "svm_fair_test","SVM Fair V1 - Test Set",method),
         method = ifelse(method == "svm_fair_test2","SVM Fair V2 - Test Set",method),
         method = ifelse(method == "svm_racist_train","SVM Includes Minority Feature - Training Set",method),
         method = ifelse(method == "svm_racist_test","SVM Includes Minority Feature - Test Set",method),
         method = ifelse(method == "svm_racist_train_all_white","SVM Includes Minority Feature - Training Set as if All White",method),
         method = ifelse(method == "svm_racist_test_all_white","SVM Includes Minority Feature - Test Set as if All White",method),
         method = ifelse(method == "log reg w minor, in-sample","Logistic Includes Minority Feature - Training Set",method),
         method = ifelse(method == "log reg w minor","Logistic Includes Minority Feature - Test Set",method),
         method = ifelse(method == "log reg w/o minor in-sample","Logistic No Minority Feature - Training Set",method),
         method = ifelse(method == "log reg w/o minor","Logistic No Minority Feature - Test Set",method),
         method = ifelse(method == "log reg trained with minorities, tested as if everyone is white",
                         "Logistic Includes Minority Feature - Test Set as if All White",method)
  )

results_subset %>% write.csv("analysis/results_subset.csv")

results_subset %>% view()

