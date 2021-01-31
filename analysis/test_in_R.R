#rm(list = ls())
rm(list = setdiff(ls(),"keep_copy"))

setwd("~/mit/95/15.095_project/analysis/")

library(tidyverse)
library(pROC)
library(e1071)
library(caret)


#error rate balance and predictive parity
#false positive rate among whites
#FP = predicted_denial == TRUE, denied == FALSE
#TP = predicted_denial == TRUE, denied == TRUE
#FN = predicted_denial == FALSE, denied == TRUE
#TN = predicted_denial == FALSE, denied == FALSE

#input: method, true label, minority/majority, predicted_label, score (not yet)
#output: method, FPR_minority, FPR_majority, TPR_minority, TPR_majority, PP_minority, PP_majority, accuracy
get_results <- function(label,prediction,maj_min,method) {
  func_df <- data.frame(matrix(nrow=length(label)))
  func_df$denied <- label
  func_df$predicted_denial <- prediction
  func_df$maj_min <- maj_min
  func_df <- func_df %>% select(-1)
  
  return_results <- func_df %>% 
    mutate(fp =  predicted_denial & !denied,
           tp =  predicted_denial &  denied,
           fn = !predicted_denial &  denied,
           tn = !predicted_denial & !denied,
           fp_tn = fp + tn,
           tp_fn = tp + fn,
           tp_tn = tp + tn,
           tp_fp = tp + fp,
           all = fp + tp + fn + tn) %>% 
    group_by(maj_min) %>% 
    summarize(across(c(fp,tp,fp_tn,tp_fn,tp_tn,tp_fp,all),sum)) %>% 
    mutate(FPR = fp / fp_tn,
           TPR = tp / tp_fn,
           PP = tp / tp_fp,
           accuracy = sum(tp_fp) / sum(all)
    ) %>% 
    select(maj_min,FPR,TPR,PP,accuracy) %>% 
    pivot_wider(names_from = maj_min, values_from = c(FPR,TPR,PP,accuracy)) %>% 
    rename(accuracy = accuracy_min) %>% 
    select(-accuracy_maj) %>% 
    mutate(method = method)
  return(return_results)
}


read.csv("data/combined.csv") %>%
  select(-X) %>% 
  count(bank,year)


# df <- read.csv("../data/combined.csv") %>%
#    select(-X)
# keep_copy <- df
df <- keep_copy

#create ID
df <- df %>% 
  mutate(ID = row_number())

#type
df$type %>% summary()
df %>% count(is.na(type))
df %>% 
  group_by(type,bank) %>% 
  summarize(mean(denied))

#purpose
df$purpose %>% summary()
df %>% count(is.na(purpose))
df %>% 
  group_by(purpose,bank) %>% 
  summarize(mean(denied))

#preapproval - Do not include!
df$preapproval %>% summary()
df %>% 
  group_by(preapproval) %>% 
  summarize(mean(denied),n())

#construction_method
df$construction_method %>% summary()
df %>% count(is.na(construction_method))
df %>% 
  group_by(construction_method,bank) %>% 
  summarize(mean(denied),n())

#occupancy
df$occupancy %>% summary()
df %>% count(is.na(occupancy))
df %>% 
  group_by(occupancy,bank) %>% 
  summarize(mean(denied),n())

#loan_amount
df$loan_amount %>% summary()
df %>% 
  group_by(denied) %>% 
  summarize(mean(loan_amount),n())

#state
df$state %>% table()
df %>% count(is.na(state))
df %>% 
  group_by(state,bank) %>% 
  summarize(mean(denied),n())
#drop missing state
df <- df %>% 
  filter(!is.na(state))

#income
df$income %>% summary()
df %>% 
  group_by(denied) %>% 
  summarize(mean(income),n())
#dropping missing income and negative income
df <- df %>% 
  filter(!is.na(income)) %>% 
  filter(income > 0)

#purch_type - DON"T INCLUDE!
df$purch_type %>% table()
df %>% 
  group_by(purch_type) %>% 
  summarize(mean(denied),n())

#rate_spread - DON"T INCLUDE
df$rate_spread %>% summary()
df %>% 
  group_by(denied) %>% 
  summarize(mean(rate_spread),n())

#HOEPA - DON"T INCLUDE!
df$HOEPA %>% table()
df %>% 
  group_by(HOEPA) %>% 
  summarize(mean(denied),n())

#lien
df$lien %>% table()
df %>% count(is.na(lien))
df %>% 
  group_by(lien,bank) %>% 
  summarize(mean(denied),n())

#cscore_type_bor
df$cscore_type_bor %>% table()
df %>% count(is.na(cscore_type_bor))
df %>% 
  group_by(cscore_type_bor,bank) %>% 
  summarize(mean(denied),n())

#cscore_type_cobor
df$cscore_type_cobor %>% table()
df %>% count(is.na(cscore_type_cobor))
df %>% 
  group_by(cscore_type_cobor,bank) %>% 
  summarize(mean(denied),n())

#denial_reason1 - DON"T INCLUDE
df$denial_reason1 %>% table()
df %>% count(is.na(denial_reason1))
df %>% 
  group_by(denial_reason1) %>% 
  summarize(mean(denied),n())

#total_cost - DON"T INCLUDE
df$total_cost %>% summary()

#orig_charges - DON"T INCLUDE
df$orig_charges %>% summary()

#don't include
df %>% 
  select(c("discount_points","lender_credits","interest_rate","prepay_penalty")) %>% 
  summary()

#dti
df$dti %>% summary()
df %>% count(is.na(dti))
df %>% 
  group_by(dti,bank) %>% 
  summarize(mean(denied),n())
#drop NA
df <- df %>% 
  filter(!is.na(dti))

#cltv
df$cltv %>% summary()
df %>% count(is.na(cltv))
df <- df %>% 
  filter(!is.na(cltv))
df %>% 
  group_by(denied) %>% 
  summarize(mean(cltv),n())

#term
df$term %>% table()
df %>% count(is.na(term))
df %>% 
  group_by(term) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)
df <- df %>%
  filter(!is.na(term)) %>% 
  mutate(term_bucket = ifelse(term < 360,"term_lte180",""),
         term_bucket = ifelse(term == 360,"term_360",term_bucket),
         term_bucket = ifelse(term > 360,"term_gt360",term_bucket)
  )
df %>% 
  group_by(term_bucket,bank) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 100)

#intro_rate_period - don't include
df$intro_rate_period %>% table()
df %>% count(is.na(intro_rate_period))
df %>% 
  group_by(intro_rate_period) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)
df %>% group_by(intro_rate_period,purpose) %>% 
  summarize(n()) %>% 
  print(n = 1000)

#balloon
df$balloon %>% table()
df %>% count(is.na(balloon))
df %>% 
  group_by(balloon,bank) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)

#io_payment
df$io_payment %>% table()
df %>% count(is.na(io_payment))
df %>% 
  group_by(io_payment,bank) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)

#neg amort - DON"T INCLUDE
df$neg_amort %>% table()
df %>% count(is.na(neg_amort))
df %>% 
  group_by(neg_amort) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)

#other_non_amort
df$other_non_amort %>% table()
df %>% count(is.na(other_non_amort))
df %>% 
  group_by(other_non_amort,bank) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)

#property_value
df$property_value %>% summary()
df %>%
  filter(!is.na(property_value)) %>% 
  group_by(denied) %>% 
  summarize(mean(property_value),n()) %>% 
  print(n = 200)

#manuf_prop_type
df$manuf_prop_type %>% table()
df %>% count(is.na(manuf_prop_type))
df %>%
  group_by(manuf_prop_type,bank) %>% 
  summarize(mean(denied),n())

#manuf_prop_interest
df$manuf_prop_interest %>% table()
df %>%
  group_by(manuf_prop_interest,bank) %>% 
  summarize(mean(denied),n()) %>% 
  print(n = 200)

#units
df$units %>% table()
df %>% count(is.na(units))
df %>%
  group_by(units,bank) %>% 
  summarize(mean(denied),n())

#mf_units - all missing
df$mf_units %>% head()
df %>% count(is.na(mf_units))
df %>%
  group_by(units) %>% 
  summarize(mean(denied),n())

#submit_type
df$submit_type %>% table()
df %>% count(is.na(submit_type))
df %>%
  group_by(submit_type,bank) %>% 
  summarize(mean(denied),n())

#iptyi  DON"T INCLUDE
df$iptyi %>% table()
df %>% count(is.na(iptyi))
df %>%
  group_by(iptyi) %>% 
  summarize(mean(denied),n())

#AUS1
df$aus1 %>% table()
df %>% count(is.na(aus1))
df %>%
  group_by(aus1,bank) %>% 
  summarize(mean(denied),n())

#reverse_mort - DON"T include
df$reverse_mort %>% table()
df %>% count(is.na(aus1))
df %>%
  group_by(aus1) %>% 
  summarize(mean(denied),n())

#oeloc
df$oeloc %>% table()
df %>% count(is.na(oeloc))
df %>%
  group_by(oeloc,bank) %>% 
  summarize(mean(denied),n())

#business_purpose
df$business_purpose %>% table()
df %>% count(is.na(business_purpose))
df %>%
  group_by(business_purpose,bank) %>% 
  summarize(mean(denied),n())

#senior
df <- df %>% 
  mutate(senior = ifelse(is.na(senior),"unk",senior))

all_cat_vars <- c("type","purpose","construction_method","occupancy","state","lien","cscore_type_bor",
                  "cscore_type_cobor","dti","term_bucket","balloon","io_payment","other_non_amort","manuf_prop_type",
                  "manuf_prop_interest","units","submit_type","oeloc","business_purpose","american_indian","asian",
                  "black","pi","nhw","hispanic","senior","gender")
flag_cat_vars <- all_cat_vars
#remove other non-amort
boa_cat_vars <- c("type","purpose","construction_method","occupancy","state","lien","cscore_type_bor",
                  "cscore_type_cobor","dti","term_bucket","balloon","io_payment","manuf_prop_type",
                  "manuf_prop_interest","units","submit_type","oeloc","business_purpose","american_indian","asian",
                  "black","pi","nhw","hispanic","senior","gender")
#remove other non-amort and submit type
boa_cat_vars <- c("type","purpose","construction_method","occupancy","state","lien","cscore_type_bor",
                  "cscore_type_cobor","dti","term_bucket","balloon","io_payment","manuf_prop_type",
                  "manuf_prop_interest","units","oeloc","business_purpose","american_indian","asian",
                  "black","pi","nhw","hispanic","senior","gender")
nfcu_cat_vars <- 
  cont_vars <- c("loan_amount","income","cltv","property_value")


#dealing with cats
cats_all <- df %>% 
  select(all_cat_vars) %>% 
  mutate(across(matches("*"),as.factor))

cats_nfcu <- df %>% 
  filter(bank == "NFCU") %>% 
  select(nfcu_cat_vars) %>% 
  mutate(across(matches("*"),as.factor))

cats_boa <- df %>% 
  filter(bank == "BOA") %>% 
  select(boa_cat_vars) %>% 
  mutate(across(matches("*"),as.factor))

cats_flag <- df %>% 
  filter(bank == "flagstar") %>% 
  select(flag_cat_vars) %>% 
  mutate(across(matches("*"),as.factor))

dummy_nfcu <- dummyVars("~ .",data = cats_nfcu)
dummy_flag <- dummyVars("~ .",data = cats_flag)
dummy_boa <- dummyVars("~ .",data = cats_boa)
dummy_all <-  dummyVars("~ .",data = cats_all)

dummy_cats <- data.frame(predict(dummy_all, newdata = cats_all)) 
dummy_cats_nfcu <- data.frame(predict(dummy_nfcu, newdata = cats_nfcu))
dummy_cats_flag <- data.frame(predict(dummy_flag, newdata = cats_flag)) 
dummy_cats_boa <- data.frame(predict(dummy_boa, newdata = cats_boa)) 
dummy_cats_flag %>% head()

rm(cats_all,cats_flag,cats_boa,cats_nfcu,dummy_all,dummy_flag,dummy_boa,dummy_nfcu)

#finishing for flagstart from now on

#cont vars and label
flag <- df %>% 
  filter(bank == "flagstar") %>% 
  select(all_of(cont_vars),denied,ID,year) %>% 
  cbind(dummy_cats_flag) %>% 
  mutate(denied = ifelse(denied,1,-1))

flag %>% write.csv("../data/flag.csv",row.names=FALSE)

#standardize
flag <- flag %>% 
  mutate(minority = american_indian.TRUE +  asian.TRUE + black.TRUE + pi.TRUE + hispanic.TRUE,
         minority = ifelse(minority >= 1,1,0)) %>% 
  mutate(across(-c(ID,denied,year,minority),~ (.x - mean(.x)) / sd(.x)))

#JL remember to remove this subset once it's all working
set.seed(1444)
sub_flag <- flag %>%
  filter(year == 2019) %>% 
  select(-year) %>% 
  sample_frac(size = .5)

total_obs <- (flag %>% filter(year == 2019) %>% dim())[1]*.25
sub_flag <- flag %>% 
  filter(year == 2019 & denied == 1) %>% 
  sample_n(size = total_obs / 2) %>% 
  rbind(flag %>% 
          filter(year == 2019 & denied == -1) %>% 
          sample_n(size = total_obs / 2)
  )

flag %>% count(year == 2019)

sub_flag %>% write.csv("for_julia.csv",row.names = FALSE)

flag %>% 
  mutate(denied = ifelse(denied == -1,1,0)) %>% 
  pull(denied) %>% 
  mean()

results <- read.csv("julia_results.csv") %>% 
  mutate(predict_denial = ifelse(results > 0,1,-1))
results <- merge(sub_flag,results,by = "ID")
results %>% count(predict_denial,denied)
results <- results %>% 
  mutate(label = (denied == 1),
         prediction = (predict_denial == 1),
         maj_min = ifelse((black.TRUE > 0),"min","maj")
  )

get_results(results$label,results$prediction,results$maj_min,"balanced")
asdf


cats_boa %>% summary()

#JL remember to remove this subset once it's all working
set.seed(1444)
df <- df %>%
  sample_frac(size = .25)

df %>% 
  select(lien,dti,type,cltv,maj_min) %>% 
  summary()

#for JULIA
x <- as.data.frame(model.matrix(denied ~ -1 + dti + type + cltv + lien + maj_min, data = df))
x %>% colnames()
#remove extra dti column
x <- x %>% 
  select(-c("dti<20%")) %>% 
  mutate(across(matches(".*"),~ (.x - mean(.x)) / sd(.x)))

design.df <- cbind(df$ID,df$denied,x) %>% 
  rename(ID = "df$ID",
         denied = "df$denied") %>% 
  mutate(denied_for_Julia = ifelse(denied,1,-1)) %>% 
  rename_all(~ gsub("-", "_", .)) %>%
  rename_all(~ gsub("%", "", .)) %>% 
  rename_all(~ gsub(">", "", .)) %>% 
  rename_all(~ gsub("<", "", .))

#write fields to send to csv
# fields_to_send = c("ID","denied_for_Julia","dti60","dti20_30","dti30_36",
#                    "dti36_40","dti41_45","dti46_49","dti50_60","dtimissing","type2",
#                    "type3","type4","cltv","lien2","maj_minmin")

fields_to_send = c("ID","denied_for_Julia","dti60","dti20_30","dti30_36",
                   "dti36_40","dti41_45","dti46_49","dti50_60","dtimissing","type2",
                   "type3","cltv","lien2","maj_minmin")

design.df %>% colnames()

design.df %>% summary()

design.df %>%
  select(all_of(fields_to_send)) %>% 
  write.csv("for_julia.csv",row.names = FALSE)

logit.df <- design.df %>% 
  select(all_of(fields_to_send),denied)

logit.df %>% colnames()
model <- glm(denied ~ . -ID - denied_for_Julia ,family=binomial(link='logit'),data=logit.df)
summary(model)
model$coefficients

mean(predict(model,logit.df,type='response') > .5)
sum(predict(model,logit.df,type='response') > .5)
sum(predict(model,logit.df,type='response') < .5)


julia_results <- read.csv("julia_results.csv") %>% 
  #label prediction
  mutate(predicted_denial = ifelse(results >= 0, TRUE,FALSE))

julia_results$results %>% summary()
julia_results$predicted_denial %>% table()
julia_results$results %>% table()
mean(julia_results$predicted_denial)

#merge results back to df
df <- merge(df,julia_results,by = "ID")

df %>% 
  count(predicted_denial,denied)

df$predicted_denial %>% table()

#julia SVM results
get_results(label = df$denied, prediction = df$predicted_denial, maj_min = df$maj_min,method="julia_svm")

#random results
random <- sample(c(rep(0,dim(df %>% filter(!denied))[1]),rep(1,dim(df %>% filter(denied))[1])))
get_results(label = df$denied, prediction = random, maj_min = df$maj_min,method="random")


#SVM package.
for_svm <- design.df %>% 
  select(denied,cltv) %>% 
  mutate(denied = ifelse(cltv < .50,TRUE,FALSE)) %>% 
  mutate(denied = factor(denied))

for_svm$denied %>% table()

svmfit <-  svm(denied ~ cltv, data = for_svm, kernel = "linear", cost = 1, scale = FALSE)
pred <- predict(svmfit,for_svm)
thing <- as.data.frame(cbind(pred,for_svm$denied))
colnames(thing) = c("pred","real")
thing %>%
  count(real,pred)
pred %>% table()

svmfit$decision.values
svmfit$coefs
svmfit$SV
cbind(svmfit$decision.values, for_svm$denied)


# model <- glm(denied ~ factor(purpose) ,family=binomial(link='logit'),data=df)
# summary(model)
# prob=predict(model,type=c("response"))
# roc(prob,df$denied)
# auc(roc(df$denied,prob))
