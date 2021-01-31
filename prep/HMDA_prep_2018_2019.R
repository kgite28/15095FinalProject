rm(list = ls())

library(tidyverse)

setwd("~/mit/95/15.095_project/prep/")

#read in and append
banks = c("NFCU","flagstar","BOA")
years = c(2018,2019)
all_data = data.frame()
for (year in years) {
  for (bank in banks) {
    base_file = "../data/"
    file <- paste(base_file,paste(bank,year,sep="_"),".txt",sep="")
    df <- read.table(file,sep="|")
    df$year = year
    df$bank = bank
    all_data <- rbind(all_data,df)
  }
}

df <- all_data

#rename or drop
rename_vec = c("RID","LEI","type","purpose","preapproval",
               "construction_method","occupancy","loan_amount","action","state",
               "county","tract","ethnicity_bor1","ethnicity_bor2","ethnicity_bor3",
               "ethnicity_bor4","ethnicity_bor5","ethnicity_cobor1","ethnicity_cobor2","ethnicity_cobor3",
               "ethnicity_cobor4","ethnicity_cobor5","ethnicity_bor_vis","ethnicity_cobor_vis","race_bor1",
               "race_bor2","race_bor3","race_bor4","race_bor5","race_cobor1",
               "race_cobor2","race_cobor3","race_cobor4","race_cobor5","race_bor_vis",
               "race_cobor_vis","sex_bor","sex_cobor","sex_bor_vis","sex_cobor_vis",
               "age_bor","senior_bor","age_cobor","senior_cobor","income",
               "purch_type","rate_spread","HOEPA","lien","cscore_type_bor",
               "cscore_type_cobor","denial_reason1","denial_reason2","denial_reason3","denial_reason4",
               "total_cost","total_points","orig_charges","discount_points","lender_credits",
               "interest_rate","prepay_penalty","dti","cltv","term",
               "intro_rate_period","balloon","io_payment","neg_amort","other_non_amort",
               "property_value","manuf_prop_type","manuf_prop_interest","units","mf_units",
               "submit_type","iptyi","aus1","aus2","aus3",
               "aus4","aus5","reverse_mort","oeloc","business_purpose",
               "year","bank"
               )


colnames(df) = rename_vec

#drop fields that probably don't matter
drop_fields = c("RID","LEI","ethnicity_bor_vis","ethnicity_cobor_vis","race_bor_vis","race_cobor_vis",
                "sex_bor_vis","sex_cobor_vis","aus2","aus3","aus4","aus5",
                "denial_reason2","denial_reason3","denial_reason4","total_points")

df <- df %>%
  select(-drop_fields)

#create gender fields. 1-male, 2-female,3-unk,4-unk,5-no coapp,6-unk
df %>% 
  group_by(bank,sex_bor) %>% 
  summarize(n = n()) %>% 
  spread(sex_bor,n)

df %>% 
  group_by(bank,sex_cobor) %>% 
  summarize(n = n()) %>% 
  spread(sex_cobor,n)

df <- 
  df %>% mutate(gender = ifelse(sex_bor == 1 & sex_cobor == 1,"male",NA),
                gender = ifelse(sex_bor == 1 & sex_cobor == 5,"male",gender),
                gender = ifelse(sex_bor == 2 & sex_cobor == 2,"female",gender),
                gender = ifelse(sex_bor == 2 & sex_cobor == 5,"female",gender),
                gender = ifelse(sex_bor == 1 & sex_cobor == 2,"joint",gender),
                gender = ifelse(sex_bor == 2 & sex_cobor == 1,"joint",gender),
                gender = ifelse(sex_bor == 3 | sex_bor == 4 | sex_bor == 6,"unk",gender),
                gender = ifelse(sex_cobor == 3 | sex_cobor == 4 | sex_cobor == 6,"unk",gender),
                )

df %>% 
  split(.$gender) %>% 
  map(~ table(.$sex_bor,.$sex_cobor))

xtabs(~sex_cobor + sex_bor + gender,data = df)

df %>% colnames()
#drop gender components
df <- df %>% 
  select(-c("sex_bor","sex_cobor"))

#race 
#1. AIAN 2. Asian  3. Black 4. PI 5. White 6. unk 7 - unk

df %>% 
  select(starts_with("race")) %>% 
  apply(2,table)

#we don't care about subcategories
df <- df %>% 
  mutate(across(starts_with("race"), ~ substr(as.character(.x),1,1)))

#assign race to category if it shows up in any of the 10 fields
race_function <- function(data,number) {
  data <- data %>% 
    mutate(across(starts_with("race"), ~ .x == number,.names = "newcol_{col}"))
  new_col <- data %>% 
    select(matches("newcol_+")) %>% 
    rowSums(na.rm = TRUE) > 0
  return(new_col)
}

df$american_indian <- race_function(df,"1")
df$asian <- race_function(df,"2")
df$black <- race_function(df,"3")
df$pi <- race_function(df,"4")
df$nhw <- race_function(df,"5")

#ethnicity
#1, 11, 12, 13, 14: hispanic, 2: not hispanic, 3, 4: unk, j5: no co-borrower

df %>% 
  select(starts_with("ethnicity")) %>% 
  apply(2,table)

#we don't care about subcategories
df <- df %>% 
  mutate(across(starts_with("ethnicity"), ~ substr(as.character(.x),1,1)))

#assign race to category if it shows up in any of the 10 fields
eth_function <- function(data,number) {
  data <- data %>% 
    mutate(across(starts_with("ethnicity"), ~ .x == number,.names = "newcol_{col}"))
  new_col <- data %>% 
    select(matches("newcol_+")) %>% 
    rowSums(na.rm = TRUE) > 0
  return(new_col)
}

df$hispanic <- eth_function(df,"1")
df$not_hispanic <- eth_function(df,"2")

#white people that are also other races will not be considered NHW
df <- df %>% 
  mutate(nhw = ifelse(hispanic,FALSE,nhw),
         nhw = ifelse(american_indian,FALSE,nhw),
         nhw = ifelse(asian,FALSE,nhw),
         nhw = ifelse(black,FALSE,nhw),
         nhw = ifelse(pi,FALSE,nhw)
  )

#nhw 

df %>% 
  select(nhw,black,american_indian,pi,asian,hispanic) %>% 
  apply(2,table)

#count no category
df %>% 
  filter(!(nhw | black | american_indian | pi | asian | hispanic)) %>% 
  dim()  

#drop component race/ethnicity fields
df %>% 
  select(-matches("race")) %>% 
  colnames()

df <- df %>% 
  select(-matches("race"),-matches("ethnicity"))

df %>% colnames()

#age
df %>% 
  select(starts_with("age")) %>% 
  apply(2,table)

df <- df %>% 
  mutate(senior_cobor = ifelse(age_cobor == "9999","None",senior_cobor),
         senior_cobor = ifelse(age_cobor == "8888","Unk",senior_cobor),
         senior_bor = ifelse(age_bor == "8888","Unk",senior_bor)
         )
df %>% 
  select(starts_with("senior")) %>% 
  apply(2,table)

df <- df %>% 
  mutate(senior = ifelse(senior_bor == "Yes" & senior_cobor == "Yes","senior",NA),
         senior = ifelse(senior_bor == "Yes" & senior_cobor == "None","senior",senior),
         senior = ifelse(senior_bor == "Yes" & senior_cobor == "No","joint",senior),
         senior = ifelse(senior_bor == "No" & senior_cobor == "Yes","joint",senior),
         senior = ifelse(senior_bor == "Unk" | senior_cobor == "Unk","unk",senior)
         )

df %>% 
  split(.$senior) %>% 
  map(~ table(.$senior_bor,.$senior_cobor))

#drop components
df <- df %>% 
  select(-starts_with("age"),-senior_bor,-senior_cobor)

#create label
df <- df %>% 
  mutate(denied = action == 3) %>% 
  filter(action %in% c(1,2,3))

write.csv(df,"../data/combined.csv")

