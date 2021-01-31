library(tidyverse)

get_results <- function(label,results,minority,method) {
  func_df <- data.frame(matrix(nrow=length(label)))
  func_df$results <- results
  func_df$denied <- label == 1
  
  denial_rate <- mean(func_df$denied)
  cutoff <- quantile(func_df$results,.77)
  
  func_df$predicted_denial <- results > cutoff
  func_df$maj_min = ifelse((minority == 1),"min","maj")
  func_df <- func_df %>% select(-1)
  
  pos.scores <- func_df %>% 
    filter(denied == 1) %>% 
    pull(results)
  
  neg.scores <- func_df %>% 
    filter(denied == 0) %>% 
    pull(results)
  resample <- 10000000
  auc <- mean(sample(pos.scores,resample,replace=T) > sample(neg.scores,resample,replace=T))
  
  return_results <- func_df %>% 
    mutate(fp =  predicted_denial & !denied,
           tp =  predicted_denial &  denied,
           fn = !predicted_denial &  denied,
           tn = !predicted_denial & !denied,
           fp_tn = fp + tn,
           tp_fn = tp + fn,
           tp_tn = tp + tn,
           tp_fp = tp + fp,
           loans = fn + tn,
           all = fp + tp + fn + tn) %>% 
    group_by(maj_min) %>% 
    summarize(across(c(fp,tp,fn,tn,fp_tn,tp_fn,tp_tn,tp_fp,all,loans),sum),
              avg_score = mean(results - cutoff)) %>% 
    mutate(FPR = fp / fp_tn,
           TPR = tp / tp_fn,
           PP = tp / tp_fp,
           accuracy1 = sum(tp_tn) / sum(all),
           accuracy2 = tp_tn / all
    ) %>% 
    select(fp,tp,fn,tn,maj_min,FPR,TPR,PP,loans,avg_score,accuracy1,accuracy2) %>% 
    pivot_wider(names_from = maj_min, values_from = c(fp,tp,fn,tn,FPR,TPR,PP,loans,accuracy1,accuracy2,avg_score)) %>% 
    rename(accuracy = accuracy1_min) %>% 
    select(-accuracy1_maj) %>% 
    mutate(method = method,
           auc = auc,
           FPR_diff = FPR_min - FPR_maj,
           FPR_pdiff = FPR_diff / ((FPR_maj + FPR_min) / 2),
           TPR_diff = TPR_min - TPR_maj,
           TPR_pdiff = TPR_diff / ((TPR_maj + TPR_min) / 2),
           PP_diff = PP_min - PP_maj,
           PP_pdiff = PP_diff / ((PP_min + PP_maj) / 2),
           score_diff = avg_score_min - avg_score_maj,
           score_pdiff = score_diff / ((avg_score_maj + avg_score_min) / 2),
           cutoff = cutoff
    ) %>% 
    relocate(method,auc,
             fp_min,fp_maj,
             tp_min,tp_maj,
             fn_min,fn_maj,
             tn_min,tn_maj,
             FPR_min,FPR_maj,FPR_diff,FPR_pdiff,
             TPR_min,TPR_maj,TPR_diff,TPR_pdiff,
             PP_min,PP_maj,PP_diff,PP_pdiff,
             avg_score_min,avg_score_maj,score_diff,score_pdiff,
             accuracy2_min,accuracy2_maj,accuracy
    )
  return(return_results)
}
