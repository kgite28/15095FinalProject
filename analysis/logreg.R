library(dplyr)

train = read.csv("train.csv")
test = read.csv("test.csv")

mean(train$minority)
mean(test$minority)



train$denied = (train$denied + 1)/2
test$denied = (test$denied + 1)/2
# 
train = train %>% select(!c(ID,type.4,purpose.32,construction_method.2,occupancy.3,state.WY,lien.2,cscore_type_bor.9,
                            cscore_type_cobor.10,dti.43,term_bucket.term_lte180,io_payment.2,other_non_amort.2,units.4,
                            submit_type.2,oeloc.2,business_purpose.2,manuf_prop_type.1,manuf_prop_interest.1,
                            manuf_prop_type.3,manuf_prop_interest.5,oeloc.1))
# ytrain = train$denied
# 
test = test %>% select(!c(ID,type.4,purpose.32,construction_method.2,occupancy.3,state.WY,lien.2,cscore_type_bor.9,
                            cscore_type_cobor.10,dti.43,term_bucket.term_lte180,io_payment.2,other_non_amort.2,units.4,
                            submit_type.2,oeloc.2,business_purpose.2,manuf_prop_type.1,manuf_prop_interest.1,
                            manuf_prop_type.3,manuf_prop_interest.5,oeloc.1))
# ytest = test$denied

mean(test$denied[test$minority == 1])
mean(test$denied[test$minority == 0])
mean(train$denied[train$minority == 1])
mean(train$denied[train$minority == 0])

########################################
# With minority differences

mod <- glm(denied~., data=train, family="binomial")

predtrain <- predict(mod, newdata=train, type="response")
predtest <- predict(mod, newdata=test, type="response")

#######################################
threshold = 0.5

sum((predtest>threshold) == test$denied)/length(test$denied)


classification <- data.frame(pred=predtrain,A=predtrain>threshold)

library(ROCR)
rocr.pred <- prediction(predtest, test$denied)
rocr.pred.df <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],
                           tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

auc <- performance(rocr.pred, "auc")@y.values[[1]]
auc

summary(mod)


library(caret)

ypreds = (predtest > threshold)
ypreds[FALSE] = 0

ypredtrain = (predtrain > threshold)
ypredtrain[FALSE] = 0

confusionMatrix(as.factor(ypreds),as.factor(test$denied) )

sum((predtest>threshold) == test$denied)/length(test$denied)

mean(ypreds[test$minority == 1])
mean(ypreds[test$minority == 0])
mean(ypredtrain[train$minority == 1])
mean(ypredtrain[train$minority == 0])


#minorities
r = confusionMatrix(as.factor(ypreds[test$minority == 1]),as.factor(test$denied[test$minority == 1]) )

r$table[4]/(r$table[4] + r$table[3]) # TPR
r$table[2]/(r$table[2] + r$table[1]) # FPR
r$table[3]/(r$table[3] + r$table[4]) # FNR

#non minorities
r = confusionMatrix(as.factor(ypreds[test$minority == 0]),as.factor(test$denied[test$minority == 0]) )

r$table[4]/(r$table[4] + r$table[3]) # TPR
r$table[2]/(r$table[2] + r$table[1]) # FPR
r$table[3]/(r$table[3] + r$table[4]) # FNR

#######################################


a = get_results(train$denied,predtrain,train$minority,"log reg w minor, in-sample")
write.csv(a,"logregwithminorities_insample.csv",row.names = FALSE)

##################
#Predict all as same 

train_m = train
test_m = test

train_m$minority = 0
test_m$minority = 0

mod <- glm(denied~., data=train_m, family="binomial")

predtrain <- predict(mod, newdata=train_m, type="response")
predtest <- predict(mod, newdata=test_m, type="response")



#######################################
threshold = 0.5

sum((predtest>threshold) == test$denied)/length(test$denied)


classification <- data.frame(pred=predtrain,A=predtrain>threshold)

library(ROCR)
rocr.pred <- prediction(predtest, test$denied)
rocr.pred.df <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]],
                           tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])

auc <- performance(rocr.pred, "auc")@y.values[[1]]
auc

summary(mod)


library(caret)

ypreds = (predtest > threshold)
ypreds[FALSE] = 0

ypredtrain = (predtrain > threshold)
ypredtrain[FALSE] = 0

confusionMatrix(as.factor(ypreds),as.factor(test$denied) )

sum((predtest>threshold) == test$denied)/length(test$denied)

#minorities
r = confusionMatrix(as.factor(ypreds[test$minority == 1]),as.factor(test$denied[test$minority == 1]) )

r$table[4]/(r$table[4] + r$table[3]) # TPR
r$table[2]/(r$table[2] + r$table[1]) # FPR
r$table[3]/(r$table[3] + r$table[4]) # FNR

#non minorities
r = confusionMatrix(as.factor(ypreds[test$minority == 0]),as.factor(test$denied[test$minority == 0]) )

r$table[4]/(r$table[4] + r$table[3]) # TPR
r$table[2]/(r$table[2] + r$table[1]) # FPR
r$table[3]/(r$table[3] + r$table[4]) # FNR
#######################################
a = get_results(train$denied,predtrain,train$minority,"log reg w/o minor in-sample")
write.csv(a,"logregwithoutminorities_insample.csv",row.names = FALSE)


##################
#Train on minorities, predict as white

test_m = test

test_m$minority = 0

mod <- glm(denied~., data=train, family="binomial")

predtrain <- predict(mod, newdata=train, type="response")
predtest <- predict(mod, newdata=test_m, type="response")

a = get_results(test_m$denied,predtest,test$minority,"log reg trained with minorities, tested as if everyone is white")
write.csv(a,"logreg_testignoringminorities.csv",row.names = FALSE)

