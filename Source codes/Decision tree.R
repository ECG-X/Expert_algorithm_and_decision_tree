library(readxl)
library(C50)
library(dplyr)
library(plyr)


data_for_new_rule <- read_excel("Computers in Biology and Medicine/Human rules vs ML/UnbalancedData.xlsx", 
                                sheet = "Training")
dataShuffled <- data_for_new_rule[sample(nrow(data_for_new_rule)), ] #shuffled the data

form <- "Outcome ~ Purble_Blue_Green + Blue_Green_Lime + Green_Lime_Yellow + Lime_Yellow_Orange + Yellow_Orange_DarkOrange + InflectionPoint"
folds <- split(dataShuffled, cut(sample(1:nrow(dataShuffled)),5))

errs.c50 <- rep(NA, length(folds))
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  train$Outcome <-as.factor(train$Outcome)
  str(train$Outcome)
  test$Outcome <-as.factor(test$Outcome)
  str(test$Outcome)
  tmp.model <- C5.0(as.formula(form), train, rules = TRUE)
  summary(tmp.model)
  tmp.predict <- predict(tmp.model, newdata=test)
  
  name <- paste0("~/Computers in Biology and Medicine/Human rules vs ML/")
  DT_results <- cbind(test,tmp.predict)
  write.csv(DT_results,paste0(name, i,"UnbalancedData.csv"))
  
  conf.mat <- table(test$Outcome, tmp.predict)
  errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}


print(sprintf("average error using k-fold cross validation and C5.0 decision tree algorithm: %.3f percent", 100*mean(errs.c50)))



############################################################

library(readxl)
library(C50)

#Generate the model 
data_for_new_rule <- read_excel("Computers in Biology and Medicine/Human rules vs ML/UnbalancedData.xlsx", 
                                sheet = "Training")
dataShuffled <- data_for_new_rule[sample(nrow(data_for_new_rule)), ] #shuffled the data
tra.data <- data_for_new_rule

vars <- c("Purble_Blue_Green", "Blue_Green_Lime","Green_Lime_Yellow","Lime_Yellow_Orange","Yellow_Orange_DarkOrange","InflectionPoint")
str(data_for_new_rule[, c(vars, "Outcome")])

#train on 80% of the data
object.C50 <- C5.0(x = tra.data[, vars], y = as.factor(tra.data$Outcome), rules = TRUE)
object.C50
summary(object.C50)
#plot(object.C50)


#test on 20% of the data
DataforAlgoirthm <- read_excel("Computers in Biology and Medicine/Human rules vs ML/UnbalancedData.xlsx", 
                               sheet = "Testing")
Outcome <- DataforAlgoirthm$Outcome
Test <- DataforAlgoirthm[, 1 : 7]
pred <- predict(object.C50, Test)
err <- (100 * sum(pred != Outcome)) / length(pred) 
err


write.table(pred,"~/Computers in Biology and Medicine/Human rules vs ML/Unbalanced_Pred_DT_on_20%Testingdata.csv", sep = ",", col.names = !file.exists("myDF.csv"))

###################################################################################


library(pROC)
PredictProb <- predict(object.C50, Test2, type = 'prob')
auc <- auc(Outcome2,PredictProb[,2])
auc
plot(roc(Outcome2,PredictProb[,2]))

####################################################################################################

Plot 

library(precrec)
library(grid)
library(ggplot2)


sscurves <- evalmod(scores = PredictProb[,2], labels = Outcome2)
autoplot(sscurves)
auc(sscurves)


sspoints <- evalmod(mode = "basic", scores = PredictProb[,2],labels = Outcome2)

# Show normalized ranks vs. error rate and accuracy
autoplot(sspoints, c("error", "accuracy"))

# Show normalized ranks vs. specificity, sensitivity, and precision
autoplot(sspoints, c("specificity", "sensitivity", "precision"))

# Show normalized ranks vs. Matthews correlation coefficient and F-score
autoplot(sspoints, c("mcc", "fscore"))

####################################################################################################

