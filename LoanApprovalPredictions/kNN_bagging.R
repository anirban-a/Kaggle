library(dplyr)
library(ggplot2)
library(car)
library(themis)
library(caret)

library(doParallel)
library(foreach)
library(ipred)

source("Pipeline.R")

df = read.csv("train.csv")

# Pre-processing

correction_df = df|>filter(person_age>100)
df = df|>filter(id!=correction_df$id)|>select(-id)
correction_df = correction_df|>select(-id)


train.test.split = function(df, tp){
  param_split = createDataPartition(df$loan_status, times = 1, p=tp, list=FALSE)
  list("train" = df[param_split,], "test"=df[-param_split,])
}

train_df = (train.test.split(df,0.8))[["train"]]

trn_cntrl = trainControl(method = "cv", number = 5)
model.knn = train(person_age~., data = train_df, method="knn", trControl=trn_cntrl)

plot(model.knn)

age_predict = predict(model.knn, correction_df|>select(-person_age)) # this is to correct the outlier ages.
correction_df$person_age = age_predict|>round()


train_df = rbind(df, correction_df)


proc_df = train_df|>pipeline() #  ==> Processed dataframe.
test_df=read.csv("test.csv")
test_df.id = test_df$id
test_df = test_df|>select(-id)|>pipeline()

# Modeling

trn_cntrl = trainControl(method = "repeatedcv", repeats = 3)


set.seed(333)

cl <- makeCluster(20)
registerDoParallel(cl)

predictions <- foreach(icount(20), .packages = 'caret', .combine = cbind) %dopar% {
  idx <-  sample(nrow(proc_df), replace = TRUE)
  train <- proc_df[idx,]
  
  model.knn = train(loan_status~., train, trControl=trn_cntrl, method="knn", tuneLength=10)
  # model.knn = train(loan_status~., train, tuneGrid=data.frame(k=c(5)), method="knn")
  predict(model.knn, newdata = test_df)
}
stopCluster(cl)
