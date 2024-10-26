library(dplyr)
library(ggplot2)
library(car)
library(themis)
library(caret)

source("Pipeline.R")

df = read.csv("train.csv")

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

age_predict = predict(model.knn, correction_df|>select(-person_age))
correction_df$person_age = age_predict|>round()


train_df = rbind(df, correction_df)


proc_df = train_df|>pipeline()

tts = train.test.split(proc_df, 0.8)

train_df = tts[["train"]]
trn_cntrl = trainControl(method = "repeatedcv", repeats = 3)
set.seed(333)
model.knn = train(loan_status~., train_df, trControl=trn_cntrl, method="knn", tuneLength=10)

plot(model.knn)

valid_df = tts[["test"]]
X_valid = valid_df|>select(-loan_status)
y_valid = valid_df$loan_status

y_pred = predict(model.knn, newdata = X_valid)
confusionMatrix(y_pred, y_valid)


test_df=read.csv("test.csv")
test_df.id = test_df$id
test_df = test_df|>select(-id)

test_df = test_df|>pipeline()

test_y_pred = predict(model.knn, newdata = test_df)

output = data.frame(id=test_df.id, loan_status=test_y_pred)
write.csv(output, "submission.csv", row.names = FALSE, quote = FALSE)
