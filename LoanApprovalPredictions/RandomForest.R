# Helper packages
library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics

# Modeling packages
library(ranger)
library(foreach)
library(parallel)
library(tidyr)
library(vip)
library(caret)
library(themis)
library(doParallel)

df = read.csv("train.csv")

# Pre-processing

correction_df = df|>filter(person_age>100)
train_df = df|>filter(id!=correction_df$id)|>select(-id)


# train.test.split = function(df, tp){
#   param_split = createDataPartition(df$loan_status, times = 1, p=tp, list=FALSE)
#   list("train" = df[param_split,], "test"=df[-param_split,])
# }

# train_df = (train.test.split(df,0.8))[["train"]]






set.seed(123)

base.model <- ranger(
  loan_status ~ .,
  data = train_df,
  classification = TRUE,
  mtry = 5,
  num.trees = 500,
  min.node.size = 1,
  replace = FALSE,
  sample.fraction = 0.8,
  importance = 'impurity',
  splitrule = 'gini'
)

base.model$confusion.matrix
features <- base.model$variable.importance %>% names()
feature.importance <- base.model$variable.importance %>% as_tibble() %>% mutate(feature=features) %>% arrange(value %>% desc())

important.feature.p <- 0.95
n_important.features <- floor(important.feature.p * nrow(feature.importance))
important.features <- feature.importance[1:n_important.features,] %>% select(feature) %>% as.matrix() %>%  as.vector()

pipeline <- function(df) {
  # convert the variable into categorical
  cat_cols = c(
    'person_home_ownership',
    'loan_intent',
    'loan_grade',
    'cb_person_default_on_file'
  )
  df <- df %>%  mutate(across(cat_cols, function(c) {
    as.factor(c) %>% as.numeric()
  })) %>% as.data.frame()
  
  # df <- df %>% to.dummy(dummy_cols)
  
  
  cols.to.retain <- important.features
  
  if ("loan_status" %in% colnames(df)) {
    print("smoting")
    # hence this will work only in case of training data.
    df$loan_status <-  as.factor(df$loan_status)
    df <-  df %>%  smote(var = 'loan_status')
    cols.to.retain <- cols.to.retain %>% append('loan_status')
  }
  
  df %>% select(all_of(cols.to.retain))
}


test_df=read.csv("test.csv")
test_df.id = test_df$id
test_df = test_df|>select(-id)
train_df = df|>filter(id!=correction_df$id)|>select(-id)

train_df <- train_df %>% pipeline()
test_df <- test_df %>% pipeline()

n_features <- ncol(train_df) - 1

# Modeling
hyper.grid <- expand.grid(
  mtry = c(n_features/3, n_features/2, floor(n_features ** 0.5), (floor((n_features+1) ** 0.5))),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5, 10)
)

group_fit_control <- trainControl(method='cv')

cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)
rf.model <- train(loan_status ~., data=train_df, method='ranger', trControl = group_fit_control, tuneGrid=hyper.grid, allowParallel=TRUE)

# Prediction..
test_y_pred <-  predict(rf.model, newdata = test_df)

stopCluster(cl)
output = data.frame(id = test_df.id, loan_status = test_y_pred)
write.csv(output,
          "submission_rf.csv",
          row.names = FALSE,
          quote = FALSE)


