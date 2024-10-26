library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)

library(caret)
library(rpart)
library(ipred)

library(rpart.control)
library(stringr)

set.seed(123)

# ames_bag2 <- train(
#   Sale_Price ~ .,
#   data = ames_train,
#   method = "treebag",
#   trControl = trainControl(method = "cv", number = 10),
#   nbagg = 200,
#   control = rpart.control(minsplit = 2, cp = 0)
# )


cl <- makeCluster(8)
registerDoParallel(cl)

predictions <- foreach(icount(160), .packages = 'rpart', .combine = cbind) %dopar% {
  index <- sample(nrow(ames_train), replace = TRUE)
  ames_train_boot <- ames_train[index, ]
  
  bagged_tree <- rpart(Sale_Price ~ .,
                       control = rpart.control(minsplit = 2, cp = 0),
                       data = ames_train)
  
  predict(bagged_tree, newdata = ames_test)
}
stopCluster(cl)


predictions_wide <- predictions %>% as.data.frame() %>% mutate(observation = 1:n(), actual = ames_test$Sale_Price)

predictions_wide %>% tidyr::gather(tree, predicted, -c(observation, actual)) %>% group_by(observation) %>%
  mutate(tree = stringr::str_extract(tree, '\\d+') %>% as.numeric()) %>% ungroup() %>%
  arrange(observation, tree) %>% group_by(observation) %>%
  mutate(avg_prediction = cummean(predicted)) %>% group_by(tree) %>%
  summarize(RMSE = RMSE(avg_prediction, actual)) %>%
  ggplot(aes(tree, RMSE)) + geom_line() + xlab('Number of trees')
