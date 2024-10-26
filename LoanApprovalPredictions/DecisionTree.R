# Note: This is not a part of the competition code. This is for practice

# Resources:
# -------------
# 1. https://bradleyboehmke.github.io/HOML/DT.html
# ---------------


# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting

# Modeling packagess
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)      # for feature effects

library(caret)

ames <-  AmesHousing::make_ames()

ames_train <- sample_frac(ames, size = 0.8)
ames_test <- ames %>% setdiff(ames_train)
na.counts <-  ames %>% sapply(FUN <-  function(col){sum(is.na(col))})
na.count.summary <-  data.frame(row.names <-  colnames(ames), "NA_count" <-  na.counts %>% as.vector())


ames_dt1 <- rpart(formula = Sale_Price ~ ., data = ames_train, method = "anova") # regression
rpart.plot(ames_dt1)

plotcp(ames_dt1)


ames_dt3 <- train( Sale_Price ~., data = ames_train, method="rpart", trControl = trainControl(method = "cv", number = 10), tuneLength = 20)

ggplot(ames_dt3)
