library(dplyr)
library(ggplot2)
library(car)
library(themis)
library(caret)


to.dummy = function(data, select_columns=c()){

  for(c in select_columns){
    data[[c]] <- data[[c]] %>% as.factor() %>% as.numeric()
    unique.cnt <- max(data[[c]]) - min(data[[c]]) + 1
    zeros <- replicate(nrow(data)*unique.cnt, 0)
    dummy.matrix <- matrix(zeros, nrow=nrow(data), ncol = unique.cnt)
    for(i in c(1:nrow(data))){
      dummy.matrix[i, data[i,c]]=1
    }
    colnames(dummy.matrix) <- c(1:unique.cnt) %>% sapply(function(id){paste0(c,id)})
    data<- data %>% cbind(dummy.matrix) %>% select(-all_of(c))
  }
  data
}

pipeline = function(df){

  
  # convert the variable into categorical
  dummy_cols = c('person_home_ownership', 'loan_intent', 'loan_grade', 'cb_person_default_on_file')
  
  df <- df %>% to.dummy(dummy_cols)  
  
  
  if("loan_status" %in% colnames(df)){
    df$loan_status = as.factor(df$loan_status)  
    df = df|>smote(var = 'loan_status')
  }  
  
  param = preProcess(df, method = c("scale", "center"))
  df = predict(param, df)
  return(df)
}
