library(tidyverse)
library(RCurl)
library(rpart)
library(randomForest)
library(caret)

#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
test_data = read_delim(url2, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

#set seed to be 7
set.seed(7)

#split the training data
train_ind = sample(seq_len(nrow(train_data)), size = 4000)
train = train_data[train_ind,]
test = train_data[-train_ind,]

##Bagging

#apply bagging method
bagging = randomForest(as.factor(train$X86) ~., data = train, 
                       mtry = 85, importance = TRUE)

#plot out-of-bag error to find the best ntree
plot(bagging, col = "darkorange")
#ntree = 240 looks decent
abline(v = 240)

#bagging with the best ntree
bagging = randomForest(as.factor(train$X86) ~., data = train, 
                        ntree = 240, mtry = 85, importance = TRUE)



bagging_pred = predict(bagging, newdata = test)

#confusion matrix and accuracy (over the testing data)
conf_tab_bg = table(Predicted = bagging_pred, Actual = test$X86)
sum(diag(conf_tab_bg)) / sum(conf_tab_bg)

#importance of vars. (Mean Decreasing Accuracy)
importance = importance(bagging)
importance[, 3:4] = abs(importance[, 3:4])
importance = sort(importance[,3], decreasing = TRUE)
head(importance, 10)


#confusion matrix and accuracy (over the target data)
bagging_pred_target = predict(bagging, newdata = test_data)
conf_tab_bg_actual = table(Predicted = bagging_pred_target, Actual = target_data$X1)
sum(diag(conf_tab_bg_actual)) / sum(conf_tab_bg_actual)

##Random Forest (mtry < 85)

#tune mtry with ntree with ntree = 240
tuned = tuneRF(x = train[, -86], y = as.factor(train$X86), 
               ntreeTry = 240, mtryStart = 42, stepFactor = 1.5, trace = FALSE)

#mtry's = 28, 42, 63
mtry_values = c(28, 42, 63)

#initialize accuracy vector
accuracy_results = numeric(length(mtry_values))

for (i in seq_along(mtry_values)) {
  
  #train rf model with mtry
  rf_model = randomForest(as.factor(X86) ~ ., data = train, mtry = mtry_values[i])
  
  #predict on the test
  rf_pred = predict(rf_model, newdata = test)
  
  #calculate accuracy
  conf_tab_33 = table(Predicted = rf_pred, Actual = test$X86)
  print(conf_tab_33)
  accuracy = sum(diag(conf_tab_33)) / sum(conf_tab_33)
  print(accuracy)
  #store accuracy
  accuracy_results[i] = accuracy
}

accuracy_results
# 28 seems to be the best

rf_model_best = randomForest(as.factor(X86) ~ ., data = train, mtry = 28)

importance_rf = as_tibble(importance(rf_model_best)) |>
  rownames_to_column("Var") |>
  arrange(desc(MeanDecreaseGini)) |>
  head(10)

rf_pred_target = predict(rf_model, newdata = test_data)

#confusion matrix and accuracy (over the target data)
conf_tab_rf_target = table(Predicted = rf_pred_target, Actual = target_data$X1)
sum(diag(conf_tab_rf_target)) / sum(conf_tab_rf_target)