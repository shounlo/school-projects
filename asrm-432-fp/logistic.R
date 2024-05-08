library(tidyverse)
library(RCurl)
library(MASS)



#importing data from repo
url1 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt")
train_data = read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url2 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/predicts.txt")
test_data = read_delim(url2, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

url3 = getURL("https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/targets.txt")
target_data = read_delim(url3, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)



#Here we look at the contents and structure of the data.
head(train_data)
str(train_data)

#Making a local training dataset
training_data = train_data

#Set seed to 7
set.seed(7)

#split the training data
train_indices = sample(seq_len(nrow(training_data)), size = 4000)
train = train_data[train_indices,]
test = train_data[-train_indices,]

#Looking through the data, we find that the variables don't have their names assigned to the columns yet so we get the names off 
#of the website dictionary and set the column names accordingly.
#colnames(training_data) = c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD", "MGODRK", "MGODPR", "MGODOV", "MGODGE", "MRELGE", "MRELSA", "MRELOV", "MFALLEEN", "MFGEKIND", "MFWEKIND", "MOPLHOOG", "MOPLMIDD", "MOPLLAAG", "MBERHOOG", "MBERZELF", "MBERBOER", "MBERMIDD", "MBERARBG", "MBERARBO", "MSKA", "MSKB1", "MSKB2", "MSKC", "MSKD", "MHHUUR", "MHKOOP", "MAUT1", "MAUT2", "MAUT0", "MZFONDS", "MZPART", "MINKM30", "MINK3045", "MINK4575", "MINK7512", "MINK123M", "MINKGEM", "MKOOPKLA", "PWAPART", "PWABEDR", "PWALAND", "PPERSAUT", "PBESAUT", "PMOTSCO", "PVRAAUT", "PAANHANG", "PTRACTOR", "PWERKT", "PBROM", "PLEVEN", "PPERSONG", "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", "PPLEZIER", "PFIETS", "PINBOED", "PBYSTAND", "AWAPART", "AWABEDR", "AWALAND", "APERSAUT", "ABESAUT", "AMOTSCO", "AVRAAUT", "AAANHANG", "ATRACTOR", "AWERKT", "ABROM", "ALEVEN", "APERSONG", "AGEZONG", "AWAOREG", "ABRAND", "AZEILPL", "APLEZIER", "AFIETS", "AINBOED", "ABYSTAND", "CARAVAN")
#colnames(training_data)

#Here we are going to check if our response variables X86 which is the caravan variable is classified as a factor or not. 
is.factor(train$X86)
#We have confirmed that every variable is considered a numeric in the data as is and for the sake of consistency we will not alter the data globaly and rather just modify within function calls.
#


heatmap_full = heatmap(as.matrix(train))
summary(heatmap_full)

#Here we try running a glm with all of the variables to see what happens. Storing this model in "logistic_model_full".
logistic_model_full = glm(X86 ~ ., data = train, family = "binomial")

#Running a summary on "logistic_model".
summary(logistic_model_full)

#Get p-values for each coefficient
p_values_full = summary(logistic_model_full)$coefficients[, 4]

#Filter variables based on significance level of 0.05
significant_variables_full = names(p_values_full)[p_values_full < 0.05]
significant_variables_full


#Using this "full" model we proceed to see what the model would predict based off the training dataset. 
glm_predict_full = predict(logistic_model_full, test, type = 'response')
head(glm_predict_full)

#Classifying values that the predict function got above 0.5 as being TRUE or 1 and anything less to be FALSE or 0.
predicted_caravan_full = ifelse(glm_predict_full >= 0.5, 1, 0)
head(predicted_caravan_full)


#Determining the accuracy of the full model
accuracy_full = mean(predicted_caravan_full==test$X86)
print(accuracy_full)

conf_tab_rf_target = table(Predicted = predicted_caravan_full, Actual = test$X86)
sum(diag(conf_tab_rf_target)) / sum(conf_tab_rf_target)

#Making metrics
TP <- conf_tab_rf_target[2,2]
FP <- conf_tab_rf_target[1,2]
TN <- conf_tab_rf_target[1,1]
FN <- conf_tab_rf_target[2,1]

#true negative rate
specificity <- TN / (TN + FP)

#true positive rate
sensitivity <- TP / (TP + FN)

#Positive predictive value
precision <- TP / (TP + FP)

cat("Specificity:", specificity, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Precision:", precision, "\n")

#Now we are going to try model selection with the forward stepwise process using BIC as our metric.
train$X86 = factor(train$X86)
forward_model_bic = step(glm(X86 ~ 1, data = train, family = "binomial"), 
                           direction = "forward", k = log(nrow(train)), trace = 1)
summary(forward_model_bic)



#Now we're going to try best selection
best_step_bic = step(glm(X86 ~ ., data = train, family = "binomial"),
                                direction = "both", k = log(nrow(train)), trace = 1)
summary(best_step_bic)

#Holy cow that took a long time, but we got the Model and it included the variables X12, X18, X47, X59, and X82. 

#Using the "best" model we proceed to see what the model would predict based off the training dataset. 
glm_predict_best = predict(best_step_bic, test, type = 'response')
head(glm_predict_best)

predicted_caravan_best = ifelse(glm_predict_best >= 0.5, 1, 0)
head(predicted_caravan_full)


#Determining the accuracy of the full model
accuracy_best = mean(predicted_caravan_best==test$X86)
print(accuracy_best)

conf_tab_rf_target_best = table(Predicted = predicted_caravan_best, Actual = test$X86)
sum(diag(conf_tab_rf_target_best)) / sum(conf_tab_rf_target_best)

#Making metrics
TP_best <- conf_tab_rf_target_best[2,2]
FP_best <- conf_tab_rf_target_best[1,2]
TN_best <- conf_tab_rf_target_best[1,1]
FN_best <- conf_tab_rf_target_best[2,1]

#true negative rate
specificity_best <- TN_best / (TN_best + FP_best)

#true positive rate
sensitivity_best <- TP_best / (TP_best + FN_best)

#Positive predictive value
precision_best <- TP_best / (TP_best + FP_best)

cat("Specificity:", specificity_best, "\n")
cat("Sensitivity:", sensitivity_best, "\n")
cat("Precision:", precision_best, "\n")



