library(readxl)
library(tidymodels)
library(caTools)
library(naniar)
library(randomForest)
library(vip)
library(dials)
library(doParallel)

df <- read_excel("C:/Users/Reynaldi/Downloads/UAS DM/Breast Cancer Wisconsin (Diagnostic).xlsx")
View(df)

head(df)
summary(df)

df$diagnosis <- as.factor(df$diagnosis)
summary(df)

df <- subset(df, select = -c(id))
View(df)

colSums(is.na(df))#no missing data

#Data Splitting
set.seed(123)
split <- sample.split(df$diagnosis, SplitRatio = 0.7)
df_train <- subset(df, split == TRUE)
df_test <- subset(df, split == FALSE)

colSums(is.na(df_train))
colSums(is.na(df_test))#no missing data

#logistic modelling
df_recipe <- recipe(diagnosis ~., data = df_train) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal_predictors())

lm <- logistic_reg()

logistic_wf <- workflow() %>%
  add_model(lm) %>%
  add_recipe(df_recipe)

fitted_result_logistic <- logistic_wf %>%
  fit(data = df_train)

fitted_result_logistic %>%
  extract_fit_parsnip() %>%
  tidy()

fitted_result_logistic %>%
  extract_fit_parsnip() %>%
  tidy(exponentiate = T)

prediction_logistic <- augment(fitted_result_logistic, df_test)
prediction_logistic %>%
  roc_curve(diagnosis, .pred_B) %>%
  autoplot()

evaluate_metrics <- metric_set(accuracy, ppv, recall, f_meas)
evaluate_metrics(data = prediction_logistic, truth = diagnosis, estimate = .pred_class)
roc_auc(prediction_logistic, diagnosis, .pred_B)

#random forest
rfm <- rand_forest(mode = "classification") %>%
  set_engine("randomForest")

rf_wf <- workflow() %>%
  add_model(rfm) %>%
  add_recipe(df_recipe)

fitted_result_rf <- rf_wf %>%
  fit(data = df_train)

prediction_rf <- augment(fitted_result_rf, df_test)
prediction_rf %>%
  roc_curve(diagnosis, .pred_B)%>%
  autoplot()

evaluate_metrics <- metric_set(accuracy, ppv, recall, f_meas)
evaluate_metrics(data = prediction_rf, truth = diagnosis, estimate = .pred_class)
roc_auc(prediction_rf, diagnosis, .pred_B)

fitted_result_rf %>%
  extract_fit_parsnip() %>%
  vip()