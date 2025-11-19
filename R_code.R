library(tidyverse)
library(dplyr)

### PREPROCESSING ###

# Load & explore data
df <- read.csv("Employee Attrition.csv")
str(df)
summary(df) # we do have average_monthly_hours > 250, we'll look into it

# Check missing values
colSums(is.na(df))
df <- df[!is.na(df$Emp.ID), ] # removing 788 empty rows as they are completely empty
colSums(is.na(df))

# Drop Emp ID (not useful for prediction)
df$Emp.ID <- NULL

# Convert categorical variables to factors
df$Work_accident <- as.factor(df$Work_accident)
df$promotion_last_5years <- as.factor(df$promotion_last_5years)
df$dept <- as.factor(df$dept)
df$salary <- factor(df$salary, levels = c("low", "medium", "high"), ordered = TRUE)

### TRANSFORM SATISFACTION VARIABLE INTO CATEGORICAL ###
# Burnout = 0.00–0.60, Happy = 0.61–1.00
df$satisfaction_cat <- ifelse(df$satisfaction_level <= 0.65, "quit", "no quit")

#df$satisfaction_cat <- cut(
#  df$satisfaction_level,
#  breaks = c(-Inf, 0.65, 1),
#  labels = c( "medium", "high"),
# include.lowest = TRUE
)

df$satisfaction_cat <- as.factor(df$satisfaction_cat)

table(df$satisfaction_cat)  # check class balance
#hist(df$satisfaction_level, breaks = 20)

# Exploratory Data Analysis
hist(df$satisfaction_level, 
     main="Satisfaction Level Distribution", col="skyblue")
hist(df$average_montly_hours, col="skyblue",
     main="Distribution of Monthly Hours",
     xlab="Average Monthly Hours") # we do have quite some employees with avg_monthly hours > 250

# Boxplot for categorical vars
boxplot(satisfaction_level ~ dept, data=df,
        main="Satisfaction by Department")
boxplot(satisfaction_level ~ salary, data=df,
        main="Satisfaction by Salary Level")
barplot(table(df$Work_accident))
barplot(table(df$promotion_last_5years))

# Boxplot for numerical vars
numeric_vars <- df %>% dplyr::select_if(is.numeric)
par(mfrow=c(2,3))  # 2 rows, 3 columns
for (col in names(numeric_vars)) {
  boxplot(numeric_vars[[col]],
          main = paste("Boxplot of", col),
          col = "skyblue",
          ylab = col)
}
par(mfrow=c(1,1))

# Train-test split
# regression
df_reg <- df %>% dplyr::select(-satisfaction_cat)
set.seed(123)
train_index <- sample(1:nrow(df_reg), 0.8 * nrow(df_reg))
train <- df_reg[train_index, ]
test  <- df_reg[-train_index, ]
nrow(train)
nrow(test)

# classification
df_class <- df %>% dplyr::select(-satisfaction_level)
set.seed(123)
class_index <- sample(1:nrow(df_class), 0.8*nrow(df_class))
train_class <- df_class[class_index, ]
test_class  <- df_class[-class_index, ]

### CIT REGRESSION ###
library(partykit)
library(caret)

# Regression CIT
cit_reg <- ctree(
  satisfaction_level ~ .,
  data = train, 
  control = ctree_control(maxdepth = 3)
)

plot(cit_reg,
     main = "CIT Regression Tree – Predicting Satisfaction Level")

# Predict on test
cit_reg_pred <- predict(cit_reg, newdata = test)  # numeric

# Evaluate regression performance
cit_reg_rmse <- RMSE(cit_reg_pred, test$satisfaction_level)
cit_reg_mae  <- MAE(cit_reg_pred,  test$satisfaction_level)
cit_reg_r2   <- R2(cit_reg_pred,   test$satisfaction_level)

cit_reg_rmse
cit_reg_mae
cit_reg_r2

### CIT CLASSIFICATION ###
set.seed(123)
cit_clf <- ctree(
  satisfaction_cat ~ last_evaluation + number_project + average_montly_hours +
    time_spend_company + Work_accident + promotion_last_5years + dept + salary,
  data = train_class,
  control = ctree_control(maxdepth = 3)
)

plot(cit_clf,
     main = "CIT Classification Tree – Unlikely to Quit/Likely to Quit")

# Predict on test (factor output)
cit_clf_pred <- predict(cit_clf, newdata = test, type = "response")

# Evaluation
cit_clf_cm <- confusionMatrix(cit_clf_pred, test_class$satisfaction_cat)
cit_clf_cm

### CART for classification ###
library(rpart)
library(rpart.plot)

# Grow CART classification tree
set.seed(123)
cart_clf <- rpart(
  satisfaction_cat ~ last_evaluation + number_project + average_montly_hours +
    time_spend_company + Work_accident + promotion_last_5years + dept + salary,
  data   = train_class,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 10, minbucket = 5, maxdepth = 5)
)

# Check CP table & prune
printcp(cart_clf)
best_cp <- cart_clf$cptable[which.min(cart_clf$cptable[, "xerror"]), "CP"]
cart_clf_pruned <- prune(cart_clf, cp = best_cp)

# Visualize pruned tree
rpart.plot(
  cart_clf_pruned,
  fallen.leaves = TRUE,
  digits = 3,
  main = "CART Classification Tree – Unlikely to Quit/Likely to Quit"
)

# Predict on test (classes)
cart_clf_pred <- predict(cart_clf_pruned, newdata = test, type = "class")

# Evaluate
cart_clf_cm <- confusionMatrix(cart_clf_pred, test_class$satisfaction_cat)
cart_clf_cm
