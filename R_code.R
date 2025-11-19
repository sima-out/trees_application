library(tidyverse)

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
# df$satisfaction_cat <- ifelse(df$satisfaction_level <= 0.60, "burnout", "happy")

df$satisfaction_cat <- cut(
  df$satisfaction_level,
  breaks = c(-Inf, 0.60, 0.90, 1),
  labels = c("very_low", "medium", "high"),
  include.lowest = TRUE
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
df_reg <- df %>% select(-satisfaction_cat)
set.seed(123)
train_index <- sample(1:nrow(df_reg), 0.8 * nrow(df_reg))
train <- df_reg[train_index, ]
test  <- df_reg[-train_index, ]
nrow(train)
nrow(test)

# classification
df_class <- df %>% select(-satisfaction_level)
set.seed(123)
class_index <- sample(1:nrow(df_class), 0.8*nrow(df_class))
train_class <- df_class[class_index, ]
test_class  <- df_class[-class_index, ]



### CART MODEL ###
library(rpart)
library(rpart.plot)

# Fit regression tree on training data
cart_tree <- rpart(satisfaction_level ~ .,
                  data = train,
                  method = "anova") # rpart automatically prune the tree using 10-fold CV
plotcp(cart_tree) # complexity parameter plot
summary(cart_tree)

# Visualize the regression tree
rpart.plot(cart_tree,
           type = 3, fallen.leaves = TRUE,
           digits = 3,
           main = "Regression Tree for Predicting Employee Satisfaction")

# Predict satisfaction on test data
cart_pred <- predict(cart_tree, newdata = test)
head(cart_pred)


### CIT MODEL ###
# install.packages("partykit")
library(partykit)

#very messy tree
c_tree <- ctree(satisfaction_level ~ .,
                data = train)
plot(c_tree, 
     main = "Conditional Inference Tree for Predicting Employee Satisfaction",
     tp_args = list(col = "blue", cex = 0.6))

# cleaner but lacks depth
limit_c_tree <- ctree(satisfaction_level ~ ., 
                data = train, 
                control = ctree_control(maxdepth = 3))
plot(limit_c_tree, 
     main = "Conditional Inference Tree for Predicting Employee Satisfaction",
     tp_args = list(col = "blue", cex = 0.6))

#prediction
c_tree_pred <- predict(c_tree, newdata = test)
head(c_tree_pred)

### EVALUATION ###
# 10 fold cross validation

library(caret)
cv <- trainControl(method = "cv", number = 10)

c_tree_cv <- train(satisfaction_level ~ ., data = train, method = "ctree",
                   trControl = cv)
c_tree_cv

# CART MAE, RMSE, R-squared
cart_mae  <- MAE(cart_pred, test$satisfaction_level)
cart_rmse <- RMSE(cart_pred, test$satisfaction_level)
cart_r2   <- R2(cart_pred, test$satisfaction_level)



