library(tidyverse)

# HELLO

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
set.seed(123)
train_index <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[train_index, ]
test  <- df[-train_index, ]
nrow(train)
nrow(test)



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




### EVALUATION ###
# CART MAE, RMSE, R-squared
cart_mae  <- MAE(cart_pred, test$satisfaction_level)
cart_rmse <- RMSE(cart_pred, test$satisfaction_level)
cart_r2   <- R2(cart_pred, test$satisfaction_level)



