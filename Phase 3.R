# LOAD PACKAGES
library(caret)
library(glmnet)
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(plotly)
library(psych)


# READING THE CSV FILE
mydataset <- read.csv("/Users/harshsamani/Desktop/Masters/Data Mining Applications/Week 4/BlackFriday.csv")
View(mydataset)
attach(mydataset)

# CHECKING THE STRUCTURE OF THE DATASET
mydataset <- subset( mydataset, select = -c(10, 11 ) )
colnames(mydataset)
head(mydataset)
str(mydataset)
summary(mydataset)

# CREATING A FUNCTION FOR CSV FILE
table <- function(input, output){
  input <- input[c("n", "mean", "sd", "min", "max")]
  write.csv(input, output)
}

# CSV FILE FOR DESCRIPTIVE STATISTICS
table(input = describe(mydataset), output = "Descriptive_Stats.csv")

# PLOT MISSING VALUES
plot_missing(as.data.frame(mydataset))

# EDA
# HISTOGRAM FOR PURCHASE
hist(Purchase)

# TO GET MODE
getmode <- function(var){
  unique <- unique(var)
  unique[which.max(tabulate(match(var, unique)))]
}
mode <- getmode(mydataset$Product_Category_2) # Mode of Product_Category_2
mode

# DATA MUNGING
mydataset$Product_Category_2 <- NULL
mydataset$Product_Category_3 <- NULL

# CORRELATION PLOT
plot_correlation(mydataset)

# Regression Model
set.seed(1000)
up_data <- sample(2, nrow(mydataset), replace = TRUE, prob = c(0.8,0.2))
train_data <- mydataset[up_data==1,]
test_data <- mydataset[up_data==2,]

head(train_data)
head(test_data)

custom_control <- trainControl(method = "repeatedcv",
                               number = 12,
                               repeats = 6,
                               verboseIter = T)

custom_control

# LINEAR REGRESSION
linear_model <- lm(Purchase ~ Age + Gender + Product_Category_1 + City_Category + Stay_In_Current_City_Years + Occupation, mydataset)
linear_model
summary(linear_model)
plot(linear_model)


# OPTIMIZATION
library(mlbench)
require(caret)
library(ggplot2)
library(corrplot)

set.seed(1000)
up_data <- sample(2, nrow(mydataset), replace = TRUE, prob = c(0.8,0.2))
train_data1 <- mydataset[up_data==1,]
test_data1 <- mydataset[up_data==2,]


up_data_train_data1 <- subset( train_data1, select = -c(1,2) )
up_data_train_data1 <- lapply(up_data_train_data1, as.numeric)
up_data_train_data1$Purchase <- factor(up_data_train_data1$Purchase)


up_data_test_data1 <- subset( test_data1, select = -c(1,2) )
up_data_test_data1 <- lapply(up_data_test_data1, as.numeric)
up_data_test_data1$Purchase <- factor(up_data_test_data1$Purchase)

str(up_data_train_data1)
str(up_data_test_data1)

install.packages("party")
library(party)
 


tree1 <- ctree(Purchase ~ Age + Gender + Product_Category_1 + City_Category + Stay_In_Current_City_Years + Occupation, data = up_data_train_data1, controls = ctree_control(mincriterion = 0.99, minsplit = 200000))
tree
plot(tree)
#Predict
predict(tree, up_data_test_data1)



