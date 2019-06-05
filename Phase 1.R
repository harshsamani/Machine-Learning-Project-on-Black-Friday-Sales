install.packages("sqldf")
install.packages("DataExplorer")
install.packages("mlbench")
library(caret)
library(glmnet)
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(plotly)
library(psych)
library(mlbench)

setwd("/Users/harshsamani/Desktop/Masters/Data Mining Applications/Week 2/")
dataset <- read.csv('BlackFriday.csv', header = TRUE)

dataset <- subset( dataset, select = -c(10, 11 ) )
colnames(dataset)
summary(dataset)
head(dataset)
str(dataset)

# Function to create CSV file
table <- function(input,output ) {
  library(psych)
  input <- input[c("n","mean","sd","min","max")]
  write.csv(input, output)
}
table(input=describe(dataset), output="Output.csv") #Descriptive Statistics

#replacing NA values to 0
dataset$Product_Category_1[dataset$Product_Category_1==NA]<-0;
dataset$Product_Category_1[is.na(dataset$Product_Category_1)]<-0;
dataset$Product_Category_1

# User ID with the same ID 
library("sqldf")
gender_dataset = sqldf("SELECT User_ID, Gender FROM dataset GROUP BY (User_ID)")
head(gender_dataset)
summary(gender_dataset$Gender)

library("ggplot2")
gender_plot  = ggplot(data = gender_dataset) + geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) + 
  labs(title = 'Gender of Customers') 
plot(gender_plot)

# Customers Purchase based on their ID
customers_total_purchase = dataset %>%group_by(User_ID) %>%summarise(Purchase_Amount = sum(Purchase))
head(customers_total_purchase)

# Count of product id (Top Selling Products)
library("dplyr")
product_dataset <- dataset %>%count(Product_ID, sort = TRUE)
head(product_dataset)

# Age rage bin created
age_customer = dataset %>% select(User_ID, Age) %>% distinct() %>% count(Age)
head(age_customer)

age_plot = ggplot(data = age_customer) + geom_bar(fill="steelblue", stat = 'identity', mapping = aes(x = Age, y = n, fill = Age))+labs(title = "Customers Age")
plot(age_plot)

hist_age <- ggplot(dataset, aes(x = Purchase, fill = Age)) +
  geom_histogram(bins = 60) +
  facet_wrap(~ Age) +
  labs(title= "Purchases Histogram by Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
hist_age

# User_ID and City_Category
user_loc <- sqldf("SELECT DISTINCT User_ID, City_Category FROM dataset")
head(user_loc)

city_category = ggplot(data = user_loc) +geom_bar(color = 'white', mapping = aes(x = City_Category, y = ..count.., fill = City_Category)) +labs(title = 'Customers Location')
city_category # most of the customers live in city C


# Purchase made by particular user id
user_purchase <- sqldf("SELECT User_ID, COUNT (User_ID)  FROM dataset GROUP BY (User_ID) ")
head(user_purchase)

purchase_plot <- ggplot(dataset, aes(x = Purchase)) +
  geom_histogram(bins = 75) +
  labs(title= "Purchases Histogram")
purchase_plot


# Regression Model

set.seed(1000)
up_data <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8,0.2))
train_data <- dataset[up_data==1,]
test_data <- dataset[up_data==2,]

head(train_data)
head(test_data)

custom_control <- trainControl(method = "repeatedcv",
                               number = 12,
                               repeats = 6,
                               verboseIter = T)
   
custom_control


ridge_model <- train(Purchase ~ Age + Gender + Product_Category_1 + City_Category + Stay_In_Current_City_Years + Occupation,
                 train_data,
                 method = 'glmnet',
                 tuneGrid = expand.grid(alpha = 1, 
                                         lambda =seq(0.0001, 1, length = 5 )),
                 trControl = custom_control)

# Results
plot(ridge_model)
ridge_model
plot(ridge_model$finalModel, xvar = "lambda", label = T)
plot(ridge_model$finalModel, xvar = "dev", label = T)
plot(varImp(ridge_model, scale = T))


p1 <- predict(ridge_model, train_data)
rmse1 <- sqrt(mean((train_data$Purchase - p1)^2))
rmse1

p2 <- predict(ridge_model, test_data)
rmse2 <- sqrt(mean((test_data$Purchase - p2)^2))
rmse2

p3 <- predict(ridge_model, dataset)
rmse3 <- sqrt(mean((dataset$Purchase - p3)^2))
rmse3
