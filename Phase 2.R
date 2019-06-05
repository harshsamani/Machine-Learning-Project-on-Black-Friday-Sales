setwd("/Users/harshsamani/Desktop/Masters/Data Mining Applications/Week 3/")
dataset <- read.csv('BlackFriday.csv', header = TRUE)

summary(dataset)
head(dataset)
str(dataset)

updata <- subset( dataset, select = -c(1,2,10,11) )

values_wnull <- replace(updata,is.na(updata),0)
values_wnull
str(values_wnull)

pairs(values_wnull[1:8])

set.seed(1000)
up_data <- sample(2, nrow(values_wnull), replace = TRUE, prob = c(0.8,0.2))
train_data <- values_wnull[up_data==1,]
test_data <- values_wnull[up_data==2,]

head(train_data)
head(test_data)

library(corrplot)
library(RColorBrewer)

datacor <- cor(values_wnull1)

# Multiple Linear Regression 

results <- lm(Purchase ~ Age + Gender + Product_Category_3 + City_Category + Stay_In_Current_City_Years + Occupation,
              data = values_wnull)
results
summary(results)
