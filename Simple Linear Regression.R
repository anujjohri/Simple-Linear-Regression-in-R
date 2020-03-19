#Import dataset

data <- read.csv(file = "C:/Users/Hp/Desktop/data science/R/Datasets/scores.csv")

summary(data)

#check datatype
str(data)

#check MV

sapply(data, function(x) sum(is.na(data)))

# check outlier

boxplot(data$Hours)   # no
boxplot(data$Score)   #yes

# Treatment of score

summary(data$Score)
upper <- 83.22 + 1.5* IQR(data$Score) ;  upper
data$Score [data$Score > upper] <- upper
boxplot(data$Score)
summary(data$Score)

# Data parition

set.seed(546)
library(caret)
Train <- createDataPartition(data$Score , p = 0.70 , list = FALSE)
training <- data[ Train ,]
testing <- data[ -Train ,]


# correlation check

cor(data)                      # it is more than 70%
plot(data$Hours , data$Score)

# Model building

model = lm(Score ~ Hours , data = data)
summary(model)

#Assumptions

par(mfrow = c(2,2))
plot(model)
library(lmtest)
dwtest(model)

plot(data$Score ~ data$Hours)

library(car)
ncvTest(model)

# Prediction

testing$predict1 <- predict(model , testing)
