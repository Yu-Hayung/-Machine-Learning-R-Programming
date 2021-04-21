# 소프트 벡터 머신 

credit1 <- credit
credit1 <- read.csv('credit.csv', header = T)

credit1$Creditability <- as.factor(credit1$Creditability)
str(credit)

library(caret)

set.seed(123)

trData <- createDataPartition(y = credit1$Creditability, p = 0.7, list = F)
head(trData)
train <- credit1[trData, ]
test <- credit1[-trData, ]

str(train)

# install.packages('e1071')
library(e1071)
result1 <- tune.svm(Creditability~., data=train, gamma=2^(-5:0), cost = 2^(0:4), kernel="radial")
result2 <- tune.svm(Creditability~., data=train, cost = 2^(0:4), kernel="linear")
result3 <- tune.svm(Creditability~., data=train, cost = 2^(0:4), degree=2:4, kernel="polynomial")

result1$best.parameters 
result2$best.parameters
result3$best.parameters

normal_svm1 <- svm(Creditability~., data=train, gamma=0.0625, cost=1, kernel = "radial")
normal_svm2 <- svm(Creditability~., data=train, cost=1, kernel="linear")
normal_svm3 <- svm(Creditability~., data=train, cost=1, degree=3, kernel = "polynomial")

summary(normal_svm1)
summary(normal_svm2)
summary(normal_svm3)

normal_svm1$index
normal_svm2$index
normal_svm3$index

normal_svm1_predict <- predict(normal_svm1, test)
str(normal_svm1_predict)

normal_svm2_predict <- predict(normal_svm2, test) 
str(normal_svm1_predict)

normal_svm3_predict <- predict(normal_svm3, test)
str(normal_svm1_predict)

# radial ,  linear , polynomial  kernel 적용 시 Confusion Matrix 구성 및 Statistics
confusionMatrix(normal_svm1_predict, test$Creditability)  # Accuracy : 0.72   
confusionMatrix(normal_svm2_predict, test$Creditability)  # Accuracy : 0.7167     
confusionMatrix(normal_svm3_predict, test$Creditability)  # Accuracy : 0.7367     


# iris 예측 ################################################################

# install.packages('kernlanb')
library(kernlab)

model1 <- ksvm(Species~ ., data = iris)
iris_predicted <- predict(model1, newdata = iris)
table(iris_predicted, iris$Species)

