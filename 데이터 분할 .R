# 데이터 세트 분할 하기 


# 편중된 분할
iris_train <- iris[1:105, ]
iris_test  <- iris[106:150, ]


# 무작위 분할 
idx <- sample(1:nrow(iris), size = nrow(iris)*0.7, replace = F )  
iris_train <- iris[idx, ]
iris_test <-  iris[-idx, ]

dim(iris_train)
dim(iris_test)


# 일관성 여부 확인 
table(iris$Species)
# setosa versicolor  virginica 
# 50         50         50 

table(iris_test$Species)
# setosa versicolor  virginica 
# 12         17         16 

table(iris_train$Species)
# setosa versicolor  virginica 
# 38         33         34 


# 일관성 해결하기 
# install.packages("doBy")
library(doBy)

iris_train <- sampleBy(~Species, frac = 0.7, data = iris)
dim(iris_train)
# 105   5
table(iris_train$Species)
# setosa versicolor  virginica 
# 35         35         35 
# 균일하게 추출되ᄂ  볼 수 있다. 

iris_train


library(caret)
train.idx <- createDataPartition(iris$Species, p = 0.7, list = F)
iris_train <- iris[train.idx, ]
iris_test  <- iris[-train.idx, ]

dim(iris_train)
# [1] 105   5
dim(iris_test)
# [1] 105   5


table(iris_train$Species)
# setosa versicolor  virginica 
# 35         35         35 

table(iris_test$Species)
# setosa versicolor  virginica 
# 15         15         15 
