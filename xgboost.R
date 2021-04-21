# 부스팅 

# install.packages('xgboost')
library(xgboost)
iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
table(iris_label)

iris$label <- iris_label

idx <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[idx, ]
test <- iris[-idx, ]


train_mat <- as.matrix(train[-c(5:6)])
dim(train_mat)
train_lab <- train$label
length(train_lab)

dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

xgb_model <- xgboost(data = dtrain, 
                     max_depth = 2,
                     eta = 1,
                     nthread = 2,
                     nrounds = 2,
                     objective = "multi:softmax",
                     num_class = 3,
                     verbose = 0)  ; xgb_model

test_mat <- as.matrix(test[-c(5:6)])
dim(test_mat)
# [1] 45  4

test_lab <- test$label 
length(test_lab)
# [1] 45


pred_iris <- predict(xgb_model, test_mat)

table(pred_iris, test_lab)

(11+11+20) / length(test_lab)
# [1] 0.9333333

importance_matrix <- xgb.importance(colnames(train_mat), model = xgb_model) ; importance_matrix

xgb.plot.importance(importance_matrix, main = 'importance_matrix')



