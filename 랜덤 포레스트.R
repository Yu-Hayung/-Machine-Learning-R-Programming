# 랜덤 포레스트 
install.packages('randomForest')
library(randomForest)
data(iris)

model = randomForest(Species ~., data =iris) ; model
#                      Number of trees: 500     (학습데이터 500개)
# No. of variables tried at each split: 2       (2개의 변수로 노드 분류)

model2 = randomForest(Species ~., data =iris, 
                      ntree = 300,
                      mtry = 4,
                      na.action = na.omit) ; model2

model3 = randomForest(Species ~., data =iris, 
                      importance = T,
                      na.action = na.omit) ; model3

varImpPlot(model)
varImpPlot(model2)
varImpPlot(model3)



# 최적의 파라미터 찾기 
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n = ntree, m= mtry) ;param

# 모델 생성 하기 
for(i in param$n){
  cat('ntree =', i, '\n')
  for( j in param$m){
    cat('mtry = ', j , '\n')
    model_iris <- randomForest(Species ~., data = iris,
                               ntree = i , 
                               mtry = j ,
                               na.action = na.omit)
    print(model_iris)
  }
}



# 래 2 ##############################################################
library(rpart)
data("stagec")
stagec 

idx <- sample(1:nrow(stagec), nrow(stagec) * 0.7)
trainData <- stagec[idx, ]
testData <- stagec[-idx, ]


library(randomForest)
rf <- randomForest(ploidy ~ ., 
                   data = trainData, 
                   ntree = 100,
                   proximity = TRUE,
                   na.action = na.omit)  ; rf
plot(rf)

importance(rf)
varImpPlot(rf , main = '변수 영향력', family="NanumGothicBold")

rf.pred <- predict(rf, newdata=testData) ; rf.pred 
table(rf.pred, testData$ploidy)
(16 + 22 + 0) / nrow(testData)
# [1] 0.8636364

plot(margin(rf))
