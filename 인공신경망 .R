# 인공신경망 

# install.packages('nnet')
library(nnet)

df = data.frame(
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no','no','no','yes','yes','yes'))
)

str(df)

model_net = nnet(y ~ ., df, size = 1)
model_net 

summary(model_net)
model_net$fitted.values

p <- predict(model_net, df, type = 'class')
table(p, df$y)

data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training <- iris[idx, ]
testing <- iris[-idx, ]
nrow(training)
nrow(testing)


model_net_iris1 = nnet(Species ~., training, size = 1)
model_net_iris1

model_net_iris3 = nnet(Species ~., training, size = 3)
model_net_iris3 

# 가중치 확인
summary(model_net_iris1)
summary(model_net_iris3)



# 모델 평가 
table(predict(model_net_iris1, testing, type = 'class'), testing$Species)
(14 + 17 + 14) / nrow(testing) # [1] 1

table(predict(model_net_iris3, testing, type = 'class'), testing$Species)
(14 + 17 + 14) / nrow(testing) # [1] 1




# 역전파 
# install.packages('neuralnet')
library(neuralnet)

idx <- sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)

training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3
training_iris$Species <- NULL 

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species ==  'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species ==  'virginica'] <- 3
testing_iris$Species <- NULL 


# 정규화 
normal <- function(x){
  return((x-min(x))/ (max(x) - min(x)))
}

training_nor <- as.data.frame(lapply(training_iris, normal))
testing_nor <- as.data.frame(lapply(testing_iris, normal))

summary(training_nor)
summary(testing_nor)

# 인공신경망 
model_net <- neuralnet(Species2 ~  Sepal.Length + Sepal.Width + 
                         Petal.Length + Petal.Width, data = training_nor, hidden = 1 )

plot(model_net)

# 모델 성능 평가 
model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result

cor(model_result$net.result, testing_nor$Species2)
# [1,] 0.9678278



# 노드 2개 인공 신경망 상관관계 ('backprop'속성 적용- 역전파 )
model_net2 <- neuralnet(Species2 ~  Sepal.Length + Sepal.Width + 
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2,
                       algorithm = 'backprop',
                       learningrate = 0.01)

model_result2 <- compute(model_net2, testing_nor[c(1:4)])
cor(model_result2$net.result, testing_nor$Species2)
# [1,] 0.9619953


