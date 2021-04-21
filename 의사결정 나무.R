# 의사결정 나무 만들기 

# install.packages('party')
library(party)

# install.packages('datasets')
library(datasets)

str(airquality)

formula <- Temp ~ Solar.R + Wind + Ozone
air_ctree <- ctree(formula , data = airquality) ; air_ctree
plot(air_ctree)

# iris

idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ]
test <- iris[-idx, ]

formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(formula, data = train) ; iris_ctree
plot(iris_ctree)

# iris 혼돈 매트릭스 생성 
pred <- predict(iris_ctree, test)
table(pred, test$Species)

(17+14+13) / nrow(test)     #  0.9777778

# 고속도로 주ᄒ  영향 변수 
# install.packages('ggplot2')
library(ggplot2)
data("mpg")


t <- sample(1:nrow(mpg), 120)
train <- mpg[-t, ]
test <- mpg[t, ]
dim(train)
dim(test)


test$drv <- factor(test$drv)
formula <- hwy ~ displ + cyl + drv
tree_model <- stree(formula, data = test)
plot(tree_model)
#결론 : 엔진크기가 작으면서 전륜이나 후륜 구동방식인 경우 주 거리가 가장좋다. 엔진이 크고 사륜은 주행거리 적다.




# AdultUCI 데이터 
# install.packages('arules')
library(arules)
data(AdultUCI)
str(AdultUCI)

names(AdultUCI)

set.seed(1234)
choice <- sample(1:nrow(AdultUCI), 10000) ; choice
adult.df <- AdultUCI[choice, ]

capital <- adult.df$`capital-gain`
hours <- adult.df$`hours-per-week`
education <- adult.df$`education-num`
race <- adult.df$race
age <- adult.df$age
income <- adult.df$income

adult.df <- data.frame(capital = capital, age = age, race = race, hours = hours, education = education ,
                       income = income)

str(adult.df)

formula <- capital ~ income + education + hours + race + age 
adult_ctree <- ctree(formula, data = adult.df)
adult_ctree
# 1) income  , 2) education 영향 1,2위 변수 

plot(adult.df)
plot(adult_ctree)

# capital(자본이득) 요약통계량 보기 
adultResult <- subset(adult.df,
                      adult.df$income == 'large' & adult.df$education > 14 )
length(adultResult$education)

summary(adultResult$capital)
boxplot(adultResult$capital)

# rpart 패키지로, 분류 분석 

# install.packages('rpart')
# install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

data(iris)
rpart_model <- rpart(Species ~., data = iris)
rpart_model

rpart.plot(rpart_model)

# weather 데이ᄐ 셋 으로 분석 

weather <- read.csv('weather.csv' , header = T)
str(weather)
weather.df <- rpart(RainTomorrow ~., data = weather[ ,c(-1, -14)], cp = 0.01)
rpart.plot(weather.df)

weather_pred <- predict(weather.df, weather) ; weather_pred 
weather_pred2 <- ifelse(weather_pred[ ,2] >= 0.5, 'Yes', 'No')
table(weather_pred2, weather$RainTomorrow)
(278 + 53) / nrow(weather)    #  0.9043716



