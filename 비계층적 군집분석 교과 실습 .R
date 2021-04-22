# 비계층적 군집 분석 

library(ggplot2)
data("diamonds")
t <- sample(1:nrow(diamonds), 1000)
test <- diamonds[t, ]
dim(test)

# 군집에 필요한 변수 추추 
mydia <- test[c('price','carat','depth', 'table')]
head(mydia)

result <- hclust(dist(mydia), method = 'average')
result

plot(result, hang = -1)


# 비계층적 군집 분석 
result2 <- kmeans(mydia, 3)
names(result2)

result2$cluster

mydia$cluster <- result2$cluster

cor(mydia[ , -5], method = 'pearson')
plot(mydia[ ,-5])


# 시각화 #######################################################################
# install.packages("mclust")
library(mclust)

# install.packages('corrgram')
library(corrgram)


corrgram(mydia[ ,-5], upper.panel = panel.conf)
corrgram(mydia[ ,-5], lower.panel = panel.conf)

plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$cluster[ , c("carat", "price")], col = c(3,5,7), pch = 9, cex = 5)


