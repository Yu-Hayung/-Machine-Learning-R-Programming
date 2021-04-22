# 교과 군집 분석 실습 

# 유클리디안 거리 
x <- matrix(1: 9, nrow = 3, by = T ) 

# dist( )  = 유클리디안 거리 함수 

x_dist <- dist(x, method = "euclidean")

# 계층적 군집 분석 #############################################################

# install.packages("cluster")
library(cluster)
x <- matrix(1:9, nrow = 3,  by = T)
dist <- dist(x, method = "euclidean")
hc <- hclust(dist)
plot(hc)


# 면접 시험 데이터 군집 분석 
interview
names(interview)

interview_df <- interview[c(2:7)]
idist <- dist(interview_df)

hc <- hclust(idist); hc
plot(hc)

plot(hc, hang = -1) # 음수 값 제외 

rect.hclust(hc, k=3, border = "red")

g1 <- subset(interview, no == 108| no == 100 | no == 107 |no == 112 | no == 115)
g2 <- subset(interview, no == 102| no == 101 | no == 104 |no == 106 | no == 113)
g3 <- subset(interview, no == 105| no == 114 | no == 109 |no == 103 | no == 111)

summary(g1)
summary(g2)
summary(g3)

# 군집 수 자르기 ################################################################
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)
rect.hclust(hc, k = 4, border = "red")

# cutree() 함수로 자를 수 있다. 
ghc <- cutree(hc, k= 3)
iris$ghc <- ghc
table(iris$ghc)

g1 <- subset(iris, ghc == 1)
summary(g1)

