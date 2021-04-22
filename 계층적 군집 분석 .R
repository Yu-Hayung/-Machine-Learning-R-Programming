# hclust() 함수를 이용하여 계층적 군집 분석 ###################################
library(cluster)

data("USArrests")
str(USArrests)

d <- dist(USArrests, method = "euclidean")
fit <- hclust( d, method = "ave")
par(mfrow=c(1,2))
plot(fit)
plot(fit, hang = -1)

par(mfrow=c(1,1))

groups <- cutree(fit, k=6)
groups

plot(fit)
rect.hclust(fit, k=6, border="red")


hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k = 3, border = "red")
rect.hclust(hca, h = 50, which = c(2,7), border = 3:4)

# agnes() 함수로 계층적 군집 분석 ##############################################

library(cluster)
agn1 <- agnes(USArrests, metric="manhattan", stand=TRUE)
agn1
par(mfrow=c(1,2))
plot(agn1)

agn2 <- agnes(daisy(USArrests), diss=TRUE, method="complete")
plot(agn2)

agn3 <- agnes(USArrests, method ="flexible", par.meth=0.6)
plot(agn3)
par(mfrow=c(1,1))


