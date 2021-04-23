# 연관 분석 


# 연관 규칙 생성 
# install.packages('arules')
library(arules)

tran <- read.transactions('tran.txt', format = "basket", sep = ',') 
inspect(tran)

rule <- apriori(tran, parameter = list(supp = 0.3 , conf = 0.1))
inspect(rule)

rule <- apriori(tran, parameter = list(suppo = 0.1, conf = 0.1))
inspect(rule)


# 트랜잭션 객체 생성 

stran <- read.transactions('demo_single', format = 'single', cols = c(1, 2))
inspect(stran)

stran2 <- read.transactions('demo_single', format = 'single', cols = c(1, 2), sep = ',', rm.duplicates = T)
inspect(stran2)
# 중복제거 , 컴마 구분 

summary(stran2)


astran2 <- apriori(stran2)
inspect(astran2)
inspect(head(sort(astran2, by = "lift")))
btran <- read.transactions("demo_basket", format = "basket", sep = ',')
inspect(btran)

# 연관 규칙 시각화 

data("Adult")
str(Adult)

adult <- as(Adult, "data.frame")
str(adult)
head(adult)

summary(Adult)

# 지지도 10% 와 신뢰도 80% 가 적용된 연관규칙 발견 
ar <- apriori(Adult, parameter = list(supp = 0.1, conf = 0.8))  # 6137rule(s) 

# install.packages('arulesViz')
library(arulesViz)

plot (ar , method = "graph", control = list(type = "items"))


# Groceries 데이터셋 으로 연관 분석  

data("Groceries")
str(Groceries)

Groceries.df <- as(Groceries, "data.frame")
head(Groceries.df)

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

plot(rules, method = "grouped")

# 최대 길이 3 이하ᄋ  생성
rules2 <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8 , maxlen = 3))

rules2 <- sort(rules2, decreasing = T, by = "confidence")
inspect(rules2)

library(arulesViz)
plot(rules2, method = "graph")

# 특정 ᄉ품 으로 서브 셋 작성과 시각화 
wmilk <- subset(rules2, rhs%in%'whole milk')
wmilk  # 18 rules 

plot(wmilk, method = "graph")

oveg <- subset(rules2, rhs %in% 'other vegetables')
oveg   # 10 rules 

plot(oveg, method = "graph")

# 아이템이 butter 또는 yogurt 만 있는 셋으로 작성 
butter_yogurt <- subset(rules2 , lhs %in% c('butter', 'yogurt'))
plot(butter_yogurt, method = "graph")

