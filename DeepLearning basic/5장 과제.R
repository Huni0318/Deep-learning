setwd("C:/Users/신상훈/Desktop/학과/다변량2/6장/Rdata(all)")

library(MASS)
library(MVN)


## 1번 다변량 정규성 확인
salmon <- read.table("salmon.txt", header = TRUE)
head(salmon)
dim(salmon)
class1 = salmon[1:50 , 3:4]
class2 = salmon[51:100 , 3:4]
result_class1 = mvn(class1)
result_class2 = mvn(class2)
result_class1
result_class2

##2 번 공분산 동질성 검정
salmon <- read.table("salmon.txt", header = TRUE)
salmon
group=as.factor(salmon$class)
plot(salmon[, 3:4], pch=unclass(salmon$class),col = c('blue','red')[group])
par(mfrow=c(1,2))
ldahist(data= salmon$x1, g=salmon$class,width=30, type="density") # 확률밀도함수 
ldahist(data= salmon$x2, g=salmon$class,width=30, type="density")
library(biotools)
x=salmon[, 3:4]
boxM(x, as.factor(salmon[, 1]))
#다변량 정규성만족,공분산행렬 동질성 성립 하지 않으므로  QDA 실시 #

QDA=qda(class~x1+x2, data=salmon , prior=c(1,1)/2)
QDA
qcluster=predict(QDA, salmon)$class
qct=table(salmon$class, qcluster)
qct
mean(salmon$class==qcluster)


####4번
salmon_m = salmon[which(salmon$sex == 1),][3:4]
salmon_w = salmon[which(salmon$sex ==2 ),][3:4]

result_1 = mvn(salmon_m)
result_2 = mvn(salmon_w)

group=as.factor(salmon$sex)
plot(salmon[, 3:4], pch=unclass(salmon$sex),col = c('blue','red')[group])
par(mfrow=c(1,2))
ldahist(data= salmon$x1, g=salmon$class,width=30, type="density") # 확률밀도함수 
ldahist(data= salmon$x2, g=salmon$class,width=30, type="density")
x=salmon[, 3:4]


boxM(x, as.factor(salmon[, 2]))
LDA=lda(sex~x1 + x2, data=salmon,prior=c(51,49)/100)
LDA
lcluster=predict(LDA, salmon)$class
lct=table(salmon$sex, lcluster)
lct
# Total percent correct
mean(salmon$sex==lcluster)
dim(salmon_m)




