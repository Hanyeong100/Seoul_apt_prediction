# DM final project
library(leaps)
library(glmnet)
library(gbm)
library(caret)
#경로 설정
getwd()
path = "/Users/song-woomin/study/23_1/dataMining/final_project"
setwd(path)

#data loading
# dat = read.csv(file="model_data.csv",header = T, fileEncoding = "euc-kr")
dat = read.csv(file = "20230531_final_data.csv",header = T)

# EDA
head(dat)
dim(dat)
str(dat)

View(dat)
colnames(dat)

# dummy variable transformation
dat$no_subway = as.factor(dat$no_subway)
dat$yes_subway = as.factor(dat$yes_subway)
dat$hyper_yes_subway = as.factor(dat$hyper_yes_subway)
dat$난방_HT001 = as.factor(dat$난방_HT001)
dat$현관구조_계단식 = as.factor(dat$현관구조_계단식)
dat$Major_Builder = as.factor(dat$Major_Builder)



# 결측치 확인
sum(is.na(dat))


colnames(dat)




#PCA를 통한 차원 축소
PCA_dat = scale(dat[,-c(1,36,29,30,31,32,33,34)],center = T,scale = T)
apart.PCA = prcomp(PCA_dat,scale. = T)
result.pca = summary(apart.PCA)
result.pca
screeplot(apart.PCA,ylim = c(1.0,4.5),type="lines", pch=1, main="scree plot")
par(cex.axis = 1.0)
dat_pca = apart.PCA$x[,1:19]

#1. 총변이에 대한 공헌도
# 80%기준치로 하면 주성분이 15까지 가능할 것 같습니다.
# 90%기준치로 하면 주성분이 19까지는 가능할 것 같습니다.
#2. 개별 고유값의 크기
# 표준편차의 값의 크기를 Kaiser의 규칙을 기준으로 하면 주성분이 9정도에서 가능하지만, 총 변이에 대한 공헌도가 매우 작기 때문ㅇ데
#Jolliffe(1986)가 제안한 0.7을 기준으로 하면 주성분 19까지도 가능하며
#3. scree plot을 통한 결정

#  scree plot을 통해 주성분이 10정도가 적당해 보이지만, 총 변이에 대한 공헌도가 0.61정도로 매우 낮다.

#결론
# 80%기준치로 하면 주성분이 18까지 가능할 것 같습니다.
# 90%기준치로 하면 주성분이 24까지는 가능할 것 같습니다.









################################################################################################################################################################
# best subset
################################################################################################################################################################






#물건금액 억단위
temp = dat[,-1]
temp$물건금액.억. = temp$물건금액.만원./10^4
temp$물건금액.억.
colnames(temp)
length(colnames(temp)) # 총 predictor 34개 

regfit.full = regsubsets(물건금액.억. ~. - 물건금액.만원., data = temp,nvmax = 34)
result.full = summary(regfit.full)

regfit.bwd = regsubsets(물건금액.억. ~. - 물건금액.만원.,data = temp,nvmax = 34,method = "backward")
result.bwd = summary(regfit.bwd)

regfit.fwd = regsubsets(물건금액.억. ~. - 물건금액.만원.,data = temp,nvmax = 34,method = "forward")
result.fwd = summary(regfit.fwd)

result.full
result.fwd
result.fwd




coef(regfit.full,34)
names(coef(regfit.full,34))

coef(regfit.bwd,34)
names(coef(regfit.bwd,34))

coef(regfit.fwd,34)
names(coef(regfit.fwd,34))

#선택된 변수



#Exhasutive
par(mfrow=c(2,2))
plot(result.full$rss, xlab="Number of Variables", ylab="RSS", type="l")

plot(result.full$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(result.full$adjr2)
points(33, result.full$adjr2[33], col="red",cex=2,pch=20)

plot(result.full$cp, xlab="Number of Variables", ylab="Cp",type='l')
which.min(result.full$cp)
points(33, result.full$cp[33], col="red", cex=2, pch=20)

which.min(result.full$bic)
plot(result.full$bic, xlab="Number of Variables", ylab="BIC", type='l')
points(32, result.full$bic[32], col="red", cex=2, pch=20)


#forward
par(mfrow=c(2,2))
plot(result.fwd$rss, xlab="Number of Variables", ylab="RSS", type="l")

plot(result.fwd$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(result.fwd$adjr2)
points(33, result.fwd$adjr2[33], col="red",cex=2,pch=20)

plot(result.fwd$cp, xlab="Number of Variables", ylab="Cp",type='l')
which.min(result.fwd$cp)
points(33, result.fwd$cp[33], col="red", cex=2, pch=20)

which.min(result.fwd$bic)
plot(result.fwd$bic, xlab="Number of Variables", ylab="BIC", type='l')
points(32, result.fwd$bic[32], col="red", cex=2, pch=20)

#backward
par(mfrow=c(2,2))
plot(result.bwd$rss, xlab="Number of Variables", ylab="RSS", type="l")

plot(result.bwd$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(result.bwd$adjr2)
points(33, result.bwd$adjr2[33], col="red",cex=2,pch=20)

plot(result.bwd$cp, xlab="Number of Variables", ylab="Cp",type='l')
which.min(result.bwd$cp)
points(33, result.bwd$cp[33], col="red", cex=2, pch=20)

which.min(result.bwd$bic)
plot(result.bwd$bic, xlab="Number of Variables", ylab="BIC", type='l')
points(32, result.bwd$bic[32], col="red", cex=2, pch=20)

# Exhaustive, Foward, Backward : adjr cp 33  bic 32

result.full$adjr2[33]
result.full$bic[32]
result.full$cp[33]


result.fwd$adjr2[33]
result.fwd$bic[32]
result.fwd$cp[33]

result.bwd$adjr2[33]
result.bwd$bic[32]
result.fwd$cp[33]

names(coef(regfit.full,33))
names(coef(regfit.full,32))


names(coef(regfit.bwd,33))
names(coef(regfit.bwd,32))

names(coef(regfit.fwd,33))
names(coef(regfit.fwd,32))


# 33 vs 32

# lm.fit33 <- lm(물건금액.천만원. ~)
coef(regfit.fwd, 33) # yes subway가 빠지네,
fit.33 = lm(물건금액.억. ~ .-yes_subway,data = temp[,-35])
summary(fit.33)

coef(regfit.fwd,32) # yes subway 근처 학교 빠짐
fit.32 = lm(물건금액.억.~.-yes_subway-min_dist_elementary,dat = temp[,-35])

anova(fit.32,fit.33)






#L1 regulization_scale


head(temp[,c(3,4,28,29,30,31,32,33)])

temp_scale = scale(temp[,-c(28,29,30,31,32,33,35,36)],scale = T,center = T)
head(temp_scale)

temp_scale = cbind(temp_scale,temp[,c(28,29,30,31,32,33,36)])
temp_scale$물건금액.억. = temp$물건금액.만원./10^3
temp_scale$물건금액.억.

head(temp_scale)
colnames(temp_scale)




#model matrix 생성.
x=model.matrix(~.,temp_scale[,-35])
y=temp_scale$물건금액.억.#y는 반응변수 


set.seed(1)
train <- sample(1:nrow(x), (nrow(x)*2)/3)
test <- (-train)
y.test <- y[test]

grid <- 1:10; grid
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)

par(mfrow = c(1,1))

set.seed(2)
cv.out <- cv.glmnet(x[train ,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min; bestlam

lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

lasso.coef = predict(lasso.mod, type="coefficients", s=bestlam)
lasso.coef[lasso.coef[,1] != 0,] 
# intercept min_dist_elementary, 금융기관, economic_growth, yes_subway1,hyper_yes_subway1
#현관구조_계단식1, 건축년도_elapsed_days






















###############################################################################################################################################################
##################################################### Multiple linear regression################################################################################
###############################################################################################################################################################
#pca data

dat_pca = as.data.frame(dat_pca)
dat_pca$물건금액.억. = temp$물건금액.억.
dat_pca = dat_pca
colnames(dat_pca)



set.seed(1)
train_idx = sample(1:nrow(dat_pca),nrow(dat_pca)/2)
train = as.data.frame(dat_pca[train_idx,])
test = as.data.frame(dat_pca[-train_idx,])[,-20]

fit.pca = lm(물건금액.억.~. ,data = train)
mean((dat_pca[-train_idx,"물건금액.억."]-predict(fit.pca,test))^2)




#feature selection
train_idx = sample(1:nrow(temp),nrow(temp) *2 / 3)
train = temp[train_idx,][,-35]
test = temp[-train_idx,]
fit.subset = fit.33 = lm(물건금액.억. ~ .-yes_subway,data = train)
summary(fit.subset)
mean((test$물건금액.억.-predict(fit.subset,test))^2)




#L1 regularization
set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- (-train)
y.test <- y[test]

grid <- 1:10; grid
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)

par(mfrow = c(1,1))

set.seed(1)
cv.out <- cv.glmnet(x[train ,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min; bestlam

lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

lasso.coef = predict(lasso.mod, type="coefficients", s=bestlam)
lasso.coef[lasso.coef[,1] == 0,] # min_dist_elementary, 금융기관, economic_growth,yes_subway1,hyper_yes_subway1, 현관구조_계단식1, 건축년도





mse.pca = numeric(10)
mse.subset = numeric(10)
mse.lasso = numeric(10)

for(i in 1:10)
{
  #PCA
  set.seed(i)
  train_idx = sample(1:nrow(dat_pca),nrow(dat_pca)*2 / 3)
  train = as.data.frame(dat_pca[train_idx,])
  test = as.data.frame(dat_pca[-train_idx,])[,-20]
  
  fit.pca = lm(물건금액.억.~. ,data = train)
  mse.pca[i] = mean((dat_pca[-train_idx,"물건금액.억."]-predict(fit.pca,test))^2)
  
  
  #feature selection
  train_idx = sample(1:nrow(temp),nrow(temp) *2 / 3)
  train = temp[train_idx,][,-35]
  test = temp[-train_idx,]
  fit.subset = fit.33 = lm(물건금액.억. ~ .-yes_subway,data = train)
  summary(fit.subset)
  mse.subset[i] = mean((test$물건금액.억.-predict(fit.subset,test))^2)
  
  
  #L1 regularization
  set.seed(1)
  train <- sample(1:nrow(x),nrow(x) * 2 / 3)
  test <- (-train)
  y.test <- y[test]
  
  grid <- 1:10; grid
  lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
  
  par(mfrow = c(1,1))
  
  set.seed(1)
  cv.out <- cv.glmnet(x[train ,], y[train], alpha=1)
  plot(cv.out)
  bestlam <- cv.out$lambda.min; bestlam
  
  lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
  mse.lasso = mean((lasso.pred-y.test)^2)
  
  
}

sqrt(mean(mse.pca)); sqrt(mean(mse.subset)); sqrt(mean(mse.lasso))








##### Gradient boosting


###PCA
set.seed(1)
train <- sample(1:nrow(dat_pca), nrow(dat_pca)/2)
test.y = dat_pca[-train,"물건금액.억."]
PCA_gbm = gbm(물건금액.억. ~ . ,distribution = "gaussian",data = dat_pca, 
                n.trees = 100,
                interaction.depth = 4,
                cv.folds = 10,
                keep.data = T,
                verbose = T,
              train.fraction = 0.5,
              shrinkage = 0.1)
summary(PCA_gbm)


yhat.pca <- predict(PCA_gbm, newdata=dat_pca[-train,], n.trees=100)
sqrt(mean((yhat.pca-test.y)^2))


### Bestsubset
set.seed(1)
train = sample(1:nrow(temp),nrow(temp)/2)
test.y = temp[-train,"물건금액.억."]
Bestsubset_gbm = gbm(물건금액.억. ~ .-yes_subway-물건금액.만원.,distribution = "gaussian",data = temp[train,] , 
                     n.trees = 100,
                     interaction.depth = 4,
                     cv.folds = 10,
                     keep.data = T,
                     verbose = T,
                     train.fraction = 0.5,
                     shrinkage = 0.1)
par(cex.axis = 0.3)
summary(Bestsubset_gbm)
plot(Bestsubset_gbm)


yhat.bestsubset <- predict(Bestsubset_gbm, newdata=temp[-train,], n.trees=100)
sqrt(mean((yhat.bestsubset-test.y)^2))



### Lasso
set.seed(1)
train = sample(1:nrow(temp_scale),nrow(temp_scale)/2)
test.y = temp_scale[-train,"물건금액.억."]
Lasso_gbm = gbm(물건금액.억. ~ .-1-min_dist_elementary-금융기관-economic_growth-건축년도_elapsed_days-yes_subway-hyper_yes_subway-
                  현관구조_계단식,data = temp_scale[-train,],
                distribution = "gaussian",
                n.trees = 100,
                interaction.depth = 4,
                cv.folds = 10,
                keep.data = T,
                verbose = T,
                train.fraction = 0.5,
                shrinkage = 0.1)
par(family = "AppleGothic")
summary(Lasso_gbm)
plot(Lasso_gbm,i = "건물면적...")
plot(Lasso_gbm)

yhat.lasso <- predict(Lasso_gbm, newdata=temp_scale[-train,], n.trees=100)
sqrt(mean((yhat.lasso-test.y)^2))









