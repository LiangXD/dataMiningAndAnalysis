# 读取数据
df.day <- read.csv('C:\\Users\\lxd\\Desktop\\分析\\Bike-Sharing-Dataset\\day.csv')
df.hour <- read.csv('C:\\Users\\lxd\\Desktop\\分析\\Bike-Sharing-Dataset\\hour.csv')
n <- sum(is.na(df.hour))
par(mfrow=c(1,1))
dotchart(as.matrix(df.day$cnt))
str(df.hour)

table(df.hour$season)
table(df.hour$weathersit)

# 将季节和天气转为因子
df.hour$season  <- factor(df.hour$season, labels = c("春季", "夏季", "秋季", "冬季"))
df.hour$weathersit <- factor(df.hour$weathersit, labels = c("好", "正常", "差", "非常差"))
table(df.hour$season)
table(df.hour$weathersit)
summary(df.hour)
# df.hour$hr <- as.factor(df.hour$hr)

library(dplyr)
library(ggplot2)
p1 <- df.hour %>%
  group_by(hr) %>%
  summarise(mcount=mean(cnt)) %>%
  ggplot(aes(x=hr,y=mcount,fill=hr)) +
  geom_bar(stat = 'identity') +
  labs(x='时间',y='平均租车数量') +
  guides(fill='none') +
  theme_minimal();p1


p2 <- df.hour %>%
  group_by(season) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = reorder(season, mcount), y = mcount, fill = season)) + 
  geom_bar(stat = 'identity') +
  labs(x = '季节', y = '平均租车数据') +
  guides(fill = 'none') +
  theme_minimal()

p3 <- df.hour %>%
  group_by(season) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x=reorder(season,mcount),y = mcount,fill=season)) +
  geom_bar(stat = 'identity',width = 1) +
  coord_polar(theta = 'y') +
  labs(x='季节',y='平均租车数量') + guides(fill='none') +
  theme_minimal()
gridExtra::grid.arrange(p2,p3,ncol=2)

p4 <- df.hour %>%
  group_by(season,hr) %>%
  summarise(mcount=mean(cnt)) %>%
  ggplot(aes(x=hr,y=mcount,colour = season)) +
  labs(x='时间段',y='平均租车数量') +
  geom_line(aes(group=season),size=1) +
  theme_bw() +
  geom_point(size=1.4)
p4

p5 <- df.hour %>%
  group_by(weathersit) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x=reorder(weathersit,mcount),y=mcount,fill=weathersit)) +
  geom_bar(stat = 'identity') +
  labs(x='天气',y='平均租车数量') +
  guides(fill='none') +
  theme_minimal()
p5

p6 <- df.hour %>%
  group_by(weathersit) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x=reorder(weathersit,mcount),y=mcount,fill=weathersit)) +
  geom_bar(stat = 'identity',width = 1) +
  coord_polar(theta = 'y') +
  labs(x='天气',y='平均租车数量') +
  guides(fill='none') + 
  theme_minimal()
p6

gridExtra::grid.arrange(p5,p6,ncol=2)


p7 <- df.hour %>% 
  group_by(weathersit, hr) %>% 
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = hr, y = mcount, colour = weathersit)) + 
  geom_line(aes(group = weathersit)) +
  labs(x='时间',y='平均租车数量') +
  theme_bw() +
  geom_point()
p7

p8 <- df.hour %>%
  group_by(holiday) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x=factor(holiday),y=mcount,fill=factor(holiday))) +
  geom_bar(stat = 'identity') +
  guides(fill='none') +
  labs(x='假期',y='平均租车数量') +
  theme_minimal()
p8

p9 <- df.hour %>%
  group_by(workingday) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = factor(workingday), y = mcount, fill = factor(workingday))) + 
  geom_bar(stat = 'identity') + 
  guides(fill = 'none') +
  labs(x = '工作日',y='平均租车数量') +
  theme_minimal()
p9

gridExtra::grid.arrange(p8,p9,ncol=2)


p10 <- df.hour %>% 
  group_by(holiday, hr) %>% 
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = hr, y = mcount, colour = factor(holiday))) + 
  geom_line(aes(group = factor(holiday))) + 
  labs(x='时间',y='平均租车数量') +
  geom_point() +
  theme_bw()
p10

p11 <- df.hour %>% 
  group_by(workingday, hr) %>% 
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = hr, y = mcount, colour = factor(workingday))) + 
  geom_line(aes(group = factor(workingday))) + 
  labs(x='时间',y='平均租车数量') +
  theme_bw() + 
  geom_point()
p11

library(corrplot)
cor(df.hour[c(11:14, 17)]) %>%
  corrplot.mixed()

library(dplyr)
df.hour1 <- df.hour %>%
  select(cnt,season,yr,hr,mnth,holiday,weekday,
         workingday,temp,atemp,weathersit,hum,windspeed)

normlize <- function(x){
  y <- (x-min(x))/(max(x)-min(x))
  return(y)
}

df.hour1 <- as.data.frame(apply(df.hour1,2,normlize))
df.hour1 <- as.data.frame(scale(df.hour1))
summary(df.hour1)
# 岭回归模型
# 简单线性模型
form <- cnt~season + yr + hr + mnth + holiday + weekday +workingday +
  weathersit + temp + atemp +hum + windspeed 
lm.sol <- lm(form, data = df.hour1)
summary(lm.sol)
library(car)
vif(lm.sol)

library(tidyverse)
library(magrittr)

n <- dim(df.hour1)
trainNum <- round(n[1]*0.8)
sample.index <- sample(1:n[1],trainNum)
trainData <- df.hour1[sample.index,]
testData <- df.hour1[-sample.index,]

# 岭回归
library(MASS)
ridge.sol <- lm.ridge(form, lambda = seq(0, 200, 0.1), data = trainData, 
                      model = TRUE)
summary(ridge.sol)
#print(ridge.sol)

names(ridge.sol)  # 变量名字
lambda <- ridge.sol$lambda[which.min(ridge.sol$GCV)] # 找到GCV最小时的lambdaGCV
cgv.coef <- ridge.sol$coef[which.min(ridge.sol$GCV)]  ##找到GCV最小时对应的系数
par(mfrow = c(1, 2))
# 画出图形，并作出lambdaGCV取最小值时的那条竖直线
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lambda), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

# 下面的语句绘出lambda同GCV之间关系的图形
plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(CGV))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)],col='red')

# 得到lambda为64.9
ridge.fit <- lm.ridge(form, lambda = 91.7, data = trainData,model = TRUE)
# 预测
summary(ridge.fit)

pred.test <- as.matrix(testData) %*% coef(ridge.fit)
pred.error <- pred.test-testData[,1]
par(mfrow=c(1,1))
plot(pred.error,type = 'b',col='red')

library(ridge)
mod <- linearRidge(form, data = trainData,lambda = seq(0,200,0.1))
prdeict
summary(mod)


library(glmnet)
# x <- model.matrix(cnt~.,trainData)[,-1]
# y <- trainData$cnt
x <- model.matrix(cnt~.,trainData)
y <- trainData[,1]
grid1 <- seq(0,200,0.1)
# ridge.model <- glmnet(x,y,alpha = 0,lambda = grid1) # alpha为0时为岭回归
cv.out <- cv.glmnet(x,y,alpha = 0,lambda = grid1) # 加入交叉验证，找出合适的lambda
print(cv.out)
plot(cv.out)
bestlambda <- cv.out$lambda.min
coff <- coef(cv.out,s='lambda.min')
predict(cv.out,newx = testData[,-1],s='lambda.min')
# 提取模型
fit <- cv.out$glmnet.fit
ridge.pred <- predict(cv.out,s=bestlambda,newx = model.matrix(cnt~.,testData))



library(lars)
x <- as.matrix(trainData[,-1])
y <- as.matrix(trainData[,1])
laa = lars(x, y, type = "lasso")
par(mfrow=c(1,1))
plot(laa)
summary(laa)
cvsol1<-cv.lars(x,y,type="lasso",mode="step")  
bestStep <- cvsol1$index[which.min(cvsol1$cv)]
coef.lasso <- laa$beta[12,]# 系数
# 饱和度
fraction <- sum(abs(laa$beta[12,]))/sum(abs(laa$beta[13,]))


cvsol2<-cv.lars(x,y,type="lasso",mode="fraction",  
                index=seq(from=0,to=1,length=100))
frac <- cvsol2$index[which.min(cvsol2$cv)]
predict(laa,newx=testData[,-1], s=12,type = 'fit',mode = 'step')

# ctree，rpart，tree回归树
library(rpart)

