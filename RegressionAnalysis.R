# 逻辑回归
# 加载数据
data1 <- read.csv2('C:\\Book\\R\\r数据分析与实战\\数据及代码\\chapter5\\data\\bankloan.csv',
                  sep =',')
# 数据命名
colnames(data1) <- c('x1','x2','x3','x4','x5','x6','x7','x8','y')
# 逻辑回归模型
glm1 =glm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8,
          family = binomial(link = logit),data=data1)
summary(glm1)

# 逐步寻优
logit.step <- step(glm1,direction = 'both')
summary(logit.step)

# 前向选择法
logit.step <- step(glm1,direction = 'forward')
summary(logit.step)

# 后向选择法
logit.step <- step(glm1,direction = 'backward')
summary(logit.step)

# 最小二乘法
library(nutshell)
data("team.batting.00to08")
head(team.batting.00to08,6)
df <- team.batting.00to08
summary(df)
cor.mtrix <- cor(df[,3:13])

form <- runs ~singles + doubles + triples + homeruns + walks +
  stolenbases + caughtstealing + hitbypitch + sacrificeflies + atbats
runs.lm <- lm(formula = form,data = df)
summary(runs.lm)

# 手动剔除变量
form.a <- runs ~ singles + doubles + triples + homeruns + walks + 
  hitbypitch + sacrificeflies + atbats
runs.lm_a <- lm(formula = form.a,data = df)
summary(runs.lm_a)
# 自动剔除变量
runs.lm_b<-step(runs.lm)

# 岭回归
cement <- data.frame(X1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10), 
                     X2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68), 
                     X3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8), 
                     X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12), 
                     Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4))

# 简单线性回归
lm.sol <- lm(Y ~ ., data = cement)
summary(lm.sol)
# 利用car包中的vif（）函数查看各自变量间的共线情况

library(car)
vif(lm.sol)
# 从结果看，各自变量的VIF值都超过10，存在多重共线性，其中，X2与X4的VIF值均超过200.
par(mfrow=c(1,1))
plot(X2 ~ X4, col = "red", data = cement)
# 
library(MASS)
ridge.sol <- lm.ridge(Y ~ ., lambda = seq(0, 2, 0.01), data = cement, 
                      model = TRUE)
names(ridge.sol)  # 变量名字
lambda <- ridge.sol$lambda[which.min(ridge.sol$GCV)]
# ridge.sol$lambda[which.min(ridge.sol$GCV)]  ##找到GCV最小时的lambdaGCV
cgv.coef <- ridge.sol$coef[which.min(ridge.sol$GCV)]  ##找到GCV最小时对应的系数
par(mfrow = c(1, 2))
# 画出图形，并作出lambdaGCV取最小值时的那条竖直线
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lambda), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

# 下面的语句绘出lambda同GCV之间关系的图形
plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(beta))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

par(mfrow = c(1, 1))
# 从上图看，lambda的选择并不是那么重要，只要不离lambda=0太近就没有多大差别。
# 下面利用ridge包中的linearRidge()函数进行自动选择岭回归参数
library(ridge)
mod <- linearRidge(Y ~ ., data = cement)
summary(mod)

#从模型运行结果看，测岭回归参数值为0.0147，各自变量的系数显著想明显提高（除了X3仍不显著）
#最后，利用Lasso回归解决共线性问题


library(lars)
## Loaded lars 1.2
x = as.matrix(cement[, 1:4])
y = as.matrix(cement[, 5])
(laa = lars(x, y, type = "lar"))  #lars函数值用于矩阵型数据
# 由此可见，LASSO的变量选择依次是X4，X1，X2，X3
plot(laa)  #绘出图
summary(laa)  #给出Cp值