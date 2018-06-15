setwd("C:/Users/lxd/Documents/dataMiningAndAnalysis")
library(Rcpp)
library(RSNNS)
df <- read.csv('C:\\Book\\R\\R学习分享\\data\\mushrooms.csv')
mushrooms.values <- df[,-1]
mushrooms.targets <- df[,1]  # 类别变量

# 将因子变量转换为数值
mushrooms.values <- as.data.frame(lapply(mushrooms.values,toNumericClassLabels))
# 将类别转换为二进制矩阵
mushrooms.targets <- decodeClassLabels(mushrooms.targets)
# 训练数据与测试数据分离
mushrooms <- splitForTrainingAndTest(mushrooms.values,mushrooms.targets,ratio = 0.15)
# 规整化训练数据
mushrooms <- normTrainingAndTestSet(mushrooms)

# 建立模型
models1 <- mlp(mushrooms$inputsTrain,mushrooms$targetsTrain,size = c(20,10),maxit=100,  
              inputsTest = mushrooms$inputsTest,targetsTest = mushrooms$targetsTest,
              linOut = T)
extractNetInfo(models1) # 提取模型对象
plotIterativeError(models1,main='mlp') # 画误差曲线
#打印网络
print(models1)
# 总结对象summary(object, origSnnsFormat = TRUE, ...)
summary(models1)
# 提取神经网络权重
weightMatrix(models1)

# 测试集预测
pred1 <- predict(models1,mushrooms$inputsTest) 
# 计算测试集的混淆矩阵
confusionMatrix1 <- confusionMatrix(mushrooms$targetsTest,pred1)
# 回归误差图
plotRegressionError(mushrooms$targetsTest,pred1)
# ROC曲线
plotROC(mushrooms$targetsTest,pred1)

# BP
models2 <- mlp(mushrooms$inputsTrain,mushrooms$targetsTrain,size = 10,maxit = 100,
               learnFunc = 'BackpropBatch',learnFuncParams = c(10,0.1),
               inputsTest = mushrooms$inputsTest,targetsTest = mushrooms$targetsTest)
pred2 <- predict(models2,mushrooms$inputsTest) 
# 计算测试集的混淆矩阵
confusionMatrix2 <- confusionMatrix(mushrooms$targetsTest,pred2)
plot(confusionMatrix2) # 画出混淆矩阵图
# SCG
models3 <- mlp(mushrooms$inputsTrain,mushrooms$targetsTrain,size = 10,learnFunc = 'SCG',
               maxit = 100,learnFuncParams = c(0,0,0,0),inputsTest = mushrooms$inputsTest,
               targetsTest = mushrooms$targetsTest)
pred3 <- predict(models3,mushrooms$inputsTest) 
confusionMatrix3 <- confusionMatrix(mushrooms$targetsTest,pred3)
plot(confusionMatrix3) 

# rbfDDA
models4 <- rbfDDA(mushrooms$inputsTrain,mushrooms$targetsTrain)
pred4 <- predict(models4,mushrooms$inputsTest) 
confusionMatrix4 <- confusionMatrix(mushrooms$targetsTest,pred4)
plot(confusionMatrix4) 

# elman
models5 <- elman(mushrooms$inputsTrain,mushrooms$targetsTrain,size = 10,maxit = 10,
                 learnFuncParams = c(0.1),inputsTest = mushrooms$inputsTest,targetsTest = mushrooms$targetsTest)
pred5 <- predict(models5,mushrooms$inputsTest) 
confusionMatrix5 <- confusionMatrix(mushrooms$targetsTest,pred5)
plot(confusionMatrix5) 

# rbf
models6 <- rbf(mushrooms$inputsTrain,mushrooms$targetsTrain,size = 10,maxit = 100,
               initFuncParams = c(-4,4,0.0,0.2,0.04),learnFuncParams = c(1e-3,0,1e-3,0.1,0.8),linOut = FALSE)

pred6 <- predict(models6,mushrooms$inputsTest) 
confusionMatrix6 <- confusionMatrix(mushrooms$targetsTest,pred6)
plot(confusionMatrix6) 

# 迭代误差曲线
par(mfrow = c(2,3))
plotIterativeError(models1,main='mlp')
plotIterativeError(models2,main='mlp+bp')
plotIterativeError(models3,main='SCG')
plotIterativeError(models4,main='rbfDDA')
plotIterativeError(models5,main='elman')
plotIterativeError(models6,main='rbf')

# 画混淆矩阵图
par(mfrow=c(2,3))
plot(confusionMatrix1,main='mlp') 
plot(confusionMatrix2,main='mlp+bp') 
plot(confusionMatrix3,main='mlp+SCG') 
plot(confusionMatrix4,main='rbfDDA') 
plot(confusionMatrix5,main='elman')
plot(confusionMatrix6,main='rbf') 


# 可视化
library(devtools)
library(scales)
library(reshape2)
# library(clusterGeneration)
source('C:\\Book\\R\\neuralAnalysis\\plot.nnet.R')
models1 <- mlp(mushrooms$inputsTrain,mushrooms$targetsTrain,size = 10,linOut = TRUE)
par(mar = numeric(4),family='serif')
plot.nnet(models1)


library(nnet)
mod <- nnet(mushrooms$inputsTrain,mushrooms$targetsTrain,
            data=data.frame(mushrooms$inputsTrain,mushrooms$targetsTrain),
            size=20,linOut=T)
plot.nnet(mod)
wts.in <- mod$wts
struct <- mod$n
plot.nnet(wts.in,struct = struct)
# H2O
library(h2o)
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
demo(h2o.glm)
