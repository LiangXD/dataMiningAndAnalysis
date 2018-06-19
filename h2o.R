library(h2o)
h2o.init()
data.set <- 'C:\\Book\\R\\H2O\\Practical_Machine_Learning_with_H2O\\datasets\\'
data <- h2o.importFile(paste0(data.set,'iris_wheader.csv'))
attr(data,'id')
data <- data[,2:5]
attr(data,'id')
#对数据命名
h2o.assign(data,'iris')
attr(data,'id')
h2o.ls()# 查看当前目录下的文件
#数据总揽
h2o.describe(data)
h2o.summary(data)
h2o.quantile(data)
h2o.levels(data)
dim(data)
#列操作
data$petal_len <- data$petal_len*1.2
data$ratio <- data$petal_wid/data$sepal_wid
h2o.sd(data$petal_len)#标准差
h2o.cor(data$ratio,data$petal_len)#相关系数
#分割数据
parts <- h2o.splitFrame(data,c(0.6,0.2))
train <- parts[[1]]
valid <- parts[[2]]
test <- parts[[3]]

# 房屋数据
df <- h2o.importFile(paste0(data.set,'ENB2012_data.csv'))
dim(df)
h2o.names(df)
h2o.getTypes(df)
h2o.describe(df)
factorList <- c('X6','X8')
df[,factorList] <- as.factor(df[,factorList])
#分割数据
splits <- h2o.splitFrame(df,0.8)
train <- splits[[1]]
test <- splits[[2]]
# 输入特征（变量）
x <- c('X1','X2','X3','X4','X5','X6','X7','X8')
y <- 'Y2'
# 训练模型
m <- h2o.randomForest(x,y,df,nfolds = 10,model_id = 'RF_defaults')
summary(m)
# 预测
pred <- h2o.predict(m,test)
pred$predict
# 画出预测值与真实值
source('C:\\Users\\lxd\\Documents\\dataMiningAndAnalysis\\makeplot.building_energy_results.R')
plotBuildingEnergy(pred$predict,test)
# 评估
performance <- h2o.performance(m,test)

# 多参数调整模型
g <- h2o.grid('randomForest',
              hyper_params = list(
                ntrees = c(40,50,100,120),
                max_depth = c(40,50,60),
                min_rows = c(1,2,3,4,5)
              ),
              x = x,y = y,training_frame = train,nfolds = 10)
summary(g)