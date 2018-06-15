library(dplyr)
# 数据框建立
name1 <- c("Bob","Mary","Jane","Kim")
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
birth <- c("1990-1","1980-2","1995-5","1996-4")
accept <- c("no","ok","ok","no")

df1 <- data.frame(name1,weight,height)
rownames(df1) <- letters[1:4] # 赋予行名

df2 <- data.frame(name2,birth,accept)

# 通过[]选择坐标,确定点
df1[2,3]
# 使用向量确定多个点,第2、4行，第1、3列
df1[c(2,4),c(1,3)]

# 通过[]选择行列名
df1["a","weight"]
df1["a",2] # 混搭

# 通过矩阵提取多个点，一列指定行，一列指定列
df1[cbind(c(1,2,1),3:1)]
df1[cbind(c("a","b"), c("weight","height"))]

# 通过[]少选择一个维度，整行整列地选择（这是比选择行多的一种方法）
df1[2,] # 取行
df1[,3] # 取列
df1[c(2,3),] # 取多行
df1[,c(2,3)] # 取多列
df1["a",]
df1[,"weight"]

df1[3] # 只接一个数字，选择列
df1[c(2,3)] # 使用向量则选择多列

df1$weight # 通过$选择列名名字来选择列
# 列名放在[]中选取列
df1["weight"]
df1[c("weight","height")] # 使用向量选择多列

select(df2,accept)  # dplyr包中函数，等价于 df2$accept