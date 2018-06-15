# 读取数据
df <- read.csv('C:\\Users\\lxd\\Desktop\\shot_logs.csv')
head(df)
dim <- dim(df)
str(df)
n <- sum(is.na(df))

# 加载函数包
library(dplyr)
library(ggplot2)
# library(tidyverse)
nba.shots <- df %>%
  select(SHOT_DIST,CLOSE_DEF_DIST) %>%
  group_by(SHOT_DIST,CLOSE_DEF_DIST) %>%
  count() 
nba.shots <- df
ggplot(nba.shots,aes(SHOT_DIST)) + geom_histogram()
ggplot(nba.shots,aes(CLOSE_DEF_DIST)) + geom_histogram() + xlim(0,20)

# 投篮命中率因素
ggplot(nba.shots,aes(SHOT_DIST,CLOSE_DEF_DIST)) +
  geom_point(aes(color=factor(SHOT_RESULT))) +
  geom_vline(xintercept = c(15,22),color='blue')


ggplot(nba.shots,aes(DRIBBLES,TOUCH_TIME))+
  geom_point(aes(color=factor(SHOT_RESULT)))+ylim(-10,20)


## 主客场影响
# 构造命中率变量
home_away <- nba.shots %>% group_by(LOCATION) %>% 
  summarise(PERCENTAGE=sum(FGM)/length(FGM)*100)

# 主客场胜负对比
wins <- nba.shots %>% group_by(GAME_ID,LOCATION) %>% filter(W=='W',FGM
                                                          ==1)
ggplot(data=wins,aes(LOCATION,fill=factor(W))) + geom_bar()

# 联盟关键先生
first_quarter_guys <- nba.shots %>% filter(PERIOD==1,SHOT_DIST>5) %>% 
  group_by(player_name) %>% 
  summarise(made=sum(FGM), # 投中与否
            points=sum(PTS), # 得分
            total_attempts=length(FGM), # 
            ave_touch=mean(TOUCH_TIME),
            ave_distance=mean(SHOT_DIST)) %>%
  mutate(percentage=made/total_attempts) %>%
  arrange(desc(percentage)) %>% filter(total_attempts>200)
best_1st<-data.frame(first_quarter_guys)

# 第四节关键先生
fourth_quarter_guys<-nba.shots %>% filter(PERIOD==4,SHOT_DIST>5) %>%
  group_by(player_name) %>% 
  summarise(made=sum(FGM), 
            points=sum(PTS),
            total_attempts=length(FGM), 
            ave_touch=mean(TOUCH_TIME),
            ave_distance=mean(SHOT_DIST)) %>%
  mutate(percentage=made/total_attempts) %>%
  arrange(desc(percentage)) %>% filter(total_attempts > 150)
best_4th<-data.frame(fourth_quarter_guys)


# 最佳/最差防守
good.def <- nba.shots %>%
  filter(SHOT_RESULT=="missed") %>%
  group_by(CLOSEST_DEFENDER) %>%
  summarise(GoodDefense = n()) %>%
  ungroup %>%
  arrange(desc(GoodDefense))
  

bad.def <- nba.shots %>%
  filter(SHOT_RESULT=="made") %>%
  group_by(CLOSEST_DEFENDER) %>%
  summarise(BadDefense = n()) %>%
  ungroup %>%
  arrange(desc(BadDefense)) 

# 对投中与否重新编码
shot_label <- c('missed','made')
df$SHOT_RESULT1 <- ifelse(df$SHOT_RESULT=='made',1,0)
form <- SHOT_RESULT1 ~ TOUCH_TIME + SHOT_DIST + CLOSE_DEF_DIST
set.seed(12345)
n <- nrow(df)
trainNum <- round(n*0.8)
trainData <- df[1:trainNum,]
testData <- df[trainNum:n,]
fit <- nnet::nnet(form,trainData,size=6)
summary(fit)

fit.neu <- neuralnet::neuralnet(formula = form,data = trainData,hidden = 6)
summary(fit.neu)


