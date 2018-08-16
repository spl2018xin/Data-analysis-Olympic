setwd("~/Desktop/datenanalyse/Final Report")
olympic <- read.csv("~/Desktop/datenanalyse/Final Report/data/athlete_events.csv")

library(ggplot2)
library(dplyr)


#####################################################################################
# 本节主要是想 focus 在 age or experience analysis
#####################################################################################



#########################################
#       1. Data Cleaning 
#########################################

# 首先需要对数据重新处理。数据显示，参加奥运项目次数的最大值是58，回到数据本身，发现数据集存在重复录入的问题。另外我们focus在non-art competitition 的项目上，
# 所以要剔除所有 art-competition 相关项目，近代奥已经不包含这块了。
# include eliminate both data points appears twice and refers to art competition.
# There are 3578 duplicate entries (1.3% of the original total dataset). 70 from 267538 data points are from art compitition, which is not interest in our research.
# After the data cleaning there are 267538 deta points left.



# eliminate duplicate entries [271116 --> 267538]
olympic <- unique(olympic)


# remove all Art competition --> Olympics without Art Competition [267538 --> 267468]
olympic_nonArt <- olympic %>% 
  filter(!grepl('Art Competitions', Sport))%>%
  arrange(Name)%>%
  group_by(Name)

#################### End.  Data Cleaning  ############################################








###################
### Experience check
###################

###PRERARE



# 参加奥运的次数 -- > 通过统计名字出现的频率来计算
Freq <- data.frame(table(olympic_nonArt$Name))
colnames(Freq) <- c("Name", "Frequency")  # 为什么上一步已经去掉了art competition的这里在计算freq的时候还会出现

#  手动去掉这些frequency = 0 的项目
Freq.all <- Freq[- grep("0", Freq$Frequency),]





#################
#  总体次数情况
#################

# 注意这里是参加奥运项目，不是奥运次数。因为存在统一奥运会上参加多于1个项目。
# 例如体操，游泳，射击等，一个人可能同时参加还有集体和个人比赛。


#######################  统计总体参加奥运项目的频率
freqtab <- data.frame(table(Freq.all$Frequency))
colnames(freqtab) <- c("Times", "Frequency")

# 所占比例
freqtab <-data.frame(freqtab %>%
                       mutate(Percentage = Frequency/sum(Frequency)) %>%
                       mutate(Percentage = Percentage))

# 保留4位小数
cbind(freqtab[,1:2],round(freqtab[,3],digits=4)) # 这个可要可不要，只用于double check!


# 可视化
pie(table(Freq.all$Frequency), radius = 1.3) # 有问题：显示不全


###########
#  结论
###########
# 按项目来看，57.21%的运动员（75844人）只参加过一届奥运的一个项目比赛
## 22.76%的运动员 (30172人)参加过两个项目。是一届的吗？有13401人是同一届奥运的两个项目，3229人参加了两次奥运

# row = # of Games, col = # of Events 
#     1     2
# 1    19 13401
# 2 13523  3229

####
temp.two <- table.ExpAthele[table.ExpAthele$Frequency == 2,]
dim(temp.two)


# 先选出frequency是2的
# 把他们弄在一张表上
temp.twice <- data.frame(temp.two$Name, temp.two$Games, temp.two$Event)
colnames(temp.twice) <- c("Name","Games", "Event")



# check他们来自哪里
# 以下晓秋写的。
T<-sqldf('select Name, count(distinct Games), count(distinct Event) from T group by Name')
table(T$`count(distinct Games)`, T$`count(distinct Event)`)
dim(T[T[2]==2 & T[3]==1,])
  
  
  
  
  

####################### 分析experienced athelets (>1 olympic games)

### data prepare:
# remove all players who just take one olympic games, include which has more than one competition in one Olympic Games





# ###check:
# 1. 个人某届奥运参加的项目数量     -> df
# 2. 整个奥运生涯参加的所有比赛     -> df
# 3. 个人参加的奥运次数             -> df


##################### 2. 整个奥运生涯参加的比赛 #########################





############
#  至少参加过1次奥运的选手
############

freq.ExpAthele <- Freq.all %>%
  filter(Frequency > 1)

# 大表. 所有至少参加过一次奥运的运动员完整信息
table.ExpAthele <- merge(x = olympic_nonArt, y = Freq, by = "Name", all = TRUE)

# summary
summary(table.ExpAthele)

# 数据集里得奖牌最多的人
# Heikki Ilmari Savolainen, was a Finnish artistic gymnast. 
# He competed in five consecutive Olympics from 1928 to 1952 and won at least one medal in each of them.

hist(table.ExpAthele$Frequency)
boxplot(table.ExpAthele$Frequency)
# 这个数据肯定要处理。不太正常，疯狂参加奥运的人也太多了。

################

# 可视化










# 接下来我想知道这些疯狂参加奥运的人成绩有没有提高（奖牌情况），是不是经验越多越好？
# 在哪些项目上是这样的。test是不是参加次数越多得奖可能性越高？
# 和年龄优势比较起来，哪个更好？







# how many Sports type in total except Art-Competitions?
YearAthele_nonArt        <- olympic_nonArt[ , c("Year", "Name")]
tempName_nonArt          <- with(YearAthele_nonArt, table(Name))
experiencedAthele_nonArt <- data.frame(count = tempName_nonArt[tempName_nonArt > 2])

# summary
summary(experiencedAthele_nonArt$count.Freq)


