setwd("~/Desktop/datenanalyse/Final Report")
olympic <- read.csv("~/Desktop/datenanalyse/Final Report/data/athlete_events.csv")

library(ggplot2)
library(dplyr)

# Dataset overview
View(olympic)

# re-arrange the dataset in term of year
olympic <- olympic %>%
  arrange(desc(Year))

# eliminate duplicate entries [271116 --> 267538]
olympic <- unique(olympic)

# remove all Art competition --> Olympics without Art Competition [267538 --> 267468]
olympic_nonArt <- olympic %>% 
  filter(!grepl('Art Competitions', Sport))%>%
  filter(!grepl('Winter', Season)) %>%
  arrange(Name)%>%
  group_by(Name)

# the trimed dataset is ordered based on name, no Winter, no Art-competietion. Since some althelets attained more than one
# competition within one olympic games and some atteined more than one olympic games.


#################### End.  Data Cleaning  ############################################


#############################################
# Missing Value (Exploratory missingness analysis) --------
#############################################

### Which variables have a lot of missings?

colSums(is.na(olympic_nonArt))

# If we exclude non-Art competietion and only summer games,there are 8650 missing values in the "Age" column, 48269 missing values in the  "Height", and
# 50262 missing values in the "Weight" column. 
# In the "Medal" column non-medal entries are recored as NA.

# Since we know without Medal means NA in the last column of the datasets, we exclude that one
# and plot the missing value map.

# exclude the last column
missingMap  <- olympic_nonArt[,1:(ncol(olympic_nonArt)-1)]


### Missingness patterns
# What are the reasons of these missing value? -->create frequency table of missing patterns
library("MissingDataGUI")
library("Amelia")
missmap(missingMap)

# 使用mice判断缺失情况 = 上图,这个更好,能给出具体的数值,都缺的,同时缺的是哪些,有多少
# left: sample size, right: exist missing value in the corresponding field?
library(mice)
md.pattern(missingMap)

# vitulization
library(VIM)
aggr_plot <- aggr(missingMap, col=c('navyblue','red'), numbers=TRUE, labels=names(missingMap), cex.axis=.7, gap=3, ylab=c("Histogram  of missing data","Pattern"))

# Missing value in Height and Weight
marginplot(missingMap[c(5,6)])
# 由于我们发现Height and Weight 中的missing value占总的missing value 的大多数,
# 我们首先分析这两个factor:
# 左边的红色箱线图表示有Height缺失的样本的missingMap的分布, 即在除Medal外的整个dataset里的分布
# 蓝色的箱线图表示的是剩下的数据点的分布。。
# 在MCAR的假设下，蓝色和红色的箱线图应该非常接近。


### Where are they? (in which block)
# There are 3% missing values without considerating the Medal item. From the Missing Map
# we can see, the most non recorded information are both weight and height, in another word,
# if an althelet's height was not recorded, with a very high probabilty that his/her weight is also missed.

# 晓秋的图: missing values happend quite often before 1960.  



### Missing value imputation
#1. check the distribution of the height, weight and age in each team and event in the corresponding Games.
#2. check Main steps used in multiple imputation



#####
# age and medals in 2016 Rio------------------------
#####

Rio2016 <- olympic%>%
  filter(Year == 2016, Season == "Summer")
ggplot(Rio2016, aes(x = Medal, y = Age)) + geom_point()


#####
# Weight and Height in olympic
#####
ggplot(Rio2016, aes(x = Height, y = Weight)) + geom_point() + facet_wrap(~Sport)


#####
# average age in each year
####
avgAge <- olympic %>%
  group_by(Year) %>%
  summarize(avgAge = mean(Age, na.rm = T))
# virtulization
ggplot(avgAge, aes(x = Year, y =  avgAge, size = avgAge, colour = avgAge)) + geom_point()

# only one year the average age of the atheles are over 30, when there was the WW2. Twice in 
# the history the average age of the athlete is under 18, one is in the 1st olympic games(1896) 
# 18, suprisely the youngest athele is just 10 years old)

# range of the age
range(olympic$Age, na.rm = T)

# average age of the athlete 
mean(olympic$Age,na.rm = T) # population mean
mean(ageMean$ageMean)       # mean of the mean

# under 18
under18<- olympic %>%
  filter(Age<=18)
# how many athlete are under 18?
table(under18$Age)
dim(under18)
# proportion of the athlete under 18
dim(under18)[1:1] /dim(olympic)[1:1]




################################### 项目分析 #############################################
# Olympics without Art Competition
temp <- olympic %>%
  filter(Sport != "Art Competitions")

# how many Sports type in total except Art-Competitions?
tempSports<-data.frame(table(temp$Sport)) # 每项运动多少人
dim(tempSports)



#################
# 历史上出了艺术类一共有65个竞技类项目。 
# 并不是所有项目都延续到今天。例如Aeronautics只出现过在1936 Summer柏林奥运会。
# 还有哪些没有延续到今天“奇怪的比赛”呢？为什么被取消？
#################

# 项目参加人数分布
summary(tempSports$Freq)
hist(tempSports$Freq, seq(0,40000, 100))







# • Lacrosse (1904 summer St. Louis，1908 summer, London) 
# [只要参赛就有奖牌。参与人数太少而取消]: 1904[36] + 1908[24]。Bronze = 12, Gold = 24,  Silver = 24

# Lacrosse at the Summer Olympics has been contested at two editions of the Summer Olympic Games — 1904 and 1908.
# Both times it had been open only to men; both times a Canadian team won the competition.
lacrosse <- temp %>%
  filter(Sport == "Lacrosse")%>%
  arrange(Year, Medal)

# 如何显示每届奖牌数量
sum(lacrosse$Year == "1904")
sum(lacrosse$Year == "1908")

table(lacrosse$Medal)

# • Cricket (1900 summer Paris)
#[只要参赛就有奖牌。参与人数太少而取消]：24 player： 12 Gold, 12 Silver
cricket <- temp%>%
  filter(Sport == "Cricket")%>%
  arrange(Year, Medal)
table(cricket$Medal)



# distribution of the Height

summary(olympic$Height)
boxplot(olympic$Height)
hist(olympic$Height)

# qqplot
library(car)
qqPlot(olympic$Height, distribution="norm", envelope=.95)

######
# all swimming data ponints
######

swimming <- olympic[olympic$Sport == "Swimming",]
table(swimming$Height)

dim(swimming)

# distribution of the height of the swimming althelets
# visualization
library(ggpubr)
ggdensity(swimming$Height, main = "Density Plot of the Height of Swimmers",
          xlab = "Height of Swimmers")
# qqplot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
ggqqplot(swimming$Height) 



# Nomality test http://www.sthda.com/english/wiki/normality-test-in-r
# Visual inspection, described in the previous section, is usually unreliable. It’s possible to use a significance test comparing the sample distribution to a normal one in order to ascertain whether data show 
# or not a serious deviation from normality.There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test.

# Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.
shapiro.test(swimming$Height)
# too much observation(23195)
# 如果只测里约的usa呢?
swimUSA16 <- swimming[swimming$Year == "2016" & swimming$NOC == "USA",]
#shaprio-wilk's test
shapiro.test(swimUSA16$Height)
# 这样其实是有问题的,因为存在一个人同时参加几个项目.身高重复计算.





#####
# test 实验区
####
##Andrew's curve: Visualization: outliers, clusters
library(lattice)
# no NA in Medal --> 无明确分组
swimUSA16.medal<-swimUSA16[!(is.na(swimUSA16$Medal)), ]

parallel(~swimUSA16.medal[4:6], swimUSA16.medal, groups = Medal,
         horizontal.axis = FALSE, scales = list(x = list(rot = 90)))


# 得奖人性别呢? --> 有明显不同
parallel(~swimUSA16.medal[4:6], swimUSA16.medal, groups = Sex,
         horizontal.axis = FALSE, scales = list(x = list(rot = 90)))


可用于看不同国家人身高体重有无分类
###########


# variance https://www.cnblogs.com/jpld/p/4594003.html
bartlett.test()



