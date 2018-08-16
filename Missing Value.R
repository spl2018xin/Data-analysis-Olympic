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
# Missing value in Age adn Height
marginplot(missingMap[c(4,5)])
# Missing value in Age adn Weight
marginplot(missingMap[c(4,6)])

# 由于我们发现Height and Weight 中的missing value占总的missing value 的大多数,
# 我们首先分析这两个factor:
# 左边的红色箱线图表示有Height缺失的样本的missingMap的分布, 即在除Medal外的整个dataset里的分布
# 蓝色的箱线图表示的是剩下的数据点的分布。。(在MCAR的假设下，蓝色和红色的箱线图应该非常接近。)
# 我们发现,height 与weight, age and height, age and weight 都有有很明显的正相关性.
# so we cannot simply imputate the missing value in age, weight and height with the their group or subgroup mean or median,
# this positive corrlation among the three variables should also be taken into consideration.

### Where are they? (in which block)
# There are 3% missing values without considerating the Medal item. From the Missing Map
# we can see, the most non recorded information are both weight and height, in another word,
# if an althelet's height was not recorded, with a very high probabilty that his/her weight is also missed.

# NA after unique ID/ Name
# 
uniName <- data.frame(unique(missingMap$Name))
colnames(uniName) <- c("Name")
uni <- merge(uniName,missingMap,by = "Name")

library(sqldf)

# sqldf does not support the FIRST() aggregate function -> take MIN() instead
uni <- sqldf('
  select Name, Sex, Age, Year, Height, Weight 
  from olympic_nonArt
  group by Name, Sex, Age, Year, Height, Weight
  order by Year
')

uni.adult <- sqldf('
  select Name, Sex, Age, Year, Height, Weight, min(Sport), min(Event), Team
  from olympic_nonArt
  group by Name, Sex, Age, Year, Height, Weight, Team
  having Age >= 18
  order by Year
')
# NA in all three variable



# 晓秋的图: missing values happend quite often before 1960.  



### Missing value imputation
#1. check the distribution of the height, weight and age in each team and event in the corresponding Games.
#2. check Main steps used in multiple imputation


