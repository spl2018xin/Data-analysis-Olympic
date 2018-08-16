setwd("~/Desktop/datenanalyse/Final Report")
olympic <- read.csv("~/Desktop/datenanalyse/Final Report/data/athlete_events.csv")

library(ggplot2)
library(dplyr)


#####
# the number of male and female in each olympic games 数据
NumGender <- olympic %>% 
  group_by(Year) %>%
  summarize(NumMale   = sum (Sex == "M"),
            NumFemale = sum (Sex == "F"))

# Grouped Bar Plot of gender 丑-图像
data <- olympic%>%
  group_by(Sex)
ggplot(data, aes(x = Year, color = Sex)) + geom_histogram(binwidth = 2) # 柱状图
ggplot(data, aes(x = Year, y = Sex, color = Sex)) + geom_col() # 柱状图，丑
# 一开始女性很少，不过占比呈逐渐增加状，女性人数到近10年才慢慢和男性差不多。
ggplot(data, aes(x = Year, y = Sex, color = Sex)) + geom_col() + facet_wrap(~Sex) # 分开的柱状图，可以用！
# 女性增加的比较缓慢，不过增加趋势比较平缓，男的人数一直多于女性，不过波动的比较大
# 总体：WW1, WW2 之间有两个缺口，从1992年开始，数据呈2年统计一次，之前是4年一次，所以比较疏松，之后特密，因为包含了冬季运动会。
 

######
# the proportion of Male and Female from each year

View(olympic %>% 
       group_by(Year) %>%
       summarize(pct.males = sum(Sex == 'M') / length(Sex) * 100,
                 pct.female = sum(Sex == 'F') / length(Sex) * 100))