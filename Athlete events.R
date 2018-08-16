setwd("~/Desktop/datenanalyse/Final Report")
olympic <- read.csv("~/Desktop/datenanalyse/Final Report/data/athlete_events.csv")


# Dataset overview
head(olympic )
str(olympic )
summary(olympic )
olympic

###########
#female

# total observations = 271116
gender.total <- data.frame(table(olympic$Sex))

# total female observations = 74522, female portion = 27.49% 
gender.total[gender.total$Var1 =="F", ] / sum(gender.total$Freq) # 习题里有类似的

# display the selected columns from the data frame
Medal.female <- athlete_events  %>%
  select(Sex, Year, Medal) %>%
  filter(Sex == "F" & Medal != "NA" )# %>%
  #summarise(thefirst = min(Year))

##########
# years

years<- data.frame(table(data$Year))
colnames(years) <- c("year","athletes")

female_1896 <- data.frame(data$Sex[data$Year == "1896"] == "F")


games <- data$Games
table(games)

sports <- data$Sport
# what kind of sports doesn't exist now?
sports_count <- data.frame(table(sports))

########################################
#       data preparation
#########

# Load packages
library("plotly")
library("tidyverse")
library("data.table")
library("gridExtra")
library("knitr")

# Load athletes_events data 
data <- read_csv("~/Desktop/datenanalyse/Final Report/data/athlete_events.csv",
                 col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

# Options
opts_chunk$set(warning=FALSE, message=FALSE)


############################################
#       5. Women in the Olympics
#
####5.1 Number of men and women over time###


# Exclude art competitions from data (I won't use them again in the kernel)
data <- data %>% filter(Sport != "Art Competitions")

# Recode year of Winter Games after 1992 to match the next Summer Games
# Thus, "Year" now applies to the Olympiad in which each Olympics occurred 
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  data$Year <- gsub(original[i], new[i], data$Year)
}
data$Year <- as.integer(data$Year)

# Table counting number of athletes by Year and Sex
counts_sex <- data %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

# Plot number of male/female athletes vs time
ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  scale_color_manual(values=c("darkblue","red")) +
  labs(title = "Number of male and female Olympians over time") +
  theme(plot.title = element_text(hjust = 0.5))




###########################################################
#       5. Women in the Olympics
#
####5.2 Number of women relative to men across countries###



# Count M/F/Total per country per Olympics 
# Keep only country-years with at least 30 athletes
counts_NOC <- data %>% filter(Year %in% c(1936,1956,1976,1996,2016)) %>%
  group_by(Year, NOC, Sex) %>%
  summarize(Count = length(unique(ID))) %>%
  spread(Sex, Count) %>%
  mutate(Total = sum(M,F,na.rm=T)) %>%
  filter(Total > 49)
names(counts_NOC)[3:4] <- c("Male","Female")
counts_NOC$Male[is.na(counts_NOC$Male)] <- 0
counts_NOC$Female[is.na(counts_NOC$Female)] <- 0
counts_NOC$Year <- as.factor(counts_NOC$Year)

# Plot female vs. male athletes by NOC / Year
ggplot(counts_NOC, aes(x=Male, y=Female, group=Year, color=Year)) +
  geom_point(alpha=0.6) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  geom_smooth(method="lm", se=FALSE) +
  labs(title = "Female vs. Male Olympians from participating NOCs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(reverse=TRUE))

#####################################################
#       5. Women in the Olympics
#
####5.3 Proportion of women on Olympic teams: 1936###


# Proportions of athletes/medals won by women from select NOCs/Years
props <- data %>% filter(Year %in% c(1936,1976,2016)) %>%
  group_by(Year, NOC, Sex) %>%
  summarize(Athletes = length(unique(ID)),
            Medals = sum(!is.na(Medal))) 
props <- dcast(setDT(props), 
               Year + NOC ~ Sex, 
               fun.aggregate = sum, 
               value.var = c("Athletes","Medals"))
props <- props %>% 
  mutate(Prop_F_athletes = Athletes_F/(Athletes_F + Athletes_M),
         Prop_F_medals = Medals_F/(Medals_F + Medals_M)) %>%
  filter(Athletes_F + Athletes_M > 49)
props$Prop_F_medals[props$Medals_M + props$Medals_F == 0] <- NA

# Data for 1936 only
props_1936 <- props %>% 
  filter(Year == 1936) %>%
  gather(Prop_F_athletes, Prop_F_medals, key="type", value="value")
levs <- props_1936 %>% 
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% select(NOC)
props_1936$NOC <- factor(props_1936$NOC, levels=c(levs$NOC))

# Plot 1936
ggplot(props_1936, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="1936 Olympics (Garmisch-Partenkirchen and Berlin)", x="Proportion female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0,1)

############################################################
#       5. Women in the Olympics
#
####5.4 Medal counts for women of different nations: 1936###

# Count number of medals awarded to each NOC at 1936 Olympics
counts_1936 <- data %>% filter(Year==1936, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
levs_1936 <- counts_1936 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_1936$NOC <- factor(counts_1936$NOC, levels=levs_1936$NOC)

# Plot 1936
ggplot(counts_1936, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Medal counts for women at the 1936 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))


############################################################
#       5. Women in the Olympics
#
####5.5 Proportion of women on Olympic teams: 1976#########

# Data for 1976 only
props_1976 <- props %>% 
  filter(Year == 1976) %>%
  gather(Prop_F_athletes, Prop_F_medals, key="type", value="value")
levs <- props_1976 %>% 
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% select(NOC)
props_1976$NOC <- factor(props_1976$NOC, levels=c(levs$NOC))

# Plot 1976
ggplot(props_1976, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="1976 Olympics (Innsbruck and Montreal)", x="Proportion female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0,1)


################################################################
#       5. Women in the Olympics
#
####5.6 Medal counts for women of different nations: 1976#######

# Count number of medals awarded to each NOC at 1976 Olympics
counts_1976 <- data %>% filter(Year==1976, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
levs_1976 <- counts_1976 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_1976$NOC <- factor(counts_1976$NOC, levels=levs_1976$NOC)

# Plot 1976
ggplot(counts_1976, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Medal counts for women at the 1976 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))

#########################################################
#       5. Women in the Olympics
#
####5.7 Proportion of women on Olympic teams: 2016######

# Data for 2014/2016 only
props_2016 <- props %>% 
  filter(Year == 2016) %>%
  gather(Prop_F_athletes, Prop_F_medals, key="type", value="value")
levs <- props_2016 %>% 
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% select(NOC)
props_2016$NOC <- factor(props_2016$NOC, levels=c(levs$NOC))

# Plot 2014/2016
ggplot(props_2016, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="2014/2016 Olympics (Sochi and Rio)", 
       x="Proportion female") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=6)) +
  xlim(0,1)

###############################################################
#       5. Women in the Olympics
#
####5.8 Medal counts for women of different nations: 2016######

# Count number of medals awarded to each NOC at 2014/2016 Olympics
counts_2016 <- data %>% filter(Year==2016, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarize(Count=length(Medal)) 

# Order NOC by total medal count
levs_2016 <- counts_2016 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(NOC)
counts_2016$NOC <- factor(counts_2016$NOC, levels=levs_2016$NOC)

# Plot 2014/2016
ggplot(counts_2016, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Medal counts for women at the 2014/2016 Olympics") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size=6))

