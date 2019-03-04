# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
# install.packages("dplyr")
library(dplyr)

setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")

dt_KPI %>% group_by(Unit) %>% summarize(Premium = sum(Premium, na.rm = TRUE)) %>% arrange(desc(Premium)) #Najvacsi profit ma Unit 7

dt_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% 
  group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit)   # najhorsi bol rok 2014

#install.packages("ggplot2")
library(ggplot2)

dt_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% 
  group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit) %>% ggplot(aes(x = Year, y = profit)) + geom_col()


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




# Your Explanation about analysis:
# 1. zistenie portfolia s najvacsim profitom (nie som si ista, co je v tomto pripade portfolio - brala som stlpec Unit)
# 2. pomocou filtrovania dat pre Unit 7 zistenie najhorsieho roku
# 3. graf - vidime z neho, ze naozaj je najhorsi rok 2014
