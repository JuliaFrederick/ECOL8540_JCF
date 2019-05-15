#Julia Frederick
#ECOL 8540 - Project Management
#julia.frederick@uga.edu
#15MAY2019
#Instructors: Dr. Drake and Dr. Park

####Packages
install.packages("lubridate", "ggplot2")
library(lubridate)
library(ggplot2)

####Functions

####Loading Data
setwd("~/ECOL8540_JCF/mers")
mers <- read.csv("cases.csv")

####Data Analysis
mers$hospitalized[890] = c('2015-02-20')
mers = mers[-471,]
mers$onset2 = ymd(mers$onset)
mers$hospitalized2 = ymd(mers$hospitalized)
day0 = min(na.omit(mers$onset2))
mers$epi.day = as.numeric(mers$onset2 - day0)

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', 
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
