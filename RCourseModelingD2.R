##Julia Frederick
##julia.frederick@uga.edu
##ECOL 8540
##Dr. Park and Dr. Drake
##Day 2 R Course Part 2 (Modeling) - 14MAY2019
##R for Data Science by Hadley Wickham and Garrett Grolemund
##https://daphnia.ecology.uga.edu/drakelab/?page_id=2323
install.packages("modelr")

library(tidyverse)
library(magrittr)
library(GGally)
library("modelr", lib.loc="~/R/win-library/3.5")

####Task 1: Using either read_csv or load, import the data set you put together in the module on 'Data wrangling in R'.
setwd("~/RWorkshop/lyme")
allLyme2 <- read_csv("lymeData.csv")

####Task 2: Use the ggpairs function #ggpairs(df,columns=c("x","y","z"))# to obtain a 4x4 summary plot of precipitation (prcp), 
#average temperature (avtemp), population size (size), number of Lyme disease cases (cases). 
ggpairs(allLyme, columns=c("prcp","avtemp","size","case"))


####Task 3: Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size
#and cases supplied when you recreate the ggpairs plot. Why do we add 1 to the number of cases?
allLyme %<>% mutate(log10size=log10(size))
allLyme %<>% mutate(log10case=log10(case+1)) #if +1 doesn't occur you get -Inf as the value for where the case load was 0
ggpairs(allLyme, columns=c("prcp","avtemp","log10size","log10case"))


####Task 4: Using set.seed(222) for reproducibility, create a new data frame to be a random sample (n=100 rows) 
#of the full data frame and plot precipitation (x-axis) vs average temperature (y-axis).
#Hints: You can make use of the dplyr function sample_n. Name your ggplot (myPlot <- ggplot...) then call it (plot it) 
#on a separate line (this will make it easy to add a subsequent layer to the plot)

set.seed(222)
sub100Lyme <- allLyme %>% sample_n(100)
sub100Lyme

myPlot <- ggplot(sub100Lyme) +
  geom_point(aes(sub100Lyme$prcp, sub100Lyme$avtemp))
myPlot

####Task 5: Add the best straight line to the plot using geom_smooth.
#Hint: You'll need to use the appropriate method which you can find in the help file (?geom_smooth)
smoothPlot = myPlot + geom_smooth(aes(sub100Lyme$prcp, sub100Lyme$avtemp), method="lm") #lm for linear models
smoothPlot

####Task 6: Create a linear model (lm) object with a call like myModel <- lm(y ~ x, data = myData) for the
#subsetted data, where y=avtemp and x=prcp. In addition, view the summary with a call along the lines of
#summary(myModel)
myModel <- lm(sub100Lyme$avtemp~sub100Lyme$prcp, data=sub100Lyme)
summary(myModel)

####Task 7: What is the slope of the line you plotted in Task 5, and is the slope significantly different from 0 (p<0.05)?
summary(myModel)$coefficients[2,4]
#Slope is 0.00672
#It is significantly different from 0 p=3.19e-6

####Task 8: Write a single line of code to generate a ggplot of total population size by year.
#Hint: you should pass the main (large) data frame to a group_by call and then a summarize call, then
#you can pass this new, unnamed data frame to ggplot using ggplot(.) and specify the aesthetics in a
#geom_point call.
task8Plot <- allLyme %>% group_by(year) %>% summarize(tPop=sum(size)) %>% ggplot(.) + geom_point(aes(year,tPop))
task8Plot

#Task 9: Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it.
by_state <- group_by(allLyme,state)

#Task 10: Next, update this new data frame so that it is nested (simply pass it to nest). Again, inspect the
#data frame by typing its name in the console so see how things changed.
by_state %<>% nest

#Task 11: Display the Georgia data in the console window.
by_state$data[[10]]

#Task 12: Write a function that takes a data frame as its argument and returns a linear model object that
#predicts size by year.
szByYear <- function(holdDF){
  predictModel <- lm(year~size, data=holdDF)
  return(predictModel)
}
model1 <- szByYear(by_state$data[[10]])

detach("package:maps", unload=TRUE) ##can also directly call from a package maps::map or purrr::map.

####Task 13: Add a column to the by_state dataframe, where each row (state) has its own model object
install.packages("purrr")
library(purrr)
by_state %<>% mutate(model= map(data, szByYear)) 

####Task 14: Run these commands and inspect "resids". What is the structure of "resids"?
by_state %<>% mutate(resids=map2(data, model, add_residuals))
by_state$resids[[10]]

####Task 15: Write a function that accepts an object of the type in the resids list, and returns a sum of the
#absolute values, i.e. ignoring sign: abs(3)+abs(-2)=5. Use the function to add a column called totalResid
#to by_state that provides the total size of residuals summed over counties and years.

#MAP function is comparing columns to each other
tRedsid <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid=map(resids, tRedsid))

####Task 16: Write a function that accepts a linear model and returns the slope (model M has slope
#M$coefficients[2]) and then use this function to create a new column called slope in the by_state data
#frame, that is the slope for each state.
findSlope <- function(y){
  s <- y$coefficients[2]
  return(s)
}
by_state %<>% mutate(slope=map(model, findSlope))

####Task 17: Plot the growth rate (slope value) for all states.
Unnested <- by_state %<>% unnest(slope)
stateSlope <- ggplot(by_state) +
  geom_point(aes(state,slope)) +
  theme(axis.text.x= element_text(angle = 90, hjust = 1))
stateSlope

####Task 18: Plot the total resisduals for all states.
by_state %<>% unnest(totalResid)
stateResid <- ggplot(by_state) +
  geom_point(aes(state,totalResid)) +
  theme(axis.text.x= element_text(angle = 90, hjust = 1))
stateResid

####Task 19: Repeat Tasks 9 and 10 using a different data frame name, by_state2.
by_state2 <- group_by(allLyme,state)
by_state2 %<>% nest

####Task 20: Write a function that accepts an element of the by_state2$data list-column and returns the
#spearman correlation coefficient between Lyme disease cases and precipitation
SpearCorr <- function(elem) {
  result <- cor.test(elem$case, elem$prcp, data=elem)
  return(result)}
SpearCorr(by_state2$data[[1]])


################################################################################################################
###################################################NOTES########################################################
################################################################################################################

#Organizing by groups ex//county, states
library(GGally)
library(magrittr)
data(cars)
cars %>% ggpairs(columns=c("speed","dist"))
  #ggpairs produces a 4x4 plot with speed plotted, distance plotted, the correlation, and a dotplot of speed vs distance
  #ggpairs function wasn't working as expected on Andrew's computer we'll see if its an issue later
library(dplyr)
cars %<>% mutate(log10speed=log10(speed)) 
cars %>% ggpairs(columns=c("log10speed", "dist"))

#Reproducible and subsampling
  x<-tibble(rnorm(10)) %>% print
  #A tibble 10x1 creates a table
  
  x %>% sample_n(5) #gives a subsample of 5 of the numbers in the previous tibble table, it's a random sample
    #if you run again you get a different random subsample
  set.seed(123); x %>% sample_n(5) #1st call 
    #seed is the start of a call. If you use set.seed and send data to others this will pull the same "random" subset
  
#Linear Modeling
library(ggplot2)
ggplot(cars) +
  geom_point(aes(speed,dist))+
  geom_smooth(aes(speed, dist), method="lm") #method lm = linear model

summary(lm(speed~dist, data=cars)) #produces a chart of the linear model , gives estimate of distance = slope, gives p values

#lists
y<-list(3.14, "eggs", lm(speed~dist,data=cars)) %>% print #can mix data types in lists

#Correlation Coefficients
corr.test(cars$speed, cars$dist, method="spearman") #produces a chart of "Spearman's rank correlation rho"

#package - modelr
  #create model from data
  #store modeling information with data
  #group and nest data for analysis (nesting follows group)
  #un-nest for visualization 