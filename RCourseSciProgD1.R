##Julia Frederick
##julia.frederick@uga.edu
##ECOL 8540
##Dr. Drake and Dr. Park
##Day 1 R Course Part 2 - 13MAY2019
##R for Data Science by Hadley Wickham and Garrett Grolemund
##https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

###############################################################################################
#################################Assignment for Scripts########################################
###############################################################################################

######Exercise 1#######
#Exercise. Write a script to load the West Nile virus data and use ggplot to create a histogram
#for the total number of cases in each state in each year. Follow the format of the prototypical
#script advocated in the presentation: Header, Load Packages, Declare Functions, Load Data, Perform Analysis.

#############################Loading Packages############################
install.packages("ggplot2","lubridate")
library(ggplot2)
library(lubridate)

#############################Declare Functions###########################
wnv$logTotal <- log(wnv$Total) ##For Exercise 2
CFR = wnv$Fatal/wnv$Total

###############################Load Data#################################
setwd("~/RWorkshop/wnv")
wnv<-read.csv('wnv.csv')

#############################Perform Analysis############################
class(wnv$Total) #interger

##Stacked Histogram
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  geom_histogram(stat = "identity", position = "stack", aes(fill = wnv$State)) +
  coord_flip() +
  labs(y = "Total Number of Cases", x = "Year")

##Wrapped Histograms by State
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

##Need Stat "Identity"## - Most correct general plot!
ggplot(data = wnv, aes(x = wnv$Year, y = wnv$Total)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

######Exercise 2#######
#Exercise2. The state-level and case burden is evidently highly skewed. Plot a histogram for the
#logarithm of the number of cases. Do this two different ways.

##Log plot in the wnv$Total
ggplot(data = wnv, aes(x = reorder(wnv$Year, log(wnv$Total), y = log(wnv$Total)))) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

##Log plot using the new logTotal column in the data set of transformed data  
ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$logTotal), y = wnv$logTotal)) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

######Exercise 3#######
#Exercise3. Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state
#in each year. Plot a histogram of the calcated CFRs.

ggplot(data = wnv, aes(x = reorder(wnv$Year, CFR), y = CFR)) +
  geom_histogram(stat = "identity", position = "stack") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

######Exercise 4#######
#Exercise 4. Use arithmetic operators, logical operators, and the function sum to verify that the
#variable Total is simply the sum of the number of febrile cases, neuroinvasive cases, and other cases.

tCases = (wnv$EncephMen+wnv$Fever+wnv$Other)
sum(wnv$Total==tCases)

######Exercise 5#######
#Exercise. Use modular arithmetic to provide an annual case count for each state rounded
#(down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated
#with this calculate, then add the errors to obtain the total error.

wnv$Rounded <- wnv$Total - wnv$Total%%12
wnv$error <- wnv$Total - wnv$Rounded
sum(wnv$error)

wnv$PerError <- (wnv$error/wnv$Total)
sum(wnv$PerError)

###############################################################################################
#################################Assignment for Functions######################################
###############################################################################################

######Exercise 1#######
#Exercise. Write a function to calculate the mean and standard error (standard deviation
#divided by the square root of the sample size) of the neuroinvasive disease rate for all the
#states in a given list and given set of years. Follow the Google R style and remember to place
#the function near the top of your script. Use your function to calculate the average severe
#disease rate in California, Colorado, and New York.

##Functions
  ##colorado and 1997:2007 are the defaults, can be replaced with anything
ndr <- function(state='Colorado', years=1997:2007){
x <- wnv[wnv$State %in% state & wnv$Year %in% years,] ##asking if ex/ New York is in the list of states with Colorado, then looking if the year is within that range
y <- data.frame(state=x$State, ndr = x$EncephMen / x$Total)
m <-aggregate(y$ndr, by=list(y$state), FUN=mean)
se<-aggregate(y$ndr, by=list(y$state), FUN=function(x) sd(x)/sqrt(length(x)))
out<-merge(m, se, by ="Group.1")
names(out) <- c('state', 'mean.ndr','se.ndr')
return(out)
}
 
disease <-ndr(state=c("California", "Colorado", "New York"))
vState=state.name

ggplot(disease, aes(x=state, y=mean.ndr, fill=state)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr)) +
  labs(x="State", y="Neuroinvasive Disease Rate", title="Neuroinvasive disease rate, 1999-2007")

diseaseAll <-ndr(vState)
ggplot(diseaseAll, aes(x=state, y=mean.ndr, fill=state)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr)) +
  labs(x="State", y="Neuroinvasive Disease Rate", title="Neuroinvasive disease rate, 1999-2007")

###############################################################################################
#################################Assignment for Pipes##########################################
###############################################################################################
install.packages("magrittr")
library(magrittr)
library(dplyr)

wnv %>%
  filter(State %in% c("California", "Colorado","New York")) %>%
  group_by(State) %>%
  summarize(mean.ndr = mean(EncephMen/Total)), se.ndr=sd(EncephMen/Total)/sqrt(length(EncephMen/Total)) %>%
  ggplot(aes(x=State, y=mean.ndr, fill=State)) +
  geom_bar(state="identity") +
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr)) +
  labs(x="State", y="Neuroinvasive Disease Rate", title="Neuroinvasive disease rate, 1999-2007")

################################################################################################################
###################################################NOTES########################################################
################################################################################################################
####Assignments
a <- 5
print(a) #print to the screen
#returns [1] 5 meaning the first value is 5


####Arithemetic 
# options +, -, *, /, ^, 
#exp(2) - e log2?? 
#log(10) - default is that it is the natural log, log10(10), log(10,base=10)

####Logic
4==6
#[1] FALSE
6==6
#[1] TRUE
## == is the way to check and see if it equal to each other
(6==6)*3
#[1] 3
#the output for a logic test can be treated as numbers (TRUE/FALSE = 1/0)
#Useful if you have a list and you only want certain portions of it, if TRUE it can pull into a new ile
4!=6
#False , != is not equal to
4>6
#FALSE
#all the other typical ones

#################Scripting##################
##Script is written in the editor, handy tools like syntax highlighting and error checking
##Prototypical script
#Header (comments about function, author, contact, date, and change log)
#Load packages ex// ggplot2, lubridate
##install.packages(), library()
##don't want to do this if 2 packages in your script have the same function but you can add/remove them throughout
##There is also a particular way to call the function directly from the package you want but we don't cover that 
#Declare functions
#Load data
#Analysis
##Script is always evaluated in sequence

################Functions###################
##A function is a program within a program... - automate a common task
#Interpretability - a function can be given an evocative name that makes the code easier to understand
#Simplicity - as requirements change updates are made only in 1 place
#Robustness - reduces the frequency of mistakes
##Functions have 3 components  - 
#1. Name
#2. Arguments
#3. Body (all of the commands to perform the operations you want)
#"Google R Style Guide" - Programmers at Google use this method when writing in R

#Declare the function
x<-c(2,6,8,4)
#c is the shorthand for combine
mean<-function(x){
  s<-sum(x)
  n<-length(x)
  m<-s/n
  return(m)}
##Don't really want to make a function named mean since there is a command "mean"
mean(x)
#[1] 5

wt_mean <- function (x,w)..... #there's more, missed it
wt_mean2<- function(x,w){
  if(length(x) != length(w)){
    stop('x and w must be the same length', call. = FALSE)
  }
  m <-sum(x*w)/sum(w)
  return(m)
}

#################Pipes#################
##The pipe (%>%)m from the magrittr package is a tool for clearly expressing a seq of multiple operations
install.packages("migrittr")
library(migrittr)
rnorm(100)%>%
  matrix(ncol=2) %>%
  plot(xlab="Var1", ylab="Var2")

##############Flow of control##################
##Conditional Exectuction (if, else, ifelse)
a<-1
b<-2
if(a==1) {c<-b
} else {c<-b+a}
print(c)

ifelse(a==2, c<-b, C<-b+a)

##Loops (for, while, repeat)
#iterative computation
j<-0
for(i in 1:5){
  j[i+1]<-i*2
  print(j)
}
#This has i basically being c(1,2,3,4,5)  
##j is a vector use [] to reference the slot in the column to 
##Can loop over anything, and a vector
for(prefix in c('b','c','m','r')){
  word<- paste(prefix, "at", sep='')
  print(word)
}
##Referencing a matrix[#,#] can be matrix[,2]

##Flow in loops (break, next)
#Can stop a loop this way

########R Help#########
##use ?whatever you want
##Description, Usage, Arguments, Details, Value (output), References, Examples



