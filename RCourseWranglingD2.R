##Julia Frederick
##julia.frederick@uga.edu
##ECOL 8540
##Dr. Park and Dr. Drake
##Day 2 R Course Part 1 (Wrangling) - 14MAY2019
##R for Data Science by Hadley Wickham and Garrett Grolemund
##https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

##!Getting the data in the shape we want it!##

####Task 1
install.packages("tidyverse","magrittr","dplyr","stringr","GGally","maptools","ggmap","maps")

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

setwd("~/RWorkshop/lyme")
ld <- read_csv("lyme.csv")
pop <- read_csv("pop.csv")
prism <- read_csv("climate.csv")


####Task 2 - Data for county codes are correct for some, only 1 digit for another, and include the full fips for some
  #The state and county are combined in a column for some of the data rows
  #using str_replace_all(where R should look to perform replacements, pattern to replace,replacing pattern)
  #regexp - identifies/finds character strings


####Task 3 - population data tidy format
#this line selects fips and the columns that being with "pop2" (2000-2014) and make them what is visually available in the sheet
pop %<>% select(fips,starts_with("pop2"))

#Finds all columns starting with "pop2", and moves them into "str_year" row, and moves the values associated into
#"size" column, and removes all data with na values
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit

#removes "pop" from before the year in "str_year" and makes a new column labelled "year"
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))

#Changes the "year column from "chr" to "int"
pop %<>% mutate(year=as.integer(year))

#removes all "0" from the before the fips codes
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))

#makes the fips an interger
pop %<>% mutate(fips=as.integer(fips))
##<dbl> is what size is, it's an non-interger number (one with decimals) however, it doesn't really matter dbl vs int


####Task 4 - lyme disease data
#Combining FIPS codes
fipsBuild <- function(st,ct){
  if (str_length(ct)==3) {
    new <- paste(as.character(st), as.character(ct), sep="") %>% as.integer}
    else if (str_length(ct)==2){
      new <- paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer}
      else {new <- paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer}}
 
ld %<>% rowwise() %>% mutate(fips=fipsBuild(STCODE,CTYCODE))

#Renaming Columns (https://medium.com/@HollyEmblem/renaming-columns-with-dplyr-in-r-55b42222cbdc)
colnames(ld)
ld <- ld %>% rename(state = STNAME, county=CTYNAME) 

####Task5 - Join the ld data frame and PRISM data frame to form a new data frame
  #Retain county-year combinations for the disease and climate data
  #(http://stat545.com/bit001_dplyr-cheatsheet.html)

ld2 <- ld #safe guard
#Code altered from Task 3
ld <- ld %<>% select(fips,starts_with("Cases"), county, state)
ld %<>% gather(starts_with("Cases"),key="str_year",value="case") %>% na.omit
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))

#combining data retaining county(fips) and year
combined_data <-inner_join(ld,prism, by=c("fips", "year"))

####Task 6: Write a line of code to additionally combine the demographic data with the Lyme disease and climate data.

allData = inner_join(combined_data, pop, by=c("fips","year"))
allData %<>% select(-c(str_year.x, str_year.y)) #removing columns I don't like

####Task 7: Write two lines of code that create two new data frames: 
#(1) to determine how many cases of Lyme disease were reported each year
lymeYear <- allData %>% group_by(year) %>% summarize(avgLyme=sum(case)) %>% print

#(2) the average number of cases in each state - averaged across county and year. 
lymeAvgState <- allData %>% group_by(state) %>% summarize(avgLyme=mean(case)) %>% print

#What was the worst year? Which three states have been most impacted on average?
  #worst year is 2009
  #3 states are Connecticut, Massachusetts, Delware

####Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same
save(allData, file='LymeData.Rda')
write_csv(allData, path='lymeData.csv')

####Task 9: Add annotations to the following lines of code with comment lines to explain what they are achieving.
#Note: in the code line with "group_by(ld.prism.pop)" you need to replace the data frame in parentheses with
#the name of your data frame in which you combined Lyme disease, climate and demography data (Task 6)
county_map <- map_data("county") #loaded in county map data
state_map <- map_data("state") #loaded in state map data
ag.fips <- group_by(allData,fips) #grouped the data by fips
ld.16y<-summarize(ag.fips,all.cases=sum(case)) #total number of cases per FIPS
ld.16y<-left_join(select(allData,c(state,county,fips)),ld.16y) #Added State and County to FIPS total Sheet
ld.16y<-distinct(ld.16y) #retains only unique rows
ld.16y %<>% rename(region=state,subregion=county) #renaming to region and subregion
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","") #removed word County from 
ld.16y$region<-tolower(ld.16y$region) #lower case the state names
ld.16y$subregion<-tolower(ld.16y$subregion) #lower case the county names
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","") #removed the word parish
ld.16y %<>% mutate(log10cases=log10(1+all.cases)) #log10 the cases into a new column
map.ld.16y<-left_join(county_map,ld.16y) #joined the ld.16y and the county map
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10))) ##made a heatmap plot

#geom_tile(state, year)

################################################################################################################
###################################################NOTES########################################################
################################################################################################################

##read.csv
  ##read_csv - slightly faster and the resulting data frame is recognized as a tibble
##objects - we can read previously saved objects with the "load" command
  ##save(df, file='get_df.Rda')
    #(file name, new name in R?)
    #any changes made in this file after being saved are not automatically updated
    #will need to re-save if we want to load with the changes we made
  ##load('get_df.Rda')
##Tidy data format
  #Each variable has its own column
  #Each obs has its own row
  #Each value has its own cell
  #####Major commands:
    #####Package dplyr
    #####gather (takes info thats not stored in the tidy data format and puts it that way), 
    #####select (take particular columns), 
    #####mutate (changes selected columns to do what you want to them)
  #String manipulation (stringr package)
    #####str_replace_all
    #####paste (base R)
#FIPS codes - like a zipcode but applies to a county in the US (today's data will be looked at on a county scale)
  ##State has a unique number, and within each state there are county codes (recycled between states) combine in #0#
  ##We will build these in R for the purposes of this exercise
#Piping
  ##Run several functions
    foo_foo <- hop(foo_foo, through=forest)
    foo_foo <- scoop(foo_foo, up= field_mice)
    foo_foo <- bop(foo_foo, on=head)
  ##String functions together
  ##Use piping
    foo_foo %>% hope(through = forest) %>% scoop(up=field_mouse) %>% bop(on=head)
#Piping and Assigning
  #Can rename/rewrite the data frame as we go, if we want to keep the environment cleaner
  x<-2
  x<-sqrt(sin(x))
  x
  #Can rewrite simply with the piping functions
  library(magrittr)
  x<-2
  x %<>% sin %>% sqrt
  x
#Applying functions to each row
  library(tidyverse)
  library(magrittr)
  pocket_money <-tibble(name=c('Jack','Jill'), savings=c(10,15), chores=c(5,2)) %>% print
    #Creates a chart
  get_sum<-function(savings,chores){
    return(sum(savings, chores))}
  pocket_money %>% rowwise %>% mutate(total=getsum(savings,chores)) %>% print
    #Creates a chart with a new column that has the total for each row, if you dont use rowwise the whole chart adds
    #goes row by row for the function
#Combining data sets
  #Base r - dataframes are combinded using the #merge# function
  #using dplyr - tibbles are combined using #join# functions
    #there are different types of joinng functions (http://stat545.com/bit001_dplyr-cheatsheet.html)
  gourmet <- tibble(
    state=c("ga","ca"),
    sauce=c("sweet", "hot"))
  greeting <- tibble(
    state=c("TX","ca"),
    word=c("howdy", "hi"))
  full_join(gourmet,greeting)
  #retains all of the data even if data is missing
  inner_join(gourmet,greeting)
  #retains only the rows where there will be complete data
#Obtaining summary information
  set.seed(123)
  grades <- tibble(
    student_ID=as.character(1:10),
    major=samples(c("E","B"),10,replace=T),
    grades=grade(c("100","100"),10,replace=T))
  grades %>% group_by(major) %>% summarize(avScore=mean(score))
  #Can use summarize instead of the aggregate command
  

