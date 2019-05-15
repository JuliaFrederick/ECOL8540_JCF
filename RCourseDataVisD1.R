##Julia Frederick
##ECOL 8540
##Dr. Drake and Dr. Park
##Day 1 R Course
##R for Data Science by Hadley Wickham and Garrett Grolemund
##https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

##Programming language, built out of statistics
##Repeatability!
  #Inability to recapture workflows (bench/computational)
  #New data can be analyzed in the same way
  #Computational workflow can be transferred to others
##Donald Kunthu - Invented the Linux computer language - literate programming language (human/computer can read)
##Statistically literate computer programming 
  #Code chunking - Visualization and statistical 
##Rstudio - development environment (there are others, this one stuck)

########################### Data Visualization #################################
##Grammar of graphics - breaks data visualization down into it's core pieces
  #Core constituents into layers
  #ggplot2 - grammar of graphics plotting software

##########Getting the data in R#########
setwd("~/RWorkshop/mers")

mers=read.csv('cases.csv')

##########Formatting some dates#########
head(mers)
  ##Shows what the data looks like
class(mers$onset)
  ##Returns "factor" to see how this particular column of data is formatted
mers$hospitalized[890] = c('2015-02-20')
  ##Changing the date on line 890
  ##[row,column]
mers = mers[-471,]
  ##Deleting that row - what the "-" indicates

install.packages("lubridate")
library(lubridate)
mers$onset2 = ymd(mers$onset)
  ##ymd = Transforms dates stored in character and numeric vectors to Date or POSIXct objects (see tz argument)
    ##Changes date format into what we want
mers$hospitalized2 = ymd(mers$hospitalized)
    ##5 failed to parse
      #Program couldn't figure out how to change this data or what it was
class(mers$onset2)
  #"Date" - no longer factor, the format has changed

day0 = min(na.omit(mers$onset2))
  ##This is indicating the earliest day of onset
  ##Question 1 - na.omit - removes cases
    #so we can have an elapsed time from the point of onset? and only have cases with onset dates

mers$epi.day = as.numeric(mers$onset2 - day0)
  ##Question 2 - Creates or coerces objects of type "numeric". is.numeric is a more general test of an object being interpretable as numbers.


##########Making a plot#########
install.packages("ggplot2")
library(ggplot2)

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  ##Creates a plot
  ##If we don't use the "+" it doesn't include the lines after the +

##Modify the plot to show the difference between the countries
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  ##This plot shows a color coded plot of what country the cases are occuring in

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', 
       caption="Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
    coord_polar()
  ##coord_flip() or coord_polar() are another layer to add to the current plot
    #flip flips the plot
    #polar makes it a circle plot

##########Univariate Plots#########
###More common plot types: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

mers$infectious.period = mers$hospitalized2-mers$onset2  ##calculate "raw" infectious period
class(mers$infectious.period)  ##These data are class "difftime
mers$infectious.period = as.numeric(mers$infectious.period, units = "days") #convert to days

##Creates a histogram
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
    #Has negative infectious periods this is occuring because of the way mers transmits
    #we want to calculate a new value 

mers$infectious.period2 = ifelse(mers$infectious.period<0,0,mers$infectious.period)
  ##ifelse - returns a value with the same shape as test
    #ifelse(test, true, false)

##Creates a plot with only the positive values
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##Creates a density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##Creates an area plot, shaded curve of the density plot
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##Other functions are geom_dotplot and geom_bar
  #dotplot - creates a dotplot
  #bar - creates a bar plot
ggplot(data=mers) +
  geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##########Bivariate Plots#########
##Looks at the relationships between variables, this section uses 2 variables
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point() + geom_smooth()
  #adding in geom_smooth shows possible societal learning with small recoveries and infectious as it changes

##Adding geom_smooth to each Country
  #Found here: https://stackoverflow.com/questions/40600824/how-to-apply-geom-smooth-for-every-group
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) + 
  geom_smooth(aes(color=country), method = "loess")

##########Faceting#########
##Multipanel plots uding facet_wrap() and facet_grid()

##Breaks data into individual graphs by country looking at the same variables using facet_wrap
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Breaks the previous wrapped data set that was broken by country into being broken by gender and country
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'France', 'Italy', 'Kuwait', 'Lebanon', 'South Korea', 'Tunisia', 'UAE', 'UK', 'Yemen')), 
       mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Breaks into case fatlity over time and across country
fatal <- ggplot(data=subset(mers, outcome %in% c('fatal', 'recovered') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'France', 'Italy', 'Kuwait', 'Lebanon', 'South Korea', 'Tunisia', 'UAE', 'UK', 'Yemen')), 
       mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(outcome ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by case outcomes and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(fatal)

##Studying variation in case fatality - looking for %
##caseFatality = aggregate
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2.exts")
##fatal = subset(mers, outcome %in% c('fatal', 'recovered') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'France', 'Italy', 'Kuwait', 'Lebanon', 'South Korea', 'Tunisia', 'UAE', 'UK', 'Yemen'))

CountryEpi <- ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) + 
  geom_smooth(aes(color=country), method = "loess")
ggplotly(CountryEpi)

epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
