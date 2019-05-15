##Julia Frederick
##julia.frederick@uga.edu
##ECOL 8540
##Dr. Drake and Dr. Park
##Day 3 R Course Part 1 (Project Management) - 15MAY2019
##R for Data Science by Hadley Wickham and Garrett Grolemund
##https://daphnia.ecology.uga.edu/drakelab/?page_id=2323

################################################################################################################
###################################################NOTES########################################################
################################################################################################################

#File Structure
  #One project - one directory
  #Where do you keep the data? - a designated data directory or within the project directory
    #Stable data (provided by an online resource) - can point to it and don't need to download
      #doi will be stable, however, unless the data is too large to download keep it in your own working directory
    #.R and .Rnd
    #multiple files by multiple people or multipl layers to analysis - should do all pre-process in R
      #can break up by pre, data, figures, analysis ect. Can have an overarching script that calls all of the other scripts
        #using function #source("preprocess.R") - will be like everything that is written in "preprocess.R" is in that script
  #Using source and auxiliary functions
  #A data analysis workflow, reporting, and the writing of scientific manuscripts
    #all within the same working directory
    #sequence of R markdown documents that contains a lot of information, and some manuscripts are written in R
    #overleaf, googledocs, github
#Keeping Track of Changes (http://github.com/jdrakephd)
  #git and Github - standard way of comp programmers to maintain their projects 
    #Some are testing for opensource lab notebooks (some doing open within the lab group, and then open to public are pub)
  #Git is a way of tracking changes
  #github is a repository, archiving offsite and can be open or closed (as a student free access, 5 private repositories)
    #can call an ask for more and they usually have no problem for academic accounts
    #Link github to Rmarkdown - Once we have created an account we will push the scripts created throughout this class
#Statistically literate programming #R/Markdown#
  #Reports, Presentations, Shiny apps, Dashboards, Books, Websites
  #See the gallery: http://rmarkdown.rstudio.com/gallery.html
  #Handout will giv instructions on how to use it, the RMarkdown document you open in R will give a template
    #Code chunks - switching between code chunks and text 

