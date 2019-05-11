#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)
library(viridis)
library(ggridges)
library(plotly)

  enrollment_eighteen_fall <- read_excel("fall_course_enrollment_analysis/fall_2018.xlsx", skip = 2) %>% 
    
    #Next, one filters the courses where the title is NA
    
    filter(!is.na(`Course Title`)) %>% 
    
    #Next, one is only intersted in courses with more than 3 people in order to not include tutorials and the like.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interstested in certain columns, so they are not selected here.
    
    select(ID =`Course ID`, 
           
           #The title is named Title
           
           Title = `Course Title`, 
           
           #Next, the department variable is also named in a simple way
           
           Department = `Course Department`,
           
           #Next one has undergraduates labeled.
           
           Undergraduates = `UGrad`,
           
           #Next, the graduates are labeled as well in a way that is presentable to appear on a chart to avoid unnecessary work later.
           
           Graduates = `Grad`)
  
  
  #The next step is to fulfill this same code execution with the other years so that the data can later be combined.
  
  enrollment_seventeen_fall <- read_excel("fall_course_enrollment_analysis/fall_2017.xlsx", skip = 3) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`Course Title`)) %>% 
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`Course ID`, 
           
           #Title replaces "Course Title" for a simpler name
           
           Title = `Course Title`, 
           
           #Department is used to refer to Course Department
           
           Department = `Course Department`, 
           
           #Undergraduates is then used for the undergraduate count
           
           Undergraduates = `UGrad`,
           
           Graduates = `Grad`)
  
  enrollment_sixteen_fall <- read_excel("fall_course_enrollment_analysis/fall_2016.xlsx", skip = 3) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`Course Title`)) %>% 
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`Course ID`, 
           
           #Setting the title of Course titles to be "Title" for simplicity
           
           Title = `Course Title`, 
           
           #Department undergoes a similar cleaning
           
           Department = `Course Department`, 
           
           #The same for enrolled
           
           Undergraduates = `UGrad`,
           
           Graduates = `Grad`)
  
  #Next, one is interested in compiling 2015 fall data
  
  enrollment_fifteen_fall <- read_excel("fall_course_enrollment_analysis/fall_2015.xlsx", skip = 0) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`COURSE ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(HCOL >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID = `COURSE ID`, 
           
           #The course title is standardized first.
           
           Title = `COURSE`, 
           
           #Next the department label is standardized.
           
           Department = `DEPARTMENT`, 
           
           #The number of Harvard college students or undergraduates is labeled according to the standard.
           
           Undergraduates = `HCOL`,
           
           #The graduate count is also labeled.
           
           Graduates = `GSAS`)
  
  #Next, one combines all the data into one large dataset so that they can all be accessed in the same place.
  
  enrollment_fall <- bind_rows("2018" = enrollment_eighteen_fall, 
                               
                               #And for 2017
                               
                               "2017" = enrollment_seventeen_fall, 
                               
                               #And for 2016
                               
                               "2016" = enrollment_sixteen_fall, 
                               
                               #And for 2015
                               
                               "2015" = enrollment_fifteen_fall, 
                               
                               #Then the id is set to year so that one retains which year each row refers to
                               
                               .id = "Year") %>% filter(Undergraduates > Graduates)
  
  
  enrollment_sixteen_spring <- read_excel("fall_course_enrollment_analysis/spring_2016.xlsx", skip = 0) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`COURSE ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(HCOL >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`COURSE ID`, 
           
           #The course titles are labeled.
           
           Title = `COURSE`, 
           
           #The departments are labeled as well.
           
           Department = `DEPARTMENT`, 
           
           #Next, the undergraduates counts are labeled
           
           Undergraduates = `HCOL`,
           
           #The graduate counts are labeled as well.
           
           Graduates = `GSAS`)
  
  
  enrollment_seventeen_spring <- read_excel("fall_course_enrollment_analysis/spring_2017.xlsx", skip = 3) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`Course ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`Course ID`, 
           
           #Setting the title of Course titles to be "Title" for simplicity
           
           Title = `Course Title`, 
           
           #Department undergoes a similar cleaning
           
           Department = `Course Department`, 
           
           #The same for enrolled
           
           Undergraduates = `UGrad`,
           
           #The graduates are also labeled.
           
           Graduates = `Grad`)
  
  
  enrollment_eighteen_spring <- read_excel("fall_course_enrollment_analysis/spring_2018.xlsx", skip = 3) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`Course ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`Course ID`, 
           
           #Setting the title of Course titles to be "Title" for simplicity
           
           Title = `Course Title`, 
           
           #Department undergoes a similar cleaning
           
           Department = `Course Department`, 
           
           #The same for enrolled
           
           Undergraduates = `UGrad`,
           
           #The graduates are also labeled.
           
           Graduates = `Grad`)
  
  enrollment_nineteen_spring <- read_excel("fall_course_enrollment_analysis/spring_2019.xlsx", skip = 3) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`Course ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`Course ID`, 
           
           #Setting the title of Course titles to be "Title" for simplicity
           
           Title = `Course Title`, 
           
           #Department undergoes a similar cleaning
           
           Department = `Course Department`, 
           
           #The same for enrolled
           
           Undergraduates = `UGrad`,
           
           #The graduates are also labeled.
           
           Graduates = `Grad`)
  
  
  enrollment_spring <- bind_rows("2018" = enrollment_eighteen_spring, 
                                 
                                 #And for 2017
                                 
                                 "2017" = enrollment_seventeen_spring, 
                                 
                                 #And for 2016
                                 
                                 "2016" = enrollment_sixteen_spring, 
                                 
                                 #And for 2019
                                 
                                 "2019" = enrollment_nineteen_spring, 
                                 
                                 #Then the id is set to year so that one retains which year each row refers to
                                 
                                 .id = "Year") %>% filter(Graduates < Undergraduates)
  
  #This function writes the newly created data to an rds file for quick reading.
  
  write_rds(enrollment_fall, "fall_enrollment.rds")
  
  #This function writes the newly created data to an rds file for quick reading.
  
  write_rds(enrollment_spring, "spring_enrollment.rds")
  