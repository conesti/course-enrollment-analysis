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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fall Course Enrollment Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("year",
                     "Select a Year",
                     c("2018", 
                       "2017", 
                       "2016", 
                       "2015"),
                     selected = "2018"
         )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        #In according with instructions and to add organization to the app, one has a panel with various tabs.
        
        tabsetPanel(type = "tabs",
        
         #The first panel shows the largest classes.                        
                    
         tabPanel("Largest Classes", "This page is dedicated to showing the largest courses in the fall.  Use the selector on the left side of the screen to select a year and the chart will update showing the eight largest courses that semester.  The bars are also colored by department so one can easily see if there is a department with many or multiple courses in the top eight.  Finding large courses can be useful for identifying possible prerequisites for certain fields of study.", plotOutput("plot1")),
         
         #Next, one can observe distributions of how enrollment sizes across courses.
         
         tabPanel("Distributions", "This tab shows various boxplots that demonstrate the distribution of course enrollment sizes by departments.  Right now, only a handful of the departments are shown, but in a future iteration of this page, one will be able to add more to the chart. ", plotOutput("plot2")),
         
         #The next panel will cover comparisons across departments.
         
         tabPanel("Graduate vs. Undergraduate Enrollment", "This tab will compare graduate and undergraduate enrollment rate across courses grouped by department."),
         
         #Then, there is a panel dedicated to explaining the project and providing 
         
         tabPanel("About", "This project is meant to compare enrollment sizes among different courses in different departments.  
                  Feel free to flip through the tabs to see the various representations of the data.  Some of the tabs are yet to come.  Feel free to check out the github repository at: https://github.com/conesti/fall-course-enrollment-analysis.  I can also be reached at chrisonesti@gmail.com.")
         
         ) 
         
         
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #One begins withi the first data set for the 2018 data
  
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
           
           Undergraduates = `UGrad`)
  
  
  #The next step is to fulfill this same code execution with the other years so that the data can later be combined.
  
  enrollment_seventeen_fall <- read_excel("fall_course_enrollment_analysis/fall_2017.xlsx", 
                                     skip = 3) %>% 
    
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
          
           Undergraduates = `UGrad`)
  
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
           
           Undergraduates = `UGrad`)
  
  enrollment_fifteen_fall <- read_excel("fall_course_enrollment_analysis/fall_2015.xlsx", skip = 0) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`COURSE ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(HCOL >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`COURSE ID`, Title = `COURSE`, Department = `DEPARTMENT`, Undergraduates = `HCOL`)
  
  #Next, one combines all the data into one large dataset so that they can all be accessed in the same place.
  
  enrollment_fall <- bind_rows("2018" = enrollment_eighteen_fall, 
                          
                          #And for 2017
                          
                          "2017" = enrollment_seventeen_fall, 
                          
                          #And for 2016
                          
                          "2016" = enrollment_sixteen_fall, 
                          
                          #And for 2015
                          
                          "2015" = enrollment_fifteen_fall, 
                          
                          #Then the id is set to year so that one retains which year each row refers to
                          
                          .id = "Year")
  
  #One begins with the first plot.
 
  output$plot1 <- renderPlot({ 
    
    #One begins with the original dataset
    
    enrollment %>% 
      
    #Then one filters the year by an inputted value that the user will be able to specify.  
      
    filter(Year == input$year) %>% 
      
    #Next, the data is arranged by the largest enrolled value.
        
    arrange(desc(Undergraduates)) %>% 
    
    #Then the top 8 courses are selected.    
      
    slice(1:8) %>% 
    
    #Next, one begins the plot with courses on the x axis and the number of enrolled students on the y axis    
      
    ggplot(aes(x = reorder(Title, -Undergraduates), y = Undergraduates, fill = Department)) + 
      
      #A column chart is necessary in this case because one is dealing with values an not frequencies.
      
      geom_col() +
      
      #The title is named so that the user can understand what the chart is about.
      
      labs(title = "Total Enrollment by Class in Selected Year", caption = "Source: Harvard Registrar") +
      
      #Next, the axis is labeled clearly.
      
      xlab("Course Title") +
      
      #The y axis follows.
      
      ylab("Number of Enrolled Undergraduates") + 
      
      #The minimal theme is applied for aesthetic purposes
      
      theme_minimal() 
    
  })
      
    output$plot2 <- renderPlot({ 
      
        #Starting with the combined enrollment data set
    
        enrollment %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% c("Government", "Economics", "Computer Science", "Statistics", "Physics", "Mathematics", "STAT", "SEAS", "MATH", "PHYS", "GOVM", "ECON")) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == input$year) %>%
        
        #Next, the plot begins.
        
        ggplot(aes(x = Department, 
                   
                   #The y axis marks the numbered of enrolled undergraduates
                   
                   y = Undergraduates, 
                   
                   #The color is by department
                   
                   fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_violin() + 
        
        #Then, the labels and titles are set.
        
        labs(title = "Departments Enrollment Distributions Fluctuate Slightly Over Years", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        #Next, the x-axis is labeled.
        
        xlab(NULL) +
        
        #Next, scaling the y-axis is necessary so that there is not a huge right tail on each of the datasets to account for very large classes.
        
        scale_y_log10() + 
        
        #Next, the coordinates are flipped in order to make reading easier for the viewer
        
        coord_flip() + 
        
        #Next, the jitter is overlayed to demonstrate the density somehow
        
        geom_jitter()
       
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

