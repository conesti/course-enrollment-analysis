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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fall Course Enrollment Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("year",
                     "Select a Year",
                     c("2018", "2017", "2016", "2015"),
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
  
  enrollment_eighteen <- read_excel("fall_2018.xlsx", skip = 2) %>% 
    
    #Next, one filters the courses where the title is NA
    
    filter(!is.na(`Course Title`)) %>% 
    
    #Next, one is only intersted in courses with more than 3 people in order to not include tutorials and the like.
    
    filter(UGrad >= 3) %>% 
    
    #Next, one is only interstested in certain columns, so they are not selected here.
    
    select(ID =`Course ID`, 
           Title = `Course Title`, 
           Department = `Course Department`, 
           Enrolled = `UGrad`)
  
  #The next step is to fulfill this same code execution with the other years so that the data can later be combined.
  
  enrollment_seventeen <- read_excel("fall_2017.xlsx", skip = 3) %>% 
    filter(!is.na(`Course Title`)) %>% 
    filter(UGrad >= 3) %>% 
    select(ID =`Course ID`, Title = `Course Title`, Department = `Course Department`, Enrolled = `UGrad`)
  
  enrollment_sixteen <- read_excel("fall_2016.xlsx", skip = 3) %>% 
    filter(!is.na(`Course Title`)) %>% 
    filter(UGrad >= 3) %>% 
    select(ID =`Course ID`, Title = `Course Title`, Department = `Course Department`, Enrolled = `UGrad`)
  
  enrollment_fifteen <- read_excel("fall_2015.xlsx", skip = 0) %>% 
    filter(!is.na(`COURSE ID`)) %>% 
    filter(HCOL >= 3) %>% 
    select(ID =`COURSE ID`, Title = `COURSE`, Department = `DEPARTMENT`, Enrolled = `HCOL`)
  
  #Next, one combines all the data into one large dataset so that they can all be accessed in the same place.
  
  enrollment <- bind_rows("2018" = enrollment_eighteen, 
                          
                          #And for 2017
                          
                          "2017" = enrollment_seventeen, 
                          
                          #And for 2016
                          
                          "2016" = enrollment_sixteen, 
                          
                          #And for 2015
                          
                          "2015" = enrollment_fifteen, 
                          
                          #Then the id is set to year so that one retains which year each row refers to
                          
                          .id = "Year")
  
  #One begins with the first plot.
 
  output$plot1 <- renderPlot({ 
    
    #One begins with the original dataset
    
    enrollment %>% 
      
    #Then one filters the year by an inputted value that the user will be able to specify.  
      
    filter(Year == input$year) %>% 
      
    #Next, the data is arranged by the largest enrolled value.
        
    arrange(desc(Enrolled)) %>% 
    
    #Then the top 8 courses are selected.    
      
    slice(1:8) %>% 
    
    #Next, one begins the plot with courses on the x axis and the number of enrolled students on the y axis    
      
    ggplot(aes(x = reorder(Title, -Enrolled), y = Enrolled, fill = Department)) + 
      
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
    
        enrollment %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% c("Government", "Economics", "Computer Science", "Statistics", "Physics", "Mathematics", "STAT", "SEAS", "MATH", "PHYS", "GOVM", "ECON")) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == input$year) %>%
        
        #Next, the plot begins.
        
        ggplot(aes(x = Department, y = Enrolled, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_boxplot() + 
        
        #Next, the coordinates ar flipped for a horizontal view.
        
        coord_flip() +
        
        #Then, the labels and titles are set.
        
        labs(title = "Departments Enrollment Distributions Fluctuate Slightly Over Years", caption = "Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        #Next, the x-axis is labeled.
        
        xlab(NULL) +
        
        #Next, scaling the y-axis is necessary so that there is not a huge right tail on each of the datasets to account for very large classes.
        
        scale_y_log10() +
        
        #The jitter allows the viewer to visualize the density a bit better.
        
        geom_jitter(alpha = .4, width = .2)
    
    
    
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

