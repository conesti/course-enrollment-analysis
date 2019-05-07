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

# Define UI for application that draws a histogram
ui <- navbarPage("Course Enrollment Analysis",
   
  
        
         #The first panel shows the largest classes.                        
                    
         tabPanel("Largest Classes", 
                  sidebarPanel(selectInput("year",
                       "Select a Year",
                       c("2018-2019", 
                         "2017-2018", 
                         "2016-2017", 
                         "2015-2016"),
                       selected = "2018-2019")
           ), mainPanel(plotlyOutput("plot1"), plotlyOutput("plot1.1"))),
         
         #Next, one can observe distributions of how enrollment sizes across courses.
         
         tabPanel("Distributions", sidebarPanel(selectInput("year",
                                                            "Select a Year",
                                                            c("2018-2019", 
                                                              "2017-2018", 
                                                              "2016-2017", 
                                                              "2015-2016"),
                                                            selected = "2018-2019")), 
                                                mainPanel(plotlyOutput("plot2"), plotlyOutput("plot2.1"))),
         
         #The next panel will cover comparisons across departments.
         
         tabPanel("Graduate vs. Undergraduate Enrollment", sidebarPanel(selectInput("year",
                                                                                    "Select a Year",
                                                                                    c("2018-2019", 
                                                                                      "2017-2018", 
                                                                                      "2016-2017", 
                                                                                      "2015-2016"),
                                                                                    selected = "2018-2019"
         ), selectInput("departments", "Departments", choices = c("Economics",
                                                                 "Government",
                                                                 "Computer Science",
                                                                 "Statistics"), 
                                                      selected = c("Economics",
                                                                   "Government",
                                                                   "Computer Science",
                                                                   "Statistics"), 
                                                      multiple = TRUE)), 
         mainPanel(plotlyOutput("plot3"), plotlyOutput("plot3.1"))),
         
         #Then, there is a panel dedicated to explaining the project and providing 
         
         tabPanel("About", htmlOutput("about"))
         
         ) 
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  spring_function <- function(x) {
    if (x == "2015-2016") {
      "2016"
    }
    else if (x == "2016-2017") {
      "2017"
    }
    else if (x == "2017-2018") {
      "2018"
    }
    else {
      "2019"
    }
  }
  
  fall_function <- function(x) {
    if (x == "2015-2016") {
      "2015"
    }
    else if (x == "2016-2017") {
      "2016"
    }
    else if (x == "2017-2018") {
      "2017"
    }
    else {
      "2018"
    }
  }
  
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
           
           Undergraduates = `UGrad`,
           
           Graduates = `Grad`)
  
  
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
    
    select(ID = `COURSE ID`, Title = `COURSE`, Department = `DEPARTMENT`, Undergraduates = `HCOL`,
           
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
                          
                          .id = "Year")
  
  
  enrollment_sixteen_spring <- read_excel("fall_course_enrollment_analysis/spring_2016.xlsx", skip = 0) %>% 
    
    #Then one is only interested in course entries, not totals.
    
    filter(!is.na(`COURSE ID`)) %>%
    
    #Next, one is only interested in courses with a significant number of undergraduates.
    
    filter(HCOL >= 3) %>% 
    
    #Next, one is only interested in this part of the data.
    
    select(ID =`COURSE ID`, Title = `COURSE`, Department = `DEPARTMENT`, Undergraduates = `HCOL`,
           
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
           
           Graduates = `Grad`)
  
  
  enrollment_spring <- bind_rows("2018" = enrollment_eighteen_spring, 
                               
                               #And for 2017
                               
                               "2017" = enrollment_seventeen_spring, 
                               
                               #And for 2016
                               
                               "2016" = enrollment_sixteen_spring, 
                               
                               #And for 2019
                               
                               "2019" = enrollment_nineteen_spring, 
                               
                               #Then the id is set to year so that one retains which year each row refers to
                               
                               .id = "Year")
  
  
  #One begins with the first plot.
  
  output$plot1 <- renderPlotly({ 
    
    #One begins with the original dataset
    
    largest <- enrollment_fall %>% 
      
    #Then one filters the year by an inputted value that the user will be able to specify.  
      
    filter(Year == fall_function(input$year)) %>% 
      
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
    
    ggplotly(largest) %>% config(displayModeBar = FALSE)
  })
  
  output$plot1.1 <- renderPlotly({ 
    
    #One begins with the original dataset
    
    largest2 <- enrollment_spring %>% 
      
      #Then one filters the year by an inputted value that the user will be able to specify.  
      
      filter(Year == spring_function(input$year)) %>% 
      
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
    
    ggplotly(largest2) %>% config(displayModeBar = FALSE)
    
  })
      
    output$plot2 <- renderPlotly({ 
      
        #Starting with the combined enrollment data set
    
        distribution <- enrollment_fall %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% c("Government", "Economics", "Computer Science", "Statistics", "Physics", "Mathematics")) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$year)) %>%
        
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
        
        geom_jitter() + 
        
        theme_minimal()
       
        ggplotly(distribution) %>% config(displayModeBar = FALSE)
    })
  
    
    output$plot2.1 <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      distribution2 <- enrollment_spring %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% c("Government", "Economics", "Computer Science", "Statistics", "Physics", "Mathematics")) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == spring_function(input$year)) %>%
        
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
        
        geom_jitter() + 
        
        theme_minimal()
      
      ggplotly(distribution2) %>% 
        config(displayModeBar = FALSE)
      
    })
    
    output$plot3 <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      fall_graduates <- enrollment_fall %>%
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$year)) %>%
        
        #Next, one can begin the plot.
      
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_boxplot() +
        
        scale_y_log10() +
        
        coord_flip() +
        
        theme_minimal()
        
      ggplotly(fall_graduates) %>% 
        
        config(displayModeBar = FALSE)
      
    
    })
    
    
    output$plot3.1 <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      spring_graduates <- enrollment_spring %>%
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$year)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_boxplot() +
        
        scale_y_log10() +
        
        coord_flip() +
        
        theme_minimal()
      
      ggplotly(spring_graduates) %>% config(displayModeBar = FALSE)
      
      
    })
    
    output$about <- renderText ({
      "<style>
h1 {color:red;}
p {color:blue;}
</style><h3 syle = colo>This project is meant to compare enrollment sizes among different courses in different departments.</h3>
      <p>Feel free to flip through the tabs to see the various representations of the data.  Some of the tabs are yet to come.  Feel free to check out the github repository at: https://github.com/conesti/fall-course-enrollment-analysis.</p> 
      <p>I can also be reached at chrisonesti@gmail.com.</p>"
      
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

