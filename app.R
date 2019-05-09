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
         tabPanel("Enrollment Time Graph", sidebarPanel(selectInput("timegraph_departments", "Department", choices = c("Economics",
                                                                                                                       "Government",
                                                                                                                       "Computer Science",
                                                                                                                       "Statistics", 
                                                                                                                       "Mathematics",
                                                                                                                       "Applied Mathematics",
                                                                                                                       "Physics",
                                                                                                                       "Chemistry", 
                                                                                                                       "English",
                                                                                                                       "History", 
                                                                                                                       "History & Literature", 
                                                                                                                       "Comparative Literature",
                                                                                                                       "Engineering Sciences",
                                                                                                                       "African & African Amer Studies",
                                                                                                                       "Anthropology",
                                                                                                                       "Applied Computation",
                                                                                                                       "Applied Physics",
                                                                                                                       "Astronomy",
                                                                                                                       "Biomedical Engineering", 
                                                                                                                       "Chemical & Physical Biology",
                                                                                                                       "Chemistry & Chemical Biology",
                                                                                                                       "East Asian Langs & Civ",
                                                                                                                       "Expository Writing",
                                                                                                                       "Freshman Seminar",
                                                                                                                       "General Education",
                                                                                                                       "Germanic Languages & Lit",
                                                                                                                       "Global Health & Health Policy",
                                                                                                                       "History of Art and Architecture", 
                                                                                                                       "History of Science",
                                                                                                                       "Human Evolutionary Biolgoy",
                                                                                                                       "Linguistics",
                                                                                                                       "Molecular & Cellular Biology",
                                                                                                                       "Music", 
                                                                                                                       "Near Eastern Languages & Civ",
                                                                                                                       "Social Studies", 
                                                                                                                       "Sociology", 
                                                                                                                       "Romance Languages & Literatures",
                                                                                                                       "South Asian Studies", 
                                                                                                                       "Stem Cell & Regenerative Biol", 
                                                                                                                       "Theater, Dance & Media",
                                                                                                                       "Women, Gender & Sexuality"
         ), 
         selected = c("Economics",
                      "Government",
                      "Computer Science",
                      "Statistics"), 
         multiple = TRUE), width = 2), 
         mainPanel(width = 10, htmlOutput("timegraph_header"), plotlyOutput("spring_timegraph"), htmlOutput("space4"), plotlyOutput("fall_timegraph"))), 
         
         tabPanel("Largest Classes", 
                  sidebarPanel(selectInput("year",
                       "Select a Year",
                       c("2018-2019", 
                         "2017-2018", 
                         "2016-2017", 
                         "2015-2016"),
                       selected = "2018-2019"),
                       width = 2
           ), mainPanel(width = 10, htmlOutput("largest_header"), plotlyOutput("fall_largest"), htmlOutput("space"), plotlyOutput("spring_largest"))),
         
         #Next, one can observe distributions of how enrollment sizes across courses.
         
         tabPanel("Distributions", sidebarPanel(selectInput("distribution_year",
                                                            "Select a Year",
                                                            c("2018-2019", 
                                                              "2017-2018", 
                                                              "2016-2017", 
                                                              "2015-2016"),
                                                            selected = "2018-2019"), selectInput("distribution_departments", "Departments", choices = c("Economics",
                                                                                                                                            "Government",
                                                                                                                                            "Computer Science",
                                                                                                                                            "Statistics",
                                                                                                                                            "Comparative Literature",
                                                                                                                                            "Engineering Sciences",
                                                                                                                                            "African & African Amer Studies",
                                                                                                                                            "Anthropology",
                                                                                                                                            "Applied Computation",
                                                                                                                                            "Applied Mathematics",
                                                                                                                                            "Applied Physics",
                                                                                                                                            "Astronomy",
                                                                                                                                            "Biomedical Engineering", 
                                                                                                                                            "Chemical & Physical Biology",
                                                                                                                                            "Chemistry & Chemical Biology",
                                                                                                                                            "East Asian Langs & Civ",
                                                                                                                                            "English",
                                                                                                                                            "Expository Writing",
                                                                                                                                            "Freshman Seminar",
                                                                                                                                            "General Education",
                                                                                                                                            "Germanic Languages & Lit",
                                                                                                                                            "Global Health & Health Policy",
                                                                                                                                            "History", 
                                                                                                                                            "History & Literature",
                                                                                                                                            "History of Art and Architecture", 
                                                                                                                                            "History of Science",
                                                                                                                                            "Human Evolutionary Biolgoy",
                                                                                                                                            "Linguistics",
                                                                                                                                            "Mathematics",
                                                                                                                                            "Molecular & Cellular Biology",
                                                                                                                                            "Music", 
                                                                                                                                            "Near Eastern Languages & Civ",
                                                                                                                                            "Physics",
                                                                                                                                            "Social Studies", 
                                                                                                                                            "Sociology", 
                                                                                                                                            "Romance Languages & Literatures",
                                                                                                                                            "South Asian Studies", 
                                                                                                                                            "Stem Cell & Regenerative Biol", 
                                                                                                                                            "Theater, Dance & Media",
                                                                                                                                            "Women, Gender & Sexuality"), 
                                                                                                 multiple = TRUE, 
                                                selected = c("Economics",
                                                             "Government",
                                                             "Computer Science",
                                                             "Statistics")), width = 2),
                                                          
                                                mainPanel(width = 10, htmlOutput("distributions_header"), plotlyOutput("fall_distributions"), htmlOutput("space2"), plotlyOutput("spring_distributions"))),
         
         #The next panel will cover comparisons across departments.
         
         tabPanel("Graduate vs. Undergraduate Enrollment", sidebarPanel(selectInput("graduate_year",
                                                                                    "Select a Year",
                                                                                    c("2018-2019", 
                                                                                      "2017-2018", 
                                                                                      "2016-2017", 
                                                                                      "2015-2016"),
                                                                                    selected = "2018-2019"
         ), selectInput("departments", "Departments", choices = c("Economics",
                                                                 "Government",
                                                                 "Computer Science",
                                                                 "Statistics", 
                                                                 "Mathematics",
                                                                 "Applied Mathematics",
                                                                 "Physics",
                                                                 "Chemistry", 
                                                                 "English",
                                                                 "History", 
                                                                 "History & Literature", 
                                                                 "Comparative Literature",
                                                                 "Engineering Sciences",
                                                                 "African & African Amer Studies",
                                                                 "Anthropology",
                                                                 "Applied Computation",
                                                                 "Applied Physics",
                                                                 "Astronomy",
                                                                 "Biomedical Engineering", 
                                                                 "Chemical & Physical Biology",
                                                                 "Chemistry & Chemical Biology",
                                                                 "East Asian Langs & Civ",
                                                                 "Expository Writing",
                                                                 "Freshman Seminar",
                                                                 "General Education",
                                                                 "Germanic Languages & Lit",
                                                                 "Global Health & Health Policy",
                                                                 "History of Art and Architecture", 
                                                                 "History of Science",
                                                                 "Human Evolutionary Biolgoy",
                                                                 "Linguistics",
                                                                 "Molecular & Cellular Biology",
                                                                 "Music", 
                                                                 "Near Eastern Languages & Civ",
                                                                 "Social Studies", 
                                                                 "Sociology", 
                                                                 "Romance Languages & Literatures",
                                                                 "South Asian Studies", 
                                                                 "Stem Cell & Regenerative Biol", 
                                                                 "Theater, Dance & Media",
                                                                 "Women, Gender & Sexuality"
                                                                 ), 
                                                      selected = c("Economics",
                                                                   "Government",
                                                                   "Computer Science",
                                                                   "Statistics"), 
                                                      multiple = TRUE), width = 2), 
         mainPanel(width = 10, htmlOutput("graduates_header"), plotlyOutput("fall_graduates"), htmlOutput("space3"), plotlyOutput("spring_graduates"))),
         
         
         
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
                          
                          .id = "Year") %>% filter(Undergraduates > Graduates)
  
  
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
                               
                               .id = "Year") %>% filter(Graduates < Undergraduates)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #One begins with the first plot.
  
  output$fall_largest <- renderPlotly({ 
    
    #One begins with the original dataset
    
    fall_largest <- enrollment_fall %>% 
      
    #Then one filters the year by an inputted value that the user will be able to specify.  
      
    filter(Year == fall_function(input$year)) %>% 
      
    #Next, the data is arranged by the largest enrolled value.
        
    arrange(desc(Undergraduates)) %>% 
    
    #Then the top 8 courses are selected.    
      
    slice(1:10) %>% 
    
    #Next, one begins the plot with courses on the x axis and the number of enrolled students on the y axis    
      
    ggplot(aes(x = reorder(Title, -Undergraduates), y = Undergraduates, fill = Department)) + 
      
      #A column chart is necessary in this case because one is dealing with values an not frequencies.
      
      geom_col() +
      
      #The title is named so that the user can understand what the chart is about.
      
      labs(title = "Largest Fall Classes in Selected Year", caption = "Source: Harvard Registrar") +
      
      #Next, the axis is labeled clearly.
      
      xlab("Course Title") +
      
      #The y axis follows.
      
      ylab("Number of Enrolled Undergraduates") + 
      
      #The minimal theme is applied for aesthetic purposes
      
      theme_minimal() 
    
    ggplotly(fall_largest) %>% config(displayModeBar = FALSE)
  })
  
  output$spring_largest <- renderPlotly({ 
    
    #One begins with the original dataset
    
    spring_largest <- enrollment_spring %>% 
      
      #Then one filters the year by an inputted value that the user will be able to specify.  
      
      filter(Year == spring_function(input$year)) %>% 
      
      #Next, the data is arranged by the largest enrolled value.
      
      arrange(desc(Undergraduates)) %>% 
      
      #Then the top 8 courses are selected.    
      
      slice(1:10) %>% 
      
      #Next, one begins the plot with courses on the x axis and the number of enrolled students on the y axis    
      
      ggplot(aes(x = reorder(Title, -Undergraduates), y = Undergraduates, fill = Department)) + 
      
      #A column chart is necessary in this case because one is dealing with values an not frequencies.
      
      geom_col() +
      
      #The title is named so that the user can understand what the chart is about.
      
      labs(title = "Largest Spring Courses in Selected Year", caption = "Source: Harvard Registrar") +
      
      #Next, the axis is labeled clearly.
      
      xlab("Course Title") +
      
      #The y axis follows.
      
      ylab("Number of Enrolled Undergraduates") + 
      
      #The minimal theme is applied for aesthetic purposes
      
      theme_minimal() 
    
    ggplotly(spring_largest) %>% config(displayModeBar = FALSE)
    
  })
      
    output$fall_distributions <- renderPlotly({ 
      
        #Starting with the combined enrollment data set
    
        fall_distributions <- enrollment_fall %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$distribution_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$distribution_year)) %>%
        
        #Next, the plot begins.
        
        ggplot(aes(x = Department, 
                   
                   #The y axis marks the numbered of enrolled undergraduates
                   
                   y = Undergraduates, 
                   
                   #The color is by department
                   
                   fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_violin() + 
        
        #Then, the labels and titles are set.
        
        labs(title = "Distribution of Fall Course Sizes by Department in Selected Year", 
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
        
        geom_jitter(width = .3) + 
        
        theme_minimal()
       
        ggplotly(fall_distributions) %>% config(displayModeBar = FALSE)
    })
  
    
    output$spring_distributions <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      spring_distributions <- enrollment_spring %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$distribution_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == spring_function(input$distribution_year)) %>%
        
        #Next, the plot begins.
        
        ggplot(aes(x = Department, 
                   
                   #The y axis marks the numbered of enrolled undergraduates
                   
                   y = Undergraduates, 
                   
                   #The color is by department
                   
                   fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_violin() + 
        
        #Then, the labels and titles are set.
        
        labs(title = "Distribution of Spring Course Sizes by Department in Selected Year", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Course Undergraduate Enrollment") +
        
        #Next, the x-axis is labeled.
        
        xlab(NULL) +
        
        #Next, scaling the y-axis is necessary so that there is not a huge right tail on each of the datasets to account for very large classes.
        
        scale_y_log10() + 
        
        #Next, the coordinates are flipped in order to make reading easier for the viewer
        
        coord_flip() + 
        
        #Next, the jitter is overlayed to demonstrate the density somehow
        
        geom_jitter(width = .3, aes(text = sprintf(Title))) + 
        
        theme_minimal()
      
      ggplotly(spring_distributions) %>% 
        config(displayModeBar = FALSE)
      
    })
    
    output$fall_graduates <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      fall_graduates <- enrollment_fall %>%
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$graduate_year)) %>%
        
        #Next, one can begin the plot.
      
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_violin() +
        
        labs(title = "Distribution of Undergraduate Spring Course Graduate Enrollments by Department in Selected Year", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Course Undergraduate Enrollment") +
        
        geom_jitter(width = .3, aes(text = sprintf(Title))) +
        
        scale_y_log10() +
        
        coord_flip() +
        
        theme_minimal()
        
      ggplotly(fall_graduates) %>% 
        
        config(displayModeBar = FALSE)
      
    
    })
    
    
    output$spring_graduates <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      spring_graduates <- enrollment_spring %>%
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$graduate_year)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_violin() +
        
        geom_jitter(width = .3, aes(text = sprintf(Title))) +
        
        labs(title = "Distribution of Undergraduate Spring Course Graduate Enrollments by Department in Selected Year", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Course Undergraduate Enrollment") +
        
        scale_y_log10() +
        
        coord_flip() +
        
        theme_minimal()
      
      ggplotly(spring_graduates) %>% config(displayModeBar = FALSE)
      
      
    })
    

   
    output$fall_timegraph <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      fall_timegraph <- enrollment_fall %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$timegraph_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        group_by(Year, Department) %>%
        
        summarize(Total = sum(Undergraduates)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Year, y = Total, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_line(aes(group = 1, color = Department)) +
        
        labs(title = "Total Enrollment in Fall Courses by Department Over Time", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        geom_point(aes(color = Department)) +
        
        theme_minimal() + 
        
        ylab("Total Enrollments in Department Courses")
      
      ggplotly(fall_timegraph) %>% 
        
        config(displayModeBar = FALSE)
      
      
    })
    
    output$spring_timegraph <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      spring_timegraph <- enrollment_spring %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$timegraph_departments) %>%
        
        #Grouping by year and Department allows one to get specific data for each department over time
        
        group_by(Year, Department) %>%
        
        #Next, the number of undergraduate enrollments are summed
        
        summarize(Total = sum(Undergraduates)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Year, y = Total, fill = Department)) +
        
        #Choosing a boxplot is helpful for showing data distributions.
        
        geom_line(aes(group = 1, color = Department)) +
        
        geom_point(aes(color = Department)) +
        
        labs(title = "Total Enrollment in Spring Courses by Department Over Time", 
             caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        theme_minimal() + 
        
        ylab("Total Enrollments in Department Courses")
      
      ggplotly(spring_timegraph) %>% 
        
        config(displayModeBar = FALSE)
      
      
    })
    
    
    
    output$space <- renderText ({
      "<br><br><br>"
      
    })
    
    output$space2 <- renderText ({
      "<br><br><br>"
      
    })
    
    output$space3 <- renderText ({
      "<br><br><br>"
      
    })
    
    output$space4 <- renderText ({
      "<br><br><br>"
      
    })
    
    output$timegraph_header <- renderText ({
      "<h2 align = center> Course Enrollment Over Time</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the total number of undergraduate enrollments across all courses in a department 
      for each school year.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    output$largest_header <- renderText ({
      "<h2 align = center> Largest Classes</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the largest classes
      for each school year.  To change the year, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    output$distributions_header <- renderText ({
      "<h2 align = center> Class Size Distributions</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the distribution of class sizes within selected departments
      for each school year.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.  
      You can also use the drop-down toggle to change the year</p>"
      
    })
    
    output$graduates_header <- renderText ({
      "<h2 align = center> Graduate Students in Undergraduate Courses</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the distribution of the number of graduate students in undergraduate courses for each school year.  An undergraduate course is defined as a course with at least 3 undergraduate students and that contains a majority of undergraduate students.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    output$about <- renderText ({
      
      "<h1 align = center> Welcome to the Enrollment Project!</h1>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>Ever wonder what the major trends are in course enrollment?  
      Using a public dataset on the Harvard Registrar, this Shiny app lets users visualize various presentations of the data from different angles.  
      Each tab demonstrates a different segment of the data and there is a customizable interactive form on the top left of every page for the user to filter the data how they see fit.  
      Feel free to browse through!</p>
      <h4 align = center>Contact and Info</h4>
      <p align = center>Email: chrisonesti@college.harvard.edu
      <p align = center>GitHub Link: <a href = https://github.com/conesti/fall-course-enrollment-analysis>https://github.com/conesti/fall-course-enrollment-analysis</a>"
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

