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
   
  
        
        #The first panel shows total enrollment across all courses in each department over time.
        
        tabPanel("Enrollment Time Graph", 
                 
                 #The sidebar panel is a place to customize the data and filter it.
                 
                 sidebarPanel(
                   
                   #The departments input is necessary to only show data from selected departments.
                   
                   selectInput("timegraph_departments", "Department", choices = c("Economics",
                                                                                                                       "Government",
                                                                                                                       "Computer Science",
                                                                                                                       "Statistics",
                                                                                                                       "African & African Amer Studies",
                                                                                                                       "Anthropology",
                                                                                                                       "Applied Mathematics",
                                                                                                                       "Applied Physics",
                                                                                                                       "Astronomy",
                                                                                                                       "Biomedical Engineering",
                                                                                                                       "Chemical & Physical Biology",
                                                                                                                       "Chemistry & Chemical Biology",
                                                                                                                       "Comparative Literature",
                                                                                                                       "East Asian Langs & Civ",
                                                                                                                       "English",
                                                                                                                       "Engineering Sciences",
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
                                                                                                                       "Women, Gender & Sexuality"), selected = c("Economics", "Government", "Computer Science", "Statistics"),  multiple = TRUE), width = 2), 
                 
                 #The main panel shows the graphs or rendered html text that corresponds to the opened tab.
                 
                 mainPanel(width = 10, htmlOutput("timegraph_header"), plotlyOutput("fall_timegraph"), htmlOutput("space4"), plotlyOutput("spring_timegraph"))), 
        
        #The next panel shows the largest classes in each year.
        
        tabPanel("Largest Classes", 
                 
                 #The sidebar panel is a place to customize the data and filter it.
                 
                  sidebarPanel(
                    
                    #Next, the input for the year allows the user to select which year the chart corresponds to.
                    
                    selectInput("year", "Select a Year", c("2018-2019", "2017-2018", "2016-2017", "2015-2016"), selected = "2018-2019"), width = 2), 
                 
                  #The main panel shows the graphs or rendered html text that corresponds to the opened tab.
                 
                  mainPanel(width = 10, htmlOutput("largest_header"), plotlyOutput("fall_largest"), htmlOutput("space"), plotlyOutput("spring_largest"))),
           
        #Next, one can observe distributions of how enrollment sizes across courses.
           
        tabPanel("Distributions", 
                 
                 #The sidebar panel is a place to customize the data and filter it.
                 
                  sidebarPanel(
                    
                    #Next, the input for the year allows the user to select which year the chart corresponds to.
                    
                    selectInput("distribution_year", "Select a Year", c("2018-2019", "2017-2018", "2016-2017", "2015-2016"), selected = "2018-2019"), 
                    
                    #The departments input is necessary to only show data from selected departments.
                    
                    selectInput("distribution_departments", "Departments", choices = c("Economics",
                                                                                                                                                          "Government",
                                                                                                                                                          "Computer Science",
                                                                                                                                                          "Statistics",
                                                                                                                                                          "African & African Amer Studies",
                                                                                                                                                          "Anthropology",
                                                                                                                                                          "Applied Mathematics",
                                                                                                                                                          "Applied Physics",
                                                                                                                                                          "Astronomy",
                                                                                                                                                          "Biomedical Engineering",
                                                                                                                                                          "Chemical & Physical Biology",
                                                                                                                                                          "Chemistry & Chemical Biology",
                                                                                                                                                          "Comparative Literature",
                                                                                                                                                          "East Asian Langs & Civ",
                                                                                                                                                          "English",
                                                                                                                                                          "Engineering Sciences",
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
                                                                                                                                                          "Women, Gender & Sexuality"), multiple = TRUE, selected = c("Economics", "Government", "Computer Science", "Statistics")), width = 2),
                 
                  #The main panel shows the graphs or rendered html text that corresponds to the opened tab.
                 
                  mainPanel(width = 10, htmlOutput("distributions_header"), plotlyOutput("fall_distributions"), htmlOutput("space2"), plotlyOutput("spring_distributions"))),
                   
        #The next panel covers the distribution of graduate students across undergraduate courses in each department.
         
        tabPanel("Graduate Enrollment in Undergraduate Courses", 
                 
                  #The sidebar panel is a place to customize the data and filter it.
                 
                  sidebarPanel(
                    
                    #Next, the input for the year allows the user to select which year the chart corresponds to.
                    
                    selectInput("graduate_year", "Select a Year", c("2018-2019", "2017-2018", "2016-2017", "2015-2016"), selected = "2018-2019"), 
                    
                    #The departments input is necessary to only show data from selected departments.
                    
                    selectInput("graduate_departments", "Departments", choices = c("Economics",
                                                                  "Government",
                                                                  "Computer Science",
                                                                  "Statistics",
                                                                  "African & African Amer Studies",
                                                                  "Anthropology",
                                                                  "Applied Mathematics",
                                                                  "Applied Physics",
                                                                  "Astronomy",
                                                                  "Biomedical Engineering",
                                                                  "Chemical & Physical Biology",
                                                                  "Chemistry & Chemical Biology",
                                                                  "Comparative Literature",
                                                                  "East Asian Langs & Civ",
                                                                  "English",
                                                                  "Engineering Sciences",
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
                                                                  "Women, Gender & Sexuality"), selected = c("Economics",
                                                                   "Government",
                                                                   "Computer Science",
                                                                   "Statistics"), multiple = TRUE), width = 2), 
                 
                  #The main panel shows the graphs or rendered html text that corresponds to the opened tab.
                 
                  mainPanel(width = 10, htmlOutput("graduates_header"), plotlyOutput("fall_graduates"), htmlOutput("space3"), plotlyOutput("spring_graduates"))),
        
        #Then, there is a panel dedicated to explaining the project and providing 
         
        tabPanel("About", htmlOutput("about"))
         
                ) 
  
  


# This function defines the server logic required to draw the graphs

server <- function(input, output) {
  
  #The spring function will take in a string with two years (both years of each school year) and output just the one that corresponds to the spring (the second of the two years).
  
  spring_function <- function(x) {
    
    #This line checks for a given school year.
    
    if (x == "2015-2016") {
      
      #Then, the second of the two years is returned.
      
      "2016"
    }
    
    #This line checks for another given school year.
    
    else if (x == "2016-2017") {
      
      #Then, the second of the two years is returned.
      
      "2017"
    }
    
    #This line checks for another given school year.
    
    else if (x == "2017-2018") {
      
      #Then, the second of the two years is returned.
      
      "2018"
    }
    
    #Otherwise, it must be referring to the most recent school year due to limited selections.
    
    else {
      
      #Then, the second of the two years is returned.
      
      "2019"
    }
  }
  
  #The fall function will take in a string with two years (both years of each school year) and output just the one that corresponds to the spring (the first of the two years).
  
  fall_function <- function(x) {
    
    #This line checks for another given school year.
    
    if (x == "2015-2016") {
      
      #Then, the first of the two years is returned.
      
      "2015"
    }
    
    #This line checks for another given school year.
    
    else if (x == "2016-2017") {
      
      #Then, the first of the two years is returned.
      
      "2016"
    }
    
    #This line checks for another given school year.
    
    else if (x == "2017-2018") {
      
      #Then, the first of the two years is returned.
      
      "2017"
    }
    
    #Otherwise, it must be referring to the most recent school year due to limited selections.
    
    else {
      
      #Then, the first of the two years is returned.
      
      "2018"
    }
  }
  

  #The data is stored in an rds file contained within the repository so that the charts can update more quickly.
  #I decided to do this because I was experiencing slow update times.
  
  #There is an r script file that was used to make these rds files, which is also included in the repository.
  
  enrollment_fall <- read_rds("fall_enrollment.rds")
  
  #The spring data is loaded as well.
  
  enrollment_spring <- read_rds("spring_enrollment.rds")
  
  
  #One begins with the first plot.
  
  output$fall_largest <- renderPlotly({ 
    
    #One begins with the original dataset
    
    fall_largest <- enrollment_fall %>% 
      
    #Then one filters the year by an input value that the user will be able to specify.  
      
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
      
      labs(title = "Largest Fall Classes in Selected Year", caption = "Source: Harvard Registrar") +
      
      #The limits are set to a standard amount for consistency
      
      ylim(0, 800) +
      
      #Next, the axis is labeled clearly.
      
      xlab("Course Title") +
      
      #The y axis follows.
      
      ylab("Number of Enrolled Undergraduates") + 
      
      #The minimal theme works well with the white background.
      
      theme_minimal()
    
    #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
    
    fl <- ggplotly(fall_largest) 
    
    #The following code is necessary to clean the hover text that displays in plotly
    
    #This code loops through the data structure containing the labels for each department
    
    for (j in 1:length(fl$x$data)) {
      
      #Within each department, the labels are looped through
      
      for (i in 1:length(fl$x$data[[j]]$text)) {
        
        #Each label is replaced with blank because they are not necessary for this bar graph.
        
        fl$x$data[[j]]$text[[i]] <- ""
      }
    }
    
    #Next, one displays the newly made plotly graph.
    
    fl %>% 
      
      #Next, the plotly bar is disabled for aesthetic purposes.
      
      config(displayModeBar = FALSE) %>% 
      
      #The x axis is disabled so there is no zooming, which is distracting.
      
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      
      #The y axis is also disabled so there is no zooming, which is distracting.
      
      layout(yaxis = list(fixedrange = TRUE))
    
  })
  
  output$spring_largest <- renderPlotly({ 
    
    #One begins with the original dataset
    
    spring_largest <- enrollment_spring %>% 
      
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
      
      labs(title = "Largest Spring Courses in Selected Year", caption = "Source: Harvard Registrar") +
      
      #The limits are set to a standard amount for consistency
      
      ylim(0, 800) +
      
      #Next, the axis is labeled clearly.
  
      xlab("Course Title") +
      
      #The y axis follows.
      
      ylab("Number of Enrolled Undergraduates") + 
      
      #The minimal theme works well with the white background.
      
      theme_minimal()
    
    #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
    
    sl <- ggplotly(spring_largest) 
    
    #The following code is necessary to clean the hover text that displays in plotly
    
    #This code loops through the data structure containing the labels for each department
    
    for (j in 1:length(sl$x$data)) {
      
      #Within each department, the labels are looped through
      
      for (i in 1:length(sl$x$data[[j]]$text)) {
        
        #Each label is replaced with a subset of the original label so there are no redundancies.
        
        sl$x$data[[j]]$text[[i]] <- ""
      }
    }
    
    #Next, one displays the newly made plotly graph.
    
    sl %>% 
      
      #Next, the plotly bar is disabled for aesthetic purposes.
      
      config(displayModeBar = FALSE) %>% 
      
      #The x axis is disabled so there is no zooming, which is distracting.
      
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      
      #The y axis is also disabled so there is no zooming, which is distracting.
      
      layout(yaxis = list(fixedrange = TRUE))
    
    
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
        
        #Choosing a violin is helpful for showing data distributions.
        
        geom_violin() + 
        
        #Then, the labels and titles are set.
        
        labs(title = "Distribution of Fall Course Sizes by Department in Selected Year", caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        #Next, the x-axis is labeled.
        
        xlab(NULL) +
        
        #Next, scaling the y-axis is necessary so that there is not a huge right tail on each of the datasets to account for very large classes.
        
        scale_y_log10() + 
        
        #Next, the coordinates are flipped in order to make reading easier for the viewer
        
        coord_flip() + 
        
        #Next, the jitter is overlayed to demonstrate the density somehow
        
        geom_jitter(width = .3, aes(text = paste("Course:", Title, "<br>",
                                                 
                                                 #The departments are good to have for convenience.
                                                 
                                                 "Department: ", Department, "<br>",
                                                 
                                                 #Next, the undergraduate enrollments are the key piece of data.
                                                 
                                                 "Undergraduates: ", Undergraduates))) + 
          
        #The minimal theme works well with the white background.
        
        theme_minimal() + 
          
          #The legend is unnecessary in this case because the data categories are clearly labeled with axis marks
          
          theme(legend.position = "none")
        
        #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
       
        fd <- ggplotly(fall_distributions)
        
        #The following code is necessary to clean the hover text that displays in plotly
        
        #This code loops through the data structure containing the labels for each department
        
        for (j in 1:length(fd$x$data)) {
          
          #Within each department, the labels are looped through
          
          for (i in 1:length(fd$x$data[[j]]$text)) {
            
            #Each label is replaced with a subset of the original label so there are no redundancies.
            
            fd$x$data[[j]]$text[[i]] <- strsplit(fd$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
          }
        }
        
        #The ggplotly object is then rendered.
        
        fd %>% 
          
          #Next, the plotly bar is disabled for aesthetic purposes.
          
          config(displayModeBar = FALSE) %>% 
          
          #The x axis is disabled so there is no zooming, which is distracting.
          
          layout(xaxis = list(fixedrange = TRUE)) %>% 
          
          #The y axis is also disabled so there is no zooming, which is distracting.
          
          layout(yaxis = list(fixedrange = TRUE))
        
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
        
        #Choosing a violin plot is helpful for showing data distributions.
        
        geom_violin() + 
        
        #Then, the labels and titles are set.
        
        labs(title = "Distribution of Spring Course Sizes by Department in Selected Year", caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        #Next, the x-axis is labeled.
        
        xlab(NULL) +
        
        #Next, scaling the y-axis is necessary so that there is not a huge right tail on each of the datasets to account for very large classes.
        
        scale_y_log10() + 
        
        #Next, the coordinates are flipped in order to make reading easier for the viewer
        
        coord_flip() + 
        
        #Next, the jitter is overlayed to demonstrate the density somehow
        
        geom_jitter(width = .3, aes(text = paste("Course:", Title, "<br>",
                                                 
                                                 #The departments are good to have for convenience.
                                                 
                                                 "Department: ", Department, "<br>",
                                                 
                                                 #Next, the undergraduate enrollments are the key piece of data.
                                                 
                                                 "Undergraduates: ", Undergraduates))) +  
        
        #The minimal theme works well with the white background.
        
        theme_minimal() + 
        
        #The legend is unnecessary in this case because the data categories are clearly labeled with axis marks
        
        theme(legend.position = "none")
      
      #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
      
      sd <- ggplotly(spring_distributions)
      
      #The following code is necessary to clean the hover text that displays in plotly
      
      #This code loops through the data structure containing the labels for each department
      
      for (j in 1:length(sd$x$data)) {
        
        #Within each department, the labels are looped through
        
        for (i in 1:length(sd$x$data[[j]]$text)) {
          
          #Each label is replaced with a subset of the original label so there are no redundancies.
          
          sd$x$data[[j]]$text[[i]] <- strsplit(sd$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
        }
      }
      
      #Then the first department is looped through again
      
      sd %>% 
        
        #Next, the plotly bar is disabled for aesthetic purposes.
        
        config(displayModeBar = FALSE) %>% 
        
        #The x axis is disabled so there is no zooming, which is distracting.
        
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        
        #The y axis is also disabled so there is no zooming, which is distracting.
        
        layout(yaxis = list(fixedrange = TRUE))
    })
    
    output$fall_graduates <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      fall_graduates <- enrollment_fall %>%
        
        #One is only interested in courses with graduates enrolled.
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$graduate_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == fall_function(input$graduate_year)) %>%
        
        #Next, one can begin the plot.
      
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing geom count is a good idea here because the data is sparse and the frequency is important
        
        geom_count() + 
        
        #The size of the points is varied to show which frequencies are higher
        
        scale_size_area() +
        
        #Next, the labels are necessary to help comprehend the chart.
        
        labs(title = "Undergraduate Fall Course Graduate Enrollments by Department in Selected Year", subtitle = "Point size represents frequency of courses with given number of graduate students", caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Course Graduate Enrollment") +
        
        #Scaling the y axis to log 10 alloows the data to be viewed in a more compact way.
        
        scale_y_log10() +
        
        #Next, the coordinates are flipped so the graph can be used horizontally.
        
        coord_flip() +
        
        #The minimal theme works well with the white background.
        
        theme_minimal() + 
        
        #The legend is unnecessary in this case because the data categories are clearly labeled with axis marks
        
        theme(legend.position = "none")
      
      #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
      
      fg <- ggplotly(fall_graduates)
      
      #The following code is necessary to clean the hover text that displays in plotly
      
      #This code loops through the data structure containing the labels for each department
      
      for (j in 1:length(fg$x$data)) {
        
        #Within each department, the labels are looped through
        
        for (i in 1:length(fg$x$data[[j]]$text)) {
          
          #Each label is replaced with a subset of the original label so there are no redundancies.
          
          fg$x$data[[j]]$text[[i]] <- strsplit(fg$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
        }
      }
      
      #Then the first department is looped through again
      
      fg %>% 
        
        #Next, the plotly bar is disabled for aesthetic purposes.
        
        config(displayModeBar = FALSE) %>% 
        
        #The x axis is disabled so there is no zooming, which is distracting.
        
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        
        #The y axis is also disabled so there is no zooming, which is distracting.
        
        layout(yaxis = list(fixedrange = TRUE))
    
    })
    
    
    output$spring_graduates <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      spring_graduates <- enrollment_spring %>%
        
        #One is interested in only courses that have some graduate students to avoid a heavy skew at 0
        
        filter(Graduates > 0) %>% 
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$graduate_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        filter(Year == spring_function(input$graduate_year)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Department, y = Graduates, fill = Department)) +
        
        #Choosing geom count is a good idea here because the data is sparse and the frequency is important
        
        geom_count() + 
        
        #The size of the points is varied to show which frequencies are higher
        
        scale_size_area() +

        #The labels allow the user to better understand the chart.
        
        labs(title = "Undergraduate Spring Course Graduate Enrollments by Department in Selected Year", caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Course Graduate Enrollment") +
        
        #The log scale is important to make the data compact.
        
        scale_y_log10() +
        
        #The coordinates are flipped so they can be viewed horizontally in a more space-effecient manner.
        
        coord_flip() +
        
        #The minimal theme works well with the white background.
        
        theme_minimal() + 
        
        #The legend is unnecessary in this case because the data categories are clearly labeled with axis marks
        
        theme(legend.position = "none")
      
      #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
      
      sg <- ggplotly(spring_graduates)
      
      #The following code is necessary to clean the hover text that displays in plotly
      
      #This code loops through the data structure containing the labels for each department
      
      for (j in 1:length(sg$x$data)) {
        
        #Within each department, the labels are looped through
        
        for (i in 1:length(sg$x$data[[j]]$text)) {
          
          #Each label is replaced with a subset of the original label so there are no redundancies.
          
          sg$x$data[[j]]$text[[i]] <- strsplit(sg$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
        }
      }
      
      #Then the first department is looped through again
      
      sg %>% 

        #Next, the plotly bar is disabled for aesthetic purposes.
        
        config(displayModeBar = FALSE) %>% 
        
        #The x axis is disabled so there is no zooming, which is distracting.
        
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        
        #The y axis is also disabled so there is no zooming, which is distracting.
        
        layout(yaxis = list(fixedrange = TRUE))
      
      
    })
    

    output$fall_timegraph <- renderPlotly({ 
      
      #Starting with the combined enrollment data set
      
      fall_timegraph <- enrollment_fall %>%
        
        #Next, for this phase, one is concerned with only a subset of the departments.  This will change later.
        
        filter(Department %in% input$timegraph_departments) %>%
        
        #This function then filters the data so only the selected year is shown.
        
        group_by(Year, Department) %>%
        
        #The summarize function is necessary here to compare all the undergraduates in a department by taking the total.
        
        summarize(Total = sum(Undergraduates)) %>%
        
        #Next, one can begin the plot.
        
        ggplot(aes(x = Year, y = Total, fill = Department)) +
        
        #The labels are necessary for understanding the axes of the chart.
        
        labs(title = "Total Enrollment in Fall Courses by Department Over Time", caption = "Source: Harvard Registrar") +
        
        #Next, the points are great and simple indicators.
        
        geom_point(aes(color = Department, text = paste("Department: ", Department, "<br>",
                                                        
                                                        #Next, the undergraduate enrollments are the key piece of data.
                                                        
                                                        "Undergraduates: ", Total, "<br>"))) +
        
        #Choosing a line is helpful for connecting the dots.
        
        geom_line(aes(group = 1, color = Department)) +
        
        #The y-axis is then labeled
        
        ylab("Total Enrollments in Department Courses") +
      
        #The minimal theme works well with the white background.
        
        theme_minimal()
      
      #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
      
      ftg <- ggplotly(fall_timegraph)
      
      #The following code is necessary to clean the hover text that displays in plotly
      
      #This code loops through the data structure containing the labels for each department
      
      for (j in 1:length(ftg$x$data)) {
        
        #Within each department, the labels are looped through
        
        for (i in 1:length(ftg$x$data[[j]]$text)) {
          
          #Each label is replaced with a subset of the original label so there are no redundancies.
          
          ftg$x$data[[j]]$text[[i]] <- strsplit(ftg$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
        }
      }
      
      #Then the first department is looped through again
      
      ftg %>% 
        
        #Next, the plotly bar is disabled for aesthetic purposes.
        
        config(displayModeBar = FALSE) %>% 
        
        #The x axis is disabled so there is no zooming, which is distracting.
        
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        
        #The y axis is also disabled so there is no zooming, which is distracting.
        
        layout(yaxis = list(fixedrange = TRUE))
      
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
        
        #Choosing a line graph is helpful to connect the dots.
        
        geom_line(aes(group = 1, color = Department)) +
      
        #Next, the points are placed to connect the lines.
        
        geom_point(aes(color = Department, text = paste("Department: ", Department, "<br>",
                                                        
                                                        #Next, the undergraduate enrollments are the key piece of data.
                                                        
                                                        "Undergraduates: ", Total, "<br>"))) +
        
        #The labels are necessary for understanding the axes of the chart.
        
        labs(title = "Total Enrollment in Spring Courses by Department Over Time", caption = "Source: Harvard Registrar") +
        
        #Next, the y-axis is labeled.
        
        ylab("Density of Course Enrollment Size Frequency") +
        
        #The minimal theme works well with the white background.
        
        theme_minimal()
      
      #Next, the ggplot is wrapped in a plotly call in order to display the chart as a plotly chart.
      
      stg <- ggplotly(spring_timegraph)
      
      #The following code is necessary to clean the hover text that displays in plotly
      
      #This code loops through the data structure containing the labels for each department
      
      for (j in 1:length(stg$x$data)) {
        
        #Within each department, the labels are looped through
        
        for (i in 1:length(stg$x$data[[j]]$text)) {
          
          #Each label is replaced with a subset of the original label so there are no redundancies.
          
          stg$x$data[[j]]$text[[i]] <- strsplit(stg$x$data[[j]]$text[[i]], '<br />')[[1]][[1]]
        }
      }
      
      #Then the first department is looped through again
      
      stg %>% 
        
        #Next, the plotly bar is disabled for aesthetic purposes.
        
        config(displayModeBar = FALSE) %>% 
        
        #The x axis is disabled so there is no zooming, which is distracting.
        
        layout(xaxis = list(fixedrange = TRUE)) %>% 
        
        #The y axis is also disabled so there is no zooming, which is distracting.
        
        layout(yaxis = list(fixedrange = TRUE))
      
    })
    
    #This output renders a space for formatting purposes
    
    output$space <- renderText ({
      
      #Three line breaks are good number
      
      "<br><br><br>"
      
    })
    
    #This output renders a space for formatting purposes
    
    output$space2 <- renderText ({
      
      #Three line breaks are good number
      
      "<br><br><br>"
      
    })
    
    #This output renders a space for formatting purposes
    
    output$space3 <- renderText ({
      
      #Three line breaks are good number
      
      "<br><br><br>"
      
    })
    
    #This output renders a space for formatting purposes
    
    output$space4 <- renderText ({
      
      #Three line breaks are good number
      
      "<br><br><br>"
      
    })
    
    #This is the header for the Time Graph tab.
    
    output$timegraph_header <- renderText ({
      
      "<h2 align = center> Course Enrollment Over Time</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the total number of undergraduate enrollments across all courses in a department 
      for each school year.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    #This is the header for the Largest Courses tab.
    
    output$largest_header <- renderText ({
      
      "<h2 align = center> Largest Classes</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the largest classes
      for each school year.  To change the year, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    #This is the header for the Distributions Tab.
    
    output$distributions_header <- renderText ({
      
      "<h2 align = center> Class Size Distributions</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the distribution of class sizes within selected departments
      for each school year.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.  
      You can also use the drop-down toggle to change the year</p>"
      
    })
    
    #This is the header for the Graduate vs. Undergraduate Enrollment tab.
    
    output$graduates_header <- renderText ({
      
      "<h2 align = center> Graduate Students in Undergraduate Courses</h2>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>The following graphs display the distribution of the number of graduate students in undergraduate courses for each school year.  An undergraduate course is defined as a course with at least 3 undergraduate students and that contains a majority of undergraduate students.  To change the departments that are displayed, simply type the name of a new one into the 
      bar in the upper left corner or delete one that is currently there using the delete key.</p>"
      
    })
    
    #This is the rendering of the About tab, which gives information about the Shiny app.
    
    output$about <- renderText ({
      
      "<h1 align = center> Welcome to the Enrollment Project!</h1>
      <p align = center style = 'margin-left:20%; margin-right: 20%'>Ever wonder what the major trends are in course enrollment?  
      Using a public dataset on the Harvard Registrar, this Shiny app lets users visualize various presentations of the data from different angles.  
      Each tab demonstrates a different segment of the data and there is a customizable interactive form on the top left of every page for the user to filter the data how they see fit.  
      All courses, unless otherwise specified, are undergraduate courses, which are defined as courses with at least three undergraduates and a majority of undergraduate students.
      Feel free to browse through!</p>
      <h4 align = center>Contact and Info</h4>
      <p align = center>Email: chrisonesti@college.harvard.edu
      <p align = center>GitHub Link: <a href = https://github.com/conesti/fall-course-enrollment-analysis>https://github.com/conesti/fall-course-enrollment-analysis</a>"
      
    })
    
}

#Briefly consulted internet and classmates's shiny apps/GitHub repos

# Run the application 
shinyApp(ui = ui, server = server)

