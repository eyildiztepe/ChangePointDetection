library(shiny)
library(shinythemes)
library(changepoint)
library(ggplot2)


ui <- navbarPage(
  title = "changepoint",
  theme = shinytheme("spacelab"),
  tabPanel('Home',
           h1("Welcome to the interactive shiny app for the", tags$a(href="https://CRAN.R-project.org/package=changepoint", target="_blank", "changepoint"),"package in R"),
           p("This dashboard allows the user to explore changepoint analysis on simulated data, as well as uploading data sets to perfrom changepoint analysis on without the need for any coding."),
           p("Analysis is performed using the", tags$a(href="https://CRAN.R-project.org/package=changepoint", target="_blank", "changepoint"), 'package. See ', tags$a(href='http://www.jstatsoft.org/v58/i03/', target='blank','`Killick R, Eckley IA (2014). "changepoint: An R package for Changepoint Analysis." Journal of Statistical Software, 58(3), 1-19`'), " for more details" ),
           h3("Simulated Data"),
           p("Here the user can create a simple data set with a single changepoint and see the output of changepoint analysis on the data."),
           p("The user can control the following:", tags$ul(tags$li("Sample Size"), tags$li("Number of Change "), tags$li("Method"), tags$li("Type of Change"))),
           h3("Load Data"),
           p("Here the user can upload their own .txt&.csv file containg a data set they wish to do changepoint analysis upon."),
           p("The user can alter the changepoint analysis to look for:", tags$ul(tags$li("Mean Changes"), tags$li("Variance Changes"),tags$li("Mean and Variance Changes"), tags$li("Penalty Choice"))),
           p("Additionally, the user can choose between allowing for a single or multiple changepoints along with other parameters for the changepoint analysis"),
           h3("Developers"),
           p("-Ahmet CALI"),
           p("-Pelin PEKER"),
           p("-Edanur Binnaz DURSUN"),
           p("-Merve AK"),
           h3("Thanks to Assoc.Prof.Dr. Engin YILDIZTEPE"),
           
           
  ),
  
  tabPanel('Simulated Data with Change Points',
           plotOutput('sim_plot'),
           downloadButton("downloadData", "Download Data"),
           hr(),
           column(4,
                  sliderInput("num_samples", "Sample Size:",
                              min = 500, max = 2000, value = 1000),
                  sliderInput("num_changepoints", "Number of Changepoints:",
                              min = 1, max = 10, value = 4),
                  checkboxInput("show_changepoints", "Show Change Points", value = FALSE),
                  textOutput("change_points_text"), # Text output for change point locations
                  
                  selectInput("changepoint_method", "Method:",
                              choices = c("AMOC", "BinSeg", "PELT"),
                              selected = "AMOC"),
                  
                  selectInput("type_of_change", "Type of Change:",
                              choices = c("Mean", "Variance", "Both"),
                              selected = "Both"),
                  
                  checkboxInput("show_predicted_changepoints", "Show Predicted Change Points", value = FALSE) # Checkbox to control showing predicted change points
           ),
           column(8,
                  verbatimTextOutput("change_points_text_output"), # Output for change point locations
                  verbatimTextOutput("predicted_changepoints_text_output") # Output for predicted change point locations
           )
  ),
  tabPanel('Load Data',
           sidebarLayout(
             sidebarPanel(width=2,
                          h3("Upload Files"),
                          p("Files should be of type '.txt' & '.csv' and should contain only one column containing the univariate data."),
                          fileInput("file1", "Choose TXT or CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          tags$hr(),
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ",")
             ),
             mainPanel(width=10,
                       plotOutput("contents"),
                       column(4,
                              tags$h3("Changepoint Parameters"),
                              uiOutput('cptTypeControlsLoad'),
                              selectInput(inputId = 'methodLoad',
                                          label = 'Changepoint Method',
                                          choices = list('PELT', 'BinSeg', 'AMOC')
                              ),
                              selectInput(inputId = 'penaltyLoad',
                                          label = 'Penalty Choice',
                                          choices = list('SIC'='SIC', 'BIC'='BIC', 'MBIC'='MBIC', 'AIC'='AIC', 'Hannan-Quinn'='Hannan-Quinn', 'Manual'='Manual'),
                                          selected='MBIC'
                              ),
                              uiOutput('binSegMaxLoad'),
                              uiOutput('manPenLoad'),
                              uiOutput('changeTypeWarning')
                       ),
                       column(4,
                              tags$h3("Changepoint Results"),
                              verbatimTextOutput("cptSummaryLoad")
                       ),
                       column(4,
                              tags$h3("Plot Options"),
                              checkboxInput("showCptsLoad",
                                            label='Show changepoints'
                              ),
                              checkboxInput("showMeanLoad",
                                            label='Show segment means'
                              )
                       )
             )
           )
  )
)


server <- function(input, output){
  data_df <- reactiveVal(NULL) # Reactive value to store data
  change_points <- reactiveVal(NULL) # Reactive value to store change points
  predicted_change_points <- reactiveVal(NULL) # Reactive value to store predicted change points
  
  output$sim_plot <- renderPlot({
    set.seed(1)
    nj <- input$num_samples
    njk <- input$num_changepoints
    njk <- ifelse(njk == 0, njk + 1, njk)
    tj <- lapply(1:nj, function(i) sort(round(runif(njk, 50, nj - 50))))
    
    data_sim <- list()
    for (k in 1:nj) {
      for (j in tj[[k]]) {
        n <- c(tj[[k]][1] - 1, diff(tj[[k]]), nj - tj[[k]][length(tj[[k]])] + 1)
        x <- sapply(n, function(y) rnorm(y, rnorm(1, 0, 10), runif(1, 1, 10)))
        data_sim[[k]] <- unlist(x)
      }
    }
    
    plot(data_sim[[1]], type = "l", col = "black", main = "Simulated Data")
    
    if (input$show_changepoints) {
      for (i in 1:length(tj[[1]])) {
        abline(v = tj[[1]][i], col = "blue", lty = 2)
      }
    }
    
    # Store data and change points as reactive values
    data_df(data_sim[[1]])
    change_points(tj[[1]])
    
    # Predict change points
    method <- input$changepoint_method
    type_of_change <- input$type_of_change
    if (!is.null(data_sim[[1]]) && !is.null(method)) {
      if(method == "BinSeg"){
        Q_value <- input$num_changepoints # Q parametresini num_changepoints de??eri ile ayarla
        if(type_of_change == "Mean") {
          cpt <- cpt.mean(data_sim[[1]], method = method, Q = Q_value)
        } else if(type_of_change == "Variance") {
          cpt <- cpt.var(data_sim[[1]], method = method, Q = Q_value)
        } else if(type_of_change == "Both") {
          cpt <- cpt.meanvar(data_sim[[1]], method = method, Q = Q_value)
        }
      } else if (method == "PELT") {
        minseglen_value <- input$num_changepoints # minseglen parametresini num_changepoints de??eri ile ayarla
        if(type_of_change == "Mean") {
          cpt <- cpt.meanvar(data_sim[[1]], method = method, minseglen = minseglen_value)
        } else if(type_of_change == "Variance") {
          cpt <- cpt.var(data_sim[[1]], method = method, minseglen = minseglen_value)
        } else if(type_of_change == "Both") {
          cpt <- cpt.meanvar(data_sim[[1]], method = method, minseglen = minseglen_value)
        }
      } else {
        if(type_of_change == "Mean") {
          cpt <- cpt.mean(data_sim[[1]], method = method)
        } else if(type_of_change == "Variance") {
          cpt <- cpt.var(data_sim[[1]], method = method)
        } else if(type_of_change == "Both") {
          cpt <- cpt.meanvar(data_sim[[1]], method = method)
        }
      }
      predicted_change_points(cpts(cpt))
    } else {
      predicted_change_points(NULL)
    }
    
    # Draw blue line for predicted change points if the checkbox is checked
    if (input$show_predicted_changepoints && !is.null(predicted_change_points())) {
      for (i in 1:length(predicted_change_points())) {
        abline(v = predicted_change_points()[i], col = "red", lty = 2)
      }
    }
  })
  
  # Render change point locations text
  output$change_points_text_output <- renderPrint({
    cp <- change_points()
    if (!is.null(cp)) {
      paste("Change Point Locations:", paste(cp, collapse = ", "))
    }
  })
  
  # Render predicted change point locations
  output$predicted_changepoints_text_output <- renderPrint({
    pcp <- predicted_change_points()
    if (!is.null(pcp)) {
      paste("Predicted Change Point Locations:", paste(pcp, collapse = ", "))
    }
  })
  
  # Render download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("simulated_data_with_changepoints.csv", sep = "")
    },
    content = function(file) {
      data <- data_df()
      cp <- change_points()
      if (!is.null(data) && !is.null(cp)) {
        # Create a data frame with data and change points in a single row
        df <- data.frame(Data = data)
        # Add the change points to the second column of the first row
        df[1, 2] <- paste(cp, collapse = ", ")
        # Write to CSV file
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  dataLoad <- reactive({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    X = df[,1]
  })
  
  output$contents <- renderPlot({
    req(input$file1)
    p = ggplot(mapping=aes(x=1:length(dataLoad()), y=dataLoad()))+
      geom_line()+
      labs(x='Time', y='Value')
    if(input$showCptsLoad){
      Cpts = cpts(cptAnalysisLoad())
      p = p + geom_vline(xintercept=Cpts, col='red', size=1.3)
    }
    if(input$showMeanLoad){
      Cpts = c(0,cpts(cptAnalysisLoad()),length(dataLoad()))
      means = c()
      for(i in 1:(length(Cpts)-1)){
        mean = mean(dataLoad()[(Cpts[i]+1):Cpts[i+1]])
        means = c(means, rep(mean,Cpts[i+1]-Cpts[i]))
      }
      p = p + geom_line(aes(x=1:length(dataLoad()), y=means), col='blue',size=1.3)
    }
    p
  })
  
  cptAnalysisLoad <- reactive({
    req(input$file1)
    if(input$cptTypeLoad == 'Mean'){
      cptAns = cpt.mean(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }else if(input$cptTypeLoad == 'Variance'){
      cptAns = cpt.var(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }else{
      cptAns = cpt.meanvar(dataLoad(), method=input$methodLoad, penalty=input$penaltyLoad, pen.value = input$manPenLoad, Q=input$maxCptLoad)
    }
  }) 
  
  output$cptSummaryLoad <- renderPrint({
    summary(cptAnalysisLoad())
  }) 
  
  output$cptTypeControlsLoad <- renderUI({
    selectInput(inputId = 'cptTypeLoad',
                label = 'Type of change to detect',
                choices = list('Mean', 'Variance', 'Mean & Variance'),
                selected='Mean & Variance'
    )
  })
  
  output$changeTypeWarning <- renderUI({
    if(input$cptTypeLoad=='Mean'){
      p("In the changepoint methods the variance is assumed to be 1. Use Mean & Variance change if this is not the case")   
    }else if(input$cptTypeLoad=='Variance'){
      p("In the changepoint methods the mean is assumed to be 0. Use Mean & Variance change if this is not the case")   
    }
  })
  
  output$manPenLoad <- renderUI({
    if(input$penaltyLoad == 'Manual'){
      numericInput(inputId = 'manPenLoad',
                   label = 'Manual Penalty',
                   value = 1,
                   min = 0, 
                   max = 10000,
                   step =0.001
      )
    }
  })  
  output$binSegMaxLoad <- renderUI({
    if(input$methodLoad == 'BinSeg'){
      numericInput(inputId = 'maxCptLoad',
                   label = 'Max number of changepoints',
                   value = 20,
                   min = 1, 
                   max = 10000,
                   step = 1
      )
    }
  })
}


shinyApp(ui = ui, server = server)
