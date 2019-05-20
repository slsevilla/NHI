library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)
library(tidyr)

################################################################################################
################################################################################################
###                                         NOTES                                            ###
################################################################################################
################################################################################################
#HQ Database Conversion: Takes in the HQ file provided with student and generates a standard database
#to use throughout the program.

####################################################################################################
#####################################################################################################
###                                             Code                                             ###
#####################################################################################################
####################################################################################################
function(input,output, session){
  
  ##############################        HQ Database Conversion        ##############################
  #################################################################################################
  
  ######################################### Input File
  ##Import file
  input_student <- reactive({
    input_student <- input$file_student
    if(is.null(input_student)) return(NULL)
    read.csv(fill=TRUE,file=input$file_student$datapath,header=TRUE,colClasses = "factor"
             )
  })
  ##Import file
  input_coach <- reactive({
    input_coach <- input$file_coach
    if(is.null(input_coach)) return(NULL)
    read.csv(fill=TRUE,file=input$file_coach$datapath,header=TRUE,colClasses = "factor"
    )
  })
  ##Create File Summary information
  output$input.student <- renderTable({
    if(is.null(input_student())) return ()
    input$hq.file1
  })
  ##Display confirmation of upload for user
  output$confirm.student <- renderUI({
    if(is.null(input_student())) return()
    tableOutput("confirm.student")
  })
  
  ######################################### User Dropdowns
  ##Generate Drop Downs for user to choose
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_FNAME<- renderUI({
      selectInput("stud_FNAME", "First Name", cb_options)
      })
    })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_MNAME<- renderUI({
      selectInput("stud_MNAME", "Middle Name", cb_options)
    })
  })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_LNAME<- renderUI({
      selectInput("stud_LNAME", "Last Name", cb_options)
    })
  })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_MF<- renderUI({
      selectInput("stud_MF", "Gender", cb_options)
    })
  })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_HS<- renderUI({
      selectInput("stud_HS", "High School", cb_options)
    })
  })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_CITY<- renderUI({
      selectInput("stud_CITY", "City", cb_options)
    })
  })
  observe({
    req(input$file_student)
    dsnames <- names(input_student())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$stud_ST<- renderUI({
      selectInput("stud_ST", "State", cb_options)
    })
    })
  
  #Coach Information
  observe({
    req(input$file_coach)
    dsnames <- names(input_coach())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$coach_FNAME<- renderUI({
      selectInput("coach_FNAME", "Coach's FName", cb_options)
    })
  })
  observe({
    req(input$file_coach)
    dsnames <- names(input_coach())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$coach_CELL<- renderUI({
      selectInput("coach_MNAME", "Coach's MName", cb_options)
    })
  })
  observe({
    req(input$file_coach)
    dsnames <- names(input_coach())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$coach_FIN.FORM<- renderUI({
      selectInput("coach_LNAME", "Coach's LName", cb_options)
    })
  })
  observe({
    req(input$file_coach)
    dsnames <- names(input_coach())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$coach_MED.FORM<- renderUI({
      selectInput("coach_Univ", "Coach's University", cb_options)
    })
  })
  
  ######################################### Generate Student Database
  ###Create database from user generated files, and matched column headers
  stud_database <- reactive({
    if(is.null(input_student)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- input_student()
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    table_out[,"STATUS"] <- ""
    
    #Append each necessary column, based on user input to the new table
    table_out$FNAME <- table_in[,input$stud_FNAME]
    table_out$MNAME <- table_in[,input$stud_MNAME]
    table_out$LNAME <- table_in[,input$stud_LNAME]
    table_out$MF <- table_in[,input$stud_MF]
    table_out$HS <- table_in[,input$stud_HS]
    table_out$CITY <- table_in[,input$stud_CITY]
    table_out$ST <- table_in[,input$stud_ST]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ######################################### Send file to download screen
  output$download_stud <- downloadHandler(
    filename = function() {"CWS_StudentDemo.csv"},
    content = function(file) {
      write.csv(stud_database(), file, row.names = FALSE)
    }
  )
  
  ######################################### Generate Student Database
  ###Create database from user generated files, and matched column headers
  coach_database <- reactive({
    if(is.null(input_coach)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- input_coach()
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    table_out[,"STATUS"] <- ""
    
    #Append each necessary column, based on user input to the new table
    table_out$FNAME <- table_in[,input$coach_FNAME]
    table_out$MNAME <- table_in[,input$coach_MNAME]
    table_out$LNAME <- table_in[,input$coach_LNAME]
    table_out$UNIV <- table_in[,input$coach_Univ]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ######################################### Send file to download screen
  output$download_coach <- downloadHandler(
    filename = function() {"CWS_CoachDemo.csv"},
    content = function(file) {
      write.csv(coach_database(), file, row.names = FALSE)
    }
  )
}