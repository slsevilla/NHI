library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)

#####################################################################################
###                                      NOTES                                   ###
###################################################################################

#####################################################################################
###                                      Code                                    ###
#####################################################################################

function(input,output, session){
######################################### Page 1 ####################################
#####################################################################################
  
  #User Input File
  ##Import file
  hq_input <- reactive({
    hq_input <- input$hq.file1
    if(is.null(hq_input)) return(NULL)
    read.csv(fill=TRUE,file=input$hq.file1$datapath,header=TRUE,colClasses = "factor"
             )
  })
  ##Create File Summary information
  output$hq.input <- renderTable({
    if(is.null(hq_input())) return ()
    input$hq.file1
  })
  ##Display confirmation of upload for user
  output$confirm.hq <- renderUI({
    if(is.null(hq_input())) return()
    tableOutput("hq.input")
  })
  
  #User Dropdowns
  ##Generate Drop Downs for user to choose
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_FNAME<- renderUI({
      selectInput("hq_FNAME", "First Name", cb_options)
      })
    })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MNAME<- renderUI({
      selectInput("hq_MNAME", "Middle Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_LNAME<- renderUI({
      selectInput("hq_LNAME", "Last Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MF<- renderUI({
      selectInput("hq_MF", "Gender", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_HS<- renderUI({
      selectInput("hq_HS", "High School", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_CITY<- renderUI({
      selectInput("hq_CITY", "City", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ST<- renderUI({
      selectInput("hq_ST", "State", cb_options)
    })
    })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_HOME<- renderUI({
      selectInput("hq_HOME", "Home #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_CELL<- renderUI({
      selectInput("hq_CELL", "Cell #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_FIN.FORM<- renderUI({
      selectInput("hq_FIN.FORM", "Financial Form", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MED.FORM<- renderUI({
      selectInput("hq_MED.FORM", "Medical Form", cb_options)
    })
  })  
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_BALANCE<- renderUI({
      selectInput("hq_BALANCE", "Balance", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P1<- renderUI({
      selectInput("hq_P1", "Parent Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2<- renderUI({
      selectInput("hq_P2", "Parent Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P1.CELL<- renderUI({
      selectInput("hq_P1.CELL", "Parent Cell#", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2.CELL<- renderUI({
      selectInput("hq_P2.CELL", "Parent Cell#", cb_options)
      })
    })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_AIR<- renderUI({
      selectInput("hq_ARRIVAL_AIR", "Airport", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_TIME<- renderUI({
      selectInput("hq_ARRIVAL_TIME", "Time", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_FLIGHT<- renderUI({
      selectInput("hq_ARRIVAL_FLIGHT", "Flight #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_CARRIER<- renderUI({
      selectInput("hq_ARRIVAL_CARRIER", "Flight Carrier", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DEPART_AIR<- renderUI({
      selectInput("hq_DEPART_AIR", "Airport", cb_options)
    })
    })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DEPART_TIME<- renderUI({
      selectInput("hq_DEPART_TIME", "Time", cb_options)
      })
    })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DORM<- renderUI({
      selectInput("hq_DORM", "Dorm Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ROOM<- renderUI({
      selectInput("hq_ROOM", "Dorm Room #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DOT<- renderUI({
      selectInput("hq_DOT", "Dot Group", cb_options)
    })
  })
  
  #Generate Student Database
  ###Create database from user generated files, and matched column headers
  hq_database <- reactive({
    if(is.null(hq_input)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- hq_input()
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    
    #Append each necessary column, based on user input to the new table
    table_out$FNAME <- table_in[,input$hq_FNAME]
    table_out$MNAME <- table_in[,input$hq_MNAME]
    table_out$LNAME <- table_in[,input$hq_LNAME]
    table_out$MF <- table_in[,input$hq_MF]
    table_out$HS <- table_in[,input$hq_HS]
    table_out$CITY <- table_in[,input$hq_CITY]
    table_out$ST <- table_in[,input$hq_ST]
    table_out$HS <- table_in[,input$hq_HS]
    table_out$HOME <- table_in[,input$hq_HOME]
    table_out$CELL <- table_in[,input$hq_CELL]
    table_out$P1 <- table_in[,input$hq_P1]
    table_out$P2 <- table_in[,input$hq_P2]
    table_out$P1.CELL <- table_in[,input$hq_P1.CELL]
    table_out$P2.CELL <- table_in[,input$hq_P2.CELL]
    
    #Create balance column, removing $
    table_out$BALANCE <- table_in[,input$hq_BALANCE]
    table_out[] <- lapply (table_out[], gsub, pattern = "$", 
                           replacement = "", fixed=TRUE)
    table_out[] <- lapply (table_out[], gsub, pattern = " -", 
                           replacement = "", fixed=TRUE)
    
    table_out$FIN.FORM <- table_in[,input$hq_FIN.FORM]
    table_out$MED.FORM <- table_in[,input$hq_MED.FORM]
    table_out$ARRIVAL_AIR <- table_in[,input$hq_ARRIVAL_AIR]
    table_out$ARRIVAL_TIME <- table_in[,input$hq_ARRIVAL_TIME]
    table_out$ARRIVAL_CARRIER <- table_in[,input$hq_ARRIVAL_CARRIER]
    table_out$ARRIVAL_FLIGHT <- table_in[,input$hq_ARRIVAL_FLIGHT]
    table_out$DEPART_AIR <- table_in[,input$hq_DEPART_AIR]
    table_out$DEPART_TIME <- table_in[,input$hq_DEPART_TIME]
    table_out$DORM <- table_in[,input$hq_DORM]
    table_out$ROOM <- table_in[,input$hq_ROOM]
    table_out$DOT <- table_in[,input$hq_DOT]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ###Send file to download screen
  output$download_hq <- downloadHandler(
    filename = function() {"Expected_StudentDemo.csv"},
    content = function(file) {
      write.csv(hq_database(), file, row.names = FALSE)
    }
  )

######################################## Page 2 #################################### 
###################################################################################  
  
  #File input
  ##Generate database for input file
  day0_expect <- reactive({
    admin1 <- input$day0.file1
    if (is.null(admin1)) return(NULL)
    read.csv(fill=TRUE,file=input$day0.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })  
  #Create File Summary information
  output$day0.expect <- renderTable({
    if(is.null(day0_expect())) return ()
    input$day0.file1
  })
  #Display confirmation of upload for user
  output$confirm.day0 <- renderUI({
    if(is.null(day0_expect())) return()
    tableOutput("day0.expect")
  })
  
  #Data Confirmations 
  ##Displays a confirmation message for the attendance/travel verification
  observeEvent(input$d0_travelverify,{
    output$confirm.d0_travelverify <- renderText({
      "Upload Complete - Download D0 Attend/Travel Verification"})  
  })
  ##Displays a confirmation message for the student self verification
  observeEvent(input$studselfverify,{
    output$confirm.studselfverify <- renderText({
      "Upload Complete - Download Student Self-Verification Forms"})  
  })
  ##Displays a confirmation message for the door signs
  observeEvent(input$studlabels,{
    output$confirm.studlabels <- renderText({
      "Upload Complete - Download Student Labels"})  
  })
  ##Displays a confirmation message for the registration  signs
  observeEvent(input$studbalance,{
    output$confirm.studbalance <- renderText({
      "Upload Complete - Download Students with Balance List"})  
  })
  ##Displays a confirmation message for the registration  signs
  observeEvent(input$studforms,{
    output$confirm.studforms <- renderText({
      "Upload Complete - Download Missing Forms List"})  
  })
  ##Displays a confirmation message for the door signs
  observeEvent(input$door_sign,{
    output$confirm.door_sign <- renderText({
      "Upload Complete - Download Door signs"})  
  })
  ##Displays a confirmation message for the rooming lists on Day 0- female
  observeEvent(input$d0_room_F,{
    output$confirm.d0_room_F <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Female Students"})  
  })
  ##Displays a confirmation message for the rooming lists on Day 0- Male
  observeEvent(input$d0_room_M,{
    output$confirm.d0_room_M <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Male Students"})  
  })
  
  #Create sub-tables
  d0_travelverify <- reactive({
    if(is.null(input$day0.file1)) return()
    d0_travelverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "P1.CELL",
                                       "P2.CELL", "ARRIVAL_AIR", "ARRIVAL_TIME",
                                       "ARRIVAL_CARRIER", "ARRIVAL_FLIGHT",
                                       "DEPART_AIR", "DEPART_TIME")]
  })
  studselfverify <- reactive({
    if(is.null(input$day0.file1)) return()
    studselfverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "CITY",
                                      "ST", "P1", "P2", "HS", "DEPART_AIR",
                                      "DEPART_TIME")]
  })
  studlabels <- reactive({
    if(is.null(input$day0.file1)) return()
    studlabels <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "HS",
                                  "DOT")]
  })
  studbalance <- reactive({
    if(is.null(input$day0.file1)) return()
    studbalance <- subset(day0_expect(), !BALANCE=="")
    studbalance <- studbalance[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                                 "HS", "BALANCE")]
  })
  studforms <- reactive({
    if(is.null(input$day0.file1)) return()
    studforms <- subset(day0_expect(), FIN.FORM=='No' | MED.FORM=='No' )
    studforms <- studforms[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                             "HS", "FIN.FORM", "MED.FORM")]
  })
  studdoor <- reactive({
    if(is.null(input$day0.file1)) return()
    studdoor <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "MF",
                                "DORM", "ROOM", "DOT")]
    studdoor <- studdoor[order(studdoor$MF, studdoor$ROOM),]
  })
  d0_room_F <- reactive({
    if(is.null(input$day0.file1)) return()
    
    ##Create list of dorm names for female students
    d0_room_F <- subset(day0_expect(),MF=="Female")
    d0_room_F <- d0_room_F[c("FNAME", "MNAME", "LNAME", "CITY", "ST","DOT", "DORM",
                             "ROOM")]
  })
  d0_room_M <- reactive({
    if(is.null(input$day0.file1)) return()
    
    ##Create list of dorm names for Male students
    d0_room_M <- subset(day0_expect(),MF=="Male")
    d0_room_M <- d0_room_M[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "DOT", "DORM",
                             "ROOM")]
  })
  
  #File Downloads
  output$download_d0_travelverify <- downloadHandler(
    filename = function() {"D0_TravelVerify.csv"},
    content = function(file) {
      write.csv(d0_travelverify(), file, row.names = FALSE)
    }
  )
  output$download_studselfverify <- downloadHandler(
    filename = function() {"StudentSelfVerify.csv"},
    content = function(file) {
      write.csv(studselfverify(), file, row.names = FALSE)
    }
  )
  output$download_studlabels <- downloadHandler(
    filename = function() {"StudentLabels.csv"},
    content = function(file) {
      write.csv(studlabels(), file, row.names = FALSE)
    }
  )
  output$download_studbalance <- downloadHandler(
    filename = function() {"StudentswithBalance.csv"},
    content = function(file) {
      write.csv(studbalance(), file, row.names = FALSE)
    }
  )
  output$download_studforms <- downloadHandler(
    filename = function() {"StudentsMissingForms.csv"},
    content = function(file) {
      write.csv(studforms(), file, row.names = FALSE)
    }
  )
  output$download_studdoor <- downloadHandler(
    filename = function() {"DoorSigns_MM.csv"},
    content = function(file) {
      write.csv(studdoor(), file, row.names = FALSE)
    }
  )
  output$download_d0_room_F <- downloadHandler(
    filename = function() {"Day0_Rooms_F_MM.csv"},
    content = function(file) {
      write.csv(d0_room_F(), file, row.names = FALSE)
    }
  ) 
  output$download_d0_room_M <- downloadHandler(
    filename = function() {"Day0_Rooms_M_MM.csv"},
    content = function(file) {
      write.csv(d0_room_M(), file, row.names = FALSE)
    }
  )

########################################### Page 3 ##################################
#####################################################################################
  
  #Input Files
  ##Takes the input file saving it to day0_expect matrix
  data_staff <- reactive({
    staff1 <- input$proto.file1
    if (is.null(staff1)) return(NULL)
    read.csv(fill=TRUE,file=input$proto.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })  
  ##Create File Summary information
  output$data.staff <- renderTable({
    if(is.null(data_staff())) return ()
    input$proto.file1
  })
  ##Display output for user
  output$confirm.proto.report <- renderUI({
    if(is.null(data_staff())) return()
    tableOutput("data.staff")
  })
  
  #Generate Reports
  output$report <- downloadHandler(
  
    ##For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
    
      # Copy the report file to a temporary directory before processing it
      tempReport <- file.path(tempdir(), "formingthecommunity.Rmd")
      file.copy("formingthecommunity.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n= hq_input())
      
      # Knit the document, passing in the `params` list, and eval it
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
      }
  )
}