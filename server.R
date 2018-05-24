library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)

#######################################################################################
###                                      NOTES                                      ###
#######################################################################################
##

#######################################################################################
###                                      Code                                      ###
#######################################################################################

function(input,output, session){
 
 ###################################################################################################################
 ####################################################### Page 1 ####################################################
  #Takes the input file saving it to data_expect matrix
  data_expect <- reactive({
    admin1 <- input$admin.pre.file1
    if (is.null(admin1)) return(NULL)
    read.csv(fill=TRUE,file=input$admin.pre.file1$datapath, header=TRUE, colClasses = "factor")
  })  
  #Create File Summary information
  output$data.expect <- renderTable({
    if(is.null(data_expect())) return ()
    input$admin.pre.file1
  })
  #Display output for user
  output$pre.confirm <- renderUI({
    if(is.null(data_expect())) return()
    tableOutput("data.expect")
  })
  
  #Data Confirmations 
  ########################################################
  
  #Displays a confirmation message for the travel verification
  observeEvent(input$d0_travelverify,{
    output$confirm.d0_travelverify <- renderText({
      "Upload Complete - Download D0 Travel Verification"})  
  })
  
  #Displays a confirmation message for the student self verification
  observeEvent(input$studselfverify,{
    output$confirm.studselfverify <- renderText({
      "Upload Complete - Download Student Self-Verification Forms"})  
  })
  
  #Displays a confirmation message for the door signs
  observeEvent(input$studlabels,{
    output$confirm.studlabels <- renderText({
      "Upload Complete - Download Student Labels"})  
  })
  #Displays a confirmation message for the registration  signs
  observeEvent(input$studbalance,{
    output$confirm.studbalance <- renderText({
      "Upload Complete - Download Students with Balance List"})  
  })
  #Displays a confirmation message for the registration  signs
  observeEvent(input$studforms,{
    output$confirm.studforms <- renderText({
      "Upload Complete - Download Missing Forms List"})  
  })

  #Displays a confirmation message for the door signs
  observeEvent(input$door_sign,{
    output$confirm.door_sign <- renderText({
      "Upload Complete - Download Door signs"})  
  })
  
  #Displays a confirmation message for the rooming lists on Day 0- female
  observeEvent(input$d0_room_F,{
    output$confirm.d0_room_F <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Female Students"})  
  })
  
  #Displays a confirmation message for the rooming lists on Day 0- Male
  observeEvent(input$d0_room_M,{
    output$confirm.d0_room_M <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Male Students"})  
  })
  ###Create sub-tables
  ########################################################
  d0_travelverify <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    d0_travelverify <- data_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "P1_Cell", "P2_Cell",
                                       "Arrival_Airport", "Trans_arrival_time", "Trans_arrival_carrier_name")]
  })
  studselfverify <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    studselfverify <- data_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST", "P1", "P2", "HS",
                                         "Depart_Airport", "Trans_depart_time")]
  })
  studlabels <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    studlabels <- data_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "HS", "Dots")]
  })
  studbalance <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    studbalance <- subset(data_expect(), !BALANCE=='$ -')
    studbalance <- studbalance[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST","HS", "BALANCE")]
  })
  studforms <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    studforms <- subset(data_expect(), FIN.FORM=='No' | MED.FORM=='No' )
    studforms <- studforms[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST","HS", "FIN.FORM", "MED.FORM")]
  })
  studdoor <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    studdoor <- data_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "Dots", "Dorm", "Room", "MF")]
    studdoor <- studdoor[order(studdoor$MF, studdoor$Room),]
  })
  d0_room_F <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    
    #Create list of dorm names for female students
    d0_room_F <- subset(data_expect(),MF=="Female")
    d0_room_F <- d0_room_F[c("FNAME", "MNAME", "LNAME", "CITY", "ST","Dots", "Dorm", "Room")]
  })
  d0_room_M <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    
    #Create list of dorm names for Male students
    d0_room_M <- subset(data_expect(),MF=="Male")
    d0_room_M <- d0_room_M[c("FNAME", "MNAME", "LNAME", "CITY", "ST","Dots", "Dorm", "Room")]
  })
  

  #####File Downloads
  ########################################################
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

  ####################################################### Page 4 ####################################################
  ###################################################################################################################
  
  ###Input Files
  ###############################################################################################################
  #Takes the input file saving it to data_expect matrix
  data_staff <- reactive({
    staff1 <- input$proto.file1
    if (is.null(staff1)) return(NULL)
    read.csv(fill=TRUE,file=input$proto.file1$datapath, header=TRUE, colClasses = "factor")
  })  
  #Create File Summary information
  output$data.staff <- renderTable({
    if(is.null(data_staff())) return ()
    input$proto.file1
  })
  #Display output for user
  output$confirm.proto.report <- renderUI({
    if(is.null(data_staff())) return()
    tableOutput("data.staff")
  })
  
  ###Generate Reports
  output$report <- downloadHandler(
  
    # For PDF output, change this to "report.pdf"
  filename = "report.docx",
  
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "formingthecommunity.Rmd")
    file.copy("formingthecommunity.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(n= studforms(),
                   data = studforms())
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
                      )
    }
  )
}