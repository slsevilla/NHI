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
  
  #Displays a confirmation message for the door signs
  observeEvent(input$door_sign,{
    output$confirm.door_sign <- renderText({
      "Upload Complete - Download Door signs"})  
  })
  
  #Displays a confirmation message for the rooming lists
  observeEvent(input$door_room,{
    output$confirm.door_room <- renderText({
      "Upload Complete - Download Rooming Lists"})  
  })
  
  #Displays a confirmation message for the door signs
  observeEvent(input$studlabels,{
    output$confirm.studlabels <- renderText({
      "Upload Complete - Download Student Labels"})  
  })
  
  #Displays a confirmation message for the registration  signs
  observeEvent(input$missingforms,{
    output$confirm.missingforms <- renderText({
      "Upload Complete - Download Missing Forms List"})  
  })
  #Displays a confirmation message for the registration  signs
  observeEvent(input$balancesheet,{
    output$confirm.balancesheet <- renderText({
      "Upload Complete - Download Students with Balance List"})  
  })
  
  ###Create sub-tables
  ########################################################
  student_owes <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    student_owes <- subset(data_expect(), !BALANCE=='$ -')
  })
  student_forms <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    student_forms <- subset(data_expect(), FIN.FORM=='No' | MED.FORM=='No' )
  })
  student_door_sign <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    student_door_sign <- data_expect()[c("FNAME", "MNAME", "LNAME", "HS", "CITY", "ST")]
  })
  student_door_room <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    
    #Create list of dorm names - will generate individual excel files for each dorm
    dorm_names <- data_expect()["DORM"]
    dorm_names <- unique(dorm_names)
  })
  

  #####File Downloads
  ########################################################
  output$download_balancesheet <- downloadHandler(
    filename = function() {"StudentswithBalance.csv"},
    content = function(file) {
      write.csv(student_owes(), file, row.names = FALSE)
    }
  )
  output$download_missingforms <- downloadHandler(
    filename = function() {"StudentsMissingForms.csv"},
    content = function(file) {
      write.csv(student_forms(), file, row.names = FALSE)
    }
  )
  output$download_door_sign <- downloadHandler(
    filename = function() {"DoorSigns_MM.csv"},
    content = function(file) {
      write.csv(student_door_sign(), file, row.names = FALSE)
    }
  )
  output$download_door_room <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in c(1,2,3,4,5)) {
        path <- paste0("sample_", i, ".csv")
        fs <- c(fs, path)
        write(i*2, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  output$test <- downloadHandler(
    filename = function() {"DoorSigns_MM.csv"},
    content = function(file) {
      write.csv(student_door_room(), file, row.names = FALSE)
    }
  )

}