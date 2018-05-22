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
  observeEvent(input$door,{
    output$confirm.door <- renderText({
      "Upload Complete - Download Door signs"})  
  })
  
  #Displays a confirmation message for the door signs
  observeEvent(input$studlabels,{
    output$confirm.studlabels <- renderText({
      "Upload Complete - Download Student Labels"})  
  })
  
  #Displays a confirmation message for the door signs
  observeEvent(input$roomassign,{
    output$confirm.roomassign <- renderText({
      "Upload Complete - Download Rooming Assignment Sheets"})  
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
  student_door <- reactive({
    if(is.null(input$admin.pre.file1)) return()
    student_door <- data_expect()[c("FNAME", "MNAME", "LNAME", "HS", "CITY", "ST")]
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
  output$download_door <- downloadHandler(
    filename = function() {"DoorSigns_MM.csv"},
    content = function(file) {
      write.csv(student_door(), file, row.names = FALSE)
    }
  )
}