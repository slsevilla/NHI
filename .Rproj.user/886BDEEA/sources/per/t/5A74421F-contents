library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)

#######################################################################################
###                                      NOTES                                      ###
#######################################################################################
##This code is written to take in the generated pCOA weighted unifrac values and generate
##a pCOA plot using the top three values. It will also allow users to upload a labels 
##document, which will allow for additional visualization of categorical labels.

#######################################################################################
###                                      Code                                      ###
#######################################################################################

function(input,output, session){
 
 ########################################################################
 #############################Input Files Page###########################
 
 #Takes the input file of filled with any registration data
 data_complete <- reactive({
  file <- input$filedir
  if (is.null(filedir))
   return(NULL)
  
  data_table <- read.table(fill=TRUE, file=input$filedir$datapath)
  return(data_table)
 })
 
 #Display the summary for the PCOA and Lables files provided by the user
 #Each file becomes separate row of summary information to view
 output$filedir <- renderTable({
  if(is.null(data_complete())) return ()
  input$filedir
 })
 output$table <- renderUI({
  if(is.null(data_complete())) return()
  else
   tabsetPanel(
    tabPanel("File Input Summary", tableOutput("filedir"))
   )
 })
 
 #Displays a confirmation message to user to continue with tasks
 observeEvent(input$goButton,{
  output$text <- renderText({
   "Directory has been uploaded - Please continue with tasks"})  
 })