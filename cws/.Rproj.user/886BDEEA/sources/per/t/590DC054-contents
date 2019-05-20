library(rgl)
library(car)
library(shiny)
library(shinyjs)
library(shinydashboard)
library("RColorBrewer")
library(rJava)
library(ReporteRsjars)
library(ReporteRs)
#####################################################################################################
#####################################################################################################
###                                            NOTES                                            ###
###################################################################################################
####################################################################################################
##to run RJava make sure that the home is set correctly using:
## Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_171')


#################################################################################################
#################################################################################################
###                                          Code                                             ###
#################################################################################################
################################################################################################

###########################               HEADER               #################################
#################################################################################################
#Create header to display, and disable the sidebar
header <- dashboardHeader(title = "Collegiate World Series - Draft")

###########################                 SIDEBAR                  ############################
#################################################################################################

##Create the sidebard 
sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem("Student Demographic Creation", tabName="Demo File"),
  menuItem("Draft Selection", tabName="Draft Students")
  )
 )

###########################        MainBody - Student Draft      #######################
#################################################################################################

######################################### Input
##File upload of student demographic file
input_student <- fluidRow(
  column(12,
         fileInput("file_stud","Upload the Student Demographic File (MUST be a CSV file)")
  )
)

input_coach <- fluidRow(
  column(12,
         fileInput("file_coach","Upload the Coach Information file (MUST be a CSV file)")
  )
)

output_student <- fluidRow(
  column(12,
         downloadButton('download_stud', 'Download new student database')
  )
)

output_coach <- fluidRow(
  column(12,
         downloadButton('download_coach','Download new coach database')
  )
)



######################################### Outputs for Database
#Generate drop downs for user to choose from to match the headers of selected file to 
#required Expected_StudentDemo file
demo.out.1 <- fluidRow(
  column(4, uiOutput("stud_FNAME")),
  column(4, uiOutput("stud_MNAME")),
  column(4, uiOutput("stud_LNAME"))
  )
demo.out.2 <- fluidRow(
  column(3, uiOutput("stud_MF")),
  column(3, uiOutput("stud_HS")),
  column(3, uiOutput("stud_CITY")),
  column(3, uiOutput("stud_ST"))
)
demo.out.3 <- fluidRow(
  column(4, uiOutput("coach_FNAME")),
  column(4, uiOutput("coach_MNAME")),
  column(4, uiOutput("coach_LNAME"))
)
demo.out.3 <- fluidRow(
  column(4, uiOutput("coach_Univ"))
)

######################################### Create one large output box
demo.combo <- fluidRow(
  column(6,
         box(title="Student Name", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, demo.out.1),
         box(title="Student Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, demo.out.2)
  ),
  column(6,
         box(title = "Coach Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, demo.out.3),
         box(title = "Coach location", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.4)
  )
)

######################################### Create page boxes
#Generate Boxes for text submission and user downloads
box.demo.1 <- box(title = "Upload Student and coach Database Files", width=12, status="primary", 
                solidHeader = TRUE, stud.input)
box.demo.2 <- box(title = "Demographic Update", width=12, status="primary", collapsible = TRUE, 
                 solidHeader = TRUE, demo.combo)

########################## Output Main Body Information ###############################
#######################################################################################
##Combine all body information, and assign outputs to each appropriate tab 
body <- dashboardBody(
 tabItems(
   tabItem(tabName="Demo File",
           box.demo.1, 
           box.demo.2
   )
 )
)
dashboardPage(header,sidebar,body)