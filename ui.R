library(rgl)
library(car)
library(shiny)
library(shinyjs)
library(shinydashboard)
library("RColorBrewer")
#######################################################################################
###                                      NOTES                                      ###
#######################################################################################


#######################################################################################
###                                      Code                                      ###
#######################################################################################



#######################################################################################
#################################HEADER INFORMATION####################################

#Create header to display, and disable the sidebar
header <- dashboardHeader(title = "Lorenzo de Zavala Youth Legislative Session")


#######################################################################################
#################################SideBar INFORMATION####################################

##Create the sidebard 
sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem("Administrative", tabName="admin"),
  menuItem("Points", tabName="points"),
  menuItem("Awards", tabName="awards")
  )
 )

#######################################################################################
########################## MainBody Information - Page 1###############################

##Create rows of data
###File upload
fcol1.1 <- fluidRow(
  column(12,
         fileInput("filedir","Upload the Student Demographic Database"),
         fileInput("filereg", "Upload the Completed Registration Database"),
         actionButton("data.dem", "Submit Demographic File (Required)"),
         actionButton("data.reg", "Submit Registration File (Optional)")
         )
  )
##Confirmation of accepted database
fcol1.2 <- fluidRow(
  column(12,
         hidden(
           verbatimTextOutput("text"))
  )
)
##Ouputs selection for user
fcol1.3 <- fluidRow(
  column(6, actionButton("doorsign", "Create Student Door Signs"))
)
fcol1.4 <- fluidRow (
  column(6, actionButton("studlabels", "Create Student Labels"))
)
fcol1.5 <- fluidRow(
  column(6, actionButton("regforms", "Generate Registration, and missing forms documents"))
)
fcol1.6 <- fluidRow(
  column(6, actionButton("roomingassign", "Create Rooming Assginment Lists"))
)
fcol1.7 <- fluidRow(
  column(6, actionButton("excurs", "Create Excursion Lists"))
)
fcol1.8 <- fluidRow(
  column(6, actionButton("regcomp", "Create Attending Student Database & Non-attendance database"))
)
###Combine all previous rows together
fcol1combined <- fluidRow(
  column(6,
         box(title="Door Signs", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, fcol1.4),
         box(title = "Student Labels", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, fcol1.4),
         box(title = "Registration Database & Forms", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, fcol1.5)
  ),
  column(6,
         box(title="Rooming Assignments", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, fcol1.6),
         box(title="Excursion Lists", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, fcol1.7),
         box(title="Attending/Abasent Student Demographics", width=NULL, status="primary", 
             collapsible=TRUE, solidHeader = TRUE, fcol1.8)
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box1.1 <- box(title = "Upload Database File", width=4, status="primary", 
              solidHeader = TRUE, fcol1.1)
box1.2 <- box(title = "Database Confirmation", width=8, status="primary", 
              solidHeader = TRUE, fcol1.2)
box1.3 <- box(title = "Ouput Files", width=8, status="primary", 
              solidHeader = TRUE, fcol1combined)

#######################################################################################
########################## MainBody Information - Page 2###############################
##Create rows of data
###File upload
fcol1.1 <- fluidRow(
  column(12,
         fileInput("filedir","Upload the Student Demographic Database"),
         actionButton("goButton", "Submit Demographic File")
  )
)
○ 1
§ Input
□ Excel 
□ sheet of active students
§ Output
□ Excel
□ Sheets for student by last name for general assembly point keeping
○ 2 
□ Input
□ Excel
□ Student demographic database
□ Student Positions database
□ Output
□ Excel
□ Student position + demographic database
○ 3
§ Input
□ Excel
□ Student position + demographic database
§ Output
□ Excel
□ Student sheets broken up my position and listed categories of specific points
○ 4
§ Input
□ 1
® Excel
® Student position + demographic database
□ 2
® Folder (Excel files)
® Student points
§ Output
□ Excel 
Student final points database


##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box1.1 <- box(title = "Upload Database File", width=4, status="primary", solidHeader = TRUE, fcol1.1)
box1.2 <- box(title = "Database Confirmation", width=8, status="primary", solidHeader = TRUE, fcol1.2)
box1.3 <- box(title = "Ouput Files", width=8, status="primary", solidHeader = TRUE, fcol1combined)



#######################################################################################
########################## Output Main Body Information ###############################

##Combine all body information, and assign outputs to each appropriate tab 
body <- dashboardBody(
 tabItems(
  tabItem(tabName="admin",
          box1.1, 
          box1.2,
          box1.3
          )
 )
)
dashboardPage(header,sidebar,body)