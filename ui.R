library(rgl)
library(car)
library(shiny)
library(shinyjs)
library(shinydashboard)
library("RColorBrewer")
library(rJava)
library(ReporteRsjars)
library(ReporteRs)
#######################################################################################
###                                      NOTES                                      ###
#######################################################################################
##to run RJava make sure that the home is set correctly using:
## Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_171')


#######################################################################################
###                                      Code                                      ###
#######################################################################################



#################################HEADER INFORMATION####################################
#######################################################################################
#Create header to display, and disable the sidebar
header <- dashboardHeader(title = "Lorenzo de Zavala Youth Legislative Session")


#################################SideBar INFORMATION####################################
#######################################################################################

##Create the sidebard 
sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem("HQ File Conversion", tabName="HQ"),
  menuItem("Day 0 Admin Tasks", tabName="admin_pre"),
  menuItem("Day1-7 Administrative Tasks", tabName="admin_post"),
  menuItem("Protocol", tabName="protocol"),
  menuItem("Points", tabName="points"),
  menuItem("Awards", tabName="awards")
  )
 )
########################## MainBody Information - Page 1###############################
#######################################################################################
#HQ Demographic File
##File upload of HQ demographic file
hq.input <- fluidRow(
  column(12,
         fileInput("hq.file1","Upload the Student Demographic Database"),
         downloadButton('download_hq', 'Download Database')
  )
)
##Confirm File Upload
hq.confirm <- fluidRow(
  column(8,
         uiOutput("confirm.hq"))
)

#Outputs for Databae Generation
##Generate drop downs for user to choose from to match the headers of selected file to 
###required Expected_StudentDemo file
hq.out.1 <- fluidRow(
  column(4, uiOutput("hq_FNAME")),
  column(4, uiOutput("hq_MNAME")),
  column(4, uiOutput("hq_LNAME"))
  )
hq.out.2 <- fluidRow(
  column(3, uiOutput("hq_MF")),
  column(3, uiOutput("hq_HS")),
  column(3, uiOutput("hq_CITY")),
  column(3, uiOutput("hq_ST"))
)
hq.out.3 <- fluidRow(
  column(4, uiOutput("hq_HOME")),
  column(4, uiOutput("hq_CELL"))
)
hq.out.4 <- fluidRow(
  column(4, uiOutput("hq_FIN.FORM")),
  column(4, uiOutput("hq_MED.FORM")),
  column(4, uiOutput("hq_BALANCE"))
)
hq.out.5 <- fluidRow(
  column(3, uiOutput("hq_P1")),
  column(3, uiOutput("hq_P2")),
  column(3, uiOutput("hq_P1.CELL")),
  column(3, uiOutput("hq_P2.CELL"))
)
hq.out.6 <- fluidRow(
  column(3, uiOutput("hq_ARRIVAL_AIR")),
  column(3, uiOutput("hq_ARRIVAL_TIME")),
  column(3, uiOutput("hq_ARRIVAL_CARRIER")),
  column(3, uiOutput("hq_ARRIVAL_FLIGHT"))
)
hq.out.7 <- fluidRow(
  column(4, uiOutput("hq_DEPART_AIR")),
  column(4, uiOutput("hq_DEPART_TIME"))
)
hq.out.8 <- fluidRow(
  column(4, uiOutput("hq_DORM")),
  column(4, uiOutput("hq_ROOM")),
  column(4, uiOutput("hq_DOT"))
)

#Create one large output box
hq.combo <- fluidRow(
  column(6,
         box(title="Name", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.1),
         box(title="Location", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.2),
         box(title="Contact", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.3),
         box(title="NHI Info", width=NULL, status="primary", collapsible=TRUE,
             solidHeader=TRUE, hq.out.4)
  ),
  column(6,
         box(title = "Parents Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.5),
         box(title = "Arrival Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.6),
         box(title = "Departure Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.7),
         box(title = "Program Info", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, hq.out.8)
  )
)

##Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.hq.1 <- box(title = "Upload Student Database File", width=4, status="primary", 
                solidHeader = TRUE, hq.input)
box.hq.2 <- box(title = "Student Database Confirmation", width=8, status="primary", 
                solidHeader = TRUE, hq.confirm)
box.hq.3 <- box(title = "Ouput Files", width=12, status="primary", collapsible = TRUE, 
                 solidHeader = TRUE, hq.combo)


########################## MainBody Information - Page 2###############################
#######################################################################################

##Expected Student Demographic Database
###File upload of excepted demographics, created from Page1
day0.input <- fluidRow(
  column(12,
      fileInput("day0.file1","Upload the Expected Student Demographic Database")#,
      )
  )
day0.confirm <- fluidRow(
  column(8,
         uiOutput("confirm.day0"))
  )

##Outputs for Registration Tasks
###Form to verify students travel, including phone number, travel plans, and parent contact
day0.out.1 <- fluidRow(
  column(6, actionButton("d0_travelverify", "Create Day0 Travel Verification Docs")),
  downloadButton('download_d0_travelverify', 'Download'),
  hidden(verbatimTextOutput("confirm.d0_travelverify"))
)
###Form for students to update during registration, including parents names, high school, t-shirt
###size and return travel plans
day0.out.2 <- fluidRow(
  column(6, actionButton("studselfverify", "Create Student Self-Verification Forms")),
  downloadButton('download_studselfverify', 'Download'),
  hidden(verbatimTextOutput("confirm.studselfverify"))
)
###Generates labels files for student badges, and any envelopes
day0.out.3 <- fluidRow (
  column(6, actionButton("studlabels", "Create Student Labels")),
  downloadButton('download_studlabels', 'Download'),
  hidden(verbatimTextOutput("confirm.studlabels"))
)
###Generates a list of students with an outstanding balance and generates a list of students
###who have not submitted medical waivers
day0.out.4 <- fluidRow(
  column(6, actionButton("studbalance", "Generate Students with Balance forms")),
  downloadButton('download_studbalance', 'Download'),
  hidden(verbatimTextOutput("confirm.studbalance")),
  column(6, actionButton("studforms", "Generate Students with Missing forms")),
  downloadButton('download_studforms', 'Download'),
  hidden(verbatimTextOutput("confirm.studforms"))
)
###Generates a document that can be used to create student door signs, and two lists of student
###dorm rooms, organized by gender
day0.out.5 <- fluidRow(
  column(6, actionButton("studdoor", "Create Student Door Signs")),
  downloadButton('download_studdoor', 'Download'),
  hidden(verbatimTextOutput("confirm.door_sign")),
  column(6, actionButton("d0_room_F", "Create Rooming Assignment Lists - Female")),
  downloadButton('download_d0_room_F', 'Download'),
  hidden(verbatimTextOutput("confirm.d0_room_F")),
  column(6, actionButton("d0_room_M", "Create Rooming Assignment Lists - Male")),
  downloadButton('download_d0_room_M', 'Download'),
  hidden(verbatimTextOutput("confirm.d0_room_M"))
)

##Create one large output box
day0.out.combo <- fluidRow(
  column(6,
         box(title="Day 0 Travel Verify", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.1),
         box(title="Student Self-Verification", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.2),
         box(title="Student Labels", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.3),
         box(title = "Student Balances & Missing Forms", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.4)
  ),
  column(6,
         box(title = "Door Signs", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.5)
  )
)

##Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.day0.1 <- box(title = "Upload Pre-Registration Database File", width=4, status="primary", 
              solidHeader = TRUE, day0.input)
box.day0.2 <- box(title = "Pre-Registration Database Confirmation", width=8, status="primary", 
              solidHeader = TRUE, day0.confirm)
box.day0.3 <- box(title = "Ouput Files", width=12, status="primary", 
              solidHeader = TRUE, day0.out.combo)
########################## MainBody Information - Page 3###############################
#######################################################################################
##Create rows for file input - Post-Registration
##File upload of registered students demographic
admin.post.reg <- fluidRow(
  column(12,
         fileInput("admin.post.file1", "Upload the Completed Registration Database"),
         actionButton("data.post.reg", "Submit Completed Registration File"),
         hidden(verbatimTextOutput("text2"))
  )
)
admin.post.confirm <- fluidRow(
  column(12,
         uiOutput("post.confirm"))
)

##Ouputs selection for user - Post Registration
admin.post.1 <- fluidRow(
  column(6, actionButton("roomingassign", "Create Rooming Assignment Lists"))
)
admin.post.2 <- fluidRow(
  column(6, actionButton("excurs", "Create Excursion Lists"))
)
admin.post.3 <- fluidRow(
  column(6, actionButton("regcomp", "Create Attending Student Database & Non-attendance database"))
)
###Combine all previous rows together
admin.post.out.combo <- fluidRow(
  column(6,
         box(title="Rooming Assignments", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post.1),
         box(title="Excursion Lists", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post.2)
  ),
  column(6,
         box(title="Attending/Absent Student Demographic Databases", width=NULL, status="primary", 
             collapsible=TRUE, solidHeader = TRUE, admin.post.3)
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options

box.post.1 <- box(title = "Upload Post Registration Database File", width=8, status="primary", 
                  solidHeader = TRUE, admin.post.reg)
box.post.2 <- box(title = "Pre-Registration Database Confirmation", width=4, status="primary", 
                  solidHeader = TRUE, admin.post.confirm)
box.post.3 <- box(title = "Ouput Files", width=12, status="primary", 
                  solidHeader = TRUE, admin.post.out.combo)

########################## MainBody Information - Page 4###############################
#######################################################################################
##Create rows of data
###Create point sheets for the General Assembly
fcol2.1 <- fluidRow(
  column(6,
         fileInput("file2.1.1","Upload the Attending Student Demographic Database"),
         actionButton("points.genass", "Submit Attending Student Demographic File")
  ),
  column(6,
         hidden(verbatimTextOutput("text2"))
         # output("download file for students - general assembly")
        )
)
###Create a Student Demographics + Position Database
fcol2.2 <- fluidRow(
  column(6,
         fileInput("file2.2.1","Upload the Attending Student Demographic Database"),
         fileInput("file2.2.2", "Upload the Student Position Database"),
         actionButton("data.demo.posi", "Submit Attending Student + Position Files")
  ),
  column(6,
         hidden(verbatimTextOutput("text2"))
         #output("download Student Demographic + Position Database")
         )
)
###Create point sheets by position
fcol2.3 <- fluidRow(
  column(6,
         fileInput("file2.3.1","Upload the Student Demographic + Position Database"),
         actionButton("points.pos", "Submit Attending Student + Position Files")
  ),
  column(6,
         hidden(verbatimTextOutput("text2"))
        # output("download Student by position point sheets")
         )
)
###Update database with daily points sheets
fcol2.4 <- fluidRow(
  column(6,
         fileInput("file2.4.1","Upload the Student Demographic + Position Database"),
        # folderloc("folder2.4", "Location of folder with points"),
         actionButton("points.tally", "Submit Points files to be tallied")
  ),
  column(6,
         hidden(verbatimTextOutput("text2"))
         #output("download final points database")
         )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box2.1 <- box(title = "General Assembly Points", width=12, 
              status="primary", solidHeader = TRUE, fcol2.1)
box2.2 <- box(title = "Student Demographic and Position Database", width=12, 
              status="primary", solidHeader = TRUE, fcol2.2)
box2.3 <- box(title = "Daily Points", width=12, 
              status="primary", solidHeader = TRUE, fcol2.3)
box2.4 <- box(title = "Final Points Database", width=12, 
              status="primary", solidHeader = TRUE, fcol2.4)

########################## MainBody Information - Page 5###############################
#######################################################################################
##Create rows of data
###Upload Forming the Community Database
proto.staff1 <- fluidRow(
  column(6,
         fileInput("proto.file1","Upload the Forming the community Database")
  ),
  column(6,
         hidden(verbatimTextOutput("proto.comfirm"))
  )
)

##Ouputs selection for user - all pre-registration tasks
proto.out.1 <- fluidRow(
  column(6, 
         downloadButton('report', 'Genearte Report'),
         hidden(verbatimTextOutput("confirm.proto.report"))
  )
)


###Combine all outputs together
proto.combo<- fluidRow(
  column(6,
         box(title="Forming the Community", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, proto.out.1)
  )
)


##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.proto.1 <- box(title = "Forming the Community", width=12, 
                   status="primary", solidHeader = TRUE, proto.staff1)
box.proto.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, proto.combo)

########################## Output Main Body Information ###############################
#######################################################################################
##Combine all body information, and assign outputs to each appropriate tab 
body <- dashboardBody(
 tabItems(
   tabItem(tabName="HQ",
           box.hq.1, 
           box.hq.2,
           box.hq.3
   ),
   tabItem(tabName="admin_pre",
          box.day0.1, 
          box.day0.2,
          box.day0.3
          ),
  tabItem(tabName="admin_post",
          box.post.1,
          box.post.2,
          box.post.3
          ),
  tabItem(tabName="protocol",
          box.proto.1,
          box.proto.2
  )
 )
)
dashboardPage(header,sidebar,body)