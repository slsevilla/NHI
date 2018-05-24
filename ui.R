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



#######################################################################################
#################################HEADER INFORMATION####################################

#Create header to display, and disable the sidebar
header <- dashboardHeader(title = "Lorenzo de Zavala Youth Legislative Session")


#######################################################################################
#################################SideBar INFORMATION####################################

##Create the sidebard 
sidebar <- dashboardSidebar(
 sidebarMenu(
  menuItem("Day 0 Admin Tasks", tabName="admin_pre"),
  menuItem("Day1-7 Administrative Tasks", tabName="admin_post"),
  menuItem("Protocol", tabName="protocol"),
  menuItem("Points", tabName="points"),
  menuItem("Awards", tabName="awards")
  )
 )

########################## MainBody Information - Page 1###############################
#######################################################################################

##Create rows for file input - Pre-Registration
###File upload of excepted demographic
admin.pre.reg <- fluidRow(
  column(12,
      fileInput("admin.pre.file1","Upload the Expected Student Demographic Database")#,
      )
  )
admin.pre.confirm <- fluidRow(
  column(8,
         uiOutput("pre.confirm"))
  )

##Ouputs selection for user - all pre-registration tasks
admin.pre.out.1 <- fluidRow(
  column(6, actionButton("d0_travelverify", "Create Day0 Travel Verification Docs")),
  downloadButton('download_d0_travelverify', 'Download'),
  hidden(verbatimTextOutput("confirm.d0_travelverify"))
)
admin.pre.out.2 <- fluidRow(
  column(6, actionButton("studselfverify", "Create Student Self-Verification Forms")),
  downloadButton('download_studselfverify', 'Download'),
  hidden(verbatimTextOutput("confirm.studselfverify"))
)
admin.pre.out.3 <- fluidRow (
  column(6, actionButton("studlabels", "Create Student Labels")),
  downloadButton('download_studlabels', 'Download'),
  hidden(verbatimTextOutput("confirm.studlabels"))
)
admin.pre.out.4 <- fluidRow(
  column(6, actionButton("studbalance", "Generate Students with Balance forms")),
  downloadButton('download_studbalance', 'Download'),
  hidden(verbatimTextOutput("confirm.studbalance")),
  column(6, actionButton("studforms", "Generate Students with Missing forms")),
  downloadButton('download_studforms', 'Download'),
  hidden(verbatimTextOutput("confirm.studforms"))
)
admin.pre.out.5 <- fluidRow(
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


###Combine all previous rows together
admin.pre.out.combo <- fluidRow(
  column(6,
         box(title="Day 0 Travel Verify", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.pre.out.1),
         box(title="Student Self-Verification", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.pre.out.2),
         box(title="Student Labels", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.pre.out.3),
         box(title = "Student Balances & Missing Forms", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.pre.out.4)
  ),
  column(6,
         box(title = "Door Signs", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.pre.out.5)
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.pre.1 <- box(title = "Upload Pre-Registration Database File", width=4, status="primary", 
              solidHeader = TRUE, admin.pre.reg)
box.pre.2 <- box(title = "Pre-Registration Database Confirmation", width=8, status="primary", 
              solidHeader = TRUE, admin.pre.confirm)
box.pre.3 <- box(title = "Ouput Files", width=12, status="primary", 
              solidHeader = TRUE, admin.pre.out.combo)

########################## MainBody Information - Page 2###############################
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

########################## MainBody Information - Page 3###############################
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

########################## MainBody Information - Page 4###############################
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
  tabItem(tabName="admin_pre",
          box.pre.1, 
          box.pre.2,
          box.pre.3
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