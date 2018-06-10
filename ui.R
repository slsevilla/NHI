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
  menuItem("HQ Database Conversion", tabName="HQ"),
  menuItem("Staff Database Conversion", tabName="Staff"),
  menuItem("Day 0 Admin Tasks", tabName="admin_pre"),
  menuItem("Day1-7 Administrative Tasks", tabName="admin_post"),
  menuItem("Protocol", tabName="protocol"),
  menuItem("Election", tabName="elect"),
  menuItem("Awards", tabName="awards"),
  menuItem("Merchandise", tabName="merch"),
  menuItem("Legislative", tabName="Leg")
  )
 )

########################## MainBody Information - HQ Database Conversion###############################
#######################################################################################################
###############################Input
##File upload of HQ demographic file
hq.input <- fluidRow(
  column(12,
         fileInput("hq.file1","Upload the Student Demographic Database"),
         downloadButton('download_hq', 'Download Database')
  )
)

################################ Outputs for Database Generation
#Generate drop downs for user to choose from to match the headers of selected file to 
#required Expected_StudentDemo file
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

################################ Create one large output box
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

################################# Create page boxes
#Generate Boxes for text submission and user downloads
box.hq.1 <- box(title = "Upload Student Database File", width=12, status="primary", 
                solidHeader = TRUE, hq.input)
box.hq.2 <- box(title = "Ouput Files", width=12, status="primary", collapsible = TRUE, 
                 solidHeader = TRUE, hq.combo)


########################## MainBody Information - Staff Database Conversion###############################
#######################################################################################
################################Staff Demographic File
##File upload of Staff demographic file
staff.db.input <- fluidRow(
  column(12,
         fileInput("staff.db1","Upload the Staff Info Database"),
         downloadButton('download_staffdemo', 'Download Database')
  )
)
##Confirm File Upload
staff.confirm <- fluidRow(
  column(8,
         uiOutput("confirm.staffdemo"))
)

#################################Outputs for Database Generation
#Generate drop downs for user to choose from to match the headers of selected file to 
#required StaffDemo file
staff.out.1 <- fluidRow(
  column(4, uiOutput("staff_FNAME")),
  column(4, uiOutput("staff_MNAME")),
  column(4, uiOutput("staff_LNAME"))
)
staff.out.2 <- fluidRow(
  column(6, uiOutput("staff_CITY")),
  column(6, uiOutput("staff_ST"))
)
staff.out.3 <- fluidRow(
  column(6, uiOutput("staff_HSSTAT")),
  column(6, uiOutput("staff_HS"))
)
staff.out.4 <- fluidRow(
  column(4, uiOutput("staff_COLSTAT")),
  column(4, uiOutput("staff_UNIV")),
  column(4, uiOutput("staff_MAJ"))
)
staff.out.5 <- fluidRow(
  column(6, uiOutput("staff_STAT")),
  column(6, uiOutput("staff_ROLE"))
)
staff.out.6 <- fluidRow(
  column(4, uiOutput("staff_GD")),
  column(4, uiOutput("staff_LDZ")),
  column(4, uiOutput("staff_CWS"))
)
#################################Create one large output box
staff.combo <- fluidRow(
  column(6,
         box(title="Name", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.1),
         box(title="Location", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.2),
         box(title="Role", width=NULL, status="primary", collapsible=TRUE,
             solidHeader=TRUE, staff.out.5)
  ),
  column(6,
         box(title="High School", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.3),
         box(title="College", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.4),
         box(title="NHI Info", width=NULL, status="primary", collapsible=TRUE,
             solidHeader=TRUE, staff.out.6)
  )
)

##################################Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.staff.1 <- box(title = "Upload Staff Database File", width=4, status="primary", 
                solidHeader = TRUE, staff.db.input)
box.staff.2 <- box(title = "Ouput Files", width=12, status="primary", collapsible = TRUE, 
                solidHeader = TRUE, staff.combo)

########################## MainBody Information - Day 0 Admin Tasks##############################
#######################################################################################

############################### Input File
###File upload of excepted demographics, created from Page1
day0.input <- fluidRow(
  column(6,fileInput("day0.file1","Upload the Expected Student Demographic Database")),
  column(6,fileInput("day0.file2", "Upload the Staff Demographic Database"))
  )

############################### Outputs for Registration Tasks
#Form to verify students travel, including phone number, travel plans, and parent contact
day0.out.1 <- fluidRow(
  downloadButton('download_d0_travelverify', 'Download Travel Verification')
  )
#Form for students to update during registration, including parents names, high school, t-shirt
###size and return travel plans
day0.out.2 <- fluidRow(
  downloadButton('download_studselfverify', 'Download Student Self-Verification')
  )
#Generates labels files for student badges, and any envelopes
day0.out.3 <- fluidRow (
  downloadButton('download_studlabels', 'Download Student Badges')
  )
#Generates a list of students with an outstanding balance and generates a list of students
#who have not submitted medical waivers
day0.out.4 <- fluidRow(
  downloadButton('download_studbalance', 'Download Student Balance Forms'),
  downloadButton('download_studforms', 'Download Student Missing Forms')
  )
#Generates a document that can be used to create student door signs, and two lists of student
#dorm rooms, organized by gender
day0.out.5 <- fluidRow(
  downloadButton('download_studdoor', 'Download Student Door Signs'),
  downloadButton('download_d0_room_F', 'Download Rooming Lists - Female'),
  downloadButton('download_d0_room_M', 'Download Rooming Lists - Male')
  )
#Generates labels for the staff badges
day0.out.6 <- fluidRow(
  downloadButton('download_stafflabels', 'Download Staff Labels')
  )

############################### Create one large output box
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
             solidHeader = TRUE, day0.out.5),
         box(title = "Staff Labels", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, day0.out.6)
  )
)

############################### Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.day0.1 <- box(title = "Upload Pre-Registration Database File", width=12, status="primary", 
              solidHeader = TRUE, day0.input)
box.day0.2 <- box(title = "Ouput Files", width=12, status="primary", 
              solidHeader = TRUE, day0.out.combo)

########################## MainBody Information - Day 1-7 Admin Tasks###############################
#######################################################################################
##Create rows for file input - Post-Registration
##File upload of registered students demographic
admin.post.reg <- fluidRow(
  column(12,
         fileInput("admin.post.file1", "Upload the Completed Registration Database")
  )
)

##Ouputs selection for user - Post Registration
admin.post.1 <- fluidRow(
  column(6, downloadButton('download_post_registered', 'Download Student Registered File')),
  column(6, downloadButton('download_post_nonattend', 'Download Student Non-Attend File'))
)

###Combine all previous rows together
admin.post.out.combo <- fluidRow(
  column(6,
         box(title="Registration Status", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post.1)
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options

box.post.1 <- box(title = "Upload Post Registration Database File", width=8, status="primary", 
                  solidHeader = TRUE, admin.post.reg)
box.post.2 <- box(title = "Ouput Files", width=12, status="primary", 
                  solidHeader = TRUE, admin.post.out.combo)

########################## MainBody Information - Awards###############################
#######################################################################################
############################### #Formining the Community Points
points.1 <- fluidRow(
  column(6,
         fileInput("points.file1","Upload the Attending Student Demographic File")
  ),
  column(6,
         downloadButton('download_points_ftc', 'Download FTC Point Sheets')
  )
)
############################### #General Convention
points.2 <- fluidRow(
  column(6,
         fileInput("points.file2","Upload the Registered Student Demographic File")
  ),
  column(3,
         uiOutput("gc.day")
  ),
  column(3,
         downloadButton('download_points_gc', 'Download GC Point Sheets')
  )
)
############################### #Legislative Session
points.3 <- fluidRow(
  column(6,
         fileInput("points.file4","Upload the Position Student Demographic File")
  ),
  column(3,
         uiOutput("leg.day")
  ),
  column(3,
         downloadButton('download_points_leg', 'Download Leg Point Sheets')
  )
)

############################### #Voting
points.4 <- fluidRow(
  column(6,
         fileInput("points.file5","Upload the Position Student Demographic File")
  ),
  column(3,
         downloadButton('download_points_voting_house', 'Download House Voting File')
  ),
  column(3,
         downloadButton('download_points_voting_senate', 'Download Senate Voting File')
  ),
  column(3,
         downloadButton('download_points_voting_executive', 'Download Executive Voting File')
  ),
  column(3,
         downloadButton('download_points_voting_judicial', 'Download Judicial Voting File')
  )
)


############################### #Daily Totals
points.out1 <- fluidRow(
  column(6,
         fileInput("points.file8","Upload the Forming the Community File")
  ),
  column(6,
         downloadButton('download_points_day1', 'Download Day 1 Totals')
  )
)
points.out2 <- fluidRow(
  column(6,
         fileInput("points.file9.1","Upload the GC Day 2 File")
  ),
  column(6,
         fileInput("points.file9.2","Upload the Election_Nomination File")
  ),
  column(3,
         downloadButton('download_points_day2', 'Download Day 2 Totals')
  )
)
points.out3 <- fluidRow(
  column(6,
         fileInput("points.file10.1","Upload the GC Day 3 File")
  ),
  column(6,
         fileInput("points.file10.2","Upload the Election_Final File")
  ),
  column(3,
         downloadButton('download_points_day3', 'Download Day 3 Totals')
  )
)
points.out4 <- fluidRow(
  column(6,
         fileInput("points.file13","Upload the Leg Day 4 File")
  ),
  column(6,
         downloadButton('download_points_day4', 'Download Day 4 Totals')
  )
)
points.out5 <- fluidRow(
  column(6,
         fileInput("points.file14","Upload the Leg Day 5 File")
  ),
  column(6,
         downloadButton('download_points_day5', 'Download Day 5 Totals')
  )
)
points.out6 <- fluidRow(
  column(3,
         fileInput("points.file15","Upload the Voting House File")
  ),
  column(3,
         fileInput("points.file16","Upload the Voting Senate File")
  ),
  column(3,
         fileInput("points.file17","Upload the Voting Judicial File")
  ),
  column(3,
         fileInput("points.file18","Upload the Voting Executive File")
  ),
  column(4,
         fileInput("points.file19","Upload the Position StudentDemo File")
  ),
  column(4,
         fileInput("points.file20","Upload the Leg Session Day 6 File")
  ),
  column(12,
         downloadButton('download_points_day6', 'Download Day 6 Totals')
  )
)

############################### Combine all outputs together
points.combo<- fluidRow(
  column(12,
         box(title="Day 1 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out1, collapsed = TRUE)
  ),
  column(12,
         box(title="Day 2 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out2, collapsed = TRUE)
  ),
  column(12,
         box(title="Day 3 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out3, collapsed = TRUE)
  ),
  column(12,
         box(title="Day 4 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out4, collapsed = TRUE)
  ),
  column(12,
         box(title="Day 5 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out5, collapsed = TRUE)
  ),
  column(12,
         box(title="Day 6 Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, points.out6)
  )
  
)

################################ Generate Boxes for text submission, verification, and user options
box.points.1 <- box(title = "Forming the Community", width=12, 
              status="primary", solidHeader = TRUE, points.1)
box.points.2 <- box(title = "General Convention", width=12, 
                    status="primary", solidHeader = TRUE, points.2)
box.points.3 <- box(title = "Legislative Session", width=12, 
                    status="primary", solidHeader = TRUE, points.3)
box.points.4 <- box(title = "Legislative Session", width=12, 
                    status="primary", solidHeader = TRUE, points.4)
box.points.7 <- box(title = "Daily Totals", width=12, 
                    status="primary", solidHeader = TRUE, points.combo)

########################## MainBody Information - Protocol###############################
#######################################################################################
##Create rows of data
###Upload Forming the Community Database
proto.ftc <- fluidRow(
  column(6,
         fileInput("proto.file1","Upload the StudentDemo_Expected File")
  ),
  column(6,
         fileInput("proto.file2","Upload the StaffDemo File")
  ),
  column(12,
         fileInput("proto.file3","Upload the FTC Template File")
  )
)

##Ouputs selection for user - all pre-registration tasks
proto.out.1 <- fluidRow(
  column(6, 
         downloadButton('download_ftc_protocol', 'Download FTC_Protocol')
  ),
  column(6, 
         downloadButton('download_ftc_staff', 'Download FTC_Staff')
  )
)


###Combine all outputs together
proto.combo<- fluidRow(
  column(12,
         box(title="Forming the Community Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, proto.out.1)
  )
)


##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.proto.1 <- box(title = "Forming the Community", width=12, 
                   status="primary", solidHeader = TRUE, proto.ftc)
box.proto.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, proto.combo)

########################## MainBody Information - Merchandise###############################
#######################################################################################
################################ Input Files
#Import Merchandise Ledger and Inventory File
merch.inv <- fluidRow(
  column(6,
         fileInput("merch.file1","Upload the Merchandise Ledger File")
  ),
 column(6,
         fileInput("merch.file2","Upload the Inventory File")
  )
)

###############################Ouputs selection for user 
merch.out.1 <- fluidRow(
  column(12, downloadButton('download_merch_ledger', 'Download Merch Ledger'))
  )
merch.out.2 <- fluidRow(
  column(6,uiOutput("merch.day")),
  column(6, downloadButton('download_merch_inv', 'Download Inventory'))
  )

############################### Combine all outputs together
merch.combo<- fluidRow(
  column(12,
         box(title="Financial Ledger", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, merch.out.1)
  ),
  column(12,
         box(title="Inventory Worksheets", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, merch.out.2)
  )
)


############################### Create boxes
###Generate Boxes for text submission, verification, and user options
box.merch.1 <- box(title = "Merchandise Inputs", width=12, 
                   status="primary", solidHeader = TRUE, merch.inv)
box.merch.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, merch.combo)

########################## MainBody Information - Elections###############################
#######################################################################################
##Create rows of data
###Upload Forming the Community Database
merch.inv <- fluidRow(
  column(6,
         fileInput("merch.file1","Upload the Merchandise Ledger File")
  ),
  column(6,
         fileInput("merch.file2","Upload the Inventory File")
  )
)

##Ouputs selection for user - all pre-registration tasks
merch.out.1 <- fluidRow(
  column(12, 
         downloadButton('download_merch_ledger', 'Download Merch Ledger')
  )
)
merch.out.2 <- fluidRow(
  column(6, 
         uiOutput("merch.day")
  ),
  column(6, 
         downloadButton('download_merch_inv', 'Download Inventory')
  )
  
)

###Combine all outputs together
merch.combo<- fluidRow(
  column(12,
         box(title="Financial Ledger", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, merch.out.1)
  ),
  column(12,
         box(title="Inventory Worksheets", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, merch.out.2)
  )
)


##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.merch.1 <- box(title = "Merchandise Inputs", width=12, 
                   status="primary", solidHeader = TRUE, merch.inv)
box.merch.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, merch.combo)

########################## Output Main Body Information ###############################
#######################################################################################
##Combine all body information, and assign outputs to each appropriate tab 
body <- dashboardBody(
 tabItems(
   tabItem(tabName="HQ",
           box.hq.1, 
           box.hq.2
   ),
   tabItem(tabName="Staff",
           box.staff.1, 
           box.staff.2
   ),
   tabItem(tabName="admin_pre",
          box.day0.1, 
          box.day0.2
          ),
  tabItem(tabName="admin_post",
          box.post.1,
          box.post.2
          ),
  tabItem(tabName="protocol",
          box.proto.1,
          box.proto.2
  ),
  tabItem(tabName="awards",
          box.points.1,
          box.points.2,
          box.points.3,
          box.points.4,
          box.points.7
  ),
  tabItem(tabName="merch",
          box.merch.1,
          box.merch.2
  )
 )
)
dashboardPage(header,sidebar,body)