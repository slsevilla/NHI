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


####################################################################################################
#####################################################################################################
###                                             Code                                             ###
#####################################################################################################
####################################################################################################

###########################               HEADER               ####################################
####################################################################################################
#Create header to display, and disable the sidebar
header <- dashboardHeader(title = "Lorenzo de Zavala Youth Legislative Session")

###########################                 SIDEBAR                  ##############################
####################################################################################################

##Create the sidebard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("HQ Database Conversion", tabName="HQ"),
    menuItem("Staff Database Conversion", tabName="Staff"),
    menuItem("Day0 Admin Tasks", tabName="admin_pre"),
    menuItem("Day1 Admin Tasks", tabName="admin_post1"),
    menuItem("Day2-7 Admin Tasks", tabName="admin_post2"),
    menuItem("Protocol", tabName="protocol"),
    menuItem("Election", tabName="elect"),
    menuItem("Awards", tabName="awards"),
    menuItem("Merchandise", tabName="merch"),
    menuItem("Legislative", tabName="leg"),
    menuItem("Judicial", tabName="jud")
  )
)

##########################        MainBody - HQ Database Conversion        ########################
#######################################################################################################

######################################### Input
##File upload of HQ demographic file
hq.input <- fluidRow(
  column(12,
         fileInput("hq.file1","Upload the HQ Provided Demographic File (MUST be a CSV file)"),
         #downloadButton('download_hq', 'Download new Database'),
         actionButton("upload_hqdemo", "Submit file to DropBox"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_uploadhqdemo"))
         )
  )
)

######################################### Outputs for Database
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
  column(4, uiOutput("hq_CELL")),
  column(4, uiOutput("hq_DOB"))
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

######################################### Create one large output box
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

######################################### Create page boxes
#Generate Boxes for text submission and user downloads
box.hq.1 <- box(title = "Upload Student Database File", width=12, status="primary", 
                solidHeader = TRUE, hq.input)
box.hq.2 <- box(title = "Output Files", width=12, status="primary", collapsible = TRUE, 
                solidHeader = TRUE, hq.combo)


##########################        MainBody - Staff Database Conversion    ##########################
########################################################################################################
################################Staff Demographic File
##File upload of Staff demographic file
staff.db.input <- fluidRow(
  column(12,
         fileInput("staff.db1","Upload the Staff Info Database (MUST be a CSV File)"),
         actionButton("upload_staffdemo", "Submit file to DropBox"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_uploadstaffdemo"))
         )
  )
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
  column(4, 
         uiOutput("staff_DEG1"),
         uiOutput("staff_UNIV1"),
         uiOutput("staff_MAJ1"),
         uiOutput("staff_COLSTAT")
  ),
  column(4,
         uiOutput("staff_DEG2"),
         uiOutput("staff_UNIV2"),
         uiOutput("staff_MAJ2")
  ),
  column(4,
         uiOutput("staff_DEG3"),
         uiOutput("staff_UNIV3"),
         uiOutput("staff_MAJ3")
  )
)
staff.out.5 <- fluidRow(
  column(6, uiOutput("staff_STAT")),
  column(6, uiOutput("staff_ROLE"))
)
staff.out.6 <- fluidRow(
  column(4, 
         uiOutput("staff_LDZPOS"),
         uiOutput("staff_GD")
  ),
  column(4, 
         uiOutput("staff_LDZYEAR"),
         uiOutput("staff_CWS")
  ),
  column(4, 
         uiOutput("staff_LDZLOC")
  )
)

#################################Create one large output box
staff.combo <- fluidRow(
  column(6,
         box(title="Name", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.1),
         box(title="Location", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.2),
         box(title="Role", width=NULL, status="primary", collapsible=TRUE,
             solidHeader=TRUE, staff.out.5),
         box(title="NHI Info", width=NULL, status="primary", collapsible=TRUE,
             solidHeader=TRUE, staff.out.6)
  ),
  column(6,
         box(title="High School", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.3),
         box(title="College", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, staff.out.4)
  )
)

##################################Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.staff.1 <- box(title = "Upload Staff Database File", width=12, status="primary", 
                   solidHeader = TRUE, staff.db.input)
box.staff.2 <- box(title = "Output Files", width=12, status="primary", collapsible = TRUE, 
                   solidHeader = TRUE, staff.combo)

##########################        MainBody - Day 0 Admin Tasks         ##############################
########################################################################################################

######################################### Input File
###File upload of excepted demographics, created from Page1
day0.input <- fluidRow(
  column(6,
         actionButton("upload_day0_studentdb", "Create Student Output Files"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_day0studentdemo"))
         )
  ),
  column(6,
         actionButton("upload_day0_staffdb", "Create Staff Output files"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_day0staffdemo")),
           downloadButton('download_day0staffdemo', 'Download Staff Files')
         )
  )
)

############################### Create one large output box
day0.out.combo <- fluidRow(
  column(6,
         box(title="Student Related Files", 
             width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, 
             "Attendance and Travel Verification", br(),
             "Student Self-Verification", br(),
             "Student Name Tag labels", br(),
             "Student Balances and Missing Forms", br(),
             "Student Dorming Information")
  ),
  column(6,
         box(title = "Staff Related Files", 
             width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, 
             "Staff Labels (Onsite_StaffLabels.csv)")
  )
)

############################### Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.day0.1 <- box(title = "Admin File Generation", width=12, status="primary", 
                  solidHeader = TRUE, day0.input)
box.day0.2 <- box(title = "Ouput Files", width=12, status="primary", 
                  solidHeader = TRUE, day0.out.combo)

##########################        MainBody - Day 1 Admin Tasks        ###############################
#######################################################################################################
################################# Input Files
admin.post.reg <- fluidRow(
  #File upload of live  updated students demographic
  column(12,
         fileInput("admin.post.file1", "Upload the Live_StudentRegistration file")
  )
)

################################Ouputs
#Selection for user - Post Registration
admin.post1.1 <- fluidRow(
  column(6, downloadButton('download_post_registered', 'Download Student Registered File')),
  column(6, downloadButton('download_post_nonattend', 'Download Student Non-Attend File'))
)
#Selection for Demographic Report File
admin.post1.2 <- fluidRow(
  column(6, downloadButton('download_post_demoreport', 'Download Demographic Report File'))
)

#Combine all previous rows together
admin.post.out.combo <- fluidRow(
  column(6,
         box(title="Registration Status", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post1.1)
  ),
  column(6,
         box(title="Demographic Report", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post1.2)
  )
)

#################################Create boxes
#Generate Boxes for text submission, verification, and user options

box.post1.1 <- box(title = "Create Post Registration Database File", width=8, status="primary", 
                   solidHeader = TRUE, admin.post.reg)
box.post1.2 <- box(title = "Output Files", width=12, status="primary", 
                   solidHeader = TRUE, admin.post.out.combo)

##########################        MainBody - Day 2 Admin Tasks        ###############################
#######################################################################################################
#Selection for user - Post Registration
admin.post2.1 <- fluidRow(
  column(12, 
         actionButton("upload_tshirts", "Create Student Tshirt Files"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_tshirt"))
         )),
  
  column(6,
         downloadButton('download_post_tshirts', 'Download TShirt Lists')
  )
)
#Selection for Demographic Report File
admin.post2.2 <- fluidRow(
  #column(6, downloadButton('download_post_demoreport', 'Download Demographic Report File'))
)

#Combine all previous rows together
admin.post.out.combo <- fluidRow(
  column(6,
         box(title="TShirt", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post2.1)
  ),
  column(6,
         box(title="", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, admin.post2.2)
  )
)

#################################Create boxes
#Generate Boxes for text submission, verification, and user options

box.post2.1 <- box(title = "Output Files", width=12, status="primary", 
                   solidHeader = TRUE, admin.post.out.combo)


##########################        MainBody - Awards                 ###############################
#######################################################################################################
############################### #Voting
points.1 <- fluidRow(
  column(6,
         fileInput("points.file1","Upload the Registrar_StudentDB_Registered File")
  ),
  column(6,
         fileInput("points.file2","Upload the Election_WinnerFillin File")
  ),
  column(6,
         fileInput("points.file3","Upload the Election_WinnerFillin File")
  ),
  column(6, 
         actionButton("upload_nomids", "Create Nomination ID Files"),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_voterid"))
         )),
  column(6, 
         downloadButton('download_voterID', 'Download Voter_ID Files')
  ),
  column(6, 
         downloadButton('download_points_elect', 'Download Award_ElectWin File')
  )
)

################################ Generate Boxes for text submission, verification, and user options
box.points.1 <- box(title = "Elections", width=12, 
                    status="primary", solidHeader = TRUE, points.1)

##########################        MainBody - Protocol             ################################
#######################################################################################################
##Create rows of data
###Upload Forming the Community Database
proto.ftc <- fluidRow(
  column(12,
         fileInput("proto.file1","Upload the FormingTheCommunityTemplate File")
  )
)

##Ouputs selection for user - all pre-registration tasks
proto.out.1 <- fluidRow(
  column(12, 
         downloadButton('download_ftc_protocol', 'Download FTC_Protocol'),
         downloadButton('download_ftc_staff', 'Download FTC_Staff')
  )
)
proto.out.2 <- fluidRow(
  column(6#, 
         #downloadButton('download_ftc_protocol', 'Download FTC_Protocol'),
         #downloadButton('download_ftc_staff', 'Download FTC_Staff')
  )
)

###Combine all outputs together
proto.combo<- fluidRow(
  column(6,
         box(title="Forming the Community Output", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, 
             "Wait 10 seconds after submission before selecting DOWNLOAD:", br(), br(), 
             proto.out.1 ),
         box(title="Awards File", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, 
             "Wait 10 seconds after submission before selecting DOWNLOAD:", br(), br(), 
             proto.out.2 )
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.proto.1 <- box(title = "Forming the Community", width=12, 
                   status="primary", solidHeader = TRUE, proto.ftc)
box.proto.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, proto.combo)

##########################        MainBody - Merchandise            ###############################
########################################################################################################
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

##########################        MainBody - Elections             ###############################
########################################################################################################
##Create rows of data
###Upload registration database, nominee database, winner database
elect.input <- fluidRow(
  column(6,
         fileInput("elect.file1","Upload the Elections-Nominations File")
  ),
  column(6,
         fileInput("elect.file2","Upload the Elections-Results File")
  ),
  column(6,
         fileInput("elect.file3","Upload the Elections-Special File")
  )
)

##Ouputs selection for user - Nominees and winners
elect.out.1 <- fluidRow(
  column(6, 
         downloadButton('download_elect_nomineeroster', 'Download Nominee Roster')
  ),
  column(6, 
         downloadButton('download_elect_judballots', 'Download Judicial Ballots')
  )
)
elect.out.2 <- fluidRow(
  column(6,
         downloadButton('download_election_commish','Download Commisioners Report')
  ),
  column(6,
         actionButton('election_registration','Update the Registrars Database- Primary Elections'),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_electionreg"))
         )
  ),
  column(6,
         actionButton('election_registration2','Update the Registrars Database- Special Elections'),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_electionreg2"))
         )
  )
)
elect.out.3 <- fluidRow(
  column(6,
         actionButton('election_ids','Create Voter ID files'),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_electionids"))
         )),
  column(6,
         actionButton('election_candidates','Create Candidate files'),
         hidden(
           div(id='text_div',
               verbatimTextOutput("confirm_electcand"))
         ))
)

###Combine all outputs together
elect.combo<- fluidRow(
  column(12,
         box(title="Nominations", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, elect.out.3)
  ),column(12,
         box(title="Nominations", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, elect.out.1)
  ),
  column(12,
         box(title="Results", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, elect.out.2)
  )
)

##Create boxed for Page 1
###Generate Boxes for text submission, verification, and user options
box.elect.1 <- box(title = "Election Inputs", width=12, 
                   status="primary", solidHeader = TRUE, elect.input)
box.elect.2 <- box(title = "Ouput Files", width=12, status="primary", 
                   solidHeader = TRUE, elect.combo)

##########################        MainBody - Legislative         ##############################
########################################################################################################



##########################        MainBody - Judicial         ##############################
########################################################################################################

######################################### Input File
###File upload of the registrar
jud.input <- fluidRow(
  column(6,fileInput("judicial_file1","Upload the Registrar Student DB Elections file"))
)

######################################### Outputs for Registration Tasks
#Form to verify students travel, including phone number, travel plans, and parent contact
jud.out.1 <- fluidRow(
  downloadButton('download_judicialtracking', 'Download the Judicial Tracking Form')
)

############################### Create one large output box
jud.out.combo <- fluidRow(
  column(6,
         box(title="Judicial Tracking", width=NULL, status="primary", collapsible = TRUE,
             solidHeader = TRUE, jud.out.1)
  )
)

############################### Create page boxes
###Generate Boxes for text submission, confirmation, and user downloads
box.jud.1 <- box(title = "Upload Pre-Registration Database File", width=12, status="primary", 
                 solidHeader = TRUE, jud.input)
box.jud.2 <- box(title = "Ouput Files", width=12, status="primary", 
                 solidHeader = TRUE, jud.out.combo)

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
    tabItem(tabName="admin_post1",
            box.post1.1,
            box.post1.2
    ),
    tabItem(tabName="admin_post2",
            box.post2.1
    ),
    tabItem(tabName="protocol",
            box.proto.1,
            box.proto.2
    ),
    tabItem(tabName="awards",
            box.points.1
    ),
    tabItem(tabName="merch",
            box.merch.1,
            box.merch.2
    ),
    tabItem(tabName="elect",
            box.elect.1,
            box.elect.2
    ),
    tabItem(tabName="jud",
            box.jud.1,
            box.jud.2
    )
  )
)
dashboardPage(header,sidebar,body)