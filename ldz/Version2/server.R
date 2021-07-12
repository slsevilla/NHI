library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)
library(tidyr)
library(rdrop2)

#####################################################################################################
#####################################################################################################
###                                            NOTES                                            ###
###################################################################################################
####################################################################################################
#HQ Database Conversion: Takes in the HQ file provided with student and generates a standard database
#to use throughout the program.

####################################################################################################
#####################################################################################################
###                                             Code                                             ###
#####################################################################################################
####################################################################################################
function(input,output, session){
  token <- readRDS("droptoken.rds")
  # Then pass the token to each drop_ function
  drop_acc(dtoken = token)
  
  ##############################        HQ Database Conversion        ##############################
  #################################################################################################
  
  ######################################### Input File
  ##Import file
  hq_input <- reactive({
    hq_input <- input$hq.file1
    if(is.null(hq_input)) return(NULL)
    read.csv(fill=TRUE,file=input$hq.file1$datapath,header=TRUE,colClasses = "factor"
    )
  })
  ##Create File Summary information
  output$hq.input <- renderTable({
    if(is.null(hq_input())) return ()
    input$hq.file1
  })
  ##Display confirmation of upload for user
  output$confirm.hq <- renderUI({
    if(is.null(hq_input())) return()
    tableOutput("hq.input")
  })
  
  ######################################### User Dropdowns
  ##Generate Drop Downs for user to choose
  observe({
    req(input$hq.file1)
    dsnames <- c("2021","2022","2023","2024")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_progyear<- renderUI({
      selectInput("hq_progyear", "Program Year", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- c("National","CO","CA","TX","NY","LDZX")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_progloc<- renderUI({
      selectInput("hq_progloc", "Program Location", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_FNAME<- renderUI({
      selectInput("hq_FNAME", "First Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MNAME<- renderUI({
      selectInput("hq_MNAME", "Middle Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_LNAME<- renderUI({
      selectInput("hq_LNAME", "Last Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MF<- renderUI({
      selectInput("hq_MF", "Gender", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_HS<- renderUI({
      selectInput("hq_HS", "High School", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_CITY<- renderUI({
      selectInput("hq_CITY", "City", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ST<- renderUI({
      selectInput("hq_ST", "State", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DOB<- renderUI({
      selectInput("hq_DOB", "Birthday", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_HOME<- renderUI({
      selectInput("hq_HOME", "Home #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_CELL<- renderUI({
      selectInput("hq_CELL", "Cell #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_FIN.FORM<- renderUI({
      selectInput("hq_FIN.FORM", "Financial Form", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_MED.FORM<- renderUI({
      selectInput("hq_MED.FORM", "Medical Form", cb_options)
    })
  })  
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_BALANCE<- renderUI({
      selectInput("hq_BALANCE", "Balance", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P1<- renderUI({
      selectInput("hq_P1", "Parent Name 1", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2<- renderUI({
      selectInput("hq_P2", "Parent Name 2", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P1.CELL<- renderUI({
      selectInput("hq_P1.CELL", "Parent Cell# 1", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2.CELL<- renderUI({
      selectInput("hq_P2.CELL", "Parent Cell# 2", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_AIR<- renderUI({
      selectInput("hq_ARRIVAL_AIR", "Airport", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_TIME<- renderUI({
      selectInput("hq_ARRIVAL_TIME", "Time", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_FLIGHT<- renderUI({
      selectInput("hq_ARRIVAL_FLIGHT", "Flight #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ARRIVAL_CARRIER<- renderUI({
      selectInput("hq_ARRIVAL_CARRIER", "Flight Carrier", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DEPART_AIR<- renderUI({
      selectInput("hq_DEPART_AIR", "Airport", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DEPART_TIME<- renderUI({
      selectInput("hq_DEPART_TIME", "Time", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DORM<- renderUI({
      selectInput("hq_DORM", "Dorm Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_ROOM<- renderUI({
      selectInput("hq_ROOM", "Dorm Room #", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_DOT<- renderUI({
      selectInput("hq_DOT", "Dot Group", cb_options)
    })
  })
  
  ######################################### Generate Student Database
  ###Create database from user generated files, and matched column headers
  hq_database <- reactive({
    if(is.null(hq_input)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- hq_input()
    
    #Add Voter ID information
    for (row in 1:nrow(table_in)){
      table_in[row,"VOTERID"]<-sample(1:100000, 1)
    }
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    table_out[,"STATUS"] <- ""
    
    #Append each necessary column, based on user input to the new table
    table_out$VOTERID <- table_in[,"VOTERID"]
    table_out$FNAME <- table_in[,input$hq_FNAME]
    table_out$MNAME <- table_in[,input$hq_MNAME]
    table_out$LNAME <- table_in[,input$hq_LNAME]
    table_out$MF <- table_in[,input$hq_MF]
    table_out$HS <- table_in[,input$hq_HS]
    table_out$CITY <- table_in[,input$hq_CITY]
    table_out$ST <- table_in[,input$hq_ST]
    table_out$DOT <- table_in[,input$hq_DOT]
    table_out$HS <- table_in[,input$hq_HS]
    table_out$HOME <- table_in[,input$hq_HOME]
    table_out$CELL <- table_in[,input$hq_CELL]
    table_out$P1 <- table_in[,input$hq_P1]
    table_out$P2 <- table_in[,input$hq_P2]
    table_out$P1.CELL <- table_in[,input$hq_P1.CELL]
    table_out$P2.CELL <- table_in[,input$hq_P2.CELL]
    table_out$DOB <- table_in[,input$hq_DOB]
    
    
    #Create balance column, removing $
    table_out$BALANCE <- table_in[,input$hq_BALANCE]
    table_out[] <- lapply (table_out[], gsub, pattern = "$", 
                           replacement = "", fixed=TRUE)
    table_out[] <- lapply (table_out[], gsub, pattern = " -", 
                           replacement = "", fixed=TRUE)
    
    table_out$FIN.FORM <- table_in[,input$hq_FIN.FORM]
    table_out$MED.FORM <- table_in[,input$hq_MED.FORM]
    table_out$ARRIVAL_AIR <- table_in[,input$hq_ARRIVAL_AIR]
    table_out$ARRIVAL_TIME <- table_in[,input$hq_ARRIVAL_TIME]
    table_out$ARRIVAL_CARRIER <- table_in[,input$hq_ARRIVAL_CARRIER]
    table_out$ARRIVAL_FLIGHT <- table_in[,input$hq_ARRIVAL_FLIGHT]
    table_out$DEPART_AIR <- table_in[,input$hq_DEPART_AIR]
    table_out$DEPART_TIME <- table_in[,input$hq_DEPART_TIME]
    table_out$DORM <- table_in[,input$hq_DORM]
    table_out$ROOM <- table_in[,input$hq_ROOM]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ######################################### Upload the file to dropbox
  #Upload to DropBox
  observeEvent(input$upload_hqdemo, {
    outputDir <- file.path(paste0("ldz/",input$hq_progyear,"_",input$hq_progloc,"/registrar"))
    fileName="Registrar_StudentDB_Expected.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(hq_database(), filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    toggle('text_div')
    output$confirm_uploadhqdemo <- renderText({"Check the file upload status: https://www.dropbox.com/home/ldz/[yourprograminfo]/registrar"})
  })
  
  #########################         Staff Database Conversion         ##############################
  #################################################################################################
  
  ######################################### User Input File
  ##Import file
  staff_db <- reactive({
    staff_db <- input$staff.db1
    if(is.null(staff_db)) return(NULL)
    read.csv(fill=TRUE,file=input$staff.db1$datapath,header=TRUE,colClasses = "factor"
    )
  })
  
  ######################################### User Dropdowns
  ##Generate Drop Downs for user to choose
  observe({
    req(input$staff.db1)
    dsnames <- c("2021","2022","2023","2024")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_progyear<- renderUI({
      selectInput("staff_progyear", "Program Year", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- c("National","CO","CA","TX","NY","LDZX")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_progloc<- renderUI({
      selectInput("staff_progloc", "Program Location", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_ROLE<- renderUI({
      selectInput("staff_ROLE", "Program Role", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_FNAME<- renderUI({
      selectInput("staff_FNAME", "First Name", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_MNAME<- renderUI({
      selectInput("staff_MNAME", "Middle Name", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_LNAME<- renderUI({
      selectInput("staff_LNAME", "Last Name", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_CITY<- renderUI({
      selectInput("staff_CITY", "Home City", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_ST<- renderUI({
      selectInput("staff_ST", "Home State", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_STAT<- renderUI({
      selectInput("staff_STAT", "Academic Status", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_HS<- renderUI({
      selectInput("staff_HS", "High School", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_UNIV1<- renderUI({
      selectInput("staff_UNIV1", "College/Univ (UG)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_UNIV2<- renderUI({
      selectInput("staff_UNIV2", "College/Univ (GRAD)", cb_options)
    })
  })  
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_UNIV3<- renderUI({
      selectInput("staff_UNIV3", "College/Univ (GRAD2)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_DEG1<- renderUI({
      selectInput("staff_DEG1", "Degree (UG)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_DEG2<- renderUI({
      selectInput("staff_DEG2", "Degree (GRAD1)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_DEG3<- renderUI({
      selectInput("staff_DEG3", "Degree (GRAD2)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_MAJ1<- renderUI({
      selectInput("staff_MAJ1", "Major (UG)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_MAJ2<- renderUI({
      selectInput("staff_MAJ2", "Major (GRAD)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_MAJ3<- renderUI({
      selectInput("staff_MAJ3", "Major (GRAD2)", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_GD<- renderUI({
      selectInput("staff_GD", "GD Info", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_LDZPOS<- renderUI({
      selectInput("staff_LDZPOS", "LDZ POS", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_LDZYEAR<- renderUI({
      selectInput("staff_LDZYEAR", "LDZ YEAR", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_LDZLOC<- renderUI({
      selectInput("staff_LDZLOC", "LDZ LOC", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_CWS<- renderUI({
      selectInput("staff_CWS", "CWS Info", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_HSSTAT<- renderUI({
      selectInput("staff_HSSTAT", "HS Level", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_COLSTAT<- renderUI({
      selectInput("staff_COLSTAT", "College Level", cb_options)
    })
  })
  
  ######################################### Generate Staff Database
  ###Create database from user generated files, and matched column headers
  staff_database <- reactive({
    if(is.null(staff_db)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- staff_db()
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    
    #Append each necessary column, based on user input to the new table
    table_out$FNAME <- table_in[,input$staff_FNAME]
    table_out$MNAME <- table_in[,input$staff_MNAME]
    table_out$LNAME <- table_in[,input$staff_LNAME]
    table_out$CITY <- table_in[,input$staff_CITY]
    table_out$ST <- table_in[,input$staff_ST]
    table_out$HSSTAT <- table_in[,input$staff_HSSTAT]
    table_out$HS <- table_in[,input$staff_HS]
    table_out$COLSTAT <- table_in[,input$staff_COLSTAT]
    table_out$UNIV1 <- table_in[,input$staff_UNIV1]
    table_out$MAJ1 <- table_in[,input$staff_MAJ1]
    table_out$DEG1 <- table_in[,input$staff_DEG1]
    table_out$UNIV2 <- table_in[,input$staff_UNIV2]
    table_out$MAJ2 <- table_in[,input$staff_MAJ2]
    table_out$DEG2 <- table_in[,input$staff_DEG2]
    table_out$UNIV3 <- table_in[,input$staff_UNIV3]
    table_out$MAJ3 <- table_in[,input$staff_MAJ3]
    table_out$DEG3 <- table_in[,input$staff_DEG3]
    table_out$STAT <- table_in[,input$staff_STAT]
    table_out$GD <- table_in[,input$staff_GD]
    table_out$LDZYEAR <- table_in[,input$staff_LDZYEAR]
    table_out$LDZPOS <- table_in[,input$staff_LDZPOS]
    table_out$LDZLOC <- table_in[,input$staff_LDZLOC]
    table_out$CWS <- table_in[,input$staff_CWS]
    table_out$ROLE <- table_in[,input$staff_ROLE]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ######################################### Upload the file to dropbox
  #Upload to DropBox
  observeEvent(input$upload_staffdemo, {
    outputDir <- file.path(paste0("ldz/",input$staff_progyear,"_",input$staff_progloc,"/staff"))
    fileName="Registrar_StaffDB.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(staff_database(), filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    toggle('text_div')
    output$confirm_uploadstaffdemo <- renderText({"Check the file upload status: https://www.dropbox.com/home/ldz/[yourprograminfo]/registrar"})
  })
  
  
  ##############################       Day 0 Admin Tasks        #################################### 
  #################################################################################################
  
  ######################################### File input
  ##Generate database for Student Demographic Database
  observe({
    dsnames <- c("2021","2022","2023","2024")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$day0_progyear<- renderUI({
      selectInput("day0_progyear", "Program Year", cb_options)
    })
  })
  observe({
    dsnames <- c("National","CO","CA","TX","NY","LDZX")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$day0_progloc<- renderUI({
      selectInput("day0_progloc", "Program Location", cb_options)
    })
  })
  observeEvent(input$upload_day0_studentdb, {
    day0_expect<- drop_read_csv(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/Registrar_StudentDB_Expected.csv"))
    toggle('text_div')
    output$confirm_day0studentdemo <- renderText({"Download below files from: https://www.dropbox.com/home/[yourporgraminfo]/registrar"})
    
    ###Attendance and Travel Verification
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    d0_travelverify <- day0_expect[c("FNAME", "MNAME", "LNAME", "CELL", "P1.CELL",
                                     "P2.CELL", "ARRIVAL_AIR", "ARRIVAL_TIME",
                                     "ARRIVAL_CARRIER", "ARRIVAL_FLIGHT",
                                     "DEPART_AIR", "DEPART_TIME")]
    fileName="Onsite_ArrivalTravelVerify.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(d0_travelverify, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ####Student Self Verification forms
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar"))
    d0_studselfverify <- day0_expect[c("FNAME", "MNAME", "LNAME", "CELL", "CITY",
                                       "ST", "P1", "P2", "HS", "DEPART_AIR",
                                       "DEPART_TIME")]
    fileName="Registrar_StudentSelfVerify.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(d0_studselfverify, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ###Student labels
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    studlabels <- day0_expect[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "HS",
                                "DOT", "DOB", "VOTERID")]
    
    fileName="Onsite_StudentLabels.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(studlabels, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ###Students with balance
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    studbalance <- subset(day0_expect, !BALANCE=="")
    studbalance <- studbalance[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                                 "HS", "BALANCE")]
    fileName="Registrar_StudentswithBalance.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(studbalance, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ##Students with missing forms
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    studforms <- subset(day0_expect, FIN.FORM=='No' | MED.FORM=='No' )
    studforms <- studforms[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                             "HS", "FIN.FORM", "MED.FORM")]
    fileName="Registrar_StudentsMissingForms.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(studforms, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ###Student Door signs
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    studdoor <- day0_expect[c("STATUS", "FNAME", "MNAME", "LNAME", "CITY", "ST", "MF",
                              "DORM", "ROOM", "DOT")]
    
    studdoor <- studdoor[order(studdoor$MF, studdoor$ROOM),]
    fileName="Onsite_StudentDoorSigns.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(studdoor, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    ###Student Dorming Lists - Female and Male
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/registrar/"))
    d0_room_F <- subset(day0_expect,MF=="Female")
    d0_room_F <- d0_room_F[c("FNAME", "MNAME", "LNAME", "CITY", "ST","DOT", "DORM",
                             "ROOM")]
    fileName="Day0_Rooms_F.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(d0_room_F, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    d0_room_M <- subset(day0_expect,MF=="Male")
    d0_room_M <- d0_room_M[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "DOT", "DORM",
                             "ROOM")]
    fileName="Day0_Rooms_M.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(d0_room_M, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
  })
  
  
  ##Download Staff database to temp, and upload the label file to DB and download to user drive
  observeEvent(input$upload_day0_staffdb, {
    #Read the Staff DB and generate database for staff labels
    staff_file<-  drop_read_csv(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/staff/Registrar_StaffDB.csv"))
    staff_labels <- staff_file[c("FNAME", "MNAME", "LNAME","CITY", "ST", "HSSTAT", "HS",
                                 "COLSTAT","UNIV1","MAJ1","UNIV3","MAJ3","ROLE")]
    
    #Confirm message
    toggle('text_div')
    output$confirm_day0staffdemo <- renderText({"Download below files from: https://www.dropbox.com/home/[yourporgraminfo]/staff"})
    
    #Download file
    output$download_day0staffdemo <- downloadHandler(
      filename = function() {"StaffLabels.csv"},
      content = function(file) {
        write.csv(staff_labels, file, row.names = FALSE)
      }
    )
    
    #Upload to Dropbox
    outputDir <- file.path(paste0("ldz/",input$day0_progyear,"_",input$day0_progloc,"/staff/"))
    fileName="StaffLabels.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(staff_labels, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
  })    
  
  ##############################      Day 1 Admin Tasks       #################################### 
  #################################################################################################
  
  ######################################### File input
  ##Generate database for input file
  day1.status <- reactive({
    day1.status <- input$admin.post.file1
    if (is.null(day1.status)) return(NULL)
    read.csv(fill=TRUE,file=input$admin.post.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  ##Display confirmation of upload for user
  output$confirm.day1 <- renderUI({
    if(is.null(day1.status())) return()
    tableOutput("day1.input")
  })
  ######################################## Program Info
  observe({
    dsnames <- c("2021","2022","2023","2024")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$day1_progyear<- renderUI({
      selectInput("day1_progyear", "Program Year", cb_options)
    })
  })
  observe({
    dsnames <- c("National","CO","CA","TX","NY","LDZX")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$day1_progloc<- renderUI({
      selectInput("day1_progloc", "Program Location", cb_options)
    })
  })
  ######################################## Create dataframe 
  #Registered students
  observeEvent(input$upload_day1_registration, {
    #Confirm message
    toggle('text_div')
    output$confirm_day1studentdemo <- renderText({"Download below files from: https://www.dropbox.com/home/[yourporgraminfo]/registrar"})
    
    #Read in the database
    registered_w <- day1.status()[c("VOTERID", "STATUS", "FNAME", "MNAME", "LNAME", "MF", "CELL", "P1.CELL",
                                    "P2.CELL", "P1", "HS", "CITY", "ST", "DOT", "ST_NAME", "DOB",
                                    "DORM", "ROOM", "DEPART_WAY","DEPART_AIR", "DEPART_TIME","TSHIRT")]
    
    #Output a table of only registered students
    registered_f <- subset(registered_w,!(STATUS=="Absent"))
    
    outputDir <- file.path(paste0("ldz/",input$day1_progyear,"_",input$day1_progloc,"/registrar/"))
    fileName="Registrar_StudentDB_Registered.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(registered_f, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    registered_f
  })
  
  #Create dataframe of non-attending students
  observeEvent(input$upload_day1_registration, {
    
    #Read in the database
    nonattend_w <- day1.status()[c("STATUS", "FNAME", "MNAME", "LNAME", "MF", "CELL", "P1.CELL",
                                   "P2.CELL", "P1", "HS", "CITY", "ST", "DORM", "ROOM")]
    
    #Output a table of only registered students
    nonattend_f <- subset(nonattend_w,(STATUS=="Absent"))
    
    outputDir <-  file.path(paste0("ldz/",input$day1_progyear,"_",input$day1_progloc,"/registrar/"))
    fileName="Registrar_StudentDB_Non-Registered.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(nonattend_f, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    nonattend_f
  })
  
  #Create demographics report
  observeEvent(input$upload_day1_registration, {
    
    #Read in the database
    nonattend_w <- day1.status()[c("STATUS", "FNAME", "MNAME", "LNAME", "MF", "CELL", "P1.CELL",
                                   "P2.CELL", "P1", "HS", "CITY", "ST", "DORM", "ROOM")]
    
    #Output a table of only registered students
    nonattend_f <- subset(nonattend_w,(STATUS=="Absent"))
    
    day1_demoreport <- nonattend_f[c("FNAME", "MNAME", "LNAME", "MF", 
                                           "HS", "CITY", "ST", "ST_NAME")]
    
    #Create new dataframe
    demoreport <- data.frame()
    
    #Gender count
    count_M <- sum(day1_demoreport$MF=="Male")
    demoreport[1,"Male"] <- count_M
    count_F <- sum(day1_demoreport$MF=="Female")
    demoreport[1,"Female"] <- count_F
    demoreport[1,"Total"] <- count_M + count_F
    
    #Gender Percent
    demoreport[1,"MalePer"] <- (count_M / (count_M + count_F))*100
    demoreport[1,"FemalePer"] <- (count_F / (count_M + count_F))*100
    
    #Location Information
    state_list <- c("Alabama", "Maryland", "Rhode Island", "Connecticut", "Montana", "Vermont",
                    "Illinois", "New York", "District of Columbia", "Arizona", "Michigan", "South Dakota",
                    "Florida", "Nevada", "Washington", "Iowa", "North Dakota", "Maine", "California", "Mississippi",
                    "Texas", "Hawaii", "New Jersey", "Wisconsin", "Kentucky", "Oklahoma", "Pennsylvania", "Alaska",
                    "Massachusetts", "South Carolina", "Delaware", "Nebraska", "Virginia", "Indiana", "North Carolina",
                    "Louisiana", "Arkansas", "Minnesota", "Tennessee", "Georgia", "New Hampshire", "West Virginia",
                    "Kansas", "Ohio", "Oregon", "Colorado", "Missouri", "Utah", "Idaho", "New Mexico", "Wyoming")
    
    #Count of each state
    i = 1
    for (a in state_list){
      count=0
      count <- sum(day1_demoreport$ST_NAME==a)
      
      if (count>0){
        demoreport[i,"LOC_COUNT"] <- count
        demoreport[i,"WORD"] <- "From the state of"
        demoreport[i,"LOC_LIST"] <- a
        i=i+1
      } else{next}
    } 
    
    value <- i-1
    demoreport[1,"ST_COUNT"] <- value
    
    #Count of each country
    location_unique <- unique(day1_demoreport$ST_NAME)
    country_list <- subset(location_unique,!(location_unique %in% state_list))
    
    for (a in country_list){
      count=0
      count <- sum(day1_demoreport$ST_NAME==a)
      
      if (count>0){
        demoreport[i,"LOC_COUNT"] <- count
        demoreport[i,"WORD"] <- "From the country of"
        demoreport[i,"LOC_LIST"] <- a
        i=i+1
      } else{next}
    }
    
    demoreport[1,"CY_COUNT"] <- i-1-value
    
    outputDir <- file.path(paste0("ldz/",input$day1_progyear,"_",input$day1_progloc,"/registrar/"))
    fileName="Registrar_DemoReport.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(demoreport, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    demoreport
  })
  
  ##############################      Day 2 Admin Tasks       #################################### 
  #################################################################################################
  observeEvent(input$upload_tshirts,{
    fulldata<- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Registered.csv")
    
    partialdata<- fulldata[c("DOT","TSHIRT","MF")]
    
    x<-partialdata%>%
      group_by(MF,DOT)%>%
      count(TSHIRT)
    
    outputDir <- file.path("national_ldz/onsite")
    fileName="Onsite_tshirts.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(x, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    toggle('text_div')
    output$confirm_tshirt <- renderText({"File sent to: https://www.dropbox.com/home/national_ldz/onsite. Proceed to DL"})
    
    
    output$download_post_tshirts<-  downloadHandler(
      filename = function() {"Onsite_tshirts.csv"},
      content = function(file) {
        write.csv(x, file, row.names = FALSE)
      }
    )
  })
  
  
  ###############################          Protocol         ########################################
  #################################################################################################
  
  ##Takes the input file saving it to staff database
  ##Generate database for input Student Demographic Database
  observeEvent(input$proto.file1, {
    proto_stud<- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Expected.csv")
    proto_staff<- drop_read_csv("national_ldz/registrar/Registrar_StaffDB.csv")
    
    proto_form <- read.csv(fill=TRUE,file=input$proto.file1$datapath, header=TRUE, 
                           colClasses = "factor")
    
    #Create dataframe of states, then a list
    states_col <- unique(proto_stud["ST"])
    states_col <- states_col[,1]
    
    states_list <- c()
    for (a in states_col){
      states_list <- c(states_list, a)
    }
    states_list <- paste(states_list, collapse=" -- ") #Past list together with a - between
    
    #Create Protocol Database, and send to dropbox
    ###############
    ftc_protocol<- proto_form[c("Year", "Program", "MC", "ED.Welcoming", "Staff.Speaker.1",
                                "Staff.Speaker.2", "Highest.Staff", "ED.Opening")]
    ftc_protocol <- merge(ftc_protocol, states_list)
    colnames(ftc_protocol)[colnames(ftc_protocol)=="y"] <- "States"
    
    output$download_ftc_protocol <- downloadHandler(
      filename = function() {"Protocol_FTC_Script.csv"},
      content = function(file) {
        write.csv(ftc_protocol, file, row.names = FALSE)
      }
    )
    
    outputDir <- file.path("national_ldz/protocol")
    fileName="Protocol_FTC_Script.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(ftc_protocol, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    #Create Staff listing, and send to dropbox
    ###############
    for (i in 1:nrow(proto_staff)){
      a <- proto_staff[i,"ROLE"]
      
      if (a=="Education Director"){
        proto_staff[i,"RANK"] <- 10
      } else if (a=="Apprentice Education Director"){
        proto_staff[i,"RANK"] <- 09
      } else if (a=="HQ Representative"){
        proto_staff[i,"RANK"] <- 08
      } else if (a=="On-Site Director"){
        proto_staff[i,"RANK"] <- 07
      } else if (a=="Assistant On-Site Director"){
        proto_staff[i,"RANK"] <- 06
      } else if (a=="Secretary of State"){
        proto_staff[i,"RANK"] <- 05
      }else if (a == "Apprentice Secretary of State"){
        proto_staff[i,"RANK"] <- 04
      } else if (a=="Senior Counselor"){
        proto_staff[i,"RANK"] <- 03
      } else if (a=="Junior Counselor"){
        proto_staff[i,"RANK"] <- 02
      } else{
        proto_staff[i,"RANK"] <- 01
      }
    }
    
    #Staff Final Database
    staff_final <- proto_staff[order(proto_staff$RANK),]
    
    output$download_ftc_staff <- downloadHandler(
      filename = function() {"Protocol_FTC_Staff.csv"},
      content = function(file) {
        write.csv(staff_final, file, row.names = FALSE)
      }
    )
    
    fileName="Protocol_FTC_Staff.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(staff_final, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
  })
  
  ####################################      Merchandise           ####################################
  ###################################################################################################
  
  ############################### #User Input File
  ##Import inventory file
  merch.fin <- reactive({
    merch.fin <- input$merch.file1
    if(is.null(merch.fin)) return(NULL)
    read.csv(fill=TRUE,file=input$merch.file1$datapath,header=TRUE
    )
  })
  merch.inv <- reactive({
    merch.inv <- input$merch.file2
    if(is.null(merch.inv)) return(NULL)
    read.csv(fill=TRUE,file=input$merch.file2$datapath,header=TRUE
    )
  })
  
  ################################Create drop down choices for user to select the day to be created
  observe({
    dsnames <- c("Inventory_Day1", "Inventory_Day2", "Inventory_Day3", "Inventory_Day4", 
                 "Inventory_Day5", "Inventory_Day6", "Inventory_Day7", "Inventory_Day8")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$merch.day<- renderUI({
      selectInput("merch_day", "Which day to create for (tomorrow's #)", cb_options)
    })
  })
  
  ################################ Perform calculations for the inventory record. 
  #This will take the starting count and subtract
  #the count sold for the day. It will reset the value of the sold value back to zero.
  merch_inv <- reactive({
    if(is.null(merch.inv)) return(NULL)
    
    #Read in the database
    merch_inv_w <- merch.inv()[c("Item.Description", "Starting.Count", "Cost.Per.Unit", "Units.Sold.Total",
                                 "Units.Paid.Square","Units.Paid.Cash")]
    
    #Define starting counter, and end equal to the number of rows
    i = 1
    n = nrow(merch_inv_w[])
    
    #Perofrm a loop of each row, subtracting the sold number from the starting value
    for (i in 1:n){  
      merch_inv_w[i,"Starting.Count"] <- merch_inv_w[i,"Starting.Count"] - merch_inv_w[i,"Units.Sold.Total"]
      merch_inv_w[i,"Units.Sold.Total"] <- 0
      i=i+1
    }
    
    #Send the updated database
    merch_inv_w[]
  })
  
  ################################ Perform calculations for the financial ledger record. 
  #This will take the data from the inventory form
  #and calculate how much money was made throughout the day. It will also tally the total 
  #number for verification with petty cash amount
  merch_finledger <- reactive({
    if(is.null(merch.inv)) return(NULL)
    if(is.null(merch.fin)) return(NULL)
    
    
    #Read in the database
    merch_inv_w <- merch.inv()[c("Cost.Per.Unit", "Units.Paid.Square","Units.Paid.Cash")]
    merch_fin_w <- merch.fin()[c("Day", "Profit.on.Square","Profit.in.Cash", "Total.Petty.Cash")]
    
    #Define starting counter, and end equal to the number of rows
    i = 1
    n <- nrow(merch_inv_w[])
    square_sum = 0
    cash_sum = 0
    
    #Perofrm a loop of each row, adding the total profits in cash or on the square
    for (i in 1:n){
      square_sum <- merch_inv_w[i,"Cost.Per.Unit"] * merch_inv_w[i,"Units.Paid.Square"] + square_sum
      cash_sum <- merch_inv_w[i,"Cost.Per.Unit"] * merch_inv_w[i,"Units.Paid.Cash"] + cash_sum
      i=i+1
    }
    
    #Create a new row based on the day and fill it with the next day
    n <- nrow(merch_fin_w)
    merch_fin_w[n+1,"Day"] <- n
    
    #Push the values obtained to the rows
    merch_fin_w[n+1,"Profit.on.Square"] <- square_sum
    merch_fin_w[n+1,"Profit.in.Cash"] <- cash_sum
    merch_fin_w[n+1,"Total.Petty.Cash"] <- merch_fin_w[n,"Total.Petty.Cash"] + cash_sum
    
    #Send the updated database
    merch_fin_w[]
  })
  
  ################################ Send file to download screen
  output$download_merch_inv <- downloadHandler(
    filename = function() {
      paste(input$merch_day, ".csv", sep="")
    },
    content = function(file) {
      write.csv(merch_inv(), file, row.names = FALSE)
    }
  )
  
  #
  output$download_merch_ledger <- downloadHandler(
    filename = function() {"MerchandiseLedger.csv"},
    content = function(file) {
      write.csv(merch_finledger(), file, row.names = FALSE)
    }
  )
  
  ####################################           Awards           ###################################
  ###################################################################################################
  
  ##################################Input data files
  #Import election databases
  observeEvent(input$upload_nomids,{
    student_reg <- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Elections_Final.csv")
    
    #Create a full name reference
    for (i in 1:nrow(student_reg)){
      student_reg[i,"FULLNAME"]<-paste(student_reg[i,"LNAME"],student_reg[i,"FNAME"],sep=",")
      student_reg[i,"vote_weight"]<-1
      student_reg[i,"email"]<-""
    }
    
    #Change column names
    colnames(student_reg)[colnames(student_reg)=="VOTERID"] <- "voter_identifier"
    colnames(student_reg)[colnames(student_reg)=="FULLNAME"] <- "name"
    colnames(student_reg)[colnames(student_reg)=="DOB"] <- "voter_key"
    
    
    #Create positional databases
    student_senate<-subset(student_reg,student_reg$POSITION=="Senator")
    student_rep <- subset(student_reg,student_reg$POSITION=="Representative")
    student_sc <- subset(student_reg,student_reg$POSITION=="Supreme Court Justice" | student_reg$POSITION=="Chief Justice")
    student_exec <- subset(student_reg,student_reg$POSITION=="President"|student_reg$POSITION=="Vice-President"|
                             student_reg$POSITION=="Cabinet")
    
    #Remove extra columns
    student_senate<-student_senate[,c("name","voter_identifier","voter_key","email","vote_weight")]
    student_rep<-student_rep[,c("name","voter_identifier","voter_key","email","vote_weight")]
    student_sc<-student_sc[,c("name","voter_identifier","voter_key","email","vote_weight")]
    student_exec<-student_exec[,c("name","voter_identifier","voter_key","email","vote_weight")]
    
    #Upload files
    outputDir <- file.path("national_ldz/awards/nominationids")
    fileName="senate_nomids.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_senate, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    fileName="house_nomids.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_rep, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    fileName="supcourt_nomids.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_sc, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    fileName="exec_nomids.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_exec, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
  })

  #Create dataframe of the election points - party nominations
  points_nomfill <- reactive({
    if(is.null(points.nomfill)) return(NULL)
    
    #Read in the database
    points_nomfill <- points.nomfill()[]
    
    for (i in 1:nrow(points_nomfill)){
      points_nomfill[i,"PRES_NOM"] <- ""
      points_nomfill[i,"VP_NOM"] <- ""
      points_nomfill[i,"SENATE_NOM"] <- ""
      points_nomfill[i,"JUD_NOM"] <- ""
    }
    
    points_nomfill
  })
  
  #Create dataframe of the election points - party
  points_nom <- reactive({
    if(is.null(points.nom)) return(NULL)
    
    #Read in the database
    points_elect <- points.nom()[]
    points_final <- points_elect[,"NAME"]
    
    
    for (i in 1:nrow(points_elect)){
      points=0
      
      #Supreme Court
      if (points_elect[i,"JUD_NOM"]=="WINNER"){
        points = points + 10
      }
      
      #President
      if (points_elect[i,"PRES_NOM"]=="WINNER"){
        points = points + 10
      }
      
      #VP
      if (points_elect[i,"VP_NOM"]=="WINNER"){
        points = points + 10
      }
      
      #Senate
      if (points_elect[i,"SENATE_WIN"]=="WINNER"){
        points = points + 10
      }
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  
  #Create dataframe of the election points - general election
  points_elect <- reactive({
    if(is.null(points.elect)) return(NULL)
    
    #Read in the database
    points_elect <- points.elect()[]
    points_final <- points_elect[,"NAME"]
    
    
    for (i in 1:nrow(points_elect)){
      points=0
      
      #Supreme Court
      if (is.na(points_elect[i,"SUPREME.JUSTICE_WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"SUPREME.JUSTICE_WIN"]=="" |
               points_elect[i,"SUPREME.JUSTICE_WIN"]=="WINNER"){
        points = points + 10
      }
      
      #President
      if (is.na(points_elect[i,"PRES_WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"PRES_WIN"]==""){
        points = points + 10
      }
      
      #VP
      if (is.na(points_elect[i,"VP_WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"VP_WIN"]==""){
        points = points + 10
      }
      
      #Senate
      if (is.na(points_elect[i,"SENATE_WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"SENATE_WIN"]=="" |
               points_elect[i,"SENATE_WIN"]=="WINNER"){
        points = points + 10
      }
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  
  ###############################Output files 
  #Election - Nominations Fill-In
  output$download_election_nomfillin <- downloadHandler(
    filename = function() {"Election_NomFillin.csv"},
    content = function(file) {
      write.csv(points_nomfill(), file, row.names = FALSE)
    }
  )
  #Election - Nominations for Party
  output$download_points_nom <- downloadHandler(
    filename = function() {"Points_Election_Nom.csv"},
    content = function(file) {
      write.csv(points_nom(), file, row.names = FALSE)
    }
  )
  #Election - General Election
  output$download_points_elect <- downloadHandler(
    filename = function() {"Points_Election_General.csv"},
    content = function(file) {
      write.csv(points_elect(), file, row.names = FALSE)
    }
  )
  
  ##############################           Elections             #################################### 
  ####################################################################################################
  
  ######################################### File input
  elect.fillin <- reactive({
    elect.fillin <- input$elect.file1
    if (is.null(elect.fillin)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  elect.win <- reactive({
    elect.win <- input$elect.file2
    if (is.null(elect.win)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })
  elect.special <- reactive({
    elect.special <- input$elect.file3
    if (is.null(elect.special)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file3$datapath, header=TRUE, 
             colClasses = "factor")
  })
  ######################################### Create dataframe 
  #Create Nomination official roster
  elect_nominees <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "VOTERID","DORM", "ROOM",
                                       "SENATE", "SUPREME.JUSTICE", "VP", "ATTORNEY", "PRES")]
    
    #Create party list
    party_list <- unique(elect_nominees$SUPREME.JUSTICE)
    party_list <- party_list[-1]
    
    #Create final database
    elect_nominees_final<-data.frame()
    
    #Sort by each position, crate new database interweaving party candidates
    justices <- subset(elect_nominees, elect_nominees$SUPREME.JUSTICE %in% party_list)
    justices <-justices[order(justices$SUPREME.JUSTICE),]
    vp <- subset(elect_nominees, elect_nominees$VP %in% party_list)
    vp <-vp[order(vp$VP),]
    pres <- subset(elect_nominees, elect_nominees$PRES %in% party_list)
    pres <-pres[order(pres$PRES),]
    senate <- subset(elect_nominees, elect_nominees$SENATE %in% party_list)
    senate <-senate[order(senate$SENATE),]
    
    #Start Counters
    i=1
    count=1
    
    #Add Justices
    while(count<10){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- justices[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- justices[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- justices[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- justices[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- justices[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- justices[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- justices[count,"ST_NAME"]
      elect_nominees_final[i,"SUPREME.JUSTICE"] <- justices[count,"SUPREME.JUSTICE"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- justices[count+9,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- justices[count+9,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- justices[count+9,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- justices[count+9,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- justices[count+9,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- justices[count+9,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- justices[count+9,"ST_NAME"]
      elect_nominees_final[i+1,"SUPREME.JUSTICE"] <- justices[count+9,"SUPREME.JUSTICE"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Add VP
    ##Start Counters
    count=1
    
    while(count<2){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- vp[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- vp[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- vp[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- vp[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- vp[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- vp[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- vp[count,"ST_NAME"]
      elect_nominees_final[i,"VP"] <- vp[count,"VP"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- vp[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- vp[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- vp[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- vp[count+1,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- vp[count+1,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- vp[count+1,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- vp[count+1,"ST_NAME"]
      elect_nominees_final[i+1,"VP"] <- vp[count+1,"VP"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Add Pres
    ###Start Counters
    count=1
    
    while(count<2){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- pres[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- pres[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- pres[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- pres[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- pres[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- pres[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- pres[count,"ST_NAME"]
      elect_nominees_final[i,"PRES"] <- pres[count,"PRES"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- pres[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- pres[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- pres[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- pres[count+1,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- pres[count+1,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- pres[count+1,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- pres[count+1,"ST_NAME"]
      elect_nominees_final[i+1,"PRES"] <- pres[count+1,"PRES"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Add Senate
    ###Start Counters
    count=1
    
    while(count<41){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- senate[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- senate[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- senate[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- senate[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- senate[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- senate[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- senate[count,"ST_NAME"]
      elect_nominees_final[i,"SENATE"] <- senate[count,"SENATE"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- senate[count+40,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- senate[count+40,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- senate[count+40,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- senate[count+40,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- senate[count+40,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- senate[count+40,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- senate[count+40,"ST_NAME"]
      elect_nominees_final[i+1,"SENATE"] <- senate[count+40,"SENATE"]
      
      #Increase Counters
      i=i+2
      count=count+1
    }
    
    elect_nominees_final
    
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/election")
    fileName="Election_NomineeRoster.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(elect_nominees_final, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    elect_nominees_final
    
  })
  #Create Nomination official roster
  elect_judballot <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "VOTERID","DORM", "ROOM",
                                       "SENATE", "SUPREME.JUSTICE", "VP", "ATTORNEY", "PRES")]
    
    #Create party list
    party_list <- unique(elect_nominees$SUPREME.JUSTICE)
    party_list <- party_list[-1]
    
    #Create final database
    elect_nominees_final<-data.frame()
    
    #Sort by each position, crate new database interweaving party candidates
    justices <- subset(elect_nominees, elect_nominees$SUPREME.JUSTICE %in% party_list)
    justices <-justices[order(justices$SUPREME.JUSTICE),]
    
    #Start Counters
    i=1
    count=1
    
    #Add Justices
    while(count<10){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- justices[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- justices[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- justices[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- justices[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- justices[count,"ST_NAME"]
      elect_nominees_final[i,"SUPREME.JUSTICE"] <- justices[count,"SUPREME.JUSTICE"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- justices[count+9,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- justices[count+9,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- justices[count+9,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- justices[count+9,"CITY"]
      elect_nominees_final[i+1,"ST_NAME"] <- justices[count+9,"ST_NAME"]
      elect_nominees_final[i+1,"SUPREME.JUSTICE"] <- justices[count+9,"SUPREME.JUSTICE"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    count=1
    
    #Add Justices
    while(count<10){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- justices[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- justices[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- justices[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- justices[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- justices[count,"ST_NAME"]
      elect_nominees_final[i,"SUPREME.JUSTICE"] <- justices[count,"SUPREME.JUSTICE"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- justices[count+9,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- justices[count+9,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- justices[count+9,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- justices[count+9,"CITY"]
      elect_nominees_final[i+1,"ST_NAME"] <- justices[count+9,"ST_NAME"]
      elect_nominees_final[i+1,"SUPREME.JUSTICE"] <- justices[count+9,"SUPREME.JUSTICE"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/election")
    fileName="Election_JudicialBallot.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(elect_nominees_final, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    elect_nominees_final
    
  })
  #Creation Commissioners Report
  elect_commish <- reactive({
    if(is.null(elect.win)) return(NULL)
    
    #Read in the database
    elect_winners <- elect.win()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "DORM", "ROOM", "VOTERID", "SENATE",
                                   "SENATE_WIN","SUPREME.JUSTICE", "SUPREME.JUSTICE_WIN","VP", "VP_WIN", "ATTORNEY",
                                   "PRES","PRES_WIN")]
    
    #Run for Announcement 1
    election_final <- subset(elect_winners, elect_winners$SUPREME.JUSTICE_WIN=="WINNER" | 
                               (elect_winners$ATTORNEY_WIN=="WINNER") | (elect_winners$VP_WIN=="WINNER") |
                               (elect_winners$PRES_WIN=="WINNER")| (elect_winners$SENATE_WIN=="WINNER")
    )
    #Order the database
    #election_final<-election_final[order(election_final$SUPREME.JUSTICE_WIN, election_final$ATTORNEY, 
    #election_final$VP_WIN,election_final$PRES_WIN, election_final$SENATE_WIN),]
    election_final<-election_final[order(election_final$SENATE_WIN, election_final$PRES_WIN, 
                                         election_final$VP_WIN,election_final$ATTORNEY_WIN, election_final$SUPREME.JUSTICE_WIN),]
    
    #Add position column
    for (i in 1:9){
      election_final[i,"POSITION"] <- "Supreme Court Justice"
      election_final[i,"RANK"] <- "6"
    }
    for (i in 10:33){
      election_final[i,"POSITION"] <- "Attorney"
      election_final[i,"RANK"] <- "7"
    }
    for (i in 34:34){
      election_final[i,"POSITION"] <- "Vice-President"
      election_final[i,"RANK"] <- "2"
    }
    for (i in 35:35){
      election_final[i,"POSITION"] <- "President"
      election_final[i,"RANK"] <- "1"
    }   
    for (i in 36:75){
      election_final[i,"POSITION"] <- "Senator"
      election_final[i,"RANK"] <- "8"
    }
    
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/election")
    fileName="Election_CommishReport.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(election_final, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    election_final
    
  })
  elect_special <- reactive({
    if(is.null(elect.special)) return(NULL)
    
    #Read in the database
    elect_special <- elect.special()[c("FNAME", "MNAME", "LNAME", "VOTERID", "HorS_Clerk",	"HorS_Srgt",
                                       "HorS_Majority", "HorS_Minor", "ChiefNominees",	"Cabinet",	"SpeakerOfHouse",
                                       "HorS_Chair","SpeakerProTemp")]
    elect_special
  })
  election_final <- reactive({
    if(is.null(elect.special)) return(NULL)
    
    #Read in the database
    elect_special <- elect.special()[c("FNAME", "MNAME", "LNAME", "VOTERID", "HorS_Clerk",	"HorS_Srgt",
                                       "HorS_Majority", "HorS_Minor", "ChiefNominees",	"Cabinet",	"SpeakerOfHouse",
                                       "HorS_Chair","SpeakerProTemp")]
    
    election_final <- subset(elect_special, elect_special$HorS_Clerk=="WINNER" | elect_special$HorS_Srgt=="WINNER" |
                               elect_special$HorS_Majority=="WINNER" | elect_special$HorS_Minor=="WINNER" |
                               elect_special$ChiefNominees=="WINNER" |	elect_special$Cabinet=="WINNER"	|
                               elect_special$SpeakerOfHouse=="WINNER" | elect_special$HorS_Chair=="WINNER"|
                               elect_special$SpeakerProTemp=="WINNER"
    )
 
    #Add position column
    for (i in 1:nrow(election_final)){
      if(election_final[i,"SpeakerOfHouse"]=="WINNER"){
        election_final[i,"POSITION"] <- "Speaker of the House"
        election_final[i,"RANK"] <- "3"
      } else if(election_final[i,"ChiefNominees"]=="WINNER"){
        election_final[i,"POSITION"] <- "Chief Justice"
        election_final[i,"RANK"] <- "5"
      } else if(election_final[i,"Cabinet"]=="WINNER"){
        election_final[i,"POSITION"] <- "Cabinet"
        election_final[i,"RANK"] <- "4"
      } else{
        election_final[i,"RANK2"] <- "1"
        election_final[i,"POSITION"] <- "Special Appt"
      }
    }
    election_final
  })
  
  observeEvent(input$election_registration2,{
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/election")
    fileName="Elections_Special_Nominees.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(elect_special(), filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/election")
    fileName="Elections_Special_Results.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(election_final(), filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    elect.special<-drop_read_csv("national_ldz/election/Elections_Special_Results.csv")
    elect.special <- elect.special[c("VOTERID","POSITION","RANK")]
    student_reg <- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Elections.csv")
    
    #Use VoterID as the row name
    row.names(student_reg)<-student_reg$VOTERID
    student_reg$POSITION<-as.character(student_reg$POSITION)
    
    #Remove all special apointments
    elect.special<-subset(elect.special,!elect.special$POSITION=="Special Appt")
    
    for(i in 1:nrow(elect.special)){
      id <- as.character(elect.special[i,"VOTERID"])
      student_reg[id,"POSITION"] <- as.character(elect.special[i,"POSITION"])
      student_reg[id,"RANK"] <- elect.special[i,"RANK"]
      
    }
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/registrar")
    fileName="Registrar_StudentDB_Elections_Final.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_reg, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    #Confirm message
    toggle('text_div')
    output$confirm_electionreg2 <- renderText({"File sent to: https://www.dropbox.com/home/national_ldz/registrar"})
  })
  
  #Create new student database with elected positions
  observeEvent(input$election_registration, {
    
    elect.commish<-drop_read_csv("national_ldz/election/Election_CommishReport.csv")
    elect.commish <- elect.commish[c("VOTERID","POSITION","RANK")]
    student_reg <- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Registered.csv")
    
    elect_reg <- merge(student_reg,elect.commish,all.x=TRUE)
    
    elect_reg$POSITION<-as.character(elect_reg$POSITION)
    
    elect_reg$POSITION[is.na(elect_reg$POSITION)]<-"Representative"
    elect_reg$RANK[is.na(elect_reg$RANK)]<-"9"
    
    #Upload to Dropbox
    outputDir <- file.path("national_ldz/registrar")
    fileName="Registrar_StudentDB_Elections.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(elect_reg, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    #Confirm message
    toggle('text_div')
    output$confirm_electionreg <- renderText({"File sent to: https://www.dropbox.com/home/national_ldz/registrar"})
    
  })
  
  #Create database of election information
  observeEvent(input$election_ids,{
    student_reg <- drop_read_csv("national_ldz/registrar/Registrar_StudentDB_Registered.csv")
    
    #Create a full name reference
    for (i in 1:nrow(student_reg)){
      student_reg[i,"FULLNAME"]<-paste(student_reg[i,"LNAME"],student_reg[i,"FNAME"],sep=",")
      student_reg[i,"vote_weight"]<-1
      student_reg[i,"email"]<-""
    }
    
    #Change column names
    colnames(student_reg)[colnames(student_reg)=="VOTERID"] <- "voter_identifier"
    colnames(student_reg)[colnames(student_reg)=="FULLNAME"] <- "name"
    colnames(student_reg)[colnames(student_reg)=="DOB"] <- "voter_key"
    
    
    #Remove extra columns
    student_reg<-student_reg[,c("name","voter_identifier","voter_key","email","vote_weight")]
    
    #Upload files
    outputDir <- file.path("national_ldz/election/nominationids")
    fileName="election_nomids.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_reg, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    #Confirm message
    toggle('text_div')
    output$confirm_electionids <- renderText({"File sent to: https://www.dropbox.com/home/national_ldz/election/nominationids"})
  })
  
  #Create database of candidates
  observeEvent(input$election_candidates,{
    student_reg <- drop_read_csv("national_ldz/election/Election_NomineeRoster.csv")
    
    #Create a full name reference
    for (i in 1:nrow(student_reg)){
      student_reg[i,"FULLNAME"]<-paste(student_reg[i,"LNAME"],student_reg[i,"FNAME"],sep=",")
    }
    
    #create needed databases
    student_justice=data.frame()
    student_exec=data.frame()
    student_senate=data.frame()
    
    num=1
    
    #Create a full name reference
    for (i in 1:nrow(student_reg)){
      if(i<19){
        student_justice[i,"question"]<-"Justice"
        student_justice[i,"title"]<-student_reg[i,"FULLNAME"]
        student_justice[i,"short_description"]<-""
        student_justice[i,"description"]<-""
        student_justice[i,"photo"]<-""
        student_justice[i,"sort"]<-""
        }
      else if (i==19 | i==21){
        student_exec[i,"question"]<-"President/VP"
        student_exec[i,"title"]<-paste(student_reg[i,"FULLNAME"],student_reg[i+1,"FULLNAME"],sep=" AND ")
        student_exec[i,"short_description"]<-""
        student_exec[i,"description"]<-""
        student_exec[i,"photo"]<-""
        student_exec[i,"sort"]<-""
      } else if (i==20 | i==22){
        next
      } else{
        if((i %% 2) == 0) {
          student_senate[i,"question"]<-paste("Senator",num,sep="")
          student_senate[i,"title"]<-student_reg[i,"FULLNAME"]
          student_senate[i,"short_description"]<-""
          student_senate[i,"description"]<-""
          student_senate[i,"photo"]<-""
          student_senate[i,"sort"]<-""
          num=num+1
        }else{
          student_senate[i,"question"]<-paste("Senator",num,sep="")
          student_senate[i,"title"]<-student_reg[i,"FULLNAME"]
          student_senate[i,"short_description"]<-""
          student_senate[i,"description"]<-""
          student_senate[i,"photo"]<-""
          student_senate[i,"sort"]<-""
      }
      }
    }
    
    #Remove NA rows
    student_senate <- na.omit(student_senate)
    student_justice <- na.omit(student_justice)
    student_exec <- na.omit(student_exec)
    

    
    #Upload files
    outputDir <- file.path("national_ldz/election/candidates")
    fileName="candidates_senate.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_senate, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    fileName="candidates_justice.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_justice, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    fileName="candidates_exec.csv"
    filePath <- file.path(tempdir(), fileName)
    write.csv(student_exec, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
    
    #Confirm message
    toggle('text_div')
    output$confirm_electcand <- renderText({"File sent to: https://www.dropbox.com/home/national_ldz/election/candidates"})
  })
  
  ###Downloads
  #Nomination Roster for MM
  output$download_elect_nomineeroster <- downloadHandler(
    filename = function() {"Election_NomineeRoster.csv"},
    content = function(file) {
      write.csv(elect_nominees(), file, row.names = FALSE)
    }
  )
  #Judicial Ballot
  output$download_elect_judballots <- downloadHandler(
    filename = function() {"Election_JudicialBallot.csv"},
    content = function(file) {
      write.csv(elect_judballot(), file, row.names = FALSE)
    }
  )
  #Election Winners for Commish Report
  output$download_election_commish <- downloadHandler(
    filename = function() {"Election_CommishReport.csv"},
    content = function(file) {
      write.csv(elect_commish(), file, row.names = FALSE)
    }
  )
  
  
  ##############################           Judicial             #################################### 
  ####################################################################################################
  
  ######################################### File input
  ##Generate database for input file
  jud.track <- reactive({
    jud.track <- input$judicial_file1
    if (is.null(jud.track)) return(NULL)
    read.csv(fill=TRUE,file=input$judicial_file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  ######################################### Create dataframe 
  #Registered students - to create nomination fill in sheet
  jud_track <- reactive({
    if(is.null(jud.track)) return(NULL)
    
    #Read in the database and subset the positions
    jud_track <- jud.track()[c("NAME","POSITION")]
    jud_track <- subset(jud_track, POSITION=="Attorney" | POSITION=="Supreme Court Justice")
    
    #Create new columns with position names
    jud_names <- c("CHEIF_JUSTICE_NOM", "TOP_6TEAMS", "TOP_4TEAMS", "TOP_2TEAMS")
    
    #Add the new columns headers into the dataframe
    for (i in 1:nrow(jud_track)){
      for(a in jud_names){
        jud_track[i,a] <- ""
      }
    }
    
    jud_track
  })
  
  ######################################### Send file to download screen
  #Judical Tracking file
  output$download_judicialtracking <- downloadHandler(
    filename = function() {"Judicial_Tracking.csv"},
    content = function(file) {
      write.csv(jud_track(), file, row.names = FALSE)
    }
  )
}