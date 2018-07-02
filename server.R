library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)
library(tidyr)

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
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    table_out[,"STATUS"] <- ""
    
    #Append each necessary column, based on user input to the new table
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
  
  ######################################### Send file to download screen
  output$download_hq <- downloadHandler(
    filename = function() {"Registrar_StudentDB_Expected.csv"},
    content = function(file) {
      write.csv(hq_database(), file, row.names = FALSE)
    }
  )

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
    output$staff_UNIV<- renderUI({
      selectInput("staff_UNIV", "College/Univ", cb_options)
    })
  })
  observe({
    req(input$staff.db1)
    dsnames <- names(staff_db())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$staff_MAJ<- renderUI({
      selectInput("staff_MAJ", "Major", cb_options)
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
    output$staff_LDZ<- renderUI({
      selectInput("staff_LDZ", "LDZ Info", cb_options)
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
    table_out$UNIV <- table_in[,input$staff_UNIV]
    table_out$MAJ <- table_in[,input$staff_MAJ]
    table_out$STAT <- table_in[,input$staff_STAT]
    table_out$GD <- table_in[,input$staff_GD]
    table_out$LDZ <- table_in[,input$staff_LDZ]
    table_out$CWS <- table_in[,input$staff_CWS]
    table_out$ROLE <- table_in[,input$staff_ROLE]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ######################################### Send file to download screen
  output$download_staffdemo <- downloadHandler(
    filename = function() {"Registrar_StaffDB.csv"},
    content = function(file) {
      write.csv(staff_database(), file, row.names = FALSE)
    }
  )
  
  
  ##############################       Day 0 Admin Tasks        #################################### 
  #################################################################################################
  
  ######################################### File input
  ##Generate database for Student Demographic Database
  day0_expect <- reactive({
    admin1 <- input$day0.file1
    if (is.null(admin1)) return(NULL)
    read.csv(fill=TRUE,file=input$day0.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  ##Generate database for input Staff Demographic Database
  staff_file <- reactive({
    admin2 <- input$day0.file2
    if (is.null(admin2)) return(NULL)
    read.csv(fill=TRUE,file=input$day0.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })
  
  ######################################### Create sub-tables
  #Attendance and Travel Verification
  d0_travelverify <- reactive({
    if(is.null(input$day0.file1)) return()
    d0_travelverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "P1.CELL",
                                       "P2.CELL", "ARRIVAL_AIR", "ARRIVAL_TIME",
                                       "ARRIVAL_CARRIER", "ARRIVAL_FLIGHT",
                                       "DEPART_AIR", "DEPART_TIME")]
  })
  #Student self verification
  studselfverify <- reactive({
    if(is.null(input$day0.file1)) return()
    studselfverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "CITY",
                                      "ST", "P1", "P2", "HS", "DEPART_AIR",
                                      "DEPART_TIME")]
  })
  #Student labels
  studlabels <- reactive({
    if(is.null(input$day0.file1)) return()
    studlabels <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "HS",
                                  "DOT")]
  })
  #Students with balance
  studbalance <- reactive({
    if(is.null(input$day0.file1)) return()
    studbalance <- subset(day0_expect(), !BALANCE=="")
    studbalance <- studbalance[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                                 "HS", "BALANCE")]
  })
  #Students with missing forms
  studforms <- reactive({
    if(is.null(input$day0.file1)) return()
    studforms <- subset(day0_expect(), FIN.FORM=='No' | MED.FORM=='No' )
    studforms <- studforms[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                             "HS", "FIN.FORM", "MED.FORM")]
  })
  #Student Doro signs
  studdoor <- reactive({
    if(is.null(input$day0.file1)) return()
    studdoor <- day0_expect()[c("STATUS", "FNAME", "MNAME", "LNAME", "CITY", "ST", "MF",
                                "DORM", "ROOM", "DOT")]
    studdoor <- subset(studdoor,!(STATUS=="NON-ATTEND"))
    studdoor <- studdoor[order(studdoor$MF, studdoor$ROOM),]
  })
  #Student Dorming Lists - Female and Male
  d0_room_F <- reactive({
    if(is.null(input$day0.file1)) return()
    
    ##Create list of dorm names for female students
    d0_room_F <- subset(day0_expect(),MF=="Female")
    d0_room_F <- d0_room_F[c("FNAME", "MNAME", "LNAME", "CITY", "ST","DOT", "DORM",
                             "ROOM")]
  })
  d0_room_M <- reactive({
    if(is.null(input$day0.file1)) return()
    
    ##Create list of dorm names for Male students
    d0_room_M <- subset(day0_expect(),MF=="Male")
    d0_room_M <- d0_room_M[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "DOT", "DORM",
                             "ROOM")]
  })
  #Staff labels for badges
  staff_labels <- reactive({
    if(is.null(input$day0.file2)) return()
    staff_labels <- staff_file()[c("First.Name", "Middle.Name", "Last.Name",
                                   "High.School.Name", "College.Univ", "City", 
                                   "State", "Role.at.Program")]
  })           
  
  ######################################### File Downloads
  #Attendance and Travel Verification
  output$download_d0_travelverify <- downloadHandler(
    filename = function() {"Onsite_ArrivalTravelVerify.csv"},
    content = function(file) {
      write.csv(d0_travelverify(), file, row.names = FALSE)
    }
  )
  #Student self verification
  output$download_studselfverify <- downloadHandler(
    filename = function() {"Registrar_StudentSelfVerify.csv"},
    content = function(file) {
      write.csv(studselfverify(), file, row.names = FALSE)
    }
  )
  #Student labels
  output$download_studlabels <- downloadHandler(
    filename = function() {"Onsite_StudentLabels.csv"},
    content = function(file) {
      write.csv(studlabels(), file, row.names = FALSE)
    }
  )
  #Students with balance
  output$download_studbalance <- downloadHandler(
    filename = function() {"Registrar_StudentswithBalance.csv"},
    content = function(file) {
      write.csv(studbalance(), file, row.names = FALSE)
    }
  )
  #Students missing forms
  output$download_studforms <- downloadHandler(
    filename = function() {"Registrar_StudentsMissingForms.csv"},
    content = function(file) {
      write.csv(studforms(), file, row.names = FALSE)
    }
  )
  #Student Door signs
  output$download_studdoor <- downloadHandler(
    filename = function() {"Onsite_StudentDoorSigns.csv"},
    content = function(file) {
      write.csv(studdoor(), file, row.names = FALSE)
    }
  )
  #Student Dorming lists by female and male
  output$download_d0_room_F <- downloadHandler(
    filename = function() {"Day0_Rooms_F.csv"},
    content = function(file) {
      write.csv(d0_room_F(), file, row.names = FALSE)
    }
  ) 
  output$download_d0_room_M <- downloadHandler(
    filename = function() {"Day0_Rooms_M.csv"},
    content = function(file) {
      write.csv(d0_room_M(), file, row.names = FALSE)
    }
  )
  #Staff labels
  output$download_stafflabels <- downloadHandler(
    filename = function() {"Onsite_StaffLabels.csv"},
    content = function(file) {
      write.csv(staff_labels(), file, row.names = FALSE)
    }
  )
  
  ##############################      Day 1-7 Admin Tasks       #################################### 
  #################################################################################################
  
  ######################################### File input
  ##Generate database for input file
  day1.status <- reactive({
    day1.status <- input$admin.post.file1
    if (is.null(day1.status)) return(NULL)
    read.csv(fill=TRUE,file=input$admin.post.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  day1.register <- reactive({
    day1.register <- input$admin.post.file2
    if (is.null(day1.register)) return(NULL)
    read.csv(fill=TRUE,file=input$admin.post.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })
  
  ######################################### Create dataframe 
  #Registered students
  day1_registered <- reactive({
    if(is.null(day1.status)) return(NULL)
    
    #Read in the database
    registered_w <- day1.status()[c("STATUS", "FNAME", "MNAME", "LNAME", "MF", "CELL", "P1.CELL",
                                    "P2.CELL", "P1", "P2", "HS", "CITY", "ST", "DOT", "ST_NAME",
                                    "DORM", "ROOM", "DEPART_AIR", "DEPART_TIME")]
    
    #Start counter, and end counter as the number of rows
    i=1
    n=nrow(registered_w)
    
    #Run through each row of the table, creating a merged name field for identification
    for (i in 1:n){
      first <- registered_w[i,"FNAME"]
      middle <- registered_w[i,"MNAME"]
      last <- registered_w[i,"LNAME"]
      
      #Merge the first and last name
      #If there is a middle name, add that as well
      names <- paste(last, first, sep=", ")
      if (!(is.null(middle))){
        names <- paste(names,middle,sep=" ")
      }
      
      #Trim any white space from the name
      names <- trimws(names, which = c("both", "left", "right"))
      
      #Add names and total points column to table
      registered_w[i,"NAME"] <- names

      #Output a table of only registered students
      registered_f <- subset(registered_w,STATUS=="Present")
    }
    registered_f
  })

  #Create dataframe of non-attending students
  day1_nonattend <- reactive({
    if(is.null(day1.status)) return(NULL)
      
    #Read in the database
    nonattend_w <- day1.status()["STATUS", "FNAME", "MNAME", "LNAME", "MF", "CELL", "P1.CELL",
                                 "P2.CELL", "P1", "P2", "HS", "CITY", "ST", "DORM", "ROOM"]
    
    #Start counter, and end counter as the number of rows
    i=1
    n=nrow(nonattend_w)
    
    #Run through each row of the table, creating a merged name field for identification
    for (i in 1:n){
      first <- nonattend_w[i,"FNAME"]
      middle <- nonattend_w[i,"MNAME"]
      last <- nonattend_w[i,"LNAME"]
        
      #Merge the first and last name
      #If there is a middle name, add that as well
      names <- paste(last, first, sep=", ")
      if (!(is.null(middle))){
        names <- paste(names,middle,sep=" ")
      }
        
      #Trim any white space from the name
      names <- trimws(names, which = c("both", "left", "right"))
        
      #Add names and total points column to table
      nonattend_w[i,"NAME"] <- names
        
      #Output a table of only registered students
      nonattend_f <- subset(nonattend_w,!(STATUS=="Present"))
    }
    nonattend_f
  })
  
  day1_demoreport <- reactive({
    if(is.null(day1.register)) return(NULL)
    
    #Read in the database
    day1_demoreport <- day1.register()[c("FNAME", "MNAME", "LNAME", "MF", 
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
    
    demoreport
  })
  
  ######################################### Send file to download screen
  #Registration file
  output$download_post_registered <- downloadHandler(
    filename = function() {"Registrar_StudentDB_Registered.csv"},
    content = function(file) {
      write.csv(day1_registered(), file, row.names = FALSE)
    }
  )
  #Non-registration file
  output$download_post_nonattend <- downloadHandler(
    filename = function() {"Registrar_StudentDB_Non-Registered.csv"},
    content = function(file) {
      write.csv(day1_nonattend(), file, row.names = FALSE)
    }
  )
  #Demographics Report
  output$download_post_demoreport <- downloadHandler(
    filename = function() {"Registrar_DemoReport.csv"},
    content = function(file) {
      write.csv(day1_demoreport(), file, row.names = FALSE)
    }
  )

  ###############################          Protocol         ########################################
  #################################################################################################
  
  ######################################### Input Files
  ##Takes the input file saving it to staff database
  proto_stud <- reactive({
    stud1 <- input$proto.file1
    if (is.null(stud1)) return(NULL)
    read.csv(fill=TRUE,file=input$proto.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  proto_staff <- reactive({
    staff1 <- input$proto.file2
    if (is.null(staff1)) return(NULL)
    read.csv(fill=TRUE,file=input$proto.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })
  proto_form <- reactive({
    form1 <- input$proto.file3
    if (is.null(form1)) return(NULL)
    read.csv(fill=TRUE,file=input$proto.file3$datapath, header=TRUE, 
             colClasses = "factor")
  }) 
  
  ######################################### Database Creation
  ftc_protocol <- reactive({
    if(is.null(input$proto.file1)) return()
    if(is.null(input$proto.file3)) return()
    
    #Create dataframe of states, then a list
    states_col <- unique(proto_stud()["ST"])
    states_col <- states_col[,1]
    
    #Create a new list
    states_list <- c()
    for (a in states_col){
      states_list <- c(states_list, a)
    }
    #Past list together with a comma between
    states_list <- paste(states_list, collapse=" -- ")

    #Protocol Database
    ftc_protocol<- proto_form()[c("Year", "Program", "MC", "ED.Welcoming", "Staff.Speaker.1",
                                  "Staff.Speaker.2", "Highest.Staff", "ED.Opening")]
    #Add states list
    ftc_protocol <- merge(ftc_protocol, states_list)

  })
  ftc_staff <- reactive({
    if(is.null(input$proto.file2)) return()

    #Create dataframe of states, then a list
    staff_ori <- proto_staff()[c("FNAME","MNAME","LNAME","CITY","ST","HSSTAT","HS","COLSTAT","UNIV","MAJ",
                                 "STAT","GD","LDZ","CWS","ROLE")]
    
    #Merge Names
    staff_update <- unite(staff_ori, NAME, c(FNAME,MNAME,LNAME), sep = " ", remove = FALSE)
    
    #Update HS/Univ
    ##HS
      i = 1
      staff_list <- staff_ori[,"HS"]
      staff_update[,"HS"] <- as.character(staff_update[,"HS"])
  
      for (a in staff_list){
        print (a)
        if(a==""){
          i=i+1
          next
        } else{
          temp <- staff_update[i,"HS"]
          staff_update[i,"HS"] <- sub("^", "from ", temp )
          i=i+1
        }
      }
      ##Univ
      i = 1
      staff_list <- staff_ori[,"UNIV"]
      staff_update[,"UNIV"] <- as.character(staff_update[,"UNIV"])
      
      for (a in staff_list){
        if(a==""){
          i=i+1
          next
        } else{
          temp <- staff_update[i,"UNIV"]
          staff_update[i,"UNIV"] <- sub("^", "from ", temp )
          i=i+1
        }
      }
    
      #Update Major
      i = 1
      staff_list <- staff_ori[,"MAJ"]
      staff_update[,"MAJ"] <- as.character(staff_update[,"MAJ"])
      
      for (a in staff_list){
        if(a==""){
          i=i+1
          next
        } else{
          temp <- staff_update[i,"MAJ"]
          staff_update[i,"MAJ"] <- sub("^", "majoring in ", temp )
          i=i+1
        }
      }
      
      #Update student status
      i = 1
      staff_list <- staff_ori[,"COLSTAT"]
      staff_update[,"HSSTAT"] <- as.character(staff_update[,"HSSTAT"])
      staff_update[,"COLSTAT"] <- as.character(staff_update[,"COLSTAT"])
      
      for (a in staff_list){
        if(a==""){
          temp <- staff_update[i,"HSSTAT"]
          staff_update[i,"HSSTAT"] <- sub("^", "a high school ", temp )
          i=i+1
        } else if (a=="Graduated"){
          staff_update[i,"COLSTAT"] <- "a professional, having graduated"
          i=i+1
        } else if (a=="Graduate"){
          staff_update[i,"COLSTAT"] <- "a graduate student "
          i=i+1
        } else{
          temp <- staff_update[i,"COLSTAT"]
          staff_update[i,"COLSTAT"] <- sub("^", "a college ", temp )
          i=i+1
        }
      }
      
      #Update student status
      i = 1
      staff_list <- staff_ori[,"ROLE"]

      for (a in staff_list){
        if (a=="Education Director"){
          staff_update[i,"RANK"] <- 10
          i=i+1
        } else if (a=="Apprentice Education Director"){
          staff_update[i,"RANK"] <- 09
          i=i+1
        } else if (a=="HQ Representative"){
          staff_update[i,"RANK"] <- 08
          i=i+1
        } else if (a=="On-Site Director"){
          staff_update[i,"RANK"] <- 07
          i=i+1
        } else if (a=="Assistant On-Site Director"){
          staff_update[i,"RANK"] <- 06
          i=i+1
        } else if (a=="Secretary of State"){
          staff_update[i,"RANK"] <- 05
          i=i+1
        }else if (a == "Assistant Secretary of State"){
          staff_update[i,"RANK"] <- 04
          i=i+1
        } else if (a=="Senior Counselor"){
          staff_update[i,"RANK"] <- 03
          i=i+1
        } else if (a=="Junior Counselor"){
          staff_update[i,"RANK"] <- 02
          i=i+1
        } else{
          staff_update[i,"RANK"] <- 01
          i=i+1
        }
      }
      
    #Staff Final Database
    staff_final <- staff_update[,c("CITY","ST","GD","LDZ","CWS","ROLE","NAME",
                                   "HS","UNIV", "MAJ", "COLSTAT", "HSSTAT", "RANK")]
    staff_final <- staff_final[order(staff_final$RANK),]
    
  })
  
  ######################################### Output Files
  output$download_ftc_protocol <- downloadHandler(
    filename = function() {"Protocol_FTC_Script.csv"},
    content = function(file) {
      write.csv(ftc_protocol(), file, row.names = FALSE)
    }
  )
  output$download_ftc_staff <- downloadHandler(
    filename = function() {"Protocol_FTC_Staff.csv"},
    content = function(file) {
      write.csv(ftc_staff(), file, row.names = FALSE)
    }
  )
  
  
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
  #Read in dataframes
  points.reg <- reactive({
    points.reg <- input$points.file1
    if(is.null(points.reg)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file1$datapath,header=TRUE
    )
  })
  points.prim <- reactive({
    points.prim <- input$points.file2
    if(is.null(points.prim)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file2$datapath,header=TRUE
    )
  })
  points.gen <- reactive({
    points.gen <- input$points.file3
    if(is.null(points.gen)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file3$datapath,header=TRUE
    )
  })
  points.exec <- reactive({
    points.exec <- input$points.file5
    if(is.null(points.exec)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file5$datapath,header=TRUE
    )
  })
  points.prop <- reactive({
    points.prop <- input$points.file13
    if(is.null(points.prop)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file13$datapath,header=TRUE
    )
  })
  points.genpoints <- reactive({
    points.genpoints <- input$points.file8
    if(is.null(points.genpoints)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file8$datapath,header=TRUE
    )
  })
  points.reg2 <- reactive({
    points.reg2 <- input$points.file6
    if(is.null(points.reg2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file6$datapath,header=TRUE
    )
  })
  points.leg <- reactive({
    points.leg <- input$points.file4
    if(is.null(points.leg)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file4$datapath,header=TRUE
    )
  })
  points.jud <- reactive({
    points.jud <- input$points.file14
    if(is.null(points.jud)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file14$datapath,header=TRUE
    )
  })
  
  #Create dataframe for nomination forms by chambers
  points_reg <- reactive({
    if(is.null(points.reg)) return(NULL)
    
    #Read in the database
    points_reg <- points.reg()[]
    points_reg <- points_reg[,c("NAME","POSITION")]
    
    position_list <- c("Supreme Court Justice","Attorney","Cabinet","Senator","Cabinet","House")
    
    points_final <- data.frame()
    b=1
    for (a in position_list){
      
      for(i in 1:nrow(points_reg)){
        value=points_reg[i,"POSITION"]
        if(a==value){
          points_final[b,"NAME"] <- points_reg[i,"NAME"]
          points_final[b,"POSITION"] <- points_reg[i,"POSITION"]
          b=b+1
        } else{next}
      }
    }
    points_final
    
  })
  #Create datframe for judicial fill in file
  points_judfill <- reactive({
    if(is.null(points.reg)) return(NULL)
    
    #Read in the database
    points_reg <- points.reg()[]
    points_reg <- points_reg[,c("NAME","POSITION")]
    
    b=1
    
    points_final <- data.frame()
    
    for(i in 1:nrow(points_reg)){
      value=points_reg[i,"POSITION"]
      if(value == "Attorney" | value=="Supreme Court Justice"){
          points_final[b,"NAME"] <- points_reg[i,"NAME"]
          points_final[b,"POSITION"] <- points_reg[i,"POSITION"]
          b=b+1
        } else{next}
      }
    
    #Create new columns 
    for (i in 1:nrow(points_final)){
      points_final[i,"Round1.Won"] <- ""
      points_final[i,"Round2.Won"] <- ""
      points_final[i,"Round3.Won"] <- ""
      points_final[i,"Round4.Won"] <- ""
      points_final[i,"Chief.Nom"] <- ""
    }
    points_final
  })
  #Create dataframe for leg fill in 
  points_legfill <- reactive({
    if(is.null(points.reg)) return(NULL)
    
    #Read in the database
    points_reg <- points.reg()[]
    points_reg <- points_reg[,c("NAME","POSITION")]
    
    b=1
    
    points_final <- data.frame()
    
    for(i in 1:nrow(points_reg)){
      value=points_reg[i,"POSITION"]
      if(value == "Cabinet" | value=="House" | value=="Senator"){
        points_final[b,"NAME"] <- points_reg[i,"NAME"]
        points_final[b,"POSITION"] <- points_reg[i,"POSITION"]
        b=b+1
      } else{next}
    }
    
    #Create new columns 
    for (i in 1:nrow(points_final)){
      points_final[i,"Speaker.House.Nom"] <- ""
      points_final[i,"House.Srgt.Arms.WIN"] <- ""
      points_final[i,"House.Clerk.WIN"] <- ""
      points_final[i,"House.Prop.Coord.WIN"] <- ""
      points_final[i,"House.Committee"] <- ""
      points_final[i,"House.Majority"] <- ""
      points_final[i,"House.Minority"] <- ""
      points_final[i,"Speaker.Pro.Temp.Nom"] <- ""
      points_final[i,"Speaker.Pro.Tep.WIN"] <- ""
      points_final[i,"Senate.Srgt.Arms.WIN"] <- ""
      points_final[i,"Senate.Clerk.WIN"] <- ""
      points_final[i,"Senate.Prop.Coord.WIN"] <- ""
      points_final[i,"Senate.Committee"] <- ""
      points_final[i,"Senate.Majority"] <- ""
      points_final[i,"Senate.Minority"] <- ""
      
    }
    points_final
  })
  #Create dataframe for exec fill in 
  points_execfill <- reactive({
    if(is.null(points.reg)) return(NULL)
    
    #Read in the database
    points_reg <- points.reg()[]
    points_reg <- points_reg[,c("NAME","POSITION")]
    
    b=1
    
    points_final <- data.frame()
    
    for(i in 1:nrow(points_reg)){
      value=points_reg[i,"POSITION"]
      if(value == "Cabinet"){
        points_final[b,"NAME"] <- points_reg[i,"NAME"]
        points_final[b,"POSITION"] <- points_reg[i,"POSITION"]
        b=b+1
      } else{next}
    }
    
    #Create new columns 
    for (i in 1:nrow(points_final)){
      points_final[i,"CAB.NOM"] <- "YES"
    }
    points_final
  })
  #Create dataframe of Primary results
  points_prim <- reactive({
    if(is.null(points.prim)) return(NULL)
    
    #Read in the database
    points_elect <- points.prim()[]
    points_final <- points_elect[]
    
    for (i in 1:nrow(points_final)){
      points=0
      
      #SENATE
      if (points_elect[i,"SENATE"]=="YES"){
        points = points + 5
      } else{points = 0 + points}
      
      #SUPREME COURT
      if (points_elect[i,"SUPREME.JUSTICE"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      #VP
      if (points_elect[i,"VP"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      #PRESIDENT
      if (points_elect[i,"PRES"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      #PARTY CHAIR
      if (points_elect[i,"PARTY.CHAIR.NOM"]=="YES"){
        points = points + 5
      } else{points = 0 + points}
      
      #PARTY CHAIR
      if (points_elect[i,"PARTY.CHAIR.WIN"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      #PARTY SEC
      if (points_elect[i,"PARTY.SEC.NOM"]=="YES"){
        points = points + 5
      } else{points = 0 + points}
      
      #PARTY SEC
      if (points_elect[i,"PARTY.SEC.WIN"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      #COMMITTEE CHAIR
      if (points_elect[i,"COMMITTEE.NOM"]=="YES"){
        points = points + 5
      } else{points = 0 + points}
      
      #COMMITTEE CHAIR
      if (points_elect[i,"COMMITTEE.WIN"]=="YES"){
        points = points + 10
      } else{points = 0 + points}
      
      points_final[i,"POINTS"] <- points
      
    }
    
    points_final
  })
  #Create dataframe of the election points - general election
  points_gen <- reactive({
    if(is.null(points.gen)) return(NULL)
    
    #Read in the database
    points_elect <- points.gen()[]
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
      else if (points_elect[i,"SENATE_WIN"]==""){
        points = points + 5
      }
      else if (points_elect[i,"SENATE_WIN"]=="WINNER"){
        points = points + 10
      }
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  #Create dataframe of the election points - general election
  points_exec <- reactive({
    if(is.null(points.exec)) return(NULL)
    
    #Read in the database
    points_elect <- points.exec()[]
    points_final <- points_elect[,"NAME"]
    
    
    for (i in 1:nrow(points_elect)){
      points=0
      
      #Cabinet
      if (is.na(points_elect[i,"CAB.NOM"])){
        points = points + 0
      }
      else if (points_elect[i,"CAB.NOM"]=="YES"){
        points = points + 10
      }
      
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  #Create dataframe of the election points - general election
  points_leg <- reactive({
    if(is.null(points.leg)) return(NULL)
    
    #Read in the database
    points_elect <- points.leg()[]
    points_final <- points_elect[,"NAME"]
    
    														
    
    for (i in 1:nrow(points_elect)){
      points=0
      
      #Cabinet
      if (is.na(points_elect[i,"Senate.Committee"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Committee"]=="YES"){
        points = points + 5
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Committee"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Committee"]=="YES"){
        points = points + 5
      }
      #Cabinet
      if (is.na(points_elect[i,"Senate.Minority"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Minority"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Senate.Majority"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Majority"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Minority"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Minority"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Majority"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Majority"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Senate.Prop.Coord.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Prop.Coord.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Senate.Clerk.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Clerk.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Senate.Srgt.Arms.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"Senate.Srgt.Arms.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Speaker.Pro.Tep.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"Speaker.Pro.Tep.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Speaker.Pro.Temp.Nom"])){
        points = points + 0
      }
      else if (points_elect[i,"Speaker.Pro.Temp.Nom"]=="YES"){
        points = points + 5
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Prop.Coord.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Prop.Coord.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Speaker.House.Nom"])){
        points = points + 0
      }
      else if (points_elect[i,"Speaker.House.Nom"]=="YES"){
        points = points + 5
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Srgt.Arms.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Srgt.Arms.WIN"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"House.Clerk.WIN"])){
        points = points + 0
      }
      else if (points_elect[i,"House.Clerk.WIN"]=="YES"){
        points = points + 10
      }
      
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  #Create dataframe of the election points - general election
  points_jud <- reactive({
    if(is.null(points.jud)) return(NULL)
    
    #Read in the database
    points_elect <- points.jud()[]
    points_final <- points_elect[,"NAME"]
    				
    for (i in 1:nrow(points_elect)){
      points=0
      
      #Cabinet
      if (is.na(points_elect[i,"Round1.Won"])){
        points = points + 0
      }
      else if (points_elect[i,"Round1.Won"]=="YES"){
        points = points + 2
      }
      #Cabinet
      if (is.na(points_elect[i,"Round2.Won"])){
        points = points + 0
      }
      else if (points_elect[i,"Round2.Won"]=="YES"){
        points = points + 2
      }
      #Cabinet
      if (is.na(points_elect[i,"Round3.Won"])){
        points = points + 0
      }
      else if (points_elect[i,"Round3.Won"]=="YES"){
        points = points + 2
      }
      #Cabinet
      if (is.na(points_elect[i,"Round4.Won"])){
        points = points + 0
      }
      else if (points_elect[i,"Round4.Won"]=="YES"){
        points = points + 10
      }
      #Cabinet
      if (is.na(points_elect[i,"Chief.Nom"])){
        points = points + 0
      }
      else if (points_elect[i,"Chief.Nom"]=="YES"){
        points = points + 20
      }
      
      
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  #Create dataframe of the election points - general election
  points_prop <- reactive({
    if(is.null(points.prop)) return(NULL)
    
    #Read in the database
    points_elect <- points.prop()[]
    points_final <- points_elect[,"NAME"]

    for (i in 1:nrow(points_elect)){
      points=0
      #Cabinet
      if (is.na(points_elect[i,"President"])){
        points = points + 0
      }
      else if (points_elect[i,"President"]=="Signed"){
        points = points + 25
      }  
      #Cabinet
      if (is.na(points_elect[i,"X3rd.Reading2"])){
        points = points + 0
      }
      else if (points_elect[i,"X3rd.Reading2"]=="Pass"){
        points = points + 20
      }  
      #Cabinet
      if (is.na(points_elect[i,"X3rd.Reading"])){
        points = points + 0
      }
      else if (points_elect[i,"X3rd.Reading"]=="Pass"){
        points = points + 20
      }  
      #Cabinet
      if (is.na(points_elect[i,"X2nd.Reading"])){
        points = points + 0
      }
      else if (points_elect[i,"X2nd.Reading"]=="Pass"){
        points = points + 10
      }      
      
      points_elect[i,"POINTS"] <- points
      
    }
    points_elect
  })
  #Create dataframe for the final points for the winners
  points_final <- reactive({
    if(is.null(points.reg2)) return(NULL)
    if(is.null(points.genpoints)) return(NULL)
    
    #Read in the database
    points_data <- points.reg2()[]
    
    points_gen <- points.genpoints()[,c("NAME","POINTS")]

    points_final <- points_data[,c("NAME","CITY","ST","P1","P2","HS","MF")]
    row.names(points_final) <- points_final$NAME
    
    for (i in 1:nrow(points_final)){
      points_final[i,"GEN"] <- ""
    }
    
    for (i in 1:nrow(points_gen)){
      name <- points_gen[i,"NAME"]
      points_final[name,"GEN"] <- points_gen[i,"POINTS"]
    }
    
    points_final
  })
  ###############################Output files 
  #Points - Chamber File
  output$download_points_chambers <- downloadHandler(
    filename = function() {"Awards_Voting_Chambers.csv"},
    content = function(file) {
      write.csv(points_reg(), file, row.names = FALSE)
    }
  )  #Election - Nominations for Party
  output$download_points_prim <- downloadHandler(
    filename = function() {"Points_Election_Party.csv"},
    content = function(file) {
      write.csv(points_prim(), file, row.names = FALSE)
    }
  )
  #Election - General Election
  output$download_points_gen <- downloadHandler(
    filename = function() {"Points_Election_General.csv"},
    content = function(file) {
      write.csv(points_gen(), file, row.names = FALSE)
    }
  )
  #Jud Fill in
  output$download_points_judfill <- downloadHandler(
    filename = function() {"Points_JudicialFillin.csv"},
    content = function(file) {
      write.csv(points_judfill(), file, row.names = FALSE)
    }
  )
  #Leg Fillin
  output$download_points_legfill <- downloadHandler(
    filename = function() {"Points_LegislativeFillin.csv"},
    content = function(file) {
      write.csv(points_legfill(), file, row.names = FALSE)
    }
  )
  #Exec Fillin
  output$download_points_execfill <- downloadHandler(
    filename = function() {"Points_ExecutiveFillin.csv"},
    content = function(file) {
      write.csv(points_execfill(), file, row.names = FALSE)
    }
  )
  #Exec Points
  output$download_points_exec <- downloadHandler(
    filename = function() {"Points_Executive.csv"},
    content = function(file) {
      write.csv(points_exec(), file, row.names = FALSE)
    }
  )
  #Jud Points
  output$download_points_jud <- downloadHandler(
    filename = function() {"Points_Executive.csv"},
    content = function(file) {
      write.csv(points_jud(), file, row.names = FALSE)
    }
  )
  #Leg Points
  output$download_points_leg <- downloadHandler(
    filename = function() {"Points_Legislative.csv"},
    content = function(file) {
      write.csv(points_leg(), file, row.names = FALSE)
    }
  )
  #Prop Points
  output$download_points_prop <- downloadHandler(
    filename = function() {"Points_Proposal.csv"},
    content = function(file) {
      write.csv(points_prop(), file, row.names = FALSE)
    }
  )
  #Awards Points
  output$download_points_win <- downloadHandler(
    filename = function() {"Points_Awards.csv"},
    content = function(file) {
      write.csv(points_final(), file, row.names = FALSE)
    }
  )
  ##############################           Elections             #################################### 
  ####################################################################################################
  
  ######################################### File input
  ##Generate database for input file
  elect.reg <- reactive({
    elect.reg <- input$elect.file1
    if (is.null(elect.reg)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })
  elect.fillin <- reactive({
    elect.fillin <- input$elect.file2
    if (is.null(elect.fillin)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })
  elect.win <- reactive({
    elect.win <- input$elect.file3
    if (is.null(elect.win)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file3$datapath, header=TRUE, 
             colClasses = "factor")
  })
  elect.commish<- reactive({
    elect.commish <- input$elect.file4
    if (is.null(elect.commish)) return(NULL)
    read.csv(fill=TRUE,file=input$elect.file4$datapath, header=TRUE, 
             colClasses = "factor")
  })
  ######################################### Create dataframe 
  #Registered students - to create general nomination fill in sheet
  elect_reg_primary <- reactive({
    if(is.null(elect.reg)) return(NULL)
    
    #Read in the database
    elect_reg <- elect.reg()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "NAME")]
    
    #Create new columns with position names
    nom_titles <- c("SENATE", "SUPREME.JUSTICE", "VP", "PRES", "PARTY.CHAIR.NOM", 
                    "PARTY.CHAIR.WIN", "PARTY.SEC.NOM", "PARTY.SEC.WIN", "COMMITTEE.NOM",
                    "COMMITTEE.WIN")
    
    #For each position add a blank row
    for(a in nom_titles){
      b=1
      
      while(b < nrow(elect_reg)+1){
        elect_reg[b,a]<-""
        b=b+1
      }
      
    }
    elect_reg
  })
  #Registered students - to create general nomination fill in sheet
  elect_reg_general <- reactive({
    if(is.null(elect.reg)) return(NULL)
    
    #Read in the database
    elect_reg <- elect.reg()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "ST_NAME", "NAME","DORM", "ROOM")]
    
    #Create new columns with position names
    nom_titles <- c("SENATE", "SUPREME.JUSTICE", "VP", "ATTORNEY", "PRES")
    
    #For each position add a blank row
    for(a in nom_titles){
      b=1
      
      while(b < nrow(elect_reg)+1){
        elect_reg[b,a]<-""
        b=b+1
      }
      
    }
    elect_reg
  })
  #Create Nomination official roster
  elect_nominees <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME","DORM", "ROOM",
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

  })
  #Create Nomination official roster
  elect_judballot <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME","DORM", "ROOM",
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
    
    elect_nominees_final

  })
  #Create Winners Document
  elect_winfill <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME","DORM", "ROOM",
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
    attorney <- subset(elect_nominees, elect_nominees$ATTORNEY %in% party_list)
    attorney <-attorney[order(attorney$ATTORNEY),]
    
    #Start Counters
    i=1
    count=1
    
    #Add Justices
    while(count<10){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- justices[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- justices[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- justices[count,"LNAME"]
      elect_nominees_final[i,"NAME"] <- justices[count,"NAME"]
      elect_nominees_final[i,"CITY"] <- justices[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- justices[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- justices[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- justices[count,"ST_NAME"]
      elect_nominees_final[i,"SUPREME.JUSTICE"] <-justices[count,"SUPREME.JUSTICE"]
      elect_nominees_final[i,"SUPREME.JUSTICE_WIN"] <- ""
      
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- justices[count+9,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- justices[count+9,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- justices[count+9,"LNAME"]
      elect_nominees_final[i+1,"NAME"] <- justices[count+9,"NAME"]
      elect_nominees_final[i+1,"CITY"] <- justices[count+9,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- justices[count+9,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- justices[count+9,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- justices[count+9,"ST_NAME"]
      elect_nominees_final[i+1,"SUPREME.JUSTICE"] <- justices[count+9,"SUPREME.JUSTICE"]
      elect_nominees_final[i+1,"SUPREME.JUSTICE_WIN"] <- ""
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Add Attorneys
    count=1
    while(count<13){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- attorney[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- attorney[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- attorney[count,"LNAME"]
      elect_nominees_final[i,"NAME"] <- attorney[count,"NAME"]
      elect_nominees_final[i,"CITY"] <- attorney[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- attorney[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- attorney[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- attorney[count,"ST_NAME"]
      elect_nominees_final[i,"ATTORNEY"] <- attorney[count,"ATTORNEY"]

      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- attorney[count+12,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- attorney[count+12,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- attorney[count+12,"LNAME"]
      elect_nominees_final[i+1,"NAME"] <- attorney[count+12,"NAME"]
      elect_nominees_final[i+1,"CITY"] <- attorney[count+12,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- attorney[count+12,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- attorney[count+12,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- attorney[count+12,"ST_NAME"]
      elect_nominees_final[i+1,"ATTORNEY"] <- attorney[count+12,"ATTORNEY"]
      
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
      elect_nominees_final[i,"NAME"] <- vp[count,"NAME"]
      elect_nominees_final[i,"CITY"] <- vp[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- vp[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- vp[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- vp[count,"ST_NAME"]
      elect_nominees_final[i,"VP"] <- vp[count,"VP"]
      elect_nominees_final[i,"VP_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- vp[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- vp[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- vp[count+1,"LNAME"]
      elect_nominees_final[i+1,"NAME"] <- vp[count+1,"NAME"]
      elect_nominees_final[i+1,"CITY"] <- vp[count+1,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- vp[count+1,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- vp[count+1,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- vp[count+1,"ST_NAME"]
      elect_nominees_final[i+1,"VP"] <- vp[count+1,"VP"]
      elect_nominees_final[i+1,"VP_WIN"] <- ""
      
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
      elect_nominees_final[i,"NAME"] <- pres[count,"NAME"]
      elect_nominees_final[i,"CITY"] <- pres[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- pres[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- pres[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- pres[count,"ST_NAME"]
      elect_nominees_final[i,"PRES"] <- pres[count,"PRES"]
      elect_nominees_final[i,"PRES_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- pres[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- pres[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- pres[count+1,"LNAME"]
      elect_nominees_final[i+1,"NAME"] <- pres[count+1,"NAME"]
      elect_nominees_final[i+1,"CITY"] <- pres[count+1,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- pres[count+1,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- pres[count+1,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- pres[count+1,"ST_NAME"]
      elect_nominees_final[i+1,"PRES"] <- pres[count+1,"PRES"]
      elect_nominees_final[i+1,"PRES_WIN"] <- ""
      
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
      elect_nominees_final[i,"NAME"] <- senate[count,"NAME"]
      elect_nominees_final[i,"CITY"] <- senate[count,"CITY"]
      elect_nominees_final[i,"DORM"] <- senate[count,"DORM"]
      elect_nominees_final[i,"ROOM"] <- senate[count,"ROOM"]
      elect_nominees_final[i,"ST_NAME"] <- senate[count,"ST_NAME"]
      elect_nominees_final[i,"SENATE"] <- senate[count,"SENATE"]
      elect_nominees_final[i,"SENATE_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- senate[count+40,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- senate[count+40,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- senate[count+40,"LNAME"]
      elect_nominees_final[i+1,"NAME"] <- senate[count+40,"NAME"]
      elect_nominees_final[i+1,"CITY"] <- senate[count+40,"CITY"]
      elect_nominees_final[i+1,"DORM"] <- senate[count+40,"DORM"]
      elect_nominees_final[i+1,"ROOM"] <- senate[count+40,"ROOM"]
      elect_nominees_final[i+1,"ST_NAME"] <- senate[count+40,"ST_NAME"]
      elect_nominees_final[i+1,"SENATE"] <- senate[count+40,"SENATE"]
      elect_nominees_final[i+1,"SENATE_WIN"] <- ""
      
      #Increase Counters
      i=i+2
      count=count+1
    }
    
    elect_nominees_final
    
  })
  #Creation Commissioners Report
  elect_commish <- reactive({
    if(is.null(elect.win)) return(NULL)
    
    #Read in the database
    elect_winners <- elect.win()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME","DORM", 
                                   "ROOM","SENATE", "SENATE_WIN","SUPREME.JUSTICE", 
                                   "SUPREME.JUSTICE_WIN", "VP", "VP_WIN", "ATTORNEY",
                                   "PRES","PRES_WIN")]
    
    #Run for Announcement 1
    election_final <- subset(elect_winners, elect_winners$SUPREME.JUSTICE_WIN=="WINNER" | 
                               !(elect_winners$ATTORNEY=="NA") |
                               (elect_winners$VP_WIN=="WINNER") |
                               (elect_winners$PRES_WIN=="WINNER")|
                               (elect_winners$SENATE_WIN=="WINNER")
                             )
    #Order the database
    election_final[order(election_final$SUPREME.JUSTICE_WIN, election_final$ATTORNEY, 
                        election_final$VP_WIN,election_final$PRES_WIN, election_final$SENATE_WIN),]
    
    #Add position column
    for (i in 1:9){
        election_final[i,"POSITION"] <- "Supreme Court Justice"
    }
    for (i in 10:33){
      election_final[i,"POSITION"] <- "Attorney"
    }
    for (i in 34:34){
      election_final[i,"POSITION"] <- "Vice-President"
    }
    for (i in 35:35){
      election_final[i,"POSITION"] <- "President"
    }   
    for (i in 36:75){
      election_final[i,"POSITION"] <- "Senator"
    }
    election_final
    
  })
  #Create new student database with elected positions
  elect_reg2 <- reactive({
    if(is.null(elect.commish)) return(NULL)
    elect_reg2 <- elect.commish()[,c("NAME","POSITION")]
    student_reg <- elect.reg()[]
    
    elect_final <- merge(student_reg,elect_reg2,all.x=TRUE)
  })
  #Create dataframe of the election points - party nomination form
  elect_nomfill <- reactive({
    if(is.null(elect.reg)) return(NULL)
    
    #Read in the database
    elect_nomfill <- elect.reg()[]
    points_final <- elect_nomfill[,c("NAME","CITY","ST")]
    
    for (i in 1:nrow(points_final)){
      points_final[i,"CHAIR_NOM"] <- ""
      points_final[i,"SEC_NOM"] <- ""
      points_final[i,"CHAIR_WIN"] <- ""
      points_final[i,"SEC_WIN"] <- ""
      points_final[i,"PRES_NOM"] <- ""
      points_final[i,"VP_NOM"] <- ""
      points_final[i,"SENATE_NOM"] <- ""
      points_final[i,"JUD_NOM"] <- ""
    }
    
    points_final
  })
  ######################################### Send file to download screen
  #Primary Nomination File to fill in
  output$download_elect_primarynomineefillin <- downloadHandler(
    filename = function() {"Election_PrimaryNomineeFillIn.csv"},
    content = function(file) {
      write.csv(elect_reg_primary(), file, row.names = FALSE)
    }
  )  #General Nomination File to fill in
  output$download_elect_generalnomineefillin <- downloadHandler(
    filename = function() {"Election_GeneralNomineeFillIn.csv"},
    content = function(file) {
      write.csv(elect_reg_general(), file, row.names = FALSE)
    }
  )
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
  #Election Winners
  output$download_elect_winnersfillin <- downloadHandler(
    filename = function() {"Election_WinnerFillIn.csv"},
    content = function(file) {
      write.csv(elect_winfill(), file, row.names = FALSE)
    }
  )
  #Election Winners for Commish Report
  output$download_election_commish <- downloadHandler(
    filename = function() {"Election_CommishReport.csv"},
    content = function(file) {
      write.csv(elect_commish(), file, row.names = FALSE)
    }
  )
  #New Student database with position information
  output$download_election_registrar <- downloadHandler(
    filename = function() {"Registrar_StudentDB_Elections.csv"},
    content = function(file) {
      write.csv(elect_reg2(), file, row.names = FALSE)
    }
  )
  #Election - Nominations Fill-In
  output$download_election_nomfillin <- downloadHandler(
    filename = function() {"Election_PartyNomFillin.csv"},
    content = function(file) {
      write.csv(elect_nomfill(), file, row.names = FALSE)
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