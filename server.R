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
    staff_labels <- staff_file()[c("FNAME", "MNAME", "LNAME", "HS", "UNIV", "CITY",
                                  "ST", "ROLE")]
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

  ####################################           Points           ###################################
  ###################################################################################################
  
  ##################################Forming the Community 
  #Import expected student demo file
  points.ftc <- reactive({
    points.ftc <- input$points.file1
    if(is.null(points.ftc)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file1$datapath,header=TRUE
    )
  })
  
  #Create dataframe with users name merged, for staff to print and track points
  points_ftc <- reactive({
    if(is.null(points.ftc)) return(NULL)
    
    #Read in the database
    points_ftc_w <- points.ftc()[c("FNAME", "MNAME", "LNAME", "DOT")]
    
    #Add  total points column to table
    points_ftc_w[,"TOTAL_PTS"] <- ""
    
    #Output a table with only name, dot group, and total points columns
    points_ftc_f <- points_ftc_w[,c("NAME", "DOT","TOTAL_PTS")]
    
    #Output database
    points_ftc_f[]
  })
  
  #Send file to download screen
  output$download_points_ftc <- downloadHandler(
    filename = function() {"Points_FormingTheCommunity.csv"},
    content = function(file) {
      write.csv(points_ftc(), file, row.names = FALSE)
    }
  )

  ############################### General Convention - Day 2 OR 3
  ##Import expected student demo file
  points.gc2 <- reactive({
    points.gc2 <- input$points.file2
    if(is.null(points.gc2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file2$datapath,header=TRUE
    )
  })
  
  #Create drop down choices for user to select the day to be created
  observe({
    dsnames <- c("Points_GC_Day2", "Points_GC_Day3")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$gc.day<- renderUI({
      selectInput("gc_day", "Which day to create for?", cb_options)
    })
  })
  
  ##Create dataframe with users name merged, for staff to print and track points
  points_gc2 <- reactive({
    if(is.null(points.gc2)) return(NULL)
    
    #Read in the database
    points_gc2_w <- points.gc2()[c("FNAME", "MNAME", "LNAME", "DOT")]
    
    
    #Add names and total points column to table
    points_gc2_w[,"TOTAL_PTS"] <- ""
      
    #Output a table with only name, dot group, and total points columns
    points_gc2_f <- points_gc2_w[,c("NAME", "DOT","TOTAL_PTS")]
    
    #Output database
    points_gc2_f[]
  })
  
  ##Send file to download screen
  output$download_points_gc <- downloadHandler(
    filename = function() {
      paste(input$gc_day, ".csv", sep="_Points")
    },
    content = function(file) {
      write.csv(points_gc2(), file, row.names = FALSE)
    }
  )
  
  ############################### Legislative Session - Day 4, 5, 6
  ##Import expected student demo file
  points.leg <- reactive({
    points.leg <- input$points.file4
    if(is.null(points.leg)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file4$datapath,header=TRUE
    )
  })
  
  #Create drop down choices for user to select the day to be created
  observe({
    dsnames <- c("Points_Leg_Day4", "Points_Leg_Day5", "Points_Leg_Day6")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$leg.day<- renderUI({
      selectInput("leg_day", "Which day to create for?", cb_options)
    })
  })
  
  ##Create dataframe with users name merged, for staff to print and track points
  points_leg <- reactive({
    if(is.null(points.leg)) return(NULL)
    
    #Read in the database
    points_leg_w <- points.leg()[c("FNAME", "MNAME", "LNAME", "POSITION")]
    
    #Add names and total points column to table
    points_leg_w[,"TOTAL_PTS"] <- ""
      
    #Output a table with only name, dot group, and total points columns
    points_leg_f <- points_leg_w[,c("NAME", "POSITION","TOTAL_PTS")]
    
    #Subset the dataframe to include only House and Senate members
    points_leg_f <- subset(points_leg_f, POSITION=="HOUSE" | POSITION== "SENATE")
    points_leg_f <- points_leg_f[order(points_leg_f$POSITION, decreasing = TRUE),]
    
    #Output database
    points_leg_f[]
  })
  
  ##Send file to download screen
  output$download_points_leg <- downloadHandler(
    filename = function() {
      paste(input$leg_day, ".csv", sep="")
    },
    content = function(file) {
      write.csv(points_leg(), file, row.names = FALSE)
    }
  )

  ############################### Voting
  
  ##Create dataframe with users name merged, for staff to print and track points
  vote_out <- reactive({
    if(is.null(vote.in)) return(NULL)
    
    #Read in the database
    vote_out_w <- vote.in()[c("FNAME", "MNAME", "LNAME", "POSITION")]
    
    #Add total points column to table
    points_leg_w[,"TOTAL_PTS"] <- ""
      
    #Output a table with only name, dot group, and total points columns
    points_leg_f <- points_leg_w[,c("NAME", "POSITION","TOTAL_PTS")]
    
    #Subset the dataframe to include only House and Senate members
    points_leg_f <- subset(points_leg_f, POSITION=="HOUSE" | POSITION== "SENATE")
    points_leg_f <- points_leg_f[order(points_leg_f$POSITION, decreasing = TRUE),]
    
    #Output database
    points_leg_f[]
  })
  
  ##Send file to download screen
  output$download_points_leg <- downloadHandler(
    filename = function() {
      paste(input$leg_day, ".csv", sep="")
    },
    content = function(file) {
      write.csv(points_leg(), file, row.names = FALSE)
    }
  )  

  ############################### Daily Points Summaries
  #################Day1
  ##Import files
  points.day1 <- reactive({
    points.day1 <- input$points.file8
    if(is.null(points.day1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file8$datapath,header=TRUE
    )
  })

  ##Create Day 1 points total 
  points_day1 <- reactive({
    if(is.null(points.day1)) return(NULL)
    
    points_ftc_w <- points.day1()[c("NAME", "TOTAL_PTS")]
    
    #Set counters at 1, and add zero to all non-points students
    i = 1
    n=nrow(points_gc2_w)
    
    for(i in 1:n){
      value = points_ftc_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_ftc_w[i,"TOTAL_PTS"] =0
      }
    }
    points_ftc_w
  })
  
  #################Day 2
  ##Import Files
  points.day2.1 <- reactive({
    points.day2.1 <- input$points.file9.1
    if(is.null(points.day2.1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file9.1$datapath,header=TRUE
    )
  })
  points.day2.2<- reactive({
    points.day2.2 <- input$points.file9.2
    if(is.null(points.day2.2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file9.2$datapath,header=TRUE
    )
  })
  
  #Create Day 2 table Summary
  points_day2 <- reactive({
    if(is.null(points.day2.1)) return(NULL)
    if(is.null(points.day2.1)) return(NULL)
    
    #######General Convention Addition    #######
    ##Create new table
    points_gc2_w <- points.day2.1()[c("NAME", "TOTAL_PTS")]

    #Set counters at 1, and add zero to all non-points students
    i = 1
    n=nrow(points_gc2_w)
    
    for(i in 1:n){
      value = points_gc2_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_gc2_w[i,"TOTAL_PTS"] =0
      }
    }
    
    #Remove Dot info from the table, assign row names
    points_gc2_f <- points_gc2_w[,c("NAME","TOTAL_PTS")]
    row.names(points_gc2_f) <- points_gc2_f$NAME
    
    ########Nominations Points    #######
    #Read in the GC Day 2 database
    points_nom_w <- points.day2.2()[c("NAME", "POSITION")]
    
    #Set student name as the row name
    row.names(points_nom_w) <- points_nom_w$NAME
    name_list <- points_nom_w$NAME
    
    #Go through each nam of the database 
    for (a in name_list){
      value=0
      position <- points_nom_w[a,"POSITION"]
      
      if(position=="SENATE"){
        value=5
      } else if (position=="JUSTICE"){
        value=15
      } else if (position=="VICE-PRESIDENT"){
        value=20
      } else if (position=="PRESIDENT"){
        value=25
      } 
      
      #Send value to points
      points_nom_w[a,"NOM_PTS"] <- value
      
      #Sum for total points
      points_nom_w[a,"GC_PTS"] <- points_gc2_f[a,"TOTAL_PTS"]
      points_nom_w[a,"TOTAL"] <- points_nom_w[a,"NOM_PTS"] + points_nom_w[a,"GC_PTS"]
    }
    
    #Create final table
    points_day2 <- points_nom_w[c("NAME","GC_PTS","NOM_PTS","TOTAL")]
    points_day2
  })
  
  #################Day 3
  ##Import Files
  points.day3.1 <- reactive({
    points.day3.1 <- input$points.file10.1
    if(is.null(points.day3.1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file10.1$datapath,header=TRUE
    )
  })
  points.day3.2<- reactive({
    points.day3.2 <- input$points.file10.2
    if(is.null(points.day3.2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file10.2$datapath,header=TRUE
    )
  })
  
  #Create Day 3 table Summary
  points_day3 <- reactive({
    if(is.null(points.day3.1)) return(NULL)
    if(is.null(points.day3.2)) return(NULL)
    
    #######General Convention Addition
    ##Create new table
    points_gc3_w <- points.day3.1()[c("NAME", "TOTAL_PTS")]
    
    #Set counters at 1, and add zero to all non-points students
    i = 1
    n=nrow(points_gc3_w)
    
    for(i in 1:n){
      value = points_gc3_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_gc3_w[i,"TOTAL_PTS"] =0
      }
    }
    
    #Remove Dot info from the table, assign row names
    points_gc3_f <- points_gc3_w[,c("NAME","TOTAL_PTS")]
    row.names(points_gc3_f) <- points_gc3_f$NAME
    
    ########Nominations Points
    #Read in the GC Day 3 database
    points_nom_w <- points.day3.2()[c("NAME", "POSITION")]
    
    #Set student name as the row name
    row.names(points_nom_w) <- points_nom_w$NAME
    name_list <- points_nom_w$NAME
    
    #Go through each nam of the database 
    for (a in name_list){
      value=0
      position <- points_nom_w[a,"POSITION"]
      
      if(position=="SENATE"){
        value=5
      } else if (position=="JUSTICE"){
        value=15
      } else if (position=="VICE-PRESIDENT"){
        value=20
      } else if (position=="PRESIDENT"){
        value=25
      } 
      
      #Send value to points
      points_nom_w[a,"WIN_PTS"] <- value
      
      #Sum for total points
      points_nom_w[a,"GC_PTS"] <- points_gc3_f[a,"TOTAL_PTS"]
      points_nom_w[a,"TOTAL"] <- points_nom_w[a,"WIN_PTS"] + points_nom_w[a,"GC_PTS"]
    }
    
    #Create final table
    points_day3 <- points_nom_w[c("NAME","GC_PTS","WIN_PTS","TOTAL")]
    points_day3
  })
  
  #################Day 4
  ##Import Files
  points.day4 <- reactive({
    points.day4 <- input$points.file13
    if(is.null(points.day4)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file13$datapath,header=TRUE
    )
  })
  
  #Create Day 4 table Summary
  points_day4 <- reactive({
    if(is.null(points_day4)) return(NULL)

    #######General Convention Addition
    ##Create new table
    points_leg4_w <- points.day4()[c("NAME", "TOTAL_PTS")]
    
    #Set counters at 1, and add zero to all non-points students
    i = 1
    n=nrow(points_leg4_w)
    
    for(i in 1:n){
      value <- points_leg4_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_leg4_w[i,"TOTAL_PTS"] =0
      }
    }
    
    #Remove Dot info from the table, assign row names
    points_leg4_f <- points_leg4_w[,c("NAME","TOTAL_PTS")]
    points_leg4_f
  })
  
  #################Day 5
  ##Import Files
  points.day5 <- reactive({
    points.day5 <- input$points.file14
    if(is.null(points.day5)) return(NULL)
       read.csv(fill=TRUE,file=input$points.file14$datapath,header=TRUE
       )
  })
      
  #Create Day 5 table Summary
  points_day5 <- reactive({
    if(is.null(points.day5)) return(NULL)
    
    #######General Convention Addition
    ##Create new table
    points_leg5_w <- points.day5()[c("NAME", "TOTAL_PTS")]
        
    #Set counters at 1, and add zero to all non-points students
    i = 1
    n=nrow(points_leg5_w)
        
    for(i in 1:n){
      value = points_leg5_w[i,"TOTAL_PTS"]
          
      if(is.na(value)){
        points_leg5_w[i,"TOTAL_PTS"] =0
        }
      }
        
      #Remove Dot info from the table, assign row names
      points_leg5_f <- points_leg5_w[,c("NAME","TOTAL_PTS")]
      points_leg5_f
    })
  
  #################Day 6
  ##Import house voting file
  points.day6.1 <- reactive({
    points.day6.1 <- input$points.file15
    if(is.null(points.day6.1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file15$datapath,header=TRUE
    )
  })
  ##Import senate voting file
  points.day6.2 <- reactive({
    points.day6.2 <- input$points.file16
    if(is.null(points.day6.2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file16$datapath,header=TRUE
    )
  })
  ##Import judicial voting file
  points.day6.3 <- reactive({
    points.day6.3 <- input$points.file17
    if(is.null(points.day6.3)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file17$datapath,header=TRUE
    )
  })
  ##Import executive voting file
  points.day6.4 <- reactive({
    points.day6.4 <- input$points.file18
    if(is.null(points.day6.4)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file18$datapath,header=TRUE
    )
  })
  ##Import studentdemo registered file
  points.day6.5 <- reactive({
    points.day6.5 <- input$points.file19
    if(is.null(points.day6.5)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file19$datapath,header=TRUE
    )
  })
  points.day6.6 <- reactive({
    points.day6.6 <- input$points.file20
    if(is.null(points.day6.6)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file20$datapath,header=TRUE
    )
  })
  
  #Create Day 6 table Summary
  points_day6 <- reactive({
    if(is.null(points.day6.5)) return(NULL)
    
    #######General Convention Addition
    ##Create new tables for each file
    points_house_w <- points.day6.1()[]
    points_senate_w <- points.day6.2()[]
    points_jud_w <- points.day6.3()[]
    points_exec_w <- points.day6.4()[]
    points_reg_w <- points.day6.5()[c("NAME")]
    
    #Assign rownames to database
    row.names(points_reg_w) <- points_reg_w$NAME
    row.names(points_house_w) <- points_house_w$NAME
    row.names(points_senate_w) <- points_senate_w$NAME
    row.names(points_jud_w) <- points_jud_w$NAME
    row.names(points_exec_w) <- points_exec_w$NAME
    
    #Create naming list
    name_list <- points_reg_w$NAME
    
    #Most Promising female
    for (a in name_list){
      value_h = 0
      value_h <- points_house_w[a,"MOST_PROM_FEM"]
      if(is.na(value_h)){value_h=0}
      
      value_s = 0
      value_s <- points_senate_w[a,"MOST_PROM_FEM"]
      if(is.na(value_s)){value_s=0}
      
      value_j = 0
      value_j <- points_jud_w[a,"MOST_PROM_FEM"]
      if(is.na(value_j)){value_j=0}
      
      value_e = 0
      value_e <- points_exec_w[a,"MOST_PROM_FEM"]
      if(is.na(value_e)){value_e=0}
      
      points_reg_w[a,"MOST_PROM_FEM"] <- value_h + value_s + value_e + value_j
    }
    
    #######Leg Session Addition
    ##Create new table
    points_leg6_w <- points.day6.6()[c("NAME", "TOTAL_PTS")]
    row.names(points_leg6_w) <- points_leg6_w$NAME
    name_list <- points_leg6_w$NAME
    
    #Most Promising female
    for (a in name_list){
      value = 0
      value <- points_leg6_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_leg6_w[i,"TOTAL_PTS"] =0
      }
      points_reg_w[a,"TOTAL_PTS"] <- value
      
    }

    #Send final document
    points_day6_f <- points_reg_w[,c("NAME","MOST_PROM_FEM","TOTAL_PTS")]
    points_day6_f
  })
  

  ###############################Output files 
  #Day1
  output$download_points_day1 <- downloadHandler(
    filename = function() {"Points_Day1.csv"},
    content = function(file) {
      write.csv(points_day1(), file, row.names = FALSE)
    }
  )
  #Day2
  output$download_points_day2 <- downloadHandler(
    filename = function() {"Points_Day2.csv"},
    content = function(file) {
      write.csv(points_day2(), file, row.names = FALSE)
    }
  )
  #Day3
  output$download_points_day3 <- downloadHandler(
    filename = function() {"Points_Day3.csv"},
    content = function(file) {
      write.csv(points_day3(), file, row.names = FALSE)
    }
  )
  #Day4
  output$download_points_day4 <- downloadHandler(
    filename = function() {"Points_Day4.csv"},
    content = function(file) {
      write.csv(points_day4(), file, row.names = FALSE)
    }
  )
  #Day5
  output$download_points_day5 <- downloadHandler(
    filename = function() {"Points_Day5.csv"},
    content = function(file) {
      write.csv(points_day5(), file, row.names = FALSE)
    }
  )
  #Day6
  output$download_points_day6 <- downloadHandler(
    filename = function() {"Points_Day6.csv"},
    content = function(file) {
      write.csv(points_day6(), file, row.names = FALSE)
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
  
  ######################################### Create dataframe 
  #Registered students - to create nomination fill in sheet
  elect_reg <- reactive({
    if(is.null(elect.reg)) return(NULL)
    
    #Read in the database
    elect_reg <- elect.reg()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "ST_NAME", "NAME")]
    
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
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME",
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

    
    #Add VP
    ##Start Counters
    count=1
    
    while(count<2){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- vp[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- vp[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- vp[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- vp[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- vp[count,"ST_NAME"]
      elect_nominees_final[i,"VP"] <- vp[count,"VP"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- vp[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- vp[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- vp[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- vp[count+1,"CITY"]
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
      elect_nominees_final[i,"ST_NAME"] <- pres[count,"ST_NAME"]
      elect_nominees_final[i,"PRES"] <- pres[count,"PRES"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- pres[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- pres[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- pres[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- pres[count+1,"CITY"]
      elect_nominees_final[i+1,"ST_NAME"] <- pres[count+1,"ST_NAME"]
      elect_nominees_final[i+1,"PRES"] <- pres[count+1,"PRES"]
      
      #Increase Counters
      i=i+2
      count = count+1
    }
    
    #Add Senate
    ###Start Counters
    count=1
    
    while(count<26){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- senate[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- senate[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- senate[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- senate[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- senate[count,"ST_NAME"]
      elect_nominees_final[i,"SENATE"] <- senate[count,"SENATE"]
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- senate[count+25,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- senate[count+25,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- senate[count+25,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- senate[count+25,"CITY"]
      elect_nominees_final[i+1,"ST_NAME"] <- senate[count+25,"ST_NAME"]
      elect_nominees_final[i+1,"SENATE"] <- senate[count+25,"SENATE"]
      
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
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME",
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
  #Create Nomination official roster
  elect_winfill <- reactive({
    if(is.null(elect.fillin)) return(NULL)
    
    #Read in the database
    elect_nominees <- elect.fillin()[c("FNAME", "MNAME", "LNAME", "CITY", "ST_NAME", "NAME",
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
      elect_nominees_final[i,"CITY"] <- justices[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- justices[count,"ST_NAME"]
      elect_nominees_final[i,"SUPREME.JUSTICE"] <-justices[count,"SUPREME.JUSTICE"]
      elect_nominees_final[i,"SUPREME.JUSTICE_WIN"] <- ""
      
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- justices[count+9,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- justices[count+9,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- justices[count+9,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- justices[count+9,"CITY"]
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
      elect_nominees_final[i,"CITY"] <- attorney[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- attorney[count,"ST_NAME"]
      elect_nominees_final[i,"ATTORNEY"] <- attorney[count,"ATTORNEY"]

      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- attorney[count+12,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- attorney[count+12,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- attorney[count+12,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- attorney[count+12,"CITY"]
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
      elect_nominees_final[i,"CITY"] <- vp[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- vp[count,"ST_NAME"]
      elect_nominees_final[i,"VP"] <- vp[count,"VP"]
      elect_nominees_final[i,"VP_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- vp[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- vp[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- vp[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- vp[count+1,"CITY"]
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
      elect_nominees_final[i,"CITY"] <- pres[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- pres[count,"ST_NAME"]
      elect_nominees_final[i,"PRES"] <- pres[count,"PRES"]
      elect_nominees_final[i,"PRES_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- pres[count+1,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- pres[count+1,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- pres[count+1,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- pres[count+1,"CITY"]
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
    
    while(count<26){
      #Party A Candidate
      elect_nominees_final[i,"FNAME"] <- senate[count,"FNAME"]
      elect_nominees_final[i,"MNAME"] <- senate[count,"MNAME"]
      elect_nominees_final[i,"LNAME"] <- senate[count,"LNAME"]
      elect_nominees_final[i,"CITY"] <- senate[count,"CITY"]
      elect_nominees_final[i,"ST_NAME"] <- senate[count,"ST_NAME"]
      elect_nominees_final[i,"SENATE"] <- senate[count,"SENATE"]
      elect_nominees_final[i,"SENATE_WIN"] <- ""
      
      #Party B Candidate
      elect_nominees_final[i+1,"FNAME"] <- senate[count+25,"FNAME"]
      elect_nominees_final[i+1,"MNAME"] <- senate[count+25,"MNAME"]
      elect_nominees_final[i+1,"LNAME"] <- senate[count+25,"LNAME"]
      elect_nominees_final[i+1,"CITY"] <- senate[count+25,"CITY"]
      elect_nominees_final[i+1,"ST_NAME"] <- senate[count+25,"ST_NAME"]
      elect_nominees_final[i+1,"SENATE"] <- senate[count+25,"SENATE"]
      elect_nominees_final[i+1,"SENATE_WIN"] <- ""
      
      #Increase Counters
      i=i+2
      count=count+1
    }
    
    elect_nominees_final
    
  })
  
  
  ######################################### Send file to download screen
  #Nomination File to fill in
  output$download_elect_nomineefillin <- downloadHandler(
    filename = function() {"Elections_NomineeFillIn.csv"},
    content = function(file) {
      write.csv(elect_reg(), file, row.names = FALSE)
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
}