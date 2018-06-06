library(rgl)
library(car)
library(shiny)
library("RColorBrewer")
library(leaflet)
library(knitr)

#####################################################################################
###                                      NOTES                                   ###
###################################################################################

#####################################################################################
###                                      Code                                    ###
#####################################################################################

function(input,output, session){
######################################### HQ Database Conversion ####################################
#####################################################################################
  
  #User Input File
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
  
  #User Dropdowns
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
      selectInput("hq_P1", "Parent Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2<- renderUI({
      selectInput("hq_P2", "Parent Name", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P1.CELL<- renderUI({
      selectInput("hq_P1.CELL", "Parent Cell#", cb_options)
    })
  })
  observe({
    req(input$hq.file1)
    dsnames <- names(hq_input())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$hq_P2.CELL<- renderUI({
      selectInput("hq_P2.CELL", "Parent Cell#", cb_options)
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
  
  #Generate Student Database
  ###Create database from user generated files, and matched column headers
  hq_database <- reactive({
    if(is.null(hq_input)) return(NULL)
    
    #Read in input table, and determine total number of rows
    table_in <- hq_input()
    
    #Create an output table that matches the number of rows 
    n<- nrow(table_in)
    table_out <- data.frame(x=1:n)
    
    #Append each necessary column, based on user input to the new table
    table_out$FNAME <- table_in[,input$hq_FNAME]
    table_out$MNAME <- table_in[,input$hq_MNAME]
    table_out$LNAME <- table_in[,input$hq_LNAME]
    table_out$MF <- table_in[,input$hq_MF]
    table_out$HS <- table_in[,input$hq_HS]
    table_out$CITY <- table_in[,input$hq_CITY]
    table_out$ST <- table_in[,input$hq_ST]
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
    table_out$DOT <- table_in[,input$hq_DOT]
    
    #Return the full, new table after removing starting column
    table_out <- subset(table_out, select=-c(x))
    table_out
  })
  
  ###Send file to download screen
  output$download_hq <- downloadHandler(
    filename = function() {"StudentDemo_Expected.csv"},
    content = function(file) {
      write.csv(hq_database(), file, row.names = FALSE)
    }
  )

######################################## Day 0 Admin Tasks #################################### 
###################################################################################  
  
  #File input
  ##Generate database for input file1
  day0_expect <- reactive({
    admin1 <- input$day0.file1
    if (is.null(admin1)) return(NULL)
    read.csv(fill=TRUE,file=input$day0.file1$datapath, header=TRUE, 
             colClasses = "factor")
  })  
  #Create File Summary information for file1
  output$day0.expect <- renderTable({
    if(is.null(day0_expect())) return ()
    input$day0.file1
  })
  #Display confirmation of upload for user for file 1
  output$confirm.day0 <- renderUI({
    if(is.null(day0_expect())) return()
    tableOutput("day0.expect")
  })
  ##Generate database for input file2
  staff_file <- reactive({
    admin2 <- input$day0.file2
    if (is.null(admin2)) return(NULL)
    read.csv(fill=TRUE,file=input$day0.file2$datapath, header=TRUE, 
             colClasses = "factor")
  })  
  #Create File Summary information for file2
  output$staff.file <- renderTable({
    if(is.null(staff_file())) return ()
    input$day0.file2
  })
  #Display confirmation of upload for user for file 1
  output$confirm.staff_file <- renderUI({
    if(is.null(staff_file())) return()
    tableOutput("staff.file")
  })
  
  #Data Confirmations 
  ##Displays a confirmation message for the attendance/travel verification
  observeEvent(input$d0_travelverify,{
    output$confirm.d0_travelverify <- renderText({
      "Upload Complete - Download D0 Attend/Travel Verification"})  
  })
  ##Displays a confirmation message for the student self verification
  observeEvent(input$studselfverify,{
    output$confirm.studselfverify <- renderText({
      "Upload Complete - Download Student Self-Update Forms"})  
  })
  ##Displays a confirmation message for the door signs
  observeEvent(input$studlabels,{
    output$confirm.studlabels <- renderText({
      "Upload Complete - Download Student Labels"})  
  })
  ##Displays a confirmation message for the registration  signs
  observeEvent(input$studbalance,{
    output$confirm.studbalance <- renderText({
      "Upload Complete - Download Students with Balance List"})  
  })
  ##Displays a confirmation message for the registration  signs
  observeEvent(input$studforms,{
    output$confirm.studforms <- renderText({
      "Upload Complete - Download Missing Forms List"})  
  })
  ##Displays a confirmation message for the door signs
  observeEvent(input$door_sign,{
    output$confirm.door_sign <- renderText({
      "Upload Complete - Download Door signs"})  
  })
  ##Displays a confirmation message for the rooming lists on Day 0- female
  observeEvent(input$d0_room_F,{
    output$confirm.d0_room_F <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Female Students"})  
  })
  ##Displays a confirmation message for the rooming lists on Day 0- Male
  observeEvent(input$d0_room_M,{
    output$confirm.d0_room_M <- renderText({
      "Upload Complete - Download Day 0 Rooming Lists for Male Students"})  
  })
  ##Displays a confirmation message for the staff labels
  observeEvent(input$stafflabels,{
    output$confirm.stafflabels <- renderText({
      "Upload Complete - Download Staff Label File"})  
  })
  
  #Create sub-tables
  d0_travelverify <- reactive({
    if(is.null(input$day0.file1)) return()
    d0_travelverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "P1.CELL",
                                       "P2.CELL", "ARRIVAL_AIR", "ARRIVAL_TIME",
                                       "ARRIVAL_CARRIER", "ARRIVAL_FLIGHT",
                                       "DEPART_AIR", "DEPART_TIME")]
  })
  studselfverify <- reactive({
    if(is.null(input$day0.file1)) return()
    studselfverify <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CELL", "CITY",
                                      "ST", "P1", "P2", "HS", "DEPART_AIR",
                                      "DEPART_TIME")]
  })
  studlabels <- reactive({
    if(is.null(input$day0.file1)) return()
    studlabels <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "HS",
                                  "DOT")]
  })
  studbalance <- reactive({
    if(is.null(input$day0.file1)) return()
    studbalance <- subset(day0_expect(), !BALANCE=="")
    studbalance <- studbalance[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                                 "HS", "BALANCE")]
  })
  studforms <- reactive({
    if(is.null(input$day0.file1)) return()
    studforms <- subset(day0_expect(), FIN.FORM=='No' | MED.FORM=='No' )
    studforms <- studforms[c("FNAME", "MNAME", "LNAME", "CELL", "CITY", "ST",
                             "HS", "FIN.FORM", "MED.FORM")]
  })
  studdoor <- reactive({
    if(is.null(input$day0.file1)) return()
    studdoor <- day0_expect()[c("FNAME", "MNAME", "LNAME", "CITY", "ST", "MF",
                                "DORM", "ROOM", "DOT")]
    studdoor <- studdoor[order(studdoor$MF, studdoor$ROOM),]
  })
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
  staff_labels <- reactive({
    if(is.null(input$day0.file2)) return()
    staff_labels <- staff_file()[c("FNAME", "MNAME", "LNAME", "HS", "UNIV", "CITY",
                                  "ST", "ROLE")]
  })           
  
  #File Downloads
  output$download_d0_travelverify <- downloadHandler(
    filename = function() {"D0_AttendTravel_Verify.csv"},
    content = function(file) {
      write.csv(d0_travelverify(), file, row.names = FALSE)
    }
  )
  output$download_studselfverify <- downloadHandler(
    filename = function() {"StudentDemo_SelfUpdate.csv"},
    content = function(file) {
      write.csv(studselfverify(), file, row.names = FALSE)
    }
  )
  output$download_studlabels <- downloadHandler(
    filename = function() {"StudentLabels.csv"},
    content = function(file) {
      write.csv(studlabels(), file, row.names = FALSE)
    }
  )
  output$download_studbalance <- downloadHandler(
    filename = function() {"StudentswithBalance.csv"},
    content = function(file) {
      write.csv(studbalance(), file, row.names = FALSE)
    }
  )
  output$download_studforms <- downloadHandler(
    filename = function() {"StudentsMissingForms.csv"},
    content = function(file) {
      write.csv(studforms(), file, row.names = FALSE)
    }
  )
  output$download_studdoor <- downloadHandler(
    filename = function() {"DoorSigns.csv"},
    content = function(file) {
    }
  )
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
  output$download_stafflabels <- downloadHandler(
    filename = function() {"StaffLabels.csv"},
    content = function(file) {
      write.csv(staff_labels(), file, row.names = FALSE)
    }
  )
  

########################################### Protocol ##################################
#####################################################################################
  
  #Input Files
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
  
  #Database Creation
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
        print (a)
        if(a==""){
          i=i+1
          next
        } else{
          temp <- staff_update[i,"UNIV"]
          print (temp)
          staff_update[i,"UNIV"] <- sub("^", "from ", temp )
          i=i+1
        }
      }
    
      #Update Major
      i = 1
      staff_list <- staff_ori[,"MAJ"]
      staff_update[,"MAJ"] <- as.character(staff_update[,"MAJ"])
      
      for (a in staff_list){
        print (a)
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
        print (a)
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
      
    #Staff Final Database
    staff_final <- staff_update[,c("CITY","ST","GD","LDZ","CWS","ROLE","NAME",
                                   "HS","UNIV", "MAJ", "COLSTAT", "HSSTAT")]
    
    
    
  })
  
  #Output Files
  output$download_ftc_protocol <- downloadHandler(
    filename = function() {"FTC_Protocol.csv"},
    content = function(file) {
      write.csv(ftc_protocol(), file, row.names = FALSE)
    }
  )
  output$download_ftc_staff <- downloadHandler(
    filename = function() {"FTC_Staff.csv"},
    content = function(file) {
      write.csv(ftc_staff(), file, row.names = FALSE)
    }
  )
  
  
######################################### Staff Database Conversion ####################################
#####################################################################################
  
  #User Input File
  ##Import file
  staff_db <- reactive({
    staff_db <- input$staff.db1
    if(is.null(staff_db)) return(NULL)
    read.csv(fill=TRUE,file=input$staff.db1$datapath,header=TRUE,colClasses = "factor"
    )
  })
  ##Create File Summary information
  output$staff.db.input <- renderTable({
    if(is.null(staff_db())) return ()
    input$staff.db1
  })
  ##Display confirmation of upload for user
  output$confirm.staffdemo <- renderUI({
    if(is.null(staff_db())) return()
    tableOutput("staff.db.input")
  })
  
  #User Dropdowns
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
  
  #Generate Student Database
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
  
  ###Send file to download screen
  output$download_staffdemo <- downloadHandler(
    filename = function() {"Staff_Demo.csv"},
    content = function(file) {
      write.csv(staff_database(), file, row.names = FALSE)
    }
  )

}