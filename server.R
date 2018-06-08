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


######################################### Merchandise ####################################
#####################################################################################
  
  #User Input File
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

  #Create drop down choices for user to select the day to be created
  observe({
    dsnames <- c("Inventory_Day1", "Inventory_Day2", "Inventory_Day3", "Inventory_Day4", 
                 "Inventory_Day5", "Inventory_Day6", "Inventory_Day7", "Inventory_Day8")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$merch.day<- renderUI({
      selectInput("merch_day", "Which day to create for (tomorrow's #)", cb_options)
    })
  })
  
  #Perform calculations for the inventory record. This will take the starting count and subtract
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
  
  #Perform calculations for the financial ledger record. This will take the data from the inventory form
  #and calculate how much money was made throughout the day. It will also tally the total number for verification
  #with petty cash amount
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
  
  

  #Send file to download screen
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

  ######################################### Points ####################################
  #####################################################################################
  #################Forming the Community  #################
  ##Import expected student demo file
  points.ftc <- reactive({
    points.ftc <- input$points.file1
    if(is.null(points.ftc)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file1$datapath,header=TRUE
    )
  })
  
  ##Create dataframe with users name merged, for staff to print and track points
  points_ftc <- reactive({
    if(is.null(points.ftc)) return(NULL)
    
    #Read in the database
    points_ftc_w <- points.ftc()[c("FNAME", "MNAME", "LNAME", "DOT")]
    
    #Start counter, and end counter as the number of rows
    i=0
    n=nrow(points_ftc_w)
    
    #Run through each row of the table, creating a merged name field for identification
    for (i in 1:n){
      first <- points_ftc_w[i,"FNAME"]
      middle <- points_ftc_w[i,"MNAME"]
      last <- points_ftc_w[i,"LNAME"]
      
      #Merge the first and last name
      #If there is a middle name, add that as well
      names <- paste(last, first, sep=", ")
      if (!(is.null(middle))){
        names <- paste(names,middle,sep=" ")
      }
      
      #Trim any white space from the name
      names <- trimws(names, which = c("both", "left", "right"))
      
      #Add names and total points column to table
      points_ftc_w[i,"NAME"] <- names
      points_ftc_w[,"TOTAL_PTS"] <- ""
      
      #Output a table with only name, dot group, and total points columns
      points_ftc_f <- points_ftc_w[,c("NAME", "DOT","TOTAL_PTS")]
    }
    
    #Output database
    points_ftc_f[]
  })
  
  ##Send file to download screen
  output$download_points_ftc <- downloadHandler(
    filename = function() {"FormingTheCommunity_Points.csv"},
    content = function(file) {
      write.csv(points_ftc(), file, row.names = FALSE)
    }
  )

  #################General Convention  #################
  ##Import expected student demo file
  points.gc <- reactive({
    points.gc <- input$points.file2
    if(is.null(points.gc)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file2$datapath,header=TRUE
    )
  })
  
  #Create drop down choices for user to select the day to be created
  observe({
    dsnames <- c("GC_Day2", "GC_Day3")
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    output$gc.day<- renderUI({
      selectInput("gc_day", "Which day to create for?", cb_options)
    })
  })
  
  ##Create dataframe with users name merged, for staff to print and track points
  points_gc <- reactive({
    if(is.null(points.gc)) return(NULL)
    
    #Read in the database
    points_gc_w <- points.gc()[c("FNAME", "MNAME", "LNAME", "DOT")]
    
    #Start counter, and end counter as the number of rows
    i=0
    n=nrow(points_gc_w)
    
    #Run through each row of the table, creating a merged name field for identification
    for (i in 1:n){
      first <- points_gc_w[i,"FNAME"]
      middle <- points_gc_w[i,"MNAME"]
      last <- points_gc_w[i,"LNAME"]
      
      #Merge the first and last name
      #If there is a middle name, add that as well
      names <- paste(last, first, sep=", ")
      if (middle==""){
        names <- paste(names,middle,sep=" ")
      }
      
      #Trim any white space from the name
      names <- trimws(names, which = c("both", "left", "right"))
      
      #Add names and total points column to table
      points_gc_w[i,"NAME"] <- names
      points_gc_w[,"TOTAL_PTS"] <- ""
      
      #Output a table with only name, dot group, and total points columns
      points_gc_f <- points_gc_w[,c("NAME", "DOT","TOTAL_PTS")]
    }
    
    #Output database
    points_gc_f[]
  })
  
  ##Send file to download screen
  output$download_points_gc <- downloadHandler(
    filename = function() {
      paste(input$gc_day, ".csv", sep="_Points")
    },
    content = function(file) {
      write.csv(points_gc(), file, row.names = FALSE)
    }
  )
  
  #################Daily Points Summaries  #################
  #Day1
  ##Import files
  points.day1 <- reactive({
    points.day1 <- input$points.file10
    if(is.null(points.day1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file10$datapath,header=TRUE
    )
  })

  ##Create Day 1 table Summary
  points_day1 <- reactive({
    if(is.null(points.day1)) return(NULL)
    
    points_ftc_w <- points.day1()[c("NAME", "TOTAL_PTS")]
    
    i = 0
    n=nrow(points_ftc_w)
    
    for(i in 1:n){
      value = points_ftc_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_ftc_w[i,"TOTAL_PTS"] =0
      }
    }
    
    #Remove Dot info from the table
    points_ftc_f <- points_ftc_w[,c("NAME","TOTAL_PTS")]
    
  })
  
  #Day 2
  ##Import Files
  points.day2.1 <- reactive({
    points.day2.1 <- input$points.file11.1
    if(is.null(points.day2.1)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file11.1$datapath,header=TRUE
    )
  })
  points.day2.2<- reactive({
    points.day2.2 <- input$points.file11.2
    if(is.null(points.day2.2)) return(NULL)
    read.csv(fill=TRUE,file=input$points.file11.2$datapath,header=TRUE
    )
  })
  
  #Create Day 2 table Summary
  points_day2 <- reactive({
    if(is.null(points.day2.1)) return(NULL)
    if(is.null(points.day2.1)) return(NULL)
    
    #######General Convention Addition    #######
    ##Create new table
    points_gc_w <- points.day2.1()[c("NAME", "TOTAL_PTS")]
    
    #Set counters at 0, and to end on the total number of rows
    i = 0
    n=nrow(points_gc_w)
    
    for(i in 1:n){
      value = points_gc_w[i,"TOTAL_PTS"]
      
      if(is.na(value)){
        points_gc_w[i,"TOTAL_PTS"] =0
      }
    }
    
    #Remove Dot info from the table, assign row names
    points_gc_f <- points_gc_w[,c("NAME","TOTAL_PTS")]
    row.names(points_gc_f) <- points_gc_f$NAME
    
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
      points_nom_w[a,"GC_PTS"] <- points_gc_f[a,"TOTAL_PTS"]
      points_nom_w[a,"TOTAL"] <- points_nom_w[a,"NOM_PTS"] + points_nom_w[a,"GC_PTS"]
    }
    
    #Create final table
    points_day2 <- points_nom_w[c("NAME","GC_PTS","NOM_PTS","TOTAL")]
    points_day2
  })
  
  ##Send daily output files to download screen
  output$download_points_day1 <- downloadHandler(
    filename = function() {"Day1_Points.csv"},
    content = function(file) {
      write.csv(points_day1(), file, row.names = FALSE)
    }
  )
  
  ##Send daily output files to download screen
  output$download_points_day2 <- downloadHandler(
    filename = function() {"Day2_Points.csv"},
    content = function(file) {
      write.csv(points_day2(), file, row.names = FALSE)
    }
  )
}