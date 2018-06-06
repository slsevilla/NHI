reg_original <- read.csv("C:\\Users\\slsevilla\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Director of Registrar\\Expected_StudentDemo.csv", header=TRUE)

sud.data <- reg_original[c("FNAME", "MNAME", "LNAME", "CELL", "P1.CELL", "P2.CELL", "arrival_airport", "arrival_time", "arrival_carrier")]

colnames(reg_original)

n <- nrow(reg_original)
n

temptable <- reg_original
temptable

temptable2 <- data.frame(x=1:n)
temptable2$MNAME<- temptable$MED.FORM
temptable2 <- subset(temptable2,select=-c(MNAME))
temptable2$x <- lapply (temptable2$x, gsub, pattern = "2", replacement = "A", fixed=TRUE)
temptable2

templist <- temptable2$x
templist

ftc <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\FormingTheCommunityTemplate.csv", header=TRUE)
staff <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\Staff_Demo.csv", header=TRUE)
std <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\StudentDemo_Expected.csv", header=TRUE)

staff_list <- staff[,"HS"]
i=1

staff$HS <- as.character(staff$HS)

for (a in staff_list){
  if(a==""){
    i=i+1
    next
  } else{
    print (a)
    print (i)
    temp <- as.character(staff[i,"HS"])
    print (temp)
    staff[i,"HS"] <- sub("^", "from ", temp )
    print (staff[i,"HS"])
    i=i+1
  }
}

