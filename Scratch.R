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

inv <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Director of Merchandise\\Inventory_Day0.csv", header=TRUE)
fin <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Director of Merchandise\\MerchandiseLedger.csv", header=TRUE)

gc <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\GC_Day2_Points.csv", header=TRUE)
nom <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\Nomination_final.csv", header=TRUE)
points_leg <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\StudentDemo_Positions.csv", header=TRUE)
reg <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\Live_StudentRegistration.csv", header=TRUE)
voting <- read.csv("C:\\Users\\sevillas2\\Google Drive\\My Documents\\Programs & Orgs\\National Hispanic Institute\\Template National LDZ\\Director Guides\\Downloaded Files\\Points_Vote_House.csv", header=TRUE)
voting$NAME <- as.character(voting$NAME)

points_leg_f <- subset(points_leg, POSITION==c("HOUSE", "SENATE"))


+ 
  points_senate_w[i,"MOST_PROM_FEMALE"] + points_jud_w[i,"MOST_PROM_FEMALE"] + 
  points_exec_w[i,"MOST_PROM_FEMALE"]