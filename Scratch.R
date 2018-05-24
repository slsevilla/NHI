reg_original <- read.csv("C:\\Users\\sevillas2\\Desktop\\LDZ\\Reg_original.csv")

sud.data <- reg_original[c("FNAME", "MNAME", "LNAME", "CELL", "P1_Cell", "P2_Cell",
                           "Arrival_Airport", "Trans_arrival_time", "Trans_arrival_carrier_name")]
colnames(reg_original)

sud.data
