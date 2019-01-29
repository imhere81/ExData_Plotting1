plot1 <- function (){
  
  # Reads the Read the Data and Prepares the Data and Stores the subset in a FILE week1.csv
  prepareTidyData()
 
  data <- read.csv("week1.csv" , sep = "," , header = TRUE)
  data$Date <- as.POSIXct(data$Date , format("%d/%m/%Y"))
  cols = c( 3, 4, 5,6,7,8,9);    
  data[,cols] = apply(data[,cols], 2, function(x) as.numeric(as.character(x)));
  data <- removeNARows(data, col (data))
  
  png(filename="ExData_Plotting1/plot11.png" ,width=480,height=480,units="px",res=75)
 
  hist(data$Global_active_power , col="red" , main="Global Active Power" ,xlab = "Global Active Power(kilowatts)")
  
  dev.off()  


}

removeNARows <- function(data, cols) {
  subdata <- complete.cases(data[, cols])
  return(data[subdata, ])
}

prepareTidyData <- function (){
  
  # colClasses = c("Date" ,"character" , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric") 
source <- read.csv("household_power_consumption.txt" , sep = ";" , header = TRUE)
print (head(source))

source$Date <- as.Date(source$Date , format("%d/%m/%Y"))

startDate = as.Date("01/02/2007" , format("%d/%m/%Y"));
endDate = as.Date("02/02/2007" , format("%d/%m/%Y"));

source$Global_active_power <- gsub("?",NA,source$Global_active_power, fixed = TRUE)
source$Global_reactive_power <- gsub("?",NA,source$Global_reactive_power, fixed = TRUE)
source$Voltage <- gsub("?",NA,source$Voltage, fixed = TRUE)
source$Global_intensity <- gsub("?",NA,source$Global_intensity, fixed = TRUE)
source$Sub_metering_1 <- gsub("?",NA,source$Sub_metering_1, fixed = TRUE)
source$Sub_metering_2 <- gsub("?",NA,source$Sub_metering_2, fixed = TRUE)
source$Sub_metering_3 <- gsub("?",NA,source$Sub_metering_3, fixed = TRUE)

data <- subset(source, Date==startDate | Date==endDate, na.rm=TRUE)
cols = c( 3, 4, 5,6,7,8,9);    
data[,cols] = apply(data[,cols], 2, function(x) as.numeric(as.character(x)));


write.csv(data, "../week1.csv" , row.names = FALSE)


}