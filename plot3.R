plot3 <- function (){
  
  library(ggplot2)
  library(dplyr)
  
  
  # Reads the Read the Data and Prepares the Data and Stores the subset in a FILE week1_3.csv
  prepareTidyData()
  
  data <- read.csv("week1_3.csv" , sep = "," , header = TRUE)
  
  data$Date <- as.POSIXct(data$Date , format("%d/%m/%Y"))
  
  cols = c( 3, 4, 5,6,7,8,9);    
  data[,cols] = apply(data[,cols], 2, function(x) as.numeric(as.character(x)));
  
  #data <- removeNARows(data, col (data))
  
  data <- mutate(data, dt = as.POSIXct(paste(format(Date, format="%d/%m/%Y"),Time), format="%d/%m/%Y %H:%M:%S")) 
  
  
  png(filename="ExData_Plotting1/plot33.png" ,width=480,height=480,units="px",res=75)
  
  # data$Date <- as.factor(weekdays(data$Date , abbreviate=TRUE))
  
  
  plot(data$dt, data$Sub_metering_1, type="n" ,ylab="Energy Sub Metering", xlab="") 
 
  
 
   
  lines(data$dt, data$Sub_metering_1 , type="l" , col="black") 
  lines(data$dt, data$Sub_metering_2 , type="l" , col="red") 
  lines(data$dt, data$Sub_metering_3 , type="l" , col="blue")
  
  legend(x = "topleft", legend=c("Sub_metering_1", "Sub_metering_2" ,"Sub_metering_3"),
          col=c("black","red", "blue"), lty=1, cex=0.8)
 
  
  
  dev.off()
  
}


removeNARows <- function(data, cols) {
  subdata <- complete.cases(data[, cols])
  return(data[subdata, ])
}

readAndParse <- function (){
  
  # colClasses = c("Date" ,"character" , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric") 
  source <- read.csv("household_power_consumption.txt" , sep = ";" , header = TRUE , na.strings = "?")
  print (head(source))
  
  source$Date <- as.Date(source$Date , format("%d/%m/%Y"))
  
  startDate = as.Date("01/02/2007" , format("%d/%m/%Y"));
  endDate = as.Date("02/02/2007" , format("%d/%m/%Y"));
  
  dates <- c(as.Date("01/02/2007" , format("%d/%m/%Y")) ,
             as.Date("02/02/2007" , format("%d/%m/%Y")))
  
  #source$Global_active_power <- gsub("?",NA,source$Global_active_power, fixed = TRUE)
  #source$Global_reactive_power <- gsub("?",NA,source$Global_reactive_power, fixed = TRUE)
  #source$Voltage <- gsub("?",NA,source$Voltage, fixed = TRUE)
  #source$Global_intensity <- gsub("?",NA,source$Global_intensity, fixed = TRUE)
  #source$Sub_metering_1 <- gsub("?",NA,source$Sub_metering_1, fixed = TRUE)
  #source$Sub_metering_2 <- gsub("?",NA,source$Sub_metering_2, fixed = TRUE)
  #source$Sub_metering_3 <- gsub("?",NA,source$Sub_metering_3, fixed = TRUE)
  
  data <- subset(source, Date%in% dates, na.rm=TRUE)
  cols = c( 3, 4, 5,6,7,8,9);    
  data[,cols] = apply(data[,cols], 2, function(x) as.numeric(as.character(x)));
  
  print (str(data))
  data <- removeNARows(data, col (data))
  write.csv(data, "week1_3.csv" , row.names = FALSE)
  
  
}