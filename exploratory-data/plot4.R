# Flat read
file <- "household_power_consumption.txt"
data <- read.table(file, header=TRUE, sep=";", na.strings=c("","?"), stringsAsFactors=FALSE)

# Convert date
data$Date <- as.Date(strptime(data$Date, format="%d/%m/%Y"))

# Subset to specif dates
data <- data[data$Date==as.Date("2007-02-01") | data$Date==as.Date("2007-02-02"),]

# Merge date and time
data$Date <- strptime(paste(data$Date, data$Time), format="%Y-%m-%d %T")
data <- data[-2]

# Open png device and plot chart
png(filename="plot4.png", width=480, height=480, units="px")
par(mfcol=c(2,2))
plot2()
plot3()
plot(data$Date, data$Voltage,
     type="l",
     xlab="datetime",
     ylab="Voltage")
plot(data$Date, data$Global_reactive_power,
     type="l",
     xlab="datetime",
     ylab="Global_reactive_power")
dev.off()