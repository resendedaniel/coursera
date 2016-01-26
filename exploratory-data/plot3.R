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
png(filename="plot3.png", width=480, height=480, units="px")
plot(data$Date, data$Sub_metering_1,
     type="n",
     xlab="",
     ylab="Energy sub metering")
points(data$Date, data$Sub_metering_1, type="l")
points(data$Date, data$Sub_metering_2, type="l", col="red")
points(data$Date, data$Sub_metering_3, type="l", col="blue")
legend("topright", pch="_", col=c("black", "red", "blue"), legend=names(data)[6:8])
dev.off()