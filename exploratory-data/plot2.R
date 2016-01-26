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
png(filename="plot2.png", width=480, height=480, units="px")
plot(data$Date, data$Global_active_power,
     type="l",
     xlab="",
     ylab="Global Active Power (kilowatts)") 
dev.off()