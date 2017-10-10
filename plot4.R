Sys.setlocale("LC_ALL","US")

# download dataset zip file
url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zipfile = "household_power_consumption.zip"
datafile = "household_power_consumption.txt"
if(!file.exists(zipfile)) download.file(url, zipfile, mode = "wb", cacheOK = FALSE)

# SLOW/FAT APPROACH: read entire file from zip and subset data.frame = 650MB/18s
# colClasses <- c(rep("character", 2), rep("numeric", 7))
# df <- read.table(unz(zipfile, datafile), header = TRUE, sep = ";", quote = "", colClasses = colClasses, na.strings = "?")
# df <- df[df$Date == "1/2/2007" | df$Date == "2/2/2007", ]
# colnames(df)[1] <- "Date_time"
# df$Date_time <- strptime(paste0(df$Date_time, df$Time), "%d/%m/%Y%H:%M:%S", tz = "UTC")
# df$Time <- NULL

# FAST/SLIM APPROACH: count lines, pre-allocate data.frame and break loops
if(!file.exists(datafile)) unzip(zipfile, setTimes = TRUE)
con <- file(datafile, open = "rt")
counter <- 0; until <- 0; lines <- 0
while (length(line <- readLines(con, n = 1)) > 0) {
    lines <- lines + 1
    if (grepl("^[12]/2/2007",line)) {
        if (counter == 0) until <- lines - 1
        counter <- counter + 1
    } else if (counter > 1) break
}
close(con)
con <- file(datafile, open = "rt")
for (i in 1:until) readLines(con, n = 1)
df <- data.frame(Date = character(counter), Time = character(counter),
                 Global_active_power = numeric(counter), Global_reactive_power = numeric(counter),
                 Voltage = numeric(counter), Global_intensity = numeric(counter),
                 Sub_metering_1 = numeric(counter), Sub_metering_2 = numeric(counter),
                 Sub_metering_3 = numeric(counter), stringsAsFactors = FALSE)
for (i in 1:counter) df[i,] <- strsplit(readLines(con, n = 1), split = ";")[[1]]
close(con)
colnames(df)[1] <- "Date_time"
df$Date_time <- strptime(paste0(df$Date_time, df$Time), "%d/%m/%Y%H:%M:%S", tz = "UTC")
df$Time <- NULL
for (i in 2:8) df[,i] <- as.numeric(df[,i])

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
with(
    df,
    plot(
        Date_time,
        Global_active_power,
        type = "l",
        xlab = "",
        ylab = "Global Active Power"
    )
)
with(
    df,
    plot(
        Date_time,
        Voltage,
        type = "l",
        xlab = "datetime",
        ylab = "Voltage"
    )
)
with(
    df,
    plot(
        Date_time,
        df$Sub_metering_1,
        col = "black",
        type = "l",
        xlab = "",
        ylab = "Energy sub metering"
    )
)
with(
    df,
    points(
        Date_time,
        df$Sub_metering_2,
        col = "red",
        type = "l"
    )
)
with(
    df,
    points(
        Date_time,
        df$Sub_metering_3,
        col = "blue",
        type = "l"
    )
)
legend(
    "topright",
    lty = 1,
    bty = "n",
    xjust = 1,
    col = c("black", "red", "blue"),
    legend = names(df)[6:8],
    text.width = strwidth(names(df)[6:8]) * 1.4
)
with(
    df,
    plot(
        Date_time,
        Global_reactive_power,
        type = "l",
        xlab = "datetime"
    )
)

dev.copy(png, file = "plot4.png")
dev.off()

