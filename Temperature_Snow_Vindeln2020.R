setwd("C:/Users/jndi0002/Desktop/PhD/Temperature_Vindeln_202010XX/")

file.list <- list.files(path = "C:/Users/jndi0002/Desktop/PhD/Temperature_Vindeln_202010XX/", pattern = "*.xlsx", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


# Load data
library("readxl")
df.list <- lapply(file.list, read_excel)
data.temp <- do.call(rbind, df.list)

# Separate date and time
library(stringr)
temp  <- cbind(str_split_fixed(data.temp$Date_time, " ", length(colnames(data.temp)) +1), data.temp[,-3])

# Rename columns
colnames(temp) <- c("Date", "Time", "Day", "Month", "Year", "Plot", "Location", "Temp")

# Calculate day, month and year
library(lubridate)
temp$Day <- day(temp$Date)
temp$Month <- month(temp$Date)
temp$Year <- year(temp$Date)

# Select only data from soil loggers and remove problematic files
temp.soil <- temp[which(temp$Location == "s"),]
temp.soil <- temp.soil[-which(temp.soil$Plot == 36),] #Incomplete data
temp.soil <- temp.soil[-which(temp.soil$Plot == 31),] #Incomplete data

# calculate daily MEAN and SD
av.temp.s <- aggregate(temp.soil$Temp~temp.soil$Date+temp.soil$Plot, FUN = mean)
sd.temp.s <- aggregate(temp.soil$Temp~temp.soil$Date+temp.soil$Plot, FUN = sd)
max.temp.s <- aggregate(temp.soil$Temp~temp.soil$Date+temp.soil$Plot, FUN = max)
min.temp.s <- aggregate(temp.soil$Temp~temp.soil$Date+temp.soil$Plot, FUN = min)

# merge everything in the same object, select variables of interest and rename columns
data.temp.s <- cbind(av.temp.s, sd.temp.s, max.temp.s, min.temp.s)
data.temp.s <- data.temp.s[,c(1,2,3,6,9,12)]
colnames(data.temp.s) <- c("Date", "Plot", "Mean_temp", "SD_temp", "Max_temp", "Min_temp")
data.temp.s$range.temp <- data.temp.s$Max_temp - data.temp.s$Min_temp


# Loop where I compare daily SD values to the SD value of the previous day, 
# and label the cases in a new variable as "SNOW" if they were equal.

# SD values have quite some decimals, so perhaps it would be better to round them up?
data.temp.s$SD_temp <- round(data.temp.s$SD_temp, 3)


list <- c()
for (i in 1:(nrow(data.temp.s))) {
  list [i] <- ifelse(data.temp.s[i, "SD_temp"] == data.temp.s[i + 1, "SD_temp"], "SNOW", "NO")
}

data.temp.s$Snow <- list
data.temp.s[which(data.temp.s$Snow == "SNOW"),]

# Comparing ranges

list <- c()
for (i in 1:(nrow(data.temp.s))) {
  list [i] <- ifelse(data.temp.s[i, "range.temp"] < 0.3, "SNOW", "NO")
}

data.temp.s$Snow.range <- list
data.temp.s[which(data.temp.s$Snow.range == "SNOW"),]

plot5 <- data.temp.s[which(data.temp.s$Plot == 5),]
plot(plot5$Mean_temp~plot5$Date)
data.temp.s[which(data.temp.s$Plot == 5),]




data.temp.s$Date <- as.Date(data.temp.s$Date)
snow <- data.frame()
for(j in levels(as.factor(data.temp.s$Plot))){
  for (i in unique(data.temp.s$Date)) {
    snow[i,j] <- ifelse(data.temp.s[which(data.temp.s$Plot == j),][data.temp.s[which(data.temp.s$Date == i),],"SD_temp"] == data.temp.s[which(data.temp.s$Plot == j),][data.temp.s[which(data.temp.s$Date == i+1),],"SD_temp"], "SNOW", "NO")
  }
}

snow[i,j] <- ifelse(data.temp.s[which(data.temp.s$Plot == j),][data.temp.s[which(data.temp.s$Date == i),],"SD_temp"] == data.temp.s[which(data.temp.s$Plot == j),][data.temp.s[which(data.temp.s$Date == i+1),],"SD_temp"], "SNOW", "NO")

data.temp.s$Snow <- list