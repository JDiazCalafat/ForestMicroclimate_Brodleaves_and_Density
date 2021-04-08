# From tree data to basal area
setwd("J:/TEMPERATURE DATA/SILJAN/202103_SILJAN_TEMPERATURE")

file.list <- list.files(path = ".", pattern = "*.xlsx", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

library(readxl)

# Load data without skipping rows and setting column names:
df.list <- lapply(file.list, read_excel)

data.temp <- do.call(rbind, df.list)

oneyear <- seq(ymd_hm('2019-10-28 18:15'),ymd_hm('2020-10-28 18:00'), by = '15 mins')
data.temp <- data.temp[which(data.temp$Date_time %in% oneyear),]

temp.air <- data.temp[which(data.temp$location == "a"),]
temp.soil <- data.temp[which(data.temp$location == "s"),]

# Data completeness
dates.air.plot <- dcast(temp.air, Date_time ~ Plot)
# soil
dates.soil.plot <- dcast(temp.soil, Date_time ~ Plot)
library(naniar)
# air
vis_miss(dates.air.plot, warn_large_data = FALSE)
# soil
vis_miss(dates.soil.plot, warn_large_data = FALSE)

library(stringr)
temp  <- cbind(str_split_fixed(data.temp$Date_time, " ", length(colnames(data.temp)) +1), data.temp[,-3])
colnames(temp) <- c("Date", "Time", "Day", "Month", "Year", "Plot", "Location", "Temp")
library(lubridate)
temp$Day <- day(temp$Date)
temp$Month <- month(temp$Date)
temp$Year <- year(temp$Date)

temp.air <- temp[which(temp$Location == "a"),]
temp.soil <- temp[which(temp$Location == "s"),]


# MEAN
av.temp.a <- aggregate(temp.air$Temp~temp.air$Plot, FUN = mean)
colnames(av.temp.a) <- c("Plot", "Mean_Temp")
av.temp.s <- aggregate(temp.soil$Temp, by = list(temp.soil$Plot), FUN = mean)
colnames(av.temp.s) <- c("Plot", "Mean_Temp")

# MAX
max.temp.a <- aggregate(temp.air$Temp~temp.air$Plot, FUN = max)
colnames(max.temp.a) <- c("Plot", "Max_Temp")
max.temp.s <- aggregate(temp.soil$Temp, by = list(temp.soil$Plot), FUN = max)
colnames(max.temp.s) <- c("Plot", "Max_Temp")

# MIN
min.temp.a <- aggregate(temp.air$Temp~temp.air$Plot, FUN = min)
colnames(min.temp.a) <- c("Plot", "Min_Temp")
min.temp.s <- aggregate(temp.soil$Temp, by = list(temp.soil$Plot), FUN = min)
colnames(min.temp.s) <- c("Plot", "Min_Temp")


data.temp.a <- cbind(av.temp.a, min.temp.a, max.temp.a)
data.temp.a <- data.temp.a[,c(1,2,4,6)]
data.temp.s <- cbind(av.temp.s, min.temp.s, max.temp.s)
data.temp.s <- data.temp.s[,c(1,2,4,6)]

# Tree data
Siljan.trees <- read.csv("C:/Users/jndi0002/Desktop/Siljan_BA.csv", header = TRUE, sep = ";")

Siljan.trees <- Siljan.trees %>%
  rename("Plot" = "ï..Plot")

Siljan.trees <- Siljan.trees[,which(colnames(Siljan.trees) %in% c("Plot", "BA_10", "BA_9", "BA_8", "BA_7", "BA_6", "BA_5", "BA_4"))] 

siljan.treetemp.a <- merge(Siljan.trees, data.temp.a, by = "Plot")
siljan.treetemp.a <- siljan.treetemp.a[-which(siljan.treetemp.a$Plot == 12),] 
siljan.treetemp.a <- siljan.treetemp.a[-which(siljan.treetemp.a$Plot == 14),] 
siljan.treetemp.a <- siljan.treetemp.a[-which(siljan.treetemp.a$Plot == 21),] 
siljan.treetemp.a <- siljan.treetemp.a[-which(siljan.treetemp.a$Plot == 30),] 

siljan.treetemp.s <- merge(Siljan.trees, data.temp.s, by = "Plot")
siljan.treetemp.s <- siljan.treetemp.s[-which(siljan.treetemp.s$Plot == 2),] 
siljan.treetemp.s <- siljan.treetemp.s[-which(siljan.treetemp.s$Plot == 13),] 
siljan.treetemp.s <- siljan.treetemp.s[-which(siljan.treetemp.s$Plot == 33),] 

library(psych)

pairs.panels(siljan.treetemp.a, 
             pch = 19,
             cex = 0.7,
             lwd = 1,
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             rug = FALSE,
             breaks = 8,
             stars = TRUE,
)

pairs.panels(siljan.treetemp.s, 
             pch = 19,
             cex = 0.7,
             lwd = 1,
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             rug = FALSE,
             breaks = 8,
             stars = TRUE,
)
