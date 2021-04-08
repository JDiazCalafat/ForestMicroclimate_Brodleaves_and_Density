setwd("J:/TEMPERATURE DATA/SANDSJÖ/Temperature_Vindeln_202010XX/")

file.list <- list.files(path = ".", pattern = "*.xlsx", all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

library("readxl")

# Load data without skipping rows and setting column names:
df.list <- lapply(file.list, read_excel)

data.temp <- do.call(rbind, df.list)

library(stringr)
temp  <- cbind(str_split_fixed(data.temp$Date_time, " ", length(colnames(data.temp)) +1), data.temp[,-3])
colnames(temp) <- c("Date", "Time", "Day", "Month", "Year", "Plot", "Location", "Temp")
library(lubridate)
temp$Day <- day(temp$Date)
temp$Month <- month(temp$Date)
temp$Year <- year(temp$Date)

summary(lm(temp$Temp~temp$Location))

temp.air <- temp[which(temp$Location == "a"),]
temp.air <- temp.air[-which(temp.air$Plot == 37),] #Incomplete data
temp.air <- temp.air[-which(temp.air$Plot == 20),] #Incomplete data
temp.air <- temp.air[-which(temp.air$Plot == 29),] #Complete data but different temp mean

temp.soil <- temp[which(temp$Location == "s"),]
temp.soil <- temp.soil[-which(temp.soil$Plot == 36),] #Incomplete data
temp.soil <- temp.soil[-which(temp.soil$Plot == 31),] #Incomplete data


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



plot(data.temp.a$Min_Temp~data.temp.a$Max_Temp)
plot(data.temp.a$Min_Temp~data.temp.a$Mean_Temp)



setwd("J:/TEMPERATURE DATA/SANDSJ?/")
trees=read.csv("Vindeln_trees.csv", header = TRUE, sep = ";", dec=",")

names(trees)[names(trees) == "?..Plot"] <- "Plot"
tree.soil <- merge(trees, data.temp.s, by = "Plot")
tree.soil <- tree.soil[,c(5,15,18,19,20)]
tree.air <- merge(trees, data.temp.a, by = "Plot")
tree.air <- tree.air[,c(5,15,18,19,20)]
names(as.factor(tree.air))

lm <- lm(tree.air$Mean_Temp~tree.air$Plot_openness)
summary(lm)

library(psych)

pairs.panels(tree.air, 
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

pairs.panels(tree.soil, 
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

# IGNORE THIS:
set.seed(42)
y <- rnorm(30, mean=4,sd=1)

sd.y <- as.numeric()
for(i in 1:10){
  sd.y[i] <- sd(y[(1+(i-1)*3):(3+(i-1)*3)])
}
sd.y
