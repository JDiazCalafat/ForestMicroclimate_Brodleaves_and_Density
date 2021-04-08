setwd("J:/TEMPERATURE DATA/SILJAN/202103_SILJAN_TEMPERATURE")

file.list <- list.files(path = ".", pattern = "*.xlsx", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

library(readxl)

# Load data without skipping rows and setting column names:
df.list <- lapply(file.list, read_excel)

data.temp <- do.call(rbind, df.list)

temp.air <- data.temp[which(data.temp$location == "a"),]
temp.soil <- data.temp[which(data.temp$location == "s"),]

#temp.air.3 <- temp.air[temp.air$Plot %in% c(1:3),]

library(lubridate)
min(temp.air$Date_time)
max(temp.air$Date_time)

timeseries.air <- seq(ymd_hm('2019-08-28 18:15'),ymd_hm('2021-03-23 15:30'), by = '15 mins')

dates.air <- data.frame(temp.air$Plot, temp.air$Date_time)

# Plot 18's recording times are off by 1 min 17 sec. Let's correct that going back in time 77 seconds
dates.air[which(dates.air$temp.air.Plot == 18),]$temp.air.Date_time <- dates.air[which(dates.air$temp.air.Plot == 18),]$temp.air.Date_time - 77


dates2.air <- data.frame(rep(99, length(timeseries.air)), timeseries.air)

library(dplyr)
dates2.air <- dates2.air %>%
  rename("temp.air.Date_time" = "timeseries.air",
         "temp.air.Plot" = "rep.99..length.timeseries.air..")

# Remove duplicates
dates.air <- dates.air[!duplicated(dates.air),]

all.air <- rbind(dates.air, dates2.air)

# Same for soil
min(temp.soil$Date_time)
max(temp.soil$Date_time)

timeseries.soil <- seq(ymd_hm('2019-08-28 18:15'),ymd_hm('2021-03-25 12:00'), by = '15 mins')

dates.soil <- data.frame(temp.soil$Plot, temp.soil$Date_time)
# Plot 30's recording times are off by 1 min 36 sec. Let's correct that going back in time 96 seconds
dates.soil[which(dates.soil$temp.soil.Plot == 30),]$temp.soil.Date_time <- dates.soil[which(dates.soil$temp.soil.Plot == 30),]$temp.soil.Date_time - 96
dates2.soil <- data.frame(rep(99, length(timeseries.soil)), timeseries.soil)

dates2.soil <- dates2.soil %>%
  rename("temp.soil.Date_time" = "timeseries.soil",
         "temp.soil.Plot" = "rep.99..length.timeseries.soil..")

# Remove duplicates
dates.soil <- dates.soil[!duplicated(dates.soil),]

all.soil <- rbind(dates.soil, dates2.soil)

# Make a column with each plot number
# air
library(reshape2)
dates.air.plot <- dcast(all.air, temp.air.Date_time ~ temp.air.Plot)

# soil
dates.soil.plot <- dcast(all.soil, temp.soil.Date_time ~ temp.soil.Plot)

library(naniar)
# air
vis_miss(dates.air.plot, warn_large_data = FALSE)
#gg_miss_upset(dates.air.plot, nsets = n_var_miss(dates.air.plot))
gg_miss_upset(dates.air.plot, nsets = 7)
gg_miss_var(dates.air.plot)
# soil
vis_miss(dates.soil.plot, warn_large_data = FALSE)
gg_miss_upset(dates.soil.plot, nsets = 7)
gg_miss_var(dates.soil.plot)

devtools::install_github("cfree14/freeR")
library(freeR)
complete(dates.soil.plot)

source("http://news.mrdwab.com/install_github.R")
install_github("mrdwab/SOfun")
almostComplete()











table <- data.frame(table(all.air$temp.air.Plot, all.air$temp.air.Date_time))

missing <- table[which(table$Freq == 0),]

write.csv2(missing, "missing_siljan_air.csv", row.names = FALSE)

table.soil <- data.frame(table(all.soil$temp.soil.Plot, all.soil$temp.soil.Date_time))
missing.soil <- table.soil[which(table.soil$Freq == 0),]
write.csv2(missing.soil, "missing_siljan_soil.csv", row.names = FALSE)


install.packages("plotly")
library(plotly)

ggplot( aes(x=date, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("bitcoin price ($)") +
  theme_ipsum()

ggplotly()

plot(missing$Var2, as.numeric(missing$Var1))

library(tidyr)
data <- data.temp[,-4]
wider <- data %>%
  pivot_wider(data, id_cols = NULL,
              names_from = Plot,
              values_from = Date_time)

completeness <- rbind(dates, dates2, all.y = TRUE)

gather <- gather(dates, key = "temp.air.Plot", value = "temp.air.Date_time")
