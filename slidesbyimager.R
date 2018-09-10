# This code captures performance metrics of  Archimedes in Array over desired time frame.
# Code developed by Kay Lee. 

rm(list = ls())

source("//USSD-PRD-MPE01/R Work/common/dcdDBHalfStateRTStatsbyPopNum.R")
source("//USSD-PRD-MPE01/R Work/common/getBeadloadInfobyPopNum.R")
source("//USSD-PRD-MPE01/R Work/common/getWatsonByPopNum.R")
source("//USSD-PRD-MPE01/R Work/common/loadProducts.R")
source("//USSD-PRD-MPE01/r work/common/queryExecutor.R")
source("//USSD-PRD-MPE01/R Work/common/dcdDBMatrixGradeImagerByPopNum.R")

library(data.table)
library(ggplot2)

# Removes RD parts from the product list
ProProds = products[products$isRDPart == F,]

startDate = "2018-06-01"
endDate = Sys.time()

Imager = c("331")

##### Pulls RTM Data directly from the Database #####

# Sets the two Dababase connections to pull newer and older data
DB1 = odbcDriverConnect(
  connection = "Driver={SQL Server};Server=USSDPRDSQL02R;Database=BMPdbArc;Trusted_Connection=yes;"
)

DB2 = odbcDriverConnect(
  connection = "Driver={SQL Server};Server=USSDPRDSQL02R;Database=BMPdbProd;Trusted_Connection=yes;"
)

# Conent to Query for last Real Time Metrics
queryQualRT = "select populated_serial_no,imaging_time,sample_label,stage,color,product_no,imaging_computer,imaging_station,registration,PercentGrayLow,PercentGray,PercentGrayLowPass,PercentGrayPass,mean_off,mean_half,mean_on \n
from dbo.imaging_stats_halfstate with (nolock) \n
where imaging_time between /queryStartDate/ and /queryEndDate/ \n
and imaging_computer like ('%ImagerImagerImager%')"

queryQualRT = sub("/queryStartDate/", paste("'", startDate, "'", sep = ""), queryQualRT)
queryQualRT = sub("/queryEndDate/", paste("'", endDate, "'", sep = ""), queryQualRT)
queryQualRT = gsub("ImagerImagerImager", 
                    paste(
                      paste(Imager),
                      sep = "")
                    , queryQualRT)

# Conent to Query for additional Real Time Metrics for repeated sections
queryQualRTSup = "select populated_serial_no,imaging_time,sample_label,stage,color,product_no,imaging_computer,imaging_station,registration,PercentGrayLow,PercentGray,PercentGrayLowPass,PercentGrayPass,mean_off,mean_half,mean_on \n
from dbo.imaging_stats_halfstate_sup with (nolock) \n
where imaging_time between /queryStartDate/ and /queryEndDate/ \n
and imaging_computer like ('%ImagerImagerImager%')"

queryQualRTSup = sub("/queryStartDate/", paste("'", startDate, "'", sep = ""), queryQualRTSup)
queryQualRTSup = sub("/queryEndDate/", paste("'", endDate, "'", sep = ""), queryQualRTSup)
queryQualRTSup = gsub("ImagerImagerImager", 
                   paste(
                     paste(Imager),
                     sep = "")
                   , queryQualRTSup)

mydata1 <- sqlQuery(DB1, query=queryQualRT)
mydata2 <- sqlQuery(DB2, query=queryQualRT)
mydata1Sup <- sqlQuery(DB1, query=queryQualRTSup)
mydata2Sup <- sqlQuery(DB2, query=queryQualRTSup)
mydata <- rbind(mydata1,mydata2,mydata1Sup,mydata2Sup)

mydata$week = strftime(mydata$imaging_time, format = "%V")
mydata$Year = strftime(mydata$imaging_time, format = "%Y")
mydata$WeekYear = paste(mydata$Year,mydata$week,sep = "-")
mydata$popID = gsub(" ", "", mydata$populated_serial_no, fixed = TRUE)

head(mydata, n = 10)

mydata$number <- "FALSE"
mydata$number[!is.na(as.numeric(mydata$popID))] <- "TRUE"

amList = unique(mydata$popID[mydata$number == "TRUE"])

Beadload = getBeadLoadInfoByPopNum(amList = amList,
                                   location = "sd",
                                   consoleOutput = T)

Beadload$week = strftime(Beadload$LoadSentrixDate, format = "%V")
Beadload$Year = strftime(Beadload$LoadSentrixDate, format = "%Y")
Beadload$WeekYear = paste(Beadload$Year,Beadload$week,sep = "-")

chipcountbyweek = aggregate(Beadload$populated_serial_no, by = list(Beadload$product_no, Beadload$WeekYear), length)
colnames(chipcountbyweek) <- c("Product","YearWeek","Count")
chipcountbyweek = dcast(chipcountbyweek, Product~YearWeek, value.var = c("Count"))

plotGL = ggplot(mydata, aes(x = WeekYear, y = PercentGrayLow, color = color)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plotGT = ggplot(mydata, aes(x = WeekYear, y = PercentGray, color = color)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plotREG = ggplot(mydata, aes(x = WeekYear, y = registration, color = color)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Print 3 plots
print(plotGL)
print(plotGT)
print(plotREG)

