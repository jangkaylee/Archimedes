# This code captures uptime of Archimedes in Array over the last 24 hours. 
# Code developed by Kay Lee. 

library(RODBC)
library(lubridate)


# Source codes
source("//USSD-PRD-MPE01/R Work/common/queryExecutor.R")
source("//ussd-prd-mpe01/R Work/common/ammsGetEquipmentUptime.R")


# Removes all objects from the workspace
rm(list=ls())


# User Input: Enter start date, end date, and department ("YEAR-MM-DD"). 
startDate = today()
endDate = today()-1
deptNum = "ARRAY"


# AMMS SQL query
ammsGetEquipmentUptime = function(
  startDate   = NULL,
  endDate     = NULL,
  deptNum = NULL,
  consoleOutput = FALSE
){
  source("//USSD-PRD-MPE01/R Work/common/queryExecutor.R")
  
  if( consoleOutput ){
    print(paste( "starting amms equip status query", Sys.time() ))
  }
  
  query = paste(
    "exec dbo.pr_UTILITY_Equipment_Status_Tracker_EqSystem_test_10172011 '",
    startDate, "'",",", "'",
    endDate, "'",",", "'",
    deptNum, "'",
    sep = ""
  )
  queryResults = queryExecutor( db = "amms", query = query )
  
  if( consoleOutput ){
    print(paste( "amms equip status query", Sys.time() ))
  }
  return( queryResults )
}




upTime = ammsGetEquipmentUptime(startDate, endDate, deptNum)
arch = upTime[upTime$eqSystem == "ARCH IMAGING",]


# Status count
archTot = nrow(arch)
downCount = length(which(arch$eqStatus == "DOWN"))
upCount = length(which(arch$eqStatus == "UP"))
engCount = length(which(arch$eqStatus == "ENG"))
downVRCount = length(which(arch$eqStatus == "DOWN_VR"))
up_attnCount = length(which(arch$eqStatus == "UP_ATTN"))

# Uptime calculations
upP1 = (archTot-downCount-downVRCount)/(archTot)
upP2 = (upCount+engCount+up_attnCount)/archTot

if(upP1 == upP2){
  print(upP1)
}  else  {
    print("Error: upP1 is not equal to upP2. (upP1 = [total-DOWN-DOWN_VR]/total. upP2 = [UP+ENG]/total)")
    sprintf("upP1 = %s and upP2 = %s ", upP1, upP2)
}


# Set working directory 
userLogin = as.character(Sys.info()["login"])
path = paste("C:\\Users\\", userLogin, "\\Documents\\R\\",sep = "")
setwd (path)

write.table(upP1, "uptime.csv",
            row.names = now(),
            col.names= FALSE,
            sep = ",", 
            append = TRUE)

# Displaying DOWN instrumnents 
down = arch[arch$eqStatus == "DOWN",]
down
down_vr = arch[arch$eqStatus == "DOWN_VR",]
down_vr
up_attn = arch[arch$eqStatus == "UP_ATTN",]
if(up_attnCount > 1){
  print(up_attn)
}  else {
    print("No UP_ATTN")
} 
