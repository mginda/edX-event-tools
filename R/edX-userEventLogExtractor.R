## ===================================================== ##
# Title:        Extracting all edX events for a single user ####
# Project:      edX user trajectory analysis
# 
# Copyright 2017 Krishna Madhavan
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#      
#
# Authors:      Krishna Madhavan, Kerrie Douglas, Taylor Williams
# Affiliation:  Purdue University
#
# 
# Description:  This script extracts all of a user's activity from the edX event log files in a user 
#                 selected folder.  It outputs the resulting data (all events tied to a single user's ID) 
#                 in two formats: a standard* JSON file and a CSV file. 
#                 (*NOTE: the edX provided logs are in NDJSON format, not the typical JSON format.)
# 
# 
# File input stack: 
#            1) A folder contining one or more "*.log.gz" event log file(s)    (source: edX)
# 
# 
# Package dependencies: jsonlite, ndjson, tcltk
#
#
# Changelog:
#   2017.08.11. Initial Code
#   2017.08.13. Added user output and user save as for output file
#   2017.08.15. Changed to extracting single user's activity
#   2017.08.28. Fixed require statement bug (moved required packages in individual require statements)
#   2017.08.29. Update to comments, file saving updates, added save to RData file (added to JSON and CSV) 
#   2017.09.29. Updating file for sharing

#
## ===================================================== ##



######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 


## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)


## _Load required packages #####
require("ndjson")     #needed to read the non-standard JSON log files (NDJSON format)
require("jsonlite")   #for working with JSON files (esp. read and write)
require("tcltk")      #for OS independant GUI file and folder selection






















































######### Main ########## 














## _Build list of all event files ####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
####"(The file picker window may have opened in the background.  Check behind this window if you do not see it.) \n")
cat("\n*****Select the folder containing the event log files.***** (The log files should be '*.log.gz'.)
    (The file picker window may have opened in the background.  Check behind this window if you do not see it.) 
    \n")
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = tclvalue(tkchooseDirectory()), 
                       
                       pattern = ".log.gz$")

# Count the number of folders
numLogFiles <- length(fileList) 



## _Save one user's complete event history #### 
#create (or reset) eventLog file to store the combined event logs
eventLog <- NULL

#Request from user provide the userID whose event data should be extracted
curUserID <- readline(prompt="What user_id? ")


# loop though all files and build a single log data frame
for(i in 1:numLogFiles){
  curFileName <- fileList[i] 
  
  #print update message to console
  message("Processing log file ", i, " of ", numLogFiles)
  print(proc.time() - start)
  
  #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
  ndData <- ndjson::stream_in(curFileName)
  
  #extract events for a single user, add to the complete eventLog for that user
  eventLog <- rbind.data.frame(eventLog, subset(ndData,ndData$context.user_id==curUserID), fill=TRUE)
  
}


## _Save data to file(s) ####
# Ask user for the save file base filename
repeat
{  
  useDefaultFilename <- readline(prompt = paste0("Would you like to use 'Full event log for ", 
                                                 curUserID, "' as the save file base filename? (Y/N): "))
  
  if(useDefaultFilename == "Y" | useDefaultFilename == 'y') #use presented default filename
  {
    saveFilename <- paste0("Full event log for ", curUserID)
    break   #exit the repeat loop
    
  }else if(useDefaultFilename == "N" | useDefaultFilename == 'n') #use user provided filename
  {
    saveFilename <- readline(prompt="Filename (without extension): ")
    break   #exit the repeat loop
  }else   #catch bad user input
  {
    message("\nPlease enter 'Y' or 'N'\n")
  }
}

# ###TW: The following directory selection is working, but using the resulting selected folder within 
#         the save functions isn't, so commenting section out for now
# # Ask user for which directory to save file to
# repeat
# {
#   useDefaultDirectory <- readline(prompt = paste0("Would you like to save files to \n'", 
#                                                   getwd(), "/output_userEventLogs/'? (Y/N): "))
#   if(useDefaultDirectory == "Y" | useDefaultDirectory == 'y') #save to working directory
#   {
#     saveDirectory <- file.path(getwd(), "output_userEventLogs")
#     break   #exit the repeat loop
#     
#   }else if(useDefaultDirectory == "N" | useDefaultDirectory == 'n') #use user provided directory
#   {
#     cat("Select a directory to save files to.")
#     saveDirectory <- tclvalue(tkchooseDirectory())
#     break   #exit the repeat loop
#   }else   #catch bad user input
#   {
#     message("\nPlease enter 'Y' or 'N'\n")
#   }
# }






#Save the full log to file back as a single JSON file (in typical JSON format, not NDJSON)
cat("\nSave user's log to .JSON.\n\n")
# write_json(x = eventLog, path = file.path(saveDirectory, paste0(saveFilename, ".json")))
write_json(x = eventLog, path = choose.files(caption = paste0("Save As... for userID ", curUserID),
                                             default = saveFilename,
                                             
                                             filters = c("JSON (.json)","*.json")))

#Save full log as a CSV file
cat("\nSave user's log to .CSV.\n\n")
# write.csv(x = eventLog, path = file.path(saveDirectory, paste0(saveFilename, ".csv")))
write.csv(x = eventLog, file = choose.files(caption = paste0("Save As... for userID ", curUserID),
                                            default = saveFilename,
                                            
                                            filters = c("CSV (.csv)","*.csv")))

#Save full log as a RData file
cat("\nSave user's log to .RData\n\n")
# save(eventLog, file = file.path(saveDirectory, paste0(saveFilename, ".RData")), precheck = TRUE)
save(eventLog, file = choose.files(caption = paste0("Save As... for userID ", curUserID),
                                   default = saveFilename,
                                   filters = c("RData (.RData)","*.RData")))


######### Finishing Details ########## 
#Indicate completion
message("\n**** Complete! ****\n")

## _Script processing time feedback #####
#print the amount of time the script required
cat("\n\n\nComplete script processing time details (in sec):\n")
print(proc.time() - start)



## _Clear environment variables
rm(list=ls())   



###########backup code for future feature development##########################

# ## example code for converting the JSON event time to POSIX 
#     (from https://stackoverflow.com/questions/9059726/r-converting-json-time-format-into-posix)
#     
# # replace [+-]hh:mm with [+-]hhmm for timezone offset
# # i.e. 2012-01-30T12:00:45+08:00 -> 2012-01-30T12:00:45+0800
# x$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',x$Time)
# # do as.POSIXlt with %z
# x$Time <- as.POSIXlt(x$Time, format="%Y-%m-%dT%H:%M:%S%z", 
#                      origin="1970-01-01",tz="GMT")
