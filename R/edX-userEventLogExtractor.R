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
#   2017.10.19. Updated file to produce user trajectory logs for list of known students based on DF or list of user IDS (by MG)
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
require("tcltk2")     #for OS independant GUI file and folder selection

####Functions 
#logCapture 
##The logCapture function is a modification of code provided by Purdue University team
##to allow mass extracting individual set of student logs based on known set of student IDs for an 
##edX course. The function creates a unique log file for each student ID in the list, 
##saved as either a JSON or CSV formatted file. The function currently set up to save as CSV,
##alternatively can be set for user defined action such as format=T csv if format=F, JSON set up possible.

logCapture <- function(curUserIDS,fileList,eventLog,path){      
  numStudents <- length(curUserIDS)
  numLogFiles <- length(fileList) 
  for(j in 1:numStudents){
    curUser <- curUserIDS[j]
    for(i in 1:numLogFiles){
      curFileName <- fileList[i] 
      #print update message to console
      message("Processing log file ", i, " of ", numLogFiles)
      print(proc.time() - start)
      #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
      ndData <- ndjson::stream_in(curFileName)
      #extract events for a single user, add to the complete eventLog for that user
      eventLog <- rbind.data.frame(eventLog, subset(ndData,ndData$context.user_id==curUser), fill=TRUE)
      id <- curUser
    }
    #Used to clean up the large amount of columns that are present in a course that are not sparsely used
    eventLog <- eventLog[,c(1,2,3,4,6,7,8,9,10,11,12,16,17,19,20,27,29,37,38,
                            42,50,109,110,111,112,113,114,115,116,117,118,119,
                            120,121,122,123,124,125,126,127,128,129,130,131,132,
                            133,134,135,136,137,138,139,248,249,250,251,252,253,
                            254,255,256,257,258,259,260,261,262,263,264,265,266,
                            267,268,269,270,271,272,273,274,275,276,277,278,279,
                            280,281,282,283,284,285,286,287,288,3007,3008,3202,
                            3203,4536,4537,4538,4539,4553,5023,5024)]
    #    eventLog <- eventLog[,!grepl("^event\\.new\\wstate\\.",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.old\\wstate\\.",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.new\\wstate\\.",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.state\\.correct\\wmap",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.state\\.done",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.state\\.input\\wstate",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.state\\.student\\wanswers",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("^event\\.state\\.submission",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("event\\.user\\wid",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("event\\.mode",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("event\\.course\\wid",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("context\\.course\\wuser\\wtags",names(eventLog))]
    #    eventLog <- eventLog[,!grepl("context\\.asides",names(eventLog))]
    
    write.csv(x = eventLog, file = paste0(path,"/",id,".csv"),
              row.names = F)
    eventLog <- NULL
  }
}

######### Main ########## 
#Test set of individual userIDs for students in an edX Course whose event data should be extracted
#Test code for function
d <- data.frame(matrix(ncol = 1, nrow = 2))
names(d) <- c("ids")
d$ids <-  c("52848848","227878041")
curUserIDS <- d$ids
rm(d)

#Creates paths used to locate directory for research data sets and save processing outputs
path_data <- c("__________/events")
#path_data = tclvalue(tkchooseDirectory())
path_output <- c("__________/output/")
#path_output = tclvalue(tkchooseDirectory())

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".log.gz$")

#create (or reset) eventLog file to store the combined event logs
eventLog <- NULL

#Log Capture function for list of users
logCapture(curUserIDS,fileList,eventLog,path=path_output)

####
#Old Data Export tools
#Save the full log to file back as a single JSON file (in typical JSON format, not NDJSON)
#cat("\nSave user's log to .JSON.\n\n")
# write_json(x = eventLog, path = file.path(saveDirectory, paste0(saveFilename, ".json")))
#write_json(x = eventLog, path = choose.files(caption = paste0("Save As... for userID ", curUserID),
#                                             default = saveFilename,
#                                             filters = c("JSON (.json)","*.json")))

#Save full log as a CSV file
#cat("\nSave user's log to .CSV.\n\n")
# write.csv(x = eventLog, path = file.path(saveDirectory, paste0(saveFilename, ".csv")))
#write.csv(x = eventLog, file = choose.files(
#  default = saveFilename,
#  filters = c("CSV (.csv)","*.csv")))

#Save full log as a RData file
#cat("\nSave user's log to .RData\n\n")
# save(eventLog, file = file.path(saveDirectory, paste0(saveFilename, ".RData")), precheck = TRUE)
#save(eventLog, file = choose.files(caption = paste0("Save As... for userID ", curUserID),
#                                   default = saveFilename,
#                                  filters = c("RData (.RData)","*.RData")))

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
