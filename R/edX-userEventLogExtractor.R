## ===================================================== ##
# Title:        Extracting all edX events for a single student ####
# Project:      edX student trajectory analysis
# 
# Copyright 2017 Taylor Williams
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
# Author(s):      Taylor Williams
# Affiliation:    Purdue University
#
# 
# Description:  This script extracts all of a student's activity from the edX event log files in a user 
#                 selected folder.  It outputs the resulting data (all events tied to a single student's ID) 
#                 in two formats: a standard* JSON file and/or a CSV file. 
#                 (*NOTE: the edX provided logs are in NDJSON (streaming JSON) format, not the typical JSON format.)
# 
# 
# File input stack: 
#            1) A folder contining one or more "*.log.gz" event log file(s)    (source: edX)
# 
# 
# Package dependencies: jsonlite, ndjson, tcltk2, dplyr
#
#
# Changelog:
#   2017.08.11. Initial Code
#   2017.08.13. Added user output and user save as for output file
#   2017.08.15. Changed to extracting single user's activity
#   2017.08.28. Fixed require statement bug (moved required packages in individual require statements)
#   2017.08.29. Update to comments, file saving updates, added save to RData file (added to JSON and CSV) 
#   2017.09.29. Updating file for sharing
#   2017.10.19. Updated file to produce student trajectory logs for list of known students based on DF or list of student IDS (by MG)
#   2017.11.09. updated to allow a list of student_id values to come from an CSV file
#   2017.11.10. added user choice for output file type
#               added saving processing time and details for each loop to a log file
#
## ===================================================== ##

######### Setup ########## 
## _Clean the environment ####
rm(list=ls()) 

## _start timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)

## _Load required packages #####
# require("ndjson")     # needed to read the non-standard JSON log files (NDJSON format)
require("jsonlite")   # for working with JSON files (esp. read and write)
require("tcltk2")     # for OS independant GUI file and folder selection
require("dplyr")      # for building tibbles (tidy data frames) 
# require("rdrop2")      # for using dropbox

####Functions ####

#logCapture 
## The logCapture function is a modification of code provided by the Purdue University team
##   to allow mass extracting individual set of student logs based on known set of student IDs for an 
##   edX course. The function creates a unique log file saved to `path_output` for each student ID in the list, 
##   saved as either a JSON file (default), a CSV (fileFormat = "CSV"), or both (fileFormat = "both"). 
LogCapture <- function(student_IDs, fileList, studentEventLog, path_output, fileFormat = "JSON"){      
  numStudents <- nrow(student_IDs)
  numLogFiles <- length(fileList) 
  for(j in 1:numStudents){
    curID <- student_IDs$student_id[j]
    
    
    # Build list of all student_id values that have already completed JSON files saved within the path
    dropboxOutputPath <- "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1"
    listCompletedIDs <- list.files(full.names = FALSE, recursive = TRUE, 
                                   path = dropboxOutputPath,
                                   pattern = ".json$", include.dirs = FALSE)
    listCompletedIDs <- sub(".*/", "", listCompletedIDs)     # remove subdirectory names
    listCompletedIDs <- sub(".json", "", listCompletedIDs)   # remove extension
    
    # if it doesn't already exist,  build the JSON event file for the current student_id
    if(!(curID %in% listCompletedIDs)){
      
      # loop through all the event files, extract any event matching the current student_id
      for(i in 1:numLogFiles){
        curFileName <- fileList[i] 
        
        #print update message to console
        message("Processing log file ", i, " of ", numLogFiles, " (for student ", j, " of ", numStudents, "). Previous student completed at ", loopSummaryLog[j-1,]$time)
        print(proc.time() - start)
        
        #read log data (NOTE: logs are in NDJSON format, not typical JSON format)
        # ndData2 <- jsonlite::stream_in(file(curFileName))
        ndData  <-   ndjson::stream_in(curFileName)
        
        #extract events for a single student, add to the complete studentEventLog for that student
        studentEventLog <- rbind.data.frame(studentEventLog, subset(ndData,ndData$context.user_id==curID), fill=TRUE)
      }
      
      
      # save all of this student's events to file
      if(fileFormat == "JSON"){
        write_json(x = studentEventLog, path = file.path(path_output, paste0(curID, ".json")))
      }else if(fileFormat == "CSV"){
        write.csv(x = studentEventLog, file  = file.path(path_output, paste0(curID, ".csv")), row.names = FALSE)
      }else if(fileFormat == "both"){
        write_json(x = studentEventLog, path = file.path(path_output, paste0(curID, ".json")))
        write.csv(x = studentEventLog, file  = file.path(path_output, paste0(curID, ".csv")), row.names = FALSE)
      }else{
        message("invalid file format selected")
        return()  #exit function
      }
      
      # Write a status log file for processing this student's events.
      #   student_id, 
      #   modulesTouched: number of modules the student interacted with (from edX-clustering pipeline), 
      #   eventCount: number of entries in the studentEventLog 
      #   date & time: timestamp 
      #   computerName: the name of the PC running this script
      #   computerProcessor: the processor details for the PC running this script
      loopSummaryLog <- add_row(loopSummaryLog,
                                student_id = curID,
                                modulesTouched = 999, #students[j,]$number_accesses,  ##TW removing for now, restore later
                                eventCount   = nrow(studentEventLog),
                                date = format(Sys.time(), "%D"),
                                time  = format(Sys.time(), "%H:%M:%S"),
                                computerName = Sys.getenv("COMPUTERNAME"),
                                computerProcessor = Sys.getenv("PROCESSOR_IDENTIFIER"))
      write.csv(x = loopSummaryLog, file  = file.path(path_output, paste0("script log.csv")), row.names = FALSE)
      
      
      # reset the student event log for the next iteration (next student)
      studentEventLog <- NULL
    }
  }# end of single student loop
  
  return(loopSummaryLog)
}# end of LogCapture function




# Main --------------------------------------------------------------------

# retrieve list of edX student_id values (from a CSV) whose event data should be extracted 
path_student_id_csv <- c("data/B1 data/access_data. all.csv")
# if(interactive()) path_student_id_csv = (tk_choose.files(caption = "CSV with student_id values")) #,
# default = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Clustering/Boeing pipeline output files/B1, run 2017.11.08/3_ClusteringOutput/access_data. all.csv"))


# students <- read.csv(path_student_id_csv, header = TRUE)


# temp id list (delete code after run) ------------------------------------
load("C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1/listIDsForTW.RData")
# students <- data.frame(student_id = uid_TW)

#remove the earlier fraction already being worked on in another process
students <- data.frame(student_id = listIDsForTW[226:300])
#   -----------------------------------------------------------------------



# extract only the stuent_id values
student_IDs <- tibble(student_id = as.numeric())
student_IDs <- add_row(student_IDs, 
                       student_id = students$student_id)

#Creates paths used to locate directory for research data sets and save processing outputs
##TW TODO: set non-interactive option
path_data <- c("data/B1 data/events/")
message("select Events directory with data")
# if(interactive()) path_data = tk_choose.dir(caption = "select Events directory with data") #, 
# default = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/Data/New_Boeing_Data_April2_2017_DO_NOT_USE_WO_KM_Permission/edx data/MITProfessionalX_SysEngxB1_3T2016/events")

path_output <- c("C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1/TW Laptop/")
message("select the output directory")
# if(interactive()) path_output = tk_choose.dir(caption = "select the output directory") #,
# default = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1") 

## _Build list of all event files for course####
#Store all the filenames of JSON formatted edX event logs within a user selected directory 
# (files ending with ".log.gz").
fileList <- list.files(full.names = TRUE, recursive = FALSE, 
                       path = path_data,
                       pattern = ".log.gz$")

#create (or reset) studentEventLog file to store the combined event logs for one student
studentEventLog <- NULL

# create loop log to store details about total number of entries for a 
#   student and the processing time required to extract them all
loopSummaryLog <- tibble(student_id = as.numeric(),
                         eventCount   = as.numeric(),
                         modulesTouched = as.numeric(),
                         date       = as.character(),
                         time       = as.character(),
                         computerName      = as.character(),
                         computerProcessor = as.character())



#Log Capture function for list of users
LogCapture(student_IDs, fileList, studentEventLog, path_output)



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
