edX Event Log Extractor for Individual Users 
==============
Authors:      Taylor Williams  
Affiliation:  Purdue University

_&lt;<https://github.com/tzwilliams/edX-event-tools>&gt;_

Please note that this project is still under active development.  As is, the code is highly experimental and will need to be cleaned before production use.

## Description
This script extracts all records of individual specified users' activity from the edX MOOC (Massive Open Online Course) event log files.  
It outputs the resulting data (all events tied to a single user's ID) in two formats: a standard JSON file and a CSV file. 

## Files
### Required Input Data Files
This pipeline requires edX-provided event log data files (NDJSON format) located within the subdirectories of an `events` folder.  

.......
    

### Output Files
CSV and JSON files for each specified user ID.

## Environment setup
At present this pipeline is designed to work within the RStudio IDE (https://www.rstudio.com/).  You will need to either install R and RStudio or use an online version of RStudio.  Alternatively, an online version is available for free on nanoHUB.org (https://nanohub.org/resources/rstudio); however, the following instructions may need to be slightly modified if using the online version.

## Instructions for using the package
1)	Create a working directory.  

1)  Download the source code from GitHub as a zip file.  Extract the zip file into the working directory created above. 

1)	The required edX datafiles ....



### Notes:
1)  


## Copyright
 Copyright 2017 Taylor Williams
 
     Licensed under the Apache License, Version 2.0 (the "License");
     you may not use this file except in compliance with the License.
     You may obtain a copy of the License at
     
     http://www.apache.org/licenses/LICENSE-2.0
     
     Unless required by applicable law or agreed to in writing, software
     distributed under the License is distributed on an "AS IS" BASIS,
     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     See the License for the specific language governing permissions and
     limitations under the License.
