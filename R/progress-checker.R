

# load completed student_id values (those with files saved in the path)
dropboxOutputPath <- "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1"
listCompletedIDs <- list.files(full.names = FALSE, recursive = TRUE, 
                               path = dropboxOutputPath,
                               pattern = ".json$", include.dirs = FALSE)
listCompletedIDs <- sub(".*/", "", listCompletedIDs)     # remove subdirectory names
listCompletedIDs <- sub(".json", "", listCompletedIDs)   # remove extension


# load all student_id values
path_student_id_csv <- c("data/B1 data/access_data. all.csv")
students <- read.csv(path_student_id_csv, header = TRUE)


# print the current status
completionSummary <- summary(students$student_id %in% listCompletedIDs)
completionSummary

# calc percent complete
completed <- as.numeric(completionSummary[3])
total <- as.numeric(completionSummary[2]) + as.numeric(completionSummary[3])

pctComplete <- completed/total*100

# print results
completionSummary
message(paste0(sprintf("%.1f", pctComplete), "% complete"))
