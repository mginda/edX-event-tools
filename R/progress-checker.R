

# load completed student_id values (those with files saved in the path)
dropboxOutputPath <- "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B4"
listCompletedIDs <- list.files(full.names = FALSE, recursive = TRUE, 
                               path = dropboxOutputPath,
                               pattern = ".json$", include.dirs = FALSE)
listCompletedIDs <- sub(".*/",   "", listCompletedIDs)   # remove subdirectory names
listCompletedIDs <- sub(".json", "", listCompletedIDs)   # remove extension


# load all student_id values
path_student_id_csv <- c("C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B4/B4 data/access_data. all.csv")
students <- read.csv(path_student_id_csv, header = TRUE)


# current status summary
completionSummary <- summary(students$student_id %in% listCompletedIDs)


# calc percent complete
completed <- as.numeric(completionSummary[3])
total <- as.numeric(completionSummary[2]) + as.numeric(completionSummary[3])
pctComplete <- completed/total*100


# identify IDs that are not yet complete
listCompletedIDs <- as.numeric(listCompletedIDs)
listIncompleteIDs <- students$student_id[!(students$student_id %in% listCompletedIDs)]


# save to file ID lists (complete and incomplete IDs)
save(list = c("listIncompleteIDs", "listCompletedIDs"), file = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B4/uid_assignmentListUpdate.RData")


# print results
timestamp()
completionSummary
message(paste0(sprintf("%.1f", pctComplete), "% complete"))
