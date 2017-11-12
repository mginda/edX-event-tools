

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


# save list of IDs that are not yet complete
listCompletedIDs <- as.numeric(listCompletedIDs)
listIncompleteIDs <- students$student_id[!(students$student_id %in% listCompletedIDs)]
save(list = c("listIncompleteIDs"), file = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1/listIncompleteIDs.RData")

# other ID lists worth saving, excluding IDs Doipayan is covering
load("C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1/uid_assignments.RData")
uid_listForTW <- listIncompleteIDs[!(listIncompleteIDs %in% uid_DR[-(1:100)])]
save(list = c("uid_listForTW", "uid_DR", "listIncompleteIDs", "listCompletedIDs"), file = "C:/Users/TaylorWilliams/Dropbox (Contextualized Eval)/Contextualized Eval Team Folder/GRADS/Taylor/_Boeing/Event logs per student/B1/uid_assignmentListUpdate.RData")


# print results
completionSummary
message(paste0(sprintf("%.1f", pctComplete), "% complete"))
