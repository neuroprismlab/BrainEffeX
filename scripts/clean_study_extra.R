# format extra study details table for app and manuscript

study_extra <- read.csv("/Users/shearer.h/Downloads/Study_Dataset Information_ BrainEffeX - Sheet1 (1).csv")

colnames(study_extra)[1] = "Study Name"
colnames(study_extra)[6] = "Task Info"
colnames(study_extra)[7] = "Scan Duration"
colnames(study_extra)[9] = "Measure Info"

# remove citation columns
study_extra <- study_extra[1:9]

# save as csv
write.csv(study_extra, file = "study_extra.csv", row.names = FALSE)