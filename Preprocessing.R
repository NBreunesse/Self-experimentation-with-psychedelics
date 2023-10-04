# preprocessing data for MLM
MLM_pre <- read.csv("C:\\Users\\nino_\\Documents\\School\\WO\\UvA\\Master\\Brain and Cognitive Sciences\\Research Project 2\\Results\\Statistics\\Current analysis\\try\\Self-exp_pre.csv")
# remove first two rows
MLM_pre <- MLM_pre[-c(1, 2), ]
# remove columns
MLM_pre <- MLM_pre[, -c(1:4, 6, 8:17)] # remove columns "StartDate", "EndDate", "Status", "IPAddress", "Duration (in seconds)", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage"
MLM_pre <- MLM_pre[, -which(names(MLM_pre) == "Raffle")] # remove column with email addresses to ensure anonymity

# make profession columns
MLM_pre$Profession <- sapply(strsplit(MLM_pre$Inclusion.criteria, ","), function(x) as.numeric(x)) # change class from string to list and make column "Profession"
research_vec <- sapply(MLM_pre$Profession, function(vec) sum(vec == 1)) # check whether 1 occurs
psychologist_vec <- sapply(MLM_pre$Profession, function(vec) sum(vec == 2))
psychiatrist_vec <- sapply(MLM_pre$Profession, function(vec) sum(vec == 3))
tripsitter_vec <- sapply(MLM_pre$Profession, function(vec) sum(vec == 4))
otherprof_vec <- sapply(MLM_pre$Profession, function(vec) sum(vec == 5))
MLM_pre$research_vec <- research_vec # make column that states whether it is a researcher
MLM_pre$psychologist_vec <- psychologist_vec
MLM_pre$psychiatrist_vec <- psychiatrist_vec
MLM_pre$tripsitter_vec <- tripsitter_vec
MLM_pre$otherprof_vec <- otherprof_vec
psych_vec <- sapply(MLM_pre$Profession, function(vec) as.integer(any(vec %in% c(2, 3))))
MLM_pre$psych_vec <- psych_vec # make column that states whether it is a psychologist or a psychiatrist
MLM_pre$Profession <- sapply(MLM_pre$Profession, paste, collapse = ", ") # unlist again to make sure it can be written into a csv file

# make motivation columns
MLM_pre$Motivation <- sapply(strsplit(MLM_pre$Motivation.to.use., ","), function(x) as.numeric(x)) # change class from string to list and make column "Profession"
fun_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 1)) # check whether 1 occurs
curious_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 2)) 
growth_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 3)) 
experiment_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 4)) 
empathize_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 5)) 
spiritual_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 6)) 
mental_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 7)) 
nature_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 8)) 
others_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 9)) 
escapism_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 10))
othermot_vec <- sapply(MLM_pre$Motivation, function(vec) sum(vec == 11)) 
MLM_pre$fun_vec <- fun_vec # make column that states whether this motivation applies
MLM_pre$curious_vec <- curious_vec
MLM_pre$growth_vec <- growth_vec
MLM_pre$experiment_vec <- experiment_vec
MLM_pre$empathize_vec <- empathize_vec
MLM_pre$spiritual_vec <- spiritual_vec
MLM_pre$mental_vec <- mental_vec
MLM_pre$nature_vec <- nature_vec
MLM_pre$others_vec <- others_vec
MLM_pre$escapism_vec <- escapism_vec
MLM_pre$othermot_vec <- othermot_vec
MLM_pre$Motivation <- sapply(MLM_pre$Motivation, paste, collapse = ", ") # unlist again to make sure it can be written into a csv file

# change column type to numeric
MLM_pre$Progress <- as.numeric(MLM_pre$Progress)
MLM_pre$APQ.r1_1 <- as.numeric(MLM_pre$APQ.r1_1)
MLM_pre$APQ.r1_2 <- as.numeric(MLM_pre$APQ.r1_2)
MLM_pre$APQ.r1_3 <- as.numeric(MLM_pre$APQ.r1_3)
MLM_pre$APQ.r1_4 <- as.numeric(MLM_pre$APQ.r1_4)
MLM_pre$APQ.r1_5 <- as.numeric(MLM_pre$APQ.r1_5)
MLM_pre$APQ.r2_1 <- as.numeric(MLM_pre$APQ.r2_1)
MLM_pre$APQ.r2_2 <- as.numeric(MLM_pre$APQ.r2_2)
MLM_pre$APQ.r2_3 <- as.numeric(MLM_pre$APQ.r2_3)
MLM_pre$APQ.r2_4 <- as.numeric(MLM_pre$APQ.r2_4)
MLM_pre$APQ.r2_5 <- as.numeric(MLM_pre$APQ.r2_5)
MLM_pre$APQ.r3_1 <- as.numeric(MLM_pre$APQ.r3_1)
MLM_pre$APQ.r3_2 <- as.numeric(MLM_pre$APQ.r3_2)
MLM_pre$APQ.r3_3 <- as.numeric(MLM_pre$APQ.r3_3)
MLM_pre$APQ.r3_4 <- as.numeric(MLM_pre$APQ.r3_4)
MLM_pre$APQ.r3_5 <- as.numeric(MLM_pre$APQ.r3_5)
MLM_pre$APQ.r4_1 <- as.numeric(MLM_pre$APQ.r4_1)
MLM_pre$APQ.r4_2 <- as.numeric(MLM_pre$APQ.r4_2)
MLM_pre$APQ.r4_3 <- as.numeric(MLM_pre$APQ.r4_3)
MLM_pre$APQ.r4_4 <- as.numeric(MLM_pre$APQ.r4_4)
MLM_pre$APQ.r4_5 <- as.numeric(MLM_pre$APQ.r4_5)
MLM_pre$Self.exp.attitudes_1 <- as.numeric(MLM_pre$Self.exp.attitudes_1)
MLM_pre$Self.exp.attitudes_2 <- as.numeric(MLM_pre$Self.exp.attitudes_2)
MLM_pre$Self.exp.attitudes_3 <- as.numeric(MLM_pre$Self.exp.attitudes_3)
MLM_pre$Self.exp.attitudes_4 <- as.numeric(MLM_pre$Self.exp.attitudes_4)
MLM_pre$Self.exp.attitudes_5 <- as.numeric(MLM_pre$Self.exp.attitudes_5)
MLM_pre$Self.exp.attitudes_6 <- as.numeric(MLM_pre$Self.exp.attitudes_6)
MLM_pre$Self.exp.attitudes_7 <- as.numeric(MLM_pre$Self.exp.attitudes_7)
MLM_pre$Self.exp.attitudes_8 <- as.numeric(MLM_pre$Self.exp.attitudes_8)
MLM_pre$Self.exp.attitudes_9 <- as.numeric(MLM_pre$Self.exp.attitudes_9)

# Convert character columns to numeric without changing non-numeric values
# MLM_pre <- apply(MLM_pre, 2, function(x) ifelse(is.na(as.numeric(x)), x, as.numeric(x))) # does not work yet (warning: In ifelse(is.na(as.numeric(x)), x, as.numeric(x)) : NAs introduced by coercion)
# MLM_pre <- apply(MLM_pre, 2, function(x) {
#   numeric_vals <- suppressWarnings(as.numeric(x))
#   non_numeric_vals <- x[is.na(numeric_vals)]
#   numeric_vals[is.na(numeric_vals)] <- non_numeric_vals
#   numeric_vals
# })

# remove participants with progress lower than 90
MLM_pre <- MLM_pre[MLM_pre$Progress >= 90, ]
# remove participants that do not consent to participate
MLM_pre <- MLM_pre[MLM_pre$Consent.form == 1, ]
# remove participants with insincere responses
MLM_pre <- MLM_pre[MLM_pre$Humorous.Responding <= 2, ]
# remove participants that give implausible answers to at least two of the three mischievous survey items (i.e., implausible height, more than 7 children, more than 11 hours of sleep)
# MLM_pre$Height <- ifelse(MLM_pre$Q139 == 2, MLM_pre$Height * 2.54, MLM_pre$Height) # first change notation of inch to numeric
# rough evaluation of whether participants that respond in cm are within normal limits
# MLM_pre$Q139 <- as.numeric(MLM_pre$Q139)
# MLM_pre <- MLM_pre[MLM_pre$Q139 != 2, ] # Remove rows where column Q139 is equal to 2
# MLM_pre <- MLM_pre[MLM_pre$Height != "NA", ] # Remove rows where column Height is non-numeric
# MLM_pre$Height <- as.numeric(MLM_pre$Height)
# MLM_pre <- MLM_pre[MLM_pre$Sleep != "NA", ]
# MLM_pre$Sleep <- as.numeric(MLM_pre$Sleep)
# MLM_pre <- MLM_pre[MLM_pre$Children != "NA", ]
# MLM_pre$Children <- as.numeric(MLM_pre$Children)
# SD.Height <- 6.35 # see C:\Users\nino_\Documents\School\WO\UvA\Master\Brain and Cognitive Sciences\Research Project 2\Literature\SD for height
# upper_bound <- 184 + 2 * SD.Height # 184 cm is the average height of the males in the tallest country (The Netherlands)
# lower_bound <- 151 - 2 * SD.Height # 151 cm is the average height of the females in the shortest country (Guatemala)
# condition_Height <- MLM_pre$Height > upper_bound | MLM_pre$Height < lower_bound
# condition_Sleep <- MLM_pre$Sleep > 11
# condition_Children <- MLM_pre$Children > 7
# conditions <- cbind(condition_Height, condition_Sleep, condition_Children)
# condition_counts <- rowSums(conditions)
# MLM_pre <- MLM_pre[condition_counts >= 2, ] # Remove rows where at least two out of three conditions are not met

# reverse code APQ scores
MLM_pre$APQ.r1_3.R <- 6 - MLM_pre$APQ.r1_3
MLM_pre$APQ.r2_3.R <- 6 - MLM_pre$APQ.r2_3
MLM_pre$APQ.r2_4.R <- 6 - MLM_pre$APQ.r2_4
MLM_pre$APQ.r3_1.R <- 6 - MLM_pre$APQ.r3_1
MLM_pre$APQ.r3_3.R <- 6 - MLM_pre$APQ.r3_3
MLM_pre$APQ.r3_5.R <- 6 - MLM_pre$APQ.r3_5
MLM_pre$APQ.r4_2.R <- 6 - MLM_pre$APQ.r4_2
MLM_pre$APQ.r4_4.R <- 6 - MLM_pre$APQ.r4_4
# sum APQ
MLM_pre$total.APQ <- rowSums(MLM_pre[, c("APQ.r1_1", "APQ.r1_2", "APQ.r1_3.R", "APQ.r1_4", "APQ.r1_5", 
                                         "APQ.r2_1", "APQ.r2_2", "APQ.r2_3.R", "APQ.r2_4.R", "APQ.r2_5", 
                                         "APQ.r3_1.R", "APQ.r3_2", "APQ.r3_3.R", "APQ.r3_4", "APQ.r3_5.R", 
                                         "APQ.r4_1", "APQ.r4_2.R", "APQ.r4_3", "APQ.r4_4.R", "APQ.r4_5")])
MLM_pre$total.APQ

# reverse code AOSE scores
MLM_pre$Self.exp.attitudes_2.R <- 6 - MLM_pre$Self.exp.attitudes_2
MLM_pre$Self.exp.attitudes_4.R <- 6 - MLM_pre$Self.exp.attitudes_4
MLM_pre$Self.exp.attitudes_6.R <- 6 - MLM_pre$Self.exp.attitudes_6
MLM_pre$Self.exp.attitudes_8.R <- 6 - MLM_pre$Self.exp.attitudes_8
# sum AOSE
MLM_pre$total.AOSE <- rowSums(MLM_pre[, c("Self.exp.attitudes_1", "Self.exp.attitudes_2.R", "Self.exp.attitudes_3", 
                                          "Self.exp.attitudes_4.R", "Self.exp.attitudes_5", "Self.exp.attitudes_6.R",
                                          "Self.exp.attitudes_7", "Self.exp.attitudes_8.R", "Self.exp.attitudes_9")])
MLM_pre$total.AOSE
  
# Specify the file path and name
file_path <- "C:\\Users\\nino_\\Documents\\School\\WO\\UvA\\Master\\Brain and Cognitive Sciences\\Research Project 2\\Results\\Statistics\\Current analysis\\try\\Self-exp_ana.csv"

# Write the dataframe to a CSV file
write.csv(MLM_pre, file = file_path, row.names = FALSE)

# Confirmation message
cat("CSV file created successfully at:", file_path, "\n")