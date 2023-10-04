# download file
Selfexp_ana <- read.csv("C:\\Users\\nino_\\Documents\\School\\WO\\UvA\\Master\\Brain and Cognitive Sciences\\Research Project 2\\Results\\Statistics\\Current analysis\\try\\Self-exp_ana.csv")

# demographics
# gender
count_male <- sum(Selfexp_ana$Gender == 1)
count_female <- sum(Selfexp_ana$Gender == 2)
count_nb <- sum(Selfexp_ana$Gender == 3) # amount of non-binary
count_NA <- sum(Selfexp_ana$Gender == 4) # prefer not to say 
total_rows <- nrow(Selfexp_ana)
percentage_male <- (count_male / total_rows) * 100
percentage_female <- (count_female / total_rows) * 100
percentage_nb <- (count_nb / total_rows) * 100
gender_counts <- table(Selfexp_ana$Gender)
gender_percentages <- prop.table(gender_counts) * 100 # Calculate the percentages
gender_percentages <- round(gender_percentages, digits = 0) # Remove decimals from Percentage column
gender_table <- data.frame(Gender = c(1, 2, 3), Count = gender_counts, Percentage = gender_percentages) # Combine counts and percentages into a table
gender_table$Count.Freq <- paste(gender_table$Count.Freq, " (", gender_percentages, ")", sep = "")
gender_table$Count.Var1 <- ifelse(gender_table$Count.Var1 == 1, "Male",
                                  ifelse(gender_table$Count.Var1 == 2, "Female", "Non-binary"))
gender_table$Gender <- NULL # remove column "Gender"
gender_table$Percentage.Var1 <- NULL # remove column "Percentage.Var1"
gender_table$Percentage.Freq <- NULL # remove column "Percentage.Freq"
names(gender_table)[1] <- "Gender" # rename column 1
names(gender_table)[2] <- "Count (%)" # rename column 2
print(gender_table)
install.packages("rempsyc")
library(rempsyc)
nice_table(gender_table,
           title = c("Table 1. Demographics", ""))

# age
mean_age <- mean(Selfexp_ana$Age)
sd_age <- sd(Selfexp_ana$Age)
ages <- Selfexp_ana$Age
age_intervals <- c(18, 35, 45, 55, 65, max(ages) + 1) # Define the age group intervals
age_labels <- c("18-34", "35-44", "45-54", "55-64", "65+") # Define the age group labels
age_groups <- cut(ages, breaks = age_intervals, labels = age_labels, include.lowest = TRUE) # Cut the ages into groups
group_counts <- table(age_groups) # Calculate the count and percentage for each age group
group_percentages <- prop.table(group_counts) * 100
group_percentages <- round(group_percentages, digits = 0) # Remove decimals from Percentage column
age_table <- data.frame(age = age_labels, count = group_counts)
age_table$count.Freq <- paste(age_table$count.Freq, " (", group_percentages, ")", sep = "")
age_table$age <- NULL # remove column "age"
names(age_table)[1] <- "Age" # rename column 1
names(age_table)[2] <- "Count (%)" # rename column 2
print(age_table)
install.packages("rempsyc")
library(rempsyc)
nice_table(age_table)

# age and gender
male_vec <- sapply(Selfexp_ana$Gender, function(vec) sum(vec == 1)) # check whether 1 occurs
female_vec <- sapply(Selfexp_ana$Gender, function(vec) sum(vec == 2)) # check whether 1 occurs
Selfexp_ana$male_vec <- male_vec
Selfexp_ana$female_vec <- female_vec
male_data <- Selfexp_ana$Age[Selfexp_ana$male == 1]
mean_age_male <- mean(Selfexp_ana$Age[Selfexp_ana$male == 1], na.rm = TRUE)
sd_age_male <- sd(Selfexp_ana$Age[Selfexp_ana$male == 1], na.rm = TRUE)
female_data <- Selfexp_ana$Age[Selfexp_ana$female == 1]
mean_age_female <- mean(Selfexp_ana$Age[Selfexp_ana$female == 1], na.rm = TRUE)
sd_age_female <- sd(Selfexp_ana$Age[Selfexp_ana$female == 1], na.rm = TRUE)
gender.age <- t.test(male_data, female_data) # Perform an independent sample t-test to test age difference between male and female
gender.age_p <- gender.age$p.value # Check the p-value
summary(gender.age)

# type of psychedelic substance
Selfexp_ana$Type.of.psychedelics <- sapply(strsplit(Selfexp_ana$Type.of.psychedelics, ","), function(x) as.numeric(x)) # change class from string to list
total_rows <- nrow(Selfexp_ana) # calculate the total rows in dataframe
LSD_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 1)) # check whether 1 occurs
LSD <- sum(LSD_vec == 1) # calculate the total number of 1s
LSD_perc <- (LSD/total_rows)*100 # calculate the percentage of 1s 
Psilocybin_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 2))
Psilocybin <- sum(Psilocybin_vec == 1)
Psilocybin_perc <- (Psilocybin/total_rows)*100
DMT_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 3))
DMT <- sum(DMT_vec == 1)
DMT_perc <- (DMT/total_rows)*100
Ayahuasca_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 4))
Ayahuasca <- sum(Ayahuasca_vec == 1)
Ayahuasca_perc <- (Ayahuasca/total_rows)*100
MeODMT_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 10))
MeODMT <- sum(MeODMT_vec == 1) # rename
MeODMT_perc <- (MeODMT/total_rows)*100
Mescaline_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 8))
Mescaline <- sum(Mescaline_vec == 1)
Mescaline_perc <- (Mescaline/total_rows)*100
MDMA_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 5))
MDMA <- sum(MDMA_vec == 1)
MDMA_perc <- (MDMA/total_rows)*100
Ketamine_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) sum(vec == 6))
Ketamine <- sum(Ketamine_vec == 1)
Ketamine_perc <- (Ketamine/total_rows)*100
Other_numbers <- c(7, 9, 11, 14) # determine which numbers are "Other"
Other_vec <- sapply(Selfexp_ana$Type.of.psychedelics, function(vec) {
  any(vec %in% c(Other_numbers))
}) # check whether "Other_numbers" occur
Other <- sum(Other_vec == TRUE) 
Other_perc <- (Other/total_rows)*100
type_percentage <- c(LSD_perc, Psilocybin_perc, DMT_perc, Ayahuasca_perc, MeODMT_perc, Mescaline_perc, MDMA_perc, Ketamine_perc, Other_perc) # make a vector with percentages
type_percentage <- round(type_percentage, digits = 0) # Remove decimals from Percentage column
type_table <- data.frame(Drug = c("LSD", "Psilocybin", "DMT", "Ayahuasca", "MeODMT", "Mescaline", "MDMA", "Ketamine", "Other"),
                    Count = c(LSD, Psilocybin, DMT, Ayahuasca, MeODMT, Mescaline, MDMA, Ketamine, Other))
type_table <- cbind(type_table, Percentage = type_percentage)
type_table$Count <- paste(type_table$Count, " (", type_percentage, ")", sep = "")
type_table$Percentage <- NULL # remove column "Percentage"
names(type_table)[2] <- "Count (%)" # rename column 2
type_table[5, 1] <- "5-MeO-DMT"

# intensity per substance
LSD_intensity_median <- median(Selfexp_ana$Trip.intensity_1, na.rm = TRUE)
Psilocybin_intensity_median <- median(Selfexp_ana$Trip.intensity_2, na.rm = TRUE)
DMT_intensity_median <- median(Selfexp_ana$Trip.intensity_4, na.rm = TRUE)
Ayahuasca_intensity_median <- median(Selfexp_ana$Trip.intensity_5, na.rm = TRUE)
MeODMT_intensity_median <- median(Selfexp_ana$Trip.intensity_3, na.rm = TRUE)
Mescaline_intensity_median <- median(Selfexp_ana$Trip.intensity_6, na.rm = TRUE)
MDMA_intensity_median <- median(Selfexp_ana$Trip.intensity_7, na.rm = TRUE)
Ketamine_intensity_median <- median(Selfexp_ana$Trip.intensity_8, na.rm = TRUE)
othersub_columns <- c(Selfexp_ana$Trip.intensity_9, Selfexp_ana$Trip.intensity_10, Selfexp_ana$Trip.intensity_11, Selfexp_ana$Trip.intensity_12, Selfexp_ana$Trip.intensity_13) # Select the columns you want to calculate the median for
Other_intensity_median <- median(othersub_columns, na.rm = TRUE)
type_intensity <- c(LSD_intensity_median, Psilocybin_intensity_median, DMT_intensity_median, Ayahuasca_intensity_median, MeODMT_intensity_median, Mescaline_intensity_median, MDMA_intensity_median, Ketamine_intensity_median, Other_intensity_median)
intensity_labels <- c("Not intense at all", "Somewhat intense", "Moderately intense", "Very intense", "Extremely intense") # Define the intensities
type_intensity <- factor(type_intensity, levels = 1:5, labels = intensity_labels, ordered = TRUE) # Replace the values in the Intensity column with the corresponding labels
type_table <- cbind(type_table, "Intensity(a)" = type_intensity)

# make type_table nice
install.packages("rempsyc")
library(rempsyc)
nice_table(type_table,
           title = c("Table 3. Type of substance", ""),
           note = c(
           "a) median subjective intensity per substance"))

# make substance columns
Selfexp_ana$LSD_vec <- LSD_vec
Selfexp_ana$Psilocybin_vec <- Psilocybin_vec
Selfexp_ana$DMT_vec <- DMT_vec
Selfexp_ana$Ayahuasca_vec <- Ayahuasca_vec
Selfexp_ana$MeODMT_vec <- MeODMT_vec
Selfexp_ana$Mescaline_vec <- Mescaline_vec
Selfexp_ana$MDMA_vec <- MDMA_vec
Selfexp_ana$Ketamine_vec <- Ketamine_vec
Selfexp_ana$Other_vec <- Other_vec

LSD_intensity <- Selfexp_ana$Trip.intensity_1[Selfexp_ana$LSD_vec == 1] # Subset the data for each group with 1 in the respective vectors
Psilocybin_intensity <- Selfexp_ana$Trip.intensity_2[Selfexp_ana$Psilocybin_vec == 1]
DMT_intensity <- Selfexp_ana$Trip.intensity_4[Selfexp_ana$DMT_vec == 1]
Ayahuasca_intensity <- Selfexp_ana$Trip.intensity_5[Selfexp_ana$Ayahuasca_vec == 1]
MeODMT_intensity <- Selfexp_ana$Trip.intensity_3[Selfexp_ana$MeODMT_vec == 1]
Mescaline_intensity <- Selfexp_ana$Trip.intensity_6[Selfexp_ana$Mescaline_vec == 1]
MDMA_intensity <- Selfexp_ana$Trip.intensity_7[Selfexp_ana$MDMA_vec == 1]
Ketamine_intensity <- Selfexp_ana$Trip.intensity_8[Selfexp_ana$Ketamine_vec == 1]
othersub_columns <- c(Selfexp_ana$Trip.intensity_9, Selfexp_ana$Trip.intensity_10, Selfexp_ana$Trip.intensity_11, Selfexp_ana$Trip.intensity_12, Selfexp_ana$Trip.intensity_13) # Select the columns you want to calculate the median for
Other_intensity <- othersub_columns[Selfexp_ana$Other_vec == 1]
# Kruskal-Wallis test intensity between substances
all_intensities <- c(LSD_intensity, Psilocybin_intensity, DMT_intensity,
                     Ayahuasca_intensity, MeODMT_intensity, Mescaline_intensity,
                     MDMA_intensity, Ketamine_intensity)
intensity_vector <- rep(c("LSD", "Psilocybin", "DMT", "Ayahuasca",
                      "MeODMT", "Mescaline", "MDMA", "Ketamine"),
                    times = sapply(list(LSD_intensity, Psilocybin_intensity,
                                        DMT_intensity, Ayahuasca_intensity,
                                        MeODMT_intensity, Mescaline_intensity,
                                        MDMA_intensity, Ketamine_intensity),
                                   length)) # Create a corresponding grouping vector
kruskal_test_intensity <- kruskal.test(all_intensities, intensity_vector)
kruskal_test_intensity
# if significant post-hoc tests
install.packages("dunn.test")
library(dunn.test)
dunn_test_intensity <- dunn.test(all_intensities, intensity_vector, method = "bonferroni") # Perform Dunn's test
dunn_test_intensity

# profession
total_rows <- nrow(Selfexp_ana) # calculate the total rows in dataframe
research <- sum(Selfexp_ana$research_vec == 1)
research_perc <- (research/total_rows)*100
psychologist <- sum(Selfexp_ana$psychologist_vec == 1)
psychologist_perc <- (psychologist/total_rows)*100
psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1)
psychiatrist_perc <- (psychiatrist/total_rows)*100
tripsitter <- sum(Selfexp_ana$tripsitter_vec == 1)
tripsitter_perc <- (tripsitter/total_rows)*100
otherprof <- sum(Selfexp_ana$otherprof_vec == 1)
otherprof_perc <- (otherprof/total_rows)*100
Profession_percentage <- c(research_perc, psychologist_perc, psychiatrist_perc, tripsitter_perc, otherprof_perc) # make a vector with percentages
Profession_percentage <- round(Profession_percentage, digits = 0) # Remove decimals from Percentage column
Profession_table <- data.frame(Profession = c("Reseacher", "Psychologist", "Psychiatrist", "Facilitators", "Other"),
                         Count = c(research, psychologist, psychiatrist, tripsitter, otherprof))
Profession_table <- cbind(Profession_table, Percentage = Profession_percentage)
Profession_table$Count <- paste(Profession_table$Count, " (", Profession_percentage, ")", sep = "")
Profession_table$Percentage <- NULL # remove column "Percentage"
names(Profession_table)[2] <- "Count (%)" # rename column 2

# age per profession
age_research <- mean(Selfexp_ana$Age[Selfexp_ana$research_vec == 1])
age_psychologist <- mean(Selfexp_ana$Age[Selfexp_ana$psychologist_vec == 1])
age_psychiatrist <- mean(Selfexp_ana$Age[Selfexp_ana$psychiatrist_vec == 1])
age_tripsitter <- mean(Selfexp_ana$Age[Selfexp_ana$tripsitter_vec == 1])
age_otherprof <- mean(Selfexp_ana$Age[Selfexp_ana$otherprof_vec == 1])
Profession_age <- c(age_research, age_psychologist, age_psychiatrist, age_tripsitter, age_otherprof)
Profession_age <- round(Profession_age, digits = 0) # Remove decimals from Percentage column
Profession_table <- cbind(Profession_table, "Age(a)" = Profession_age)
Profession_table$`Age(a)` <- as.character(Profession_table$`Age(a)`)

# education per profession
education_research <- median(Selfexp_ana$Level.of.education[Selfexp_ana$research_vec == 1])
education_research <- round(education_research)
education_psychologist <- median(Selfexp_ana$Level.of.education[Selfexp_ana$psychologist_vec == 1])
education_psychiatrist <- median(Selfexp_ana$Level.of.education[Selfexp_ana$psychiatrist_vec == 1])
education_tripsitter <- median(Selfexp_ana$Level.of.education[Selfexp_ana$tripsitter_vec == 1])
education_otherprof <- median(Selfexp_ana$Level.of.education[Selfexp_ana$otherprof_vec == 1])
Profession_education <- c(education_research, education_psychologist, education_psychiatrist, education_tripsitter, education_otherprof)
education_labels <- c("Less than secondary school", "Secondary school", "MBO/associates degree", "Bachelor/undergraduate", "Master/graduate", "PhD/postgraduate") # Define the intensities
Profession_education <- factor(Profession_education, levels = 1:6, labels = education_labels, ordered = TRUE) # Replace the values in the Profession_education  with the corresponding labels
Profession_table <- cbind(Profession_table, "Education(b)" = Profession_education)

# Median frequency of use per profession
Selfexp_ana$Frequency[is.na(Selfexp_ana$Frequency)] <- 0 # set psychedelic-naive (i.e. NA) to 0
frequency_research <- median(Selfexp_ana$Frequency[Selfexp_ana$research_vec == 1])
frequency_psychologist <- median(Selfexp_ana$Frequency[Selfexp_ana$psychologist_vec == 1])
frequency_psychologist <- round(frequency_psychologist)
frequency_psychiatrist <- median(Selfexp_ana$Frequency[Selfexp_ana$psychiatrist_vec == 1])
frequency_tripsitter <- median(Selfexp_ana$Frequency[Selfexp_ana$tripsitter_vec == 1])
frequency_otherprof <- median(Selfexp_ana$Frequency[Selfexp_ana$otherprof_vec == 1])
Profession_frequency <- c(education_research, education_psychologist, education_psychiatrist, education_tripsitter, education_otherprof)
frequency_labels <- c("Never", "Only once", "Between 1 and 5 times", "Between 5 and 10 times", "Between 10 and 25 times", "Between 25 and 50 times", "More than 50 times") # Define the intensities
Profession_frequency <- factor(Profession_frequency, levels = 1:7, labels = frequency_labels, ordered = TRUE) # Replace the values in the Profession_education  with the corresponding labels
Profession_table <- cbind(Profession_table, "Frequency(c)" = Profession_frequency)

# Median last use per profession
Recency_prof <- Selfexp_ana[, c("Recency", "research_vec", "psychologist_vec", "psychiatrist_vec", "tripsitter_vec", "otherprof_vec")] # make new dataframe "Recency_prof"
Recency_prof <- Recency_prof[complete.cases(Recency_prof$Recency), ] # remove psychedelic-naive participants
recency_research <- median(Recency_prof$Recency[Recency_prof$research_vec == 1])
recency_psychologist <- median(Recency_prof$Recency[Recency_prof$psychologist_vec == 1])
recency_psychologist <- round(recency_psychologist)
recency_psychiatrist <- median(Recency_prof$Recency[Recency_prof$psychiatrist_vec == 1])
recency_tripsitter <- median(Recency_prof$Recency[Recency_prof$tripsitter_vec == 1])
recency_otherprof <- median(Recency_prof$Recency[Recency_prof$otherprof == 1])
Profession_recency <- c(recency_research, recency_psychologist, recency_psychiatrist, recency_tripsitter, recency_otherprof)
recency_labels <- c("Less than a week ago", "Between a week and a month ago", "Between 1 and 3 months ago", "Between 3 and 6 months ago", "Between 6 months and a year ago", "Between 1 and 2 years ago", "Between 2 and 5 years ago", "More than 5 years ago") # Define the recencies
Profession_recency <- factor(Profession_recency, levels = 1:8, labels = recency_labels, ordered = TRUE) # Replace the values in the Profession_education  with the corresponding labels
Profession_table <- cbind(Profession_table, "Recency(d)" = Profession_recency)

# make Profession_table nice
nice_table(Profession_table,
           title = c("Table 2. Professions", ""),
           note = c(
             "a) mean age per profession. b) median level of education per profession. c) median frequency of use per profession. d) median last use per profession"))

# difference in age between professions
tripsitter_age <- Selfexp_ana$Age[Selfexp_ana$tripsitter_vec == 1] # Subset the data for each group with 1 in the respective vectors.
psychologist_age <- Selfexp_ana$Age[Selfexp_ana$psychologist_vec == 1]
psychiatrist_age <- Selfexp_ana$Age[Selfexp_ana$psychiatrist_vec == 1]
research_age <- Selfexp_ana$Age[Selfexp_ana$research_vec == 1]
anova_prof.age <- aov(Age ~ group, data = data.frame(Age = c(tripsitter_age, psychologist_age, psychiatrist_age, research_age),
                                                   group = rep(c("tripsitter", "psychologist", "psychiatrist", "research"),
                                                               c(length(tripsitter_age), length(psychologist_age),
                                                                 length(psychiatrist_age), length(research_age))))) # Perform one-way ANOVA.
p_value_prof.age <- summary(anova_prof.age)[[1]]$"Pr(>F)"[1] # Check the p-value for significance.
f_statistic_prof.age <- summary(anova_prof.age)[[1]]$"F value"[1]# check the F-statistic 
degrees_of_freedom_prof.age <- summary(anova_prof.age)[[1]]$"Df"[1]# check the degrees of freedom
if (p_value_prof.age < 0.05) {
  cat("There is a significant difference in Age between the groups.\n")
} else {
  cat("There is no significant difference in Age between the groups.\n")
}
# if the ANOVA is significant do post-hoc tests
tukey_hsd_prof.age <- TukeyHSD(anova_prof.age)
print(tukey_hsd_prof.age)
plot(tukey_hsd_prof.age)

# motivation and profession
total_rows <- nrow(Selfexp_ana) # calculate the total rows in dataframe
fun_total <- sum(Selfexp_ana$fun_vec)
fun_perc <- (fun_total/total_rows)*100
curious_total <- sum(Selfexp_ana$curious_vec)
curious_perc <- (curious_total/total_rows)*100
growth_total <- sum(Selfexp_ana$growth_vec)
growth_perc <- (growth_total/total_rows)*100
experiment_total <- sum(Selfexp_ana$experiment_vec)
experiment_perc <- (experiment_total/total_rows)*100
empathize_total <- sum(Selfexp_ana$empathize_vec)
empathize_perc <- (empathize_total/total_rows)*100
spiritual_total <- sum(Selfexp_ana$spiritual_vec)
spiritual_perc <- (spiritual_total/total_rows)*100
mental_total <- sum(Selfexp_ana$mental_vec)
mental_perc <- (mental_total/total_rows)*100
nature_total <- sum(Selfexp_ana$nature_vec)
nature_perc <- (nature_total/total_rows)*100
others_total <- sum(Selfexp_ana$others_vec)
others_perc <- (others_total/total_rows)*100
escapism_total <- sum(Selfexp_ana$escapism_vec)
escapism_perc <- (escapism_total/total_rows)*100
othermot_total <- sum(Selfexp_ana$othermot_vec)
othermot_perc <- (othermot_total/total_rows)*100
fun_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$fun_vec == 1) # calculate how many researchers have selected the motivation "recreational/for fun"
fun_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$fun_vec == 1)
fun_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$fun_vec == 1)
fun_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$fun_vec == 1)
curious_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$curious_vec == 1)
curious_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$curious_vec == 1)
curious_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$curious_vec == 1)
curious_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$curious_vec == 1)
growth_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$growth_vec == 1)
growth_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$growth_vec == 1)
growth_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$growth_vec == 1)
growth_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$growth_vec == 1)
experiment_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$experiment_vec == 1)
experiment_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$experiment_vec == 1)
experiment_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$experiment_vec == 1)
experiment_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$experiment_vec == 1)
empathize_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$empathize_vec == 1)
empathize_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$empathize_vec == 1)
empathize_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$empathize_vec == 1)
empathize_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$empathize_vec == 1)
spiritual_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$spiritual_vec == 1)
spiritual_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$spiritual_vec == 1)
spiritual_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$spiritual_vec == 1)
spiritual_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$spiritual_vec == 1)
mental_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$mental_vec == 1)
mental_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$mental_vec == 1)
mental_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$mental_vec == 1)
mental_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$mental_vec == 1)
nature_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$nature_vec == 1)
nature_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$nature_vec == 1)
nature_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$nature_vec == 1)
nature_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$nature_vec == 1)
others_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$others_vec == 1)
others_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$others_vec == 1)
others_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$others_vec == 1)
others_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$others_vec == 1)
escapism_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$escapism_vec == 1)
escapism_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$escapism_vec == 1)
escapism_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$escapism_vec == 1)
escapism_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$escapism_vec == 1)
othermot_research <- sum(Selfexp_ana$research_vec == 1 & Selfexp_ana$othermot_vec == 1)
othermot_psychologist <- sum(Selfexp_ana$psychologist_vec == 1 & Selfexp_ana$othermot_vec == 1)
othermot_psychiatrist <- sum(Selfexp_ana$psychiatrist_vec == 1 & Selfexp_ana$othermot_vec == 1)
othermot_facilitator <- sum(Selfexp_ana$tripsitter_vec == 1 & Selfexp_ana$othermot_vec == 1)
Motivation_table <- data.frame(Motivation = c("Recreational/for fun", "Curiosity", "For personal growth", "To assess feasibility or safety of an experiment", "To empathize with patients/clients", "Spiritual/religious purpose", "To improve mental health", "To feel closer to nature", "To feel closer to others", "Escapism", "Other"),
                               Count = c(fun_total, curious_total, growth_total, experiment_total, empathize_total, spiritual_total, mental_total, nature_total, others_total, escapism_total, othermot_total),
                               Researcher = c(fun_research, curious_research, growth_research, experiment_research, empathize_research, spiritual_research, mental_research, nature_research, others_research, escapism_research, othermot_research),
                               Psychologists = c(fun_psychologist, curious_psychologist, growth_psychologist, experiment_psychologist, empathize_psychologist, spiritual_psychologist, mental_psychologist, nature_psychologist, others_psychologist, escapism_psychologist, othermot_psychologist),
                               Psychiatrist = c(fun_psychiatrist, curious_psychiatrist, growth_psychiatrist, experiment_psychiatrist, empathize_psychiatrist, spiritual_psychiatrist, mental_psychiatrist, nature_psychiatrist, others_psychiatrist, escapism_psychiatrist, othermot_psychiatrist),
                               Facilitators = c(fun_facilitator, curious_facilitator, growth_facilitator, experiment_facilitator, empathize_facilitator, spiritual_facilitator, mental_facilitator, nature_facilitator, others_facilitator, escapism_facilitator, othermot_facilitator))
motivation_percentage <- c(fun_perc, curious_perc, growth_perc, experiment_perc, empathize_perc, spiritual_perc, mental_perc, nature_perc, others_perc, escapism_perc, othermot_perc) # make a vector with percentages
motivation_percentage <- round(motivation_percentage, digits = 0) # Remove decimals from Percentage column
Motivation_table <- cbind(Motivation_table, Percentage = motivation_percentage)
Motivation_table$Count <- paste(Motivation_table$Count, " (", motivation_percentage, ")", sep = "")
Motivation_table$Percentage <- NULL # remove column "Percentage"
names(Motivation_table)[2] <- "Count (%)" # rename column 2

# make Motivation_table nice
nice_table(Motivation_table,
           title = c("Table 4. Motivation for psychoactive substance use", ""),
           note = c("Total count per motivation. Motivations per profession.")) # not yet happy with this wording

# APQ scores and gender
APQ_male <- Selfexp_ana$total.APQ[Selfexp_ana$Gender == 1]
APQ_female <- Selfexp_ana$total.APQ[Selfexp_ana$Gender == 2]
APQ_nb <- Selfexp_ana$total.APQ[Selfexp_ana$Gender == 3]
APQ_NA <- Selfexp_ana$total.APQ[Selfexp_ana$Gender == 4]
APQ_mean_male <- mean(APQ_male)
APQ_mean_female <- mean(APQ_female)
APQ_sd_male <- sd(APQ_male)
APQ_sd_female <- sd(APQ_female)
print(APQ_mean_male)
print(APQ_mean_female)
print(APQ_sd_male)
print(APQ_sd_female)
Gender_APQ <- t.test(APQ_male, APQ_female)
print(Gender_APQ)

# total APQ median + IQR + CI
APQ_median <- median(Selfexp_ana$total.APQ)
print(APQ_median)
q1_APQ <- quantile(Selfexp_ana$total.APQ, 0.25)
q3_APQ <- quantile(Selfexp_ana$total.APQ, 0.75)
iqr_APQ <- q3_APQ - q1_APQ
ci_APQ <- t.test(Selfexp_ana$total.APQ)$conf.int
cat("APQ_total - Md = ", APQ_median, "(IQR =", q1_APQ, "-", q3_APQ, ", 95% CI =", ci_APQ[1], "-", ci_APQ[2], ")\n")

# female APQ median + IQR + CI
APQ_median_female <- median(APQ_female)
print(APQ_median_female)
q1_APQ_female <- quantile(APQ_female, 0.25)
q3_APQ_female <- quantile(APQ_female, 0.75)
iqr_APQ_female <- q3_APQ_female - q1_APQ_female
ci_APQ_female <- t.test(APQ_female)$conf.int
cat("APQ_female - Md = ", APQ_median_female, "(IQR =", q1_APQ_female, "-", q3_APQ_female, ", 95% CI =", ci_APQ_female[1], "-", ci_APQ_female[2], ")\n")

# male APQ median + IQR + CI
APQ_median_male <- median(APQ_male)
print(APQ_median_male)
q1_APQ_male <- quantile(APQ_male, 0.25)
q3_APQ_male <- quantile(APQ_male, 0.75)
iqr_APQ_male <- q3_APQ_male - q1_APQ_male
ci_APQ_male <- t.test(APQ_male)$conf.int
cat("APQ_male - Md = ", APQ_median_male, "(IQR =", q1_APQ_male, "-", q3_APQ_male, ", 95% CI =", ci_APQ_male[1], "-", ci_APQ_male[2], ")\n")

# AOSE scores between genders
AOSE_male <- Selfexp_ana$total.AOSE[Selfexp_ana$Gender == 1]
AOSE_female <- Selfexp_ana$total.AOSE[Selfexp_ana$Gender == 2]
AOSE_nb <- Selfexp_ana$total.AOSE[Selfexp_ana$Gender == 3]
AOSE_NA <- Selfexp_ana$total.AOSE[Selfexp_ana$Gender == 4]
AOSE_mean_male <- mean(AOSE_male)
AOSE_mean_female <- mean(AOSE_female)
AOSE_sd_male <- sd(AOSE_male)
AOSE_sd_female <- sd(AOSE_female)
print(AOSE_mean_male)
print(AOSE_mean_female)
print(AOSE_sd_male)
print(AOSE_sd_female)
Gender_AOSE <- t.test(AOSE_male, AOSE_female)
print(Gender_AOSE)

# total AOSE median + IQR + CI
AOSE_median <- median(Selfexp_ana$total.AOSE)
print(AOSE_median)
q1_AOSE <- quantile(Selfexp_ana$total.AOSE, 0.25)
q3_AOSE <- quantile(Selfexp_ana$total.AOSE, 0.75)
iqr_AOSE <- q3_AOSE - q1_AOSE
ci_AOSE <- t.test(Selfexp_ana$total.AOSE)$conf.int
cat("AOSE_total - Md = ", AOSE_median, "(IQR =", q1_AOSE, "-", q3_AOSE, ", 95% CI =", ci_AOSE[1], "-", ci_AOSE[2], ")\n")

# female AOSE median + IQR + CI
AOSE_median_female <- median(AOSE_female)
print(AOSE_median_female)
q1_AOSE_female <- quantile(AOSE_female, 0.25)
q3_AOSE_female <- quantile(AOSE_female, 0.75)
iqr_AOSE_female <- q3_AOSE_female - q1_AOSE_female
ci_AOSE_female <- t.test(AOSE_female)$conf.int
cat("AOSE_female - Md = ", AOSE_median_female, "(IQR =", q1_AOSE_female, "-", q3_AOSE_female, ", 95% CI =", ci_AOSE_female[1], "-", ci_AOSE_female[2], ")\n")

# male AOSE median + IQR + CI
AOSE_median_male <- median(AOSE_male)
print(AOSE_median_male)
q1_AOSE_male <- quantile(AOSE_male, 0.25)
q3_AOSE_male <- quantile(AOSE_male, 0.75)
iqr_AOSE_male <- q3_AOSE_male - q1_AOSE_male
ci_AOSE_male <- t.test(AOSE_male)$conf.int
cat("AOSE_male - Md = ", AOSE_median_male, "(IQR =", q1_AOSE_male, "-", q3_AOSE_male, ", 95% CI =", ci_AOSE_male[1], "-", ci_AOSE_male[2], ")\n")

# APQ and age
Age_APQ <- lm(total.APQ ~ Age, data = Selfexp_ana)
summary(Age_APQ)

# AOSE and age
Age_AOSE <- lm(total.AOSE ~ Age, data = Selfexp_ana)
summary(Age_AOSE)

# APQ subscores
APQ_legal <- Selfexp_ana$APQ.r2_5 + Selfexp_ana$APQ.r4_4.R + Selfexp_ana$APQ.r3_4 + Selfexp_ana$APQ.r2_2 + Selfexp_ana$APQ.r2_4.R
APQ_effects <- Selfexp_ana$APQ.r4_1 + Selfexp_ana$APQ.r4_5 + Selfexp_ana$APQ.r4_2.R + Selfexp_ana$APQ.r2_3.R + Selfexp_ana$APQ.r1_4
APQ_risks <- Selfexp_ana$APQ.r1_5 + Selfexp_ana$APQ.r3_1.R + Selfexp_ana$APQ.r2_1 + Selfexp_ana$APQ.r3_5.R + Selfexp_ana$APQ.r3_2
APQ_openness <- Selfexp_ana$APQ.r1_2 + Selfexp_ana$APQ.r3_3.R + Selfexp_ana$APQ.r4_3 + Selfexp_ana$APQ.r1_1 + Selfexp_ana$APQ.r1_3.R
Selfexp_ana$APQ_legal <- APQ_legal
Selfexp_ana$APQ_effects <- APQ_effects
Selfexp_ana$APQ_risks <- APQ_risks
Selfexp_ana$APQ_openness <- APQ_openness
# median, IQR and 95% CI
median_APQ_legal <- median(APQ_legal)
q1_APQ_legal <- quantile(APQ_legal, 0.25)
q3_APQ_legal <- quantile(APQ_legal, 0.75)
iqr_APQ_legal <- q3_APQ_legal - q1_APQ_legal
ci_APQ_legal <- t.test(APQ_legal)$conf.int

median_APQ_effects <- median(APQ_effects)
q1_APQ_effects <- quantile(APQ_effects, 0.25)
q3_APQ_effects <- quantile(APQ_effects, 0.75)
iqr_APQ_effects <- q3_APQ_effects - q1_APQ_effects
ci_APQ_effects <- t.test(APQ_effects)$conf.int

median_APQ_risks <- median(APQ_risks)
q1_APQ_risks <- quantile(APQ_risks, 0.25)
q3_APQ_risks <- quantile(APQ_risks, 0.75)
iqr_APQ_risks <- q3_APQ_risks - q1_APQ_risks
ci_APQ_risks <- t.test(APQ_risks)$conf.int

median_APQ_openness <- median(APQ_openness)
q1_APQ_openness <- quantile(APQ_openness, 0.25)
q3_APQ_openness <- quantile(APQ_openness, 0.75)
iqr_APQ_openness <- q3_APQ_openness - q1_APQ_openness
ci_APQ_openness <- t.test(APQ_openness)$conf.int

cat("APQ_legal - Md = ", median_APQ_legal, "(IQR =", q1_APQ_legal, "-", q3_APQ_legal, ", 95% CI =", ci_APQ_legal[1], "-", ci_APQ_legal[2], ")\n")
cat("APQ_effects - Md = ", median_APQ_effects, "(IQR =", q1_APQ_effects, "-", q3_APQ_effects, ", 95% CI =", ci_APQ_effects[1], "-", ci_APQ_effects[2], ")\n")
cat("APQ_risks - Md = ", median_APQ_risks, "(IQR =", q1_APQ_risks, "-", q3_APQ_risks, ", 95% CI =", ci_APQ_risks[1], "-", ci_APQ_risks[2], ")\n")
cat("APQ_openness - Md = ", median_APQ_openness, "(IQR =", q1_APQ_openness, "-", q3_APQ_openness, ", 95% CI =", ci_APQ_openness[1], "-", ci_APQ_openness[2], ")\n")

# difference in APQ subscores
t_test_legal_effects <- t.test(APQ_legal, APQ_effects, paired = TRUE)
print(t_test_legal_effects)
t_test_legal_risks <- t.test(APQ_legal, APQ_risks, paired = TRUE)
print(t_test_legal_risks)
t_test_legal_openness <- t.test(APQ_legal, APQ_openness, paired = TRUE)
print(t_test_legal_openness)
t_test_effects_risks <- t.test(APQ_effects, APQ_risks, paired = TRUE)
print(t_test_effects_risks)
t_test_effects_openness <- t.test(APQ_effects, APQ_openness, paired = TRUE)
print(t_test_effects_openness)
t_test_risks_openness <- t.test(APQ_risks, APQ_openness, paired = TRUE)
print(t_test_risks_openness)
Bonferroni <- 0.05 / 6
Bonferroni_legal_effects <- t_test_legal_effects$p.value * 6
Bonferroni_legal_risks <- t_test_legal_risks$p.value * 6
Bonferroni_legal_openness <- t_test_legal_openness$p.value * 6
Bonferroni_effects_risks <- t_test_effects_risks$p.value * 6
Bonferroni_effects_openness <- t_test_effects_openness$p.value * 6
Bonferroni_risks_openness <- t_test_risks_openness$p.value * 6
if (t_test_legal_effects$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_legal and APQ_effects, p = ", Bonferroni_legal_effects, "\n")
} else {
  cat("There is no significant difference between APQ_legal and APQ_effects\n")
}
if (t_test_legal_risks$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_legal and APQ_risks", Bonferroni_legal_risks, "\n")
} else {
  cat("There is no significant difference between APQ_legal and APQ_risks.\n")
}
if (t_test_legal_openness$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_legal and APQ_openness", Bonferroni_legal_openness, "\n")
} else {
  cat("There is no significant difference between APQ_legal and APQ_openness.\n")
}
if (t_test_effects_risks$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_effects and APQ_risks", Bonferroni_effects_risks, "\n")
} else {
  cat("There is no significant difference between APQ_effects and APQ_risks.\n")
}
if (t_test_effects_openness$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_effects and APQ_openness", Bonferroni_effects_openness, "\n")
} else {
  cat("There is no significant difference between APQ_effects and APQ_openness.\n")
}
if (t_test_risks_openness$p.value < Bonferroni) {
  cat("There is a significant difference between APQ_risks and APQ_openness", Bonferroni_risks_openness, "\n")
} else {
  cat("There is no significant difference between APQ_risks and APQ_openness.\n")
}

# APQ subscores correlation matrix (perhaps irrelevant)
install.packages("corrr")
library(corrr)
APQ_subscores <- data.frame(
  legal = APQ_legal,
  effects = APQ_effects,
  risks = APQ_risks,
  openness = APQ_openness
)
APQ_cor_matrix <- correlate(APQ_subscores, method = "pearson")
print(APQ_cor_matrix)
install.packages("psych")
library(psych)
cor_matrix <- cor(subscores) # Calculate the correlation matrix
corPlot(cor_matrix) # Create a heat plot of the correlation matrix using corPlot

# APQ per profession
APQ_research <- Selfexp_ana$total.APQ[Selfexp_ana$research_vec == 1]
APQ_psychologist <- Selfexp_ana$total.APQ[Selfexp_ana$psychologist_vec == 1]
APQ_psychiatrist <- Selfexp_ana$total.APQ[Selfexp_ana$psychiatrist_vec == 1]
APQ_tripsitter <- Selfexp_ana$total.APQ[Selfexp_ana$tripsitter_vec == 1]

# research APQ median + IQR + CI
APQ_median_research <- median(APQ_research)
print(APQ_median_research)
q1_APQ_research <- quantile(APQ_research, 0.25)
q3_APQ_research <- quantile(APQ_research, 0.75)
iqr_APQ_research <- q3_APQ_research - q1_APQ_research
ci_APQ_research <- t.test(APQ_research)$conf.int
cat("APQ_research - Md =", APQ_median_research, "(IQR =", q1_APQ_research, "-", q3_APQ_research, ", 95% CI =", ci_APQ_research[1], "-", ci_APQ_research[2], ")\n")

# psychologist APQ median + IQR + CI
APQ_median_psychologist <- median(APQ_psychologist)
print(APQ_median_psychologist)
q1_APQ_psychologist <- quantile(APQ_psychologist, 0.25)
q3_APQ_psychologist <- quantile(APQ_psychologist, 0.75)
iqr_APQ_psychologist <- q3_APQ_psychologist - q1_APQ_psychologist
ci_APQ_psychologist <- t.test(APQ_psychologist)$conf.int
cat("APQ_psychologist - Md =", APQ_median_psychologist, "(IQR =", q1_APQ_psychologist, "-", q3_APQ_psychologist, ", 95% CI =", ci_APQ_psychologist[1], "-", ci_APQ_psychologist[2], ")\n")

# psychiatrist APQ median + IQR + CI
APQ_median_psychiatrist <- median(APQ_psychiatrist)
print(APQ_median_psychiatrist)
q1_APQ_psychiatrist <- quantile(APQ_psychiatrist, 0.25)
q3_APQ_psychiatrist <- quantile(APQ_psychiatrist, 0.75)
iqr_APQ_psychiatrist <- q3_APQ_psychiatrist - q1_APQ_psychiatrist
ci_APQ_psychiatrist <- t.test(APQ_psychiatrist)$conf.int
cat("APQ_psychiatrist - Md =", APQ_median_psychiatrist, "(IQR =", q1_APQ_psychiatrist, "-", q3_APQ_psychiatrist, ", 95% CI =", ci_APQ_psychiatrist[1], "-", ci_APQ_psychiatrist[2], ")\n")

# tripsitter APQ median + IQR + CI
APQ_median_tripsitter <- median(APQ_tripsitter)
print(APQ_median_tripsitter)
q1_APQ_tripsitter <- quantile(APQ_tripsitter, 0.25)
q3_APQ_tripsitter <- quantile(APQ_tripsitter, 0.75)
iqr_APQ_tripsitter <- q3_APQ_tripsitter - q1_APQ_tripsitter
ci_APQ_tripsitter <- t.test(APQ_tripsitter)$conf.int
cat("APQ_tripsitter - Md =", APQ_median_tripsitter, "(IQR =", q1_APQ_tripsitter, "-", q3_APQ_tripsitter, ", 95% CI =", ci_APQ_tripsitter[1], "-", ci_APQ_tripsitter[2], ")\n")

# AOSE per profession
AOSE_research <- Selfexp_ana$total.AOSE[Selfexp_ana$research_vec == 1]
AOSE_psychologist <- Selfexp_ana$total.AOSE[Selfexp_ana$psychologist_vec == 1]
AOSE_psychiatrist <- Selfexp_ana$total.AOSE[Selfexp_ana$psychiatrist_vec == 1]
AOSE_tripsitter <- Selfexp_ana$total.AOSE[Selfexp_ana$tripsitter_vec == 1]

# research AOSE median + IQR + CI
AOSE_median_research <- median(AOSE_research)
print(AOSE_median_research)
q1_AOSE_research <- quantile(AOSE_research, 0.25)
q3_AOSE_research <- quantile(AOSE_research, 0.75)
iqr_AOSE_research <- q3_AOSE_research - q1_AOSE_research
ci_AOSE_research <- t.test(AOSE_research)$conf.int
cat("AOSE_research - Md =", AOSE_median_research, "(IQR =", q1_AOSE_research, "-", q3_AOSE_research, ", 95% CI =", ci_AOSE_research[1], "-", ci_AOSE_research[2], ")\n")

# psychologist AOSE median + IQR + CI
AOSE_median_psychologist <- median(AOSE_psychologist)
print(AOSE_median_psychologist)
q1_AOSE_psychologist <- quantile(AOSE_psychologist, 0.25)
q3_AOSE_psychologist <- quantile(AOSE_psychologist, 0.75)
iqr_AOSE_psychologist <- q3_AOSE_psychologist - q1_AOSE_psychologist
ci_AOSE_psychologist <- t.test(AOSE_psychologist)$conf.int
cat("AOSE_psychologist - Md =", AOSE_median_psychologist, "(IQR =", q1_AOSE_psychologist, "-", q3_AOSE_psychologist, ", 95% CI =", ci_AOSE_psychologist[1], "-", ci_AOSE_psychologist[2], ")\n")

# psychiatrist AOSE median + IQR + CI
AOSE_median_psychiatrist <- median(AOSE_psychiatrist)
print(AOSE_median_psychiatrist)
q1_AOSE_psychiatrist <- quantile(AOSE_psychiatrist, 0.25)
q3_AOSE_psychiatrist <- quantile(AOSE_psychiatrist, 0.75)
iqr_AOSE_psychiatrist <- q3_AOSE_psychiatrist - q1_AOSE_psychiatrist
ci_AOSE_psychiatrist <- t.test(AOSE_psychiatrist)$conf.int
cat("AOSE_psychiatrist - Md =", AOSE_median_psychiatrist, "(IQR =", q1_AOSE_psychiatrist, "-", q3_AOSE_psychiatrist, ", 95% CI =", ci_AOSE_psychiatrist[1], "-", ci_AOSE_psychiatrist[2], ")\n")

# tripsitter AOSE median + IQR + CI
AOSE_median_tripsitter <- median(AOSE_tripsitter)
print(AOSE_median_tripsitter)
q1_AOSE_tripsitter <- quantile(AOSE_tripsitter, 0.25)
q3_AOSE_tripsitter <- quantile(AOSE_tripsitter, 0.75)
iqr_AOSE_tripsitter <- q3_AOSE_tripsitter - q1_AOSE_tripsitter
ci_AOSE_tripsitter <- t.test(AOSE_tripsitter)$conf.int
cat("AOSE_tripsitter - Md =", AOSE_median_tripsitter, "(IQR =", q1_AOSE_tripsitter, "-", q3_AOSE_tripsitter, ", 95% CI =", ci_AOSE_tripsitter[1], "-", ci_AOSE_tripsitter[2], ")\n")

# APQ subscores per profession
research_APQ_legal <- Selfexp_ana$APQ_legal[Selfexp_ana$research_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_APQ_legal <- Selfexp_ana$APQ_legal[Selfexp_ana$psychologist_vec == 1]
psychiatrist_APQ_legal <- Selfexp_ana$APQ_legal[Selfexp_ana$psychiatrist_vec == 1]
tripsitter_APQ_legal <- Selfexp_ana$APQ_legal[Selfexp_ana$tripsitter_vec == 1]
research_APQ_effects <- Selfexp_ana$APQ_effects[Selfexp_ana$research_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_APQ_effects <- Selfexp_ana$APQ_effects[Selfexp_ana$psychologist_vec == 1]
psychiatrist_APQ_effects <- Selfexp_ana$APQ_effects[Selfexp_ana$psychiatrist_vec == 1]
tripsitter_APQ_effects <- Selfexp_ana$APQ_effects[Selfexp_ana$tripsitter_vec == 1]
research_APQ_risks <- Selfexp_ana$APQ_risks[Selfexp_ana$research_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_APQ_risks <- Selfexp_ana$APQ_risks[Selfexp_ana$psychologist_vec == 1]
psychiatrist_APQ_risks <- Selfexp_ana$APQ_risks[Selfexp_ana$psychiatrist_vec == 1]
tripsitter_APQ_risks <- Selfexp_ana$APQ_risks[Selfexp_ana$tripsitter_vec == 1]
research_APQ_openness <- Selfexp_ana$APQ_openness[Selfexp_ana$research_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_APQ_openness <- Selfexp_ana$APQ_openness[Selfexp_ana$psychologist_vec == 1]
psychiatrist_APQ_openness <- Selfexp_ana$APQ_openness[Selfexp_ana$psychiatrist_vec == 1]
tripsitter_APQ_openness <- Selfexp_ana$APQ_openness[Selfexp_ana$tripsitter_vec == 1]

# CFA
install.packages("lavaan")  # Install the package
library(lavaan)             # Load the package
CFA_model <- ' 
  Factor1 =~ APQ.r2_5 + APQ.r4_4.R + APQ.r3_4 + APQ.r2_2 + APQ.r2_4.R
  Factor2 =~ APQ.r4_1 + APQ.r4_5 + APQ.r4_2.R + APQ.r2_3.R + APQ.r1_4
  Factor3 =~ APQ.r1_5 + APQ.r3_1.R + APQ.r2_1 + APQ.r3_5.R + APQ.r3_2
  Factor4 =~ APQ.r1_2 + APQ.r3_3.R + APQ.r4_3 + APQ.r1_1 + APQ.r1_3.R
'
fit <- cfa(CFA_model, data = Selfexp_ana)
lavInspect(fit, "cov.lv")
summary(fit)
fit_indices <- fitMeasures(fit, c("RMSEA", "SRMR", "CFI", "chisq", "df"))
print(fit_indices)
CFA_fit <- cfa(CFA_model, data = Selfexp_ana)
aic <- AIC(CFA_fit) # Obtain AIC values
bic <- BIC(CFA_fit) # Obtain BIC values
cat("AIC:", aic, "\n") # Print the AIC values
cat("BIC:", bic, "\n") # Print the BIC values

install.packages("semPlot")
library(semPlot)
semPaths(fit, "std", layout = "tree")


# EFA
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
selected_AOSEcolumns <- Selfexp_ana[c("Self.exp.attitudes_1", "Self.exp.attitudes_2.R", "Self.exp.attitudes_3", "Self.exp.attitudes_4.R",
                          "Self.exp.attitudes_5", "Self.exp.attitudes_6.R", "Self.exp.attitudes_7", "Self.exp.attitudes_8.R",
                          "Self.exp.attitudes_9")]
efa_result <- fa(selected_AOSEcolumns, nfactors = 3, rotate = "promax")
print(efa_result$loadings) # Print the factor loadings
print(efa_result$communality) # Print the commonalities
print(efa_result$scores) # Print the factor scores
fa.diagram(efa_result, main = selected_AOSEcolumns)
eigenvalues_EFA <- efa_result$values[1:3]
total_variance_EFA <- sum(eigenvalues_EFA) # Calculate total variance explained
variance_percentages_EFA <- (eigenvalues_EFA / total_variance_EFA) * 100 # Calculate the percentage of variance explained by each factor
print(variance_percentages_EFA)

# H1
Psych_exp.perc <- sum(Selfexp_ana$Psychedelic.experien == 2 & (Selfexp_ana$Psychedelic.experien == 1 | Selfexp_ana$Psychedelic.experien == 2), na.rm = TRUE) / sum(!is.na(Selfexp_ana$Psychedelic.experien) & (Selfexp_ana$Psychedelic.experien == 1 | Selfexp_ana$Psychedelic.experien == 2)) * 100 # Calculate the percentage of 2s in column 'psychedelic.experien'
Psych_exp <- Selfexp_ana[Selfexp_ana$Psychedelic.experien == 2, ] # select from dataframe Selfexp_ana those participants that responded that they have done psychedelics at least once 
Psych_nai <- Selfexp_ana[Selfexp_ana$Psychedelic.experien == 1, ] # select from dataframe Selfexp_ana those participants that responded that they have not done psychedelics 
# convert character to numeric (Psych_exp$Motivation.to.use. <- as.numeric(Psych_exp$Motivation.to.use.))
# calculate the % of Psych_exp that selected to assess feasibility or safety of an experiment or to empathize with patients/clients on Motivation.to.use

# pie chart prevalence of self-use and self-experimentation
Prevalence_labels <- c("Psychedelic Naive", "Self-experimentation", "Self-use")
Prevalence_values <- c(3.09, 36.08, 60.82) # filled in manually
library(ggplot2)
Prevalence_data <- data.frame(Category = Prevalence_labels, Value = Prevalence_values)
total <- sum(Prevalence_values)
Prevalence_data$Percentage <- (Prevalence_data$Value / total) * 100
desaturated_blue <- hcl(h = 240, c = 70, l = 60)  # Adjust the saturation (c) value
pie_colors <- c("gray", "orange", desaturated_blue)
ggplot(Prevalence_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size=7) +
  labs(title = "Psychedelic use and self-experimentation") +
  scale_fill_manual(values = pie_colors) +  # Set the colors
  theme_void()  # Remove unnecessary elements
  theme(legend.key.size = unit(1.5, "cm"))

# 1.1-4 prep
Selfexp_ana$Frequency[is.na(Selfexp_ana$Frequency)] <- 0 # set psychedelic-naive (i.e. NA) to 0
contingency_table_tripsitter <- table(Selfexp_ana$Frequency, Selfexp_ana$tripsitter_vec) # Create a contingency table Frequency/tripsitte
contingency_table_research <- table(Selfexp_ana$Frequency, Selfexp_ana$research_vec) # Create a contingency table Frequency/researcher
contingency_table_psychologist <- table(Selfexp_ana$Frequency, Selfexp_ana$psychologist_vec) # Create a contingency table Frequency/psychologist-psychiatrist
contingency_table_psychiatrist <- table(Selfexp_ana$Frequency, Selfexp_ana$psychiatrist_vec) # Create a contingency table Frequency/psych-psychiatrist
contingency_table_psych <- table(Selfexp_ana$Frequency, Selfexp_ana$psych_vec) # Create a contingency table Frequency/psych-psychiatrist

# 1.1a
contingency_table_1.1a <- cbind(contingency_table_tripsitter, contingency_table_psych) # combine contingency_table_tripsitter and contingency_table_psych
contingency_table_1.1a <- contingency_table_1.1a[, -c(1, 3)] # remove column 1 and 3 (those where tripsitter_vec/psychologist_vec = 0)
colnames(contingency_table_1.1a) <- c("Facilitator", "Psychologist/Psychiatrist")
rownames(contingency_table_1.1a) <- c("0 times", "1 time", "1-5 times", "5-10 times", "10-25 times", "25-50 times", ">50 times")
Chisq_1.1a <- chisq.test(contingency_table_1.1a) # perform chi-squared test
print(Chisq_1.1a$statistic) # Print test statistics
print(Chisq_1.1a$expected) # Print expected frequencies
print(Chisq_1.1a$p.value) # Print p-value
print(Chisq_1.1a$parameter) # Get the degrees of freedom (df)
print(sum(Chisq_1.1a$observed)) # Get the total count (N)
fisher_1.1a <- fisher.test(contingency_table_1.1a) # perform Fisher's Exact 
print(fisher_1.1a$p.value) # Print p-value

# Combine categories: some cells have very low expected frequencies, merging categories or collapsing cells can increase the expected counts.
contingency_table_1.1a[1, ] <- colSums(contingency_table_1.1a[1:4, ]) 
contingency_table_1.1a[2, ] <- colSums(contingency_table_1.1a[5:7, ])
contingency_table_1.1a <- contingency_table_1.1a[c(1, 2), ]
rownames(contingency_table_1.1a) <- c("0-10 times", ">5 times")
Chisq_1.1a <- chisq.test(contingency_table_1.1a)
print(Chisq_1.1a$statistic) 
print(Chisq_1.1a$expected) 
print(Chisq_1.1a$p.value)

# 1.1b
contingency_table_1.1b <- cbind(contingency_table_tripsitter, contingency_table_research) # combine contingency_table_tripsitter and contingency_table_research
contingency_table_1.1b <- contingency_table_1.1b[, -c(1, 3)] # remove column 1 and 3 (those where tripsitter_vec/research_vec = 0)
colnames(contingency_table_1.1b) <- c("Facilitator", "Researcher")
rownames(contingency_table_1.1b) <- c("0 times", "1 time", "1-5 times", "5-10 times", "10-25 times", "25-50 times", ">50 times")
Chisq_1.1b <- chisq.test(contingency_table_1.1b) # perform chi-squared test
print(Chisq_1.1b$statistic) # Print test statistics
print(Chisq_1.1b$expected) # Print expected frequencies
print(Chisq_1.1b$p.value) # Print p-value
print(Chisq_1.1b$parameter) # Get the degrees of freedom (df)
print(sum(Chisq_1.1b$observed)) # Get the total count (N)
fisher_1.1b <- fisher.test(contingency_table_1.1b) # perform Fisher's Exact
print(fisher_1.1b$p.value) # Print p-value

# Combine categories: some cells have very low expected frequencies, merging categories or collapsing cells can increase the expected counts.
contingency_table_1.1b[1, ] <- colSums(contingency_table_1.1b[1:4, ]) 
contingency_table_1.1b[2, ] <- colSums(contingency_table_1.1b[5:7, ])
contingency_table_1.1b <- contingency_table_1.1b[c(1, 2), ]
rownames(contingency_table_1.1b) <- c("0-10 times", ">5 times")
Chisq_1.1b <- chisq.test(contingency_table_1.1b)
print(Chisq_1.1b$statistic) 
print(Chisq_1.1b$expected) 
print(Chisq_1.1b$p.value)

# 1.2
contingency_table_1.2 <- cbind(contingency_table_research, contingency_table_psych) # combine contingency_table_research and contingency_table_psych 
contingency_table_1.2 <- contingency_table_1.2[, -c(1, 3)] # remove column 1 and 3 (those where research_vec/psych_vec = 0)
colnames(contingency_table_1.2) <- c("Researcher", "Psychologist/Psychiatrist")
rownames(contingency_table_1.2) <- c("0 times", "1 time", "1-5 times", "5-10 times", "10-25 times", "25-50 times", ">50 times")
Chisq_1.2 <- chisq.test(contingency_table_1.2) # perform chi-squared test
print(Chisq_1.2$statistic) # Print test statistics
print(Chisq_1.2$expected) # Print expected frequencies
print(Chisq_1.2$p.value) # Print p-value
print(Chisq_1.2$parameter) # Get the degrees of freedom (df)
print(sum(Chisq_1.2$observed)) # Get the total count (N)
fisher_1.2 <- fisher.test(contingency_table_1.2) # perform Fisher's Exact 
print(fisher_1.2$p.value) # Print p-value

# Combine categories: some cells have very low expected frequencies, merging categories or collapsing cells can increase the expected counts.
contingency_table_1.2[1, ] <- colSums(contingency_table_1.2[1:4, ]) 
contingency_table_1.2[2, ] <- colSums(contingency_table_1.2[5:7, ])
contingency_table_1.2 <- contingency_table_1.2[c(1, 2), ]
rownames(contingency_table_1.2) <- c("0-10 times", ">5 times")
Chisq_1.2 <- chisq.test(contingency_table_1.2)
print(Chisq_1.2$statistic) 
print(Chisq_1.2$expected) 
print(Chisq_1.2$p.value)

# 1.3
contingency_table_1.3 <- cbind(contingency_table_psychologist, contingency_table_psychiatrist) # combine contingency_table_psychologist and contingency_table_psychiatrist
contingency_table_1.3 <- contingency_table_1.3[, -c(1, 3)] # remove column 1 and 3 
colnames(contingency_table_1.3) <- c("Psyhologist", "Psychiatrist")
rownames(contingency_table_1.3) <- c("0 times", "1 time", "1-5 times", "5-10 times", "10-25 times", "25-50 times", ">50 times")
Chisq_1.3 <- chisq.test(contingency_table_1.3) # perform chi-squared test
print(Chisq_1.3$statistic) # Print test statistics
print(Chisq_1.3$expected) # Print expected frequencies
print(Chisq_1.3$p.value) # Print p-value
print(Chisq_1.3$parameter) # Get the degrees of freedom (df)
print(sum(Chisq_1.3$observed)) # Get the total count (N)
fisher_1.3 <- fisher.test(contingency_table_1.3) # perform Fisher's Exact 
print(fisher_1.3$p.value) # Print p-value

# Combine categories: some cells have very low expected frequencies, merging categories or collapsing cells can increase the expected counts.
contingency_table_1.3[1, ] <- colSums(contingency_table_1.3[1:4, ]) 
contingency_table_1.3[2, ] <- colSums(contingency_table_1.3[5:7, ])
contingency_table_1.3 <- contingency_table_1.3[c(1, 2), ]
rownames(contingency_table_1.3) <- c("0-10 times", ">5 times")
Chisq_1.3 <- chisq.test(contingency_table_1.3)
print(Chisq_1.3$statistic) 
print(Chisq_1.3$expected) 
print(Chisq_1.3$p.value)

# 1.4
contingency_table_1.4 <- table(Selfexp_ana$Frequency, Selfexp_ana$Gender) # Create a contingency table Frequency/Gender
contingency_table_1.4 <- contingency_table_1.4[, c(1, 2)] # remove Non-binary (because of low frequency)
colnames(contingency_table_1.4) <- c("Male", "Female")
rownames(contingency_table_1.4) <- c("0 times", "1 time", "1-5 times", "5-10 times", "10-25 times", "25-50 times", ">50 times")
Chisq_1.4 <- chisq.test(contingency_table_1.4)
print(Chisq_1.4$statistic) 
print(Chisq_1.4$expected) 
print(Chisq_1.4$p.value)
print(Chisq_1.4$parameter) # Get the degrees of freedom (df)
print(sum(Chisq_1.4$observed)) # Get the total count (N)
fisher_1.1b <- fisher.test(contingency_table_1.1b) # perform Fisher's Exact
print(fisher_1.1b$p.value) # Print p-value

# Combine categories: some cells have very low expected frequencies, merging categories or collapsing cells can increase the expected counts.
contingency_table_1.4[1, ] <- colSums(contingency_table_1.4[1:4, ]) 
contingency_table_1.4[2, ] <- colSums(contingency_table_1.4[5:7, ])
contingency_table_1.4 <- contingency_table_1.4[c(1, 2), ]
rownames(contingency_table_1.4) <- c("0-10 times", ">5 times")
Chisq_1.4 <- chisq.test(contingency_table_1.4)
print(Chisq_1.4$statistic) 
print(Chisq_1.4$expected) 
print(Chisq_1.4$p.value)

# H2
# correlation analysis (Pearson’s r) between total APQ scores and total AOSE scores
cor.APQ.AOSE <- cor.test(Selfexp_ana$total.APQ, Selfexp_ana$total.AOSE)
summary(cor.APQ.AOSE)
print(cor.APQ.AOSE$estimate)  # Pearson's correlation coefficient
print(cor.APQ.AOSE$p.value)  # Significance (p-value)

# 3.1
APQ_3.1 <- t.test(Psych_exp$total.APQ, Psych_nai$total.APQ) # not enough psychedelic-naive participants
print(APQ_3.1)
mean_APQ_exp <- mean(Psych_exp$total.APQ)
sd_APQ_exp <- sd(Psych_exp$total.APQ)
mean_APQ_nai <- mean(Psych_nai$total.APQ)
sd_APQ_nai <- sd(Psych_nai$total.APQ)
AOSE_3.1 <- t.test(Psych_exp$total.AOSE, Psych_nai$total.AOSE) # not enough psychedelic-naive participants
print(AOSE_3.1)
mean_AOSE_exp <- mean(Psych_exp$total.AOSE)
sd_AOSE_exp <- sd(Psych_exp$total.AOSE)
mean_AOSE_nai <- mean(Psych_nai$total.AOSE)
sd_AOSE_nai <- sd(Psych_nai$total.AOSE)

# experienced APQ median + IQR + CI
APQ_median_exp <- median(Psych_exp$total.APQ)
print(APQ_median_exp)
q1_APQ_exp <- quantile(Psych_exp$total.APQ, 0.25)
q3_APQ_exp <- quantile(Psych_exp$total.APQ, 0.75)
iqr_APQ_exp <- q3_APQ_exp - q1_APQ_exp
ci_APQ_exp <- t.test(Psych_exp$total.APQ)$conf.int
cat("APQ_exp - Md = ", APQ_median_exp, "(IQR =", q1_APQ_exp, "-", q3_APQ_exp, ", 95% CI =", ci_APQ_exp[1], "-", ci_APQ_exp[2], ")\n")

# naive APQ median + IQR + CI
APQ_median_nai <- median(Psych_nai$total.APQ)
print(APQ_median_nai)
q1_APQ_nai <- quantile(Psych_nai$total.APQ, 0.25)
q3_APQ_nai <- quantile(Psych_nai$total.APQ, 0.75)
iqr_APQ_nai <- q3_APQ_nai - q1_APQ_nai
ci_APQ_nai <- t.test(Psych_nai$total.APQ)$conf.int
cat("APQ_nai - Md = ", APQ_median_nai, "(IQR =", q1_APQ_nai, "-", q3_APQ_nai, ", 95% CI =", ci_APQ_nai[1], "-", ci_APQ_nai[2], ")\n")

# experienced AOSE median + IQR + CI
AOSE_median_exp <- median(Psych_exp$total.AOSE)
print(AOSE_median_exp)
q1_AOSE_exp <- quantile(Psych_exp$total.AOSE, 0.25)
q3_AOSE_exp <- quantile(Psych_exp$total.AOSE, 0.75)
iqr_AOSE_exp <- q3_AOSE_exp - q1_AOSE_exp
ci_AOSE_exp <- t.test(Psych_exp$total.AOSE)$conf.int
cat("AOSE_exp - Md = ", AOSE_median_exp, "(IQR =", q1_AOSE_exp, "-", q3_AOSE_exp, ", 95% CI =", ci_AOSE_exp[1], "-", ci_AOSE_exp[2], ")\n")

# naive AOSE median + IQR + CI
AOSE_median_nai <- median(Psych_nai$total.AOSE)
print(AOSE_median_nai)
q1_AOSE_nai <- quantile(Psych_nai$total.AOSE, 0.25)
q3_AOSE_nai <- quantile(Psych_nai$total.AOSE, 0.75)
iqr_AOSE_nai <- q3_AOSE_nai - q1_AOSE_nai
ci_AOSE_nai <- t.test(Psych_nai$total.AOSE)$conf.int
cat("AOSE_nai - Md = ", AOSE_median_nai, "(IQR =", q1_AOSE_nai, "-", q3_AOSE_nai, ", 95% CI =", ci_AOSE_nai[1], "-", ci_AOSE_nai[2], ")\n")

# 3.2 frequency
# regression analysis APQ
library(stats) # Load the required package (stats is typically loaded by default)
APQ_formula_3.2 <- Selfexp_ana$total.APQ ~ Selfexp_ana$Frequency + Selfexp_ana$Age + Selfexp_ana$Gender + Selfexp_ana$Level.of.education + Selfexp_ana$SES.ladder # Specify the model formula
APQ_GLM_model_3.2 <- glm(APQ_formula_3.2, data = Selfexp_ana, family = gaussian(link = "identity")) # Fit the generalized linear regression model
summary(APQ_GLM_model_3.2) # View the summary of the model
# regression analysis AOSE
library(stats) 
AOSE_formula_3.2 <- Selfexp_ana$total.AOSE ~ Selfexp_ana$Frequency + Selfexp_ana$Age + Selfexp_ana$Gender + Selfexp_ana$Level.of.education + Selfexp_ana$SES.ladder
AOSE_GLM_model_3.2 <- glm(AOSE_formula_3.2, data = Selfexp_ana, family = gaussian(link = "identity")) 
summary(AOSE_GLM_model_3.2)

# 3.3 significance
# correlate the three questions regarding significance
Psych_exp$Griffiths.2008.3 <- 6 - Psych_exp$Griffiths.2008.3 # reverse code Griffiths.2008.3
cor.sign <- cor(Psych_exp[, c("Griffiths.2008.1", "Griffiths.2008.2", "Griffiths.2008.3")], method = "spearman")
print(cor.sign)
# if the correlation is > .7 the items will be pooled together and transferred to a continuous scale
if (all(cor.sign > 0.7)) { # Check if correlation is greater than 0.7
  Psych_exp$Griffiths.2008.1 <- Psych_exp$Griffiths.2008.1 / 8 # transform into continuous scale
  Psych_exp$Griffiths.2008.2 <- Psych_exp$Griffiths.2008.2 / 6
  Psych_exp$Griffiths.2008.3 <- Psych_exp$Griffiths.2008.3 / 5
  Significance <- rowMeans(Psych_exp[, c("Griffiths.2008.1", "Griffiths.2008.2", "Griffiths.2008.3")]) # pool together Griffiths.2008.1-3
  print(Significance)
} else {
  print("Not all numbers in the correlation matrix are greater than 0.7.")
}
# regression analysis APQ
if (all(cor.sign > 0.7)) {
  library(stats) # Load the required package (stats is typically loaded by default)
  APQ_formula_3.3 <- Psych_exp$total.APQ ~ Significance + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.3 <- glm(APQ_formula_3.3, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  summary(APQ_GLM_model_3.3) # View the summary of the model
} else {
  APQ_formula_3.3_1 <- Psych_exp$total.APQ ~ Psych_exp$Griffiths.2008.1 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.3_1 <- glm(APQ_formula_3.3_1, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(APQ_GLM_model_3.3_1))
  APQ_formula_3.3_2 <- Psych_exp$total.APQ ~ Psych_exp$Griffiths.2008.2 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.3_2 <- glm(APQ_formula_3.3_2, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(APQ_GLM_model_3.3_2))
  APQ_formula_3.3_3 <- Psych_exp$total.APQ ~ Psych_exp$Griffiths.2008.3 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.3_3 <- glm(APQ_formula_3.3_3, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(APQ_GLM_model_3.3_3))
}
# regression analysis AOSE
if (all(cor.sign > 0.7)) {
  library(stats) # Load the required package (stats is typically loaded by default)
  AOSE_formula_3.3 <- Psych_exp$total.AOSE ~ Significance + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.3 <- glm(AOSE_formula_3.3, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  summary(AOSE_GLM_model_3.3) # View the summary of the model
} else {
  AOSE_formula_3.3_1 <- Psych_exp$total.AOSE ~ Psych_exp$Griffiths.2008.1 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.3_1 <- glm(AOSE_formula_3.3_1, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(AOSE_GLM_model_3.3_1))
  AOSE_formula_3.3_2 <- Psych_exp$total.AOSE ~ Psych_exp$Griffiths.2008.2 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.3_2 <- glm(AOSE_formula_3.3_2, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(AOSE_GLM_model_3.3_2))
  AOSE_formula_3.3_3 <- Psych_exp$total.AOSE ~ Psych_exp$Griffiths.2008.3 + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.3_3 <- glm(AOSE_formula_3.3_3, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(AOSE_GLM_model_3.3_3))
}

# 3.4 valence
# correlate the two questions regarding valence
Psych_exp$Unpleasant.experienc <- 6 - Psych_exp$Unpleasant.experienc # reverse code Unpleasant.experienc
cor.val <- cor(Psych_exp[, c("Pleasant.experience", "Unpleasant.experienc")], method = "spearman")
print(cor.val)
# if the correlation is > .7 the items will be pooled together and transferred to a continuous scale
if (all(cor.val > 0.7)) { # Check if correlation is greater than 0.7
  Psych_exp$Pleasant.experience <- Psych_exp$Pleasant.experience / 5 # transform into continuous scale
  Psych_exp$Unpleasant.experienc <- Psych_exp$Unpleasant.experienc / 5
  Valence <- rowMeans(Psych_exp[, c("Pleasant.experience", "Unpleasant.experienc")]) # pool together Pleasant and Unpleasant
  print(Valence)
} else {
  print("Not all numbers in the correlation matrix are greater than 0.7.")
}
# regression analysis APQ
if (all(cor.val > 0.7)) {
  library(stats) # Load the required package (stats is typically loaded by default)
  APQ_formula_3.4 <- Psych_exp$total.APQ ~ Valence + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.4 <- glm(APQ_formula_3.4, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  summary(APQ_GLM_model_3.4) # View the summary of the model
} else {
  APQ_formula_3.4_1 <- Psych_exp$total.APQ ~ Psych_exp$Pleasant.experience + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.4_1 <- glm(APQ_formula_3.4_1, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(APQ_GLM_model_3.4_1))
  APQ_formula_3.4_2 <- Psych_exp$total.APQ ~ Psych_exp$Unpleasant.experienc + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  APQ_GLM_model_3.4_2 <- glm(APQ_formula_3.4_2, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(APQ_GLM_model_3.4_2))
}
# regression analysis AOSE
if (all(cor.val > 0.7)) {
  library(stats) # Load the required package (stats is typically loaded by default)
  AOSE_formula_3.4 <- Psych_exp$total.AOSE ~ Valence + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.4 <- glm(AOSE_formula_3.4, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  summary(AOSE_GLM_model_3.4) # View the summary of the model
} else {
  AOSE_formula_3.4_1 <- Psych_exp$total.AOSE ~ Psych_exp$Pleasant.experience + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.4_1 <- glm(AOSE_formula_3.4_1, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(AOSE_GLM_model_3.4_1))
  AOSE_formula_3.4_2 <- Psych_exp$total.AOSE ~ Psych_exp$Unpleasant.experienc + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
  AOSE_GLM_model_3.4_2 <- glm(AOSE_formula_3.4_2, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
  print(summary(AOSE_GLM_model_3.4_2))
}

# 3.5 Recency
# regression analysis APQ
library(stats) # Load the required package (stats is typically loaded by default)
APQ_formula_3.2 <- Psych_exp$total.APQ ~ Psych_exp$Recency + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder # Specify the model formula
APQ_GLM_model_3.2 <- glm(APQ_formula_3.2, data = Psych_exp, family = gaussian(link = "identity")) # Fit the generalized linear regression model
summary(APQ_GLM_model_3.2) # View the summary of the model
# regression analysis AOSE
library(stats) 
AOSE_formula_3.2 <- Psych_exp$total.AOSE ~ Psych_exp$Recency + Psych_exp$Age + Psych_exp$Gender + Psych_exp$Level.of.education + Psych_exp$SES.ladder
AOSE_GLM_model_3.2 <- glm(AOSE_formula_3.2, data = Psych_exp, family = gaussian(link = "identity")) 
summary(AOSE_GLM_model_3.2)

# 3.6 APQ/AOSE per profession
# APQ
tripsitter_APQ <- Selfexp_ana$total.APQ[Selfexp_ana$tripsitter_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_APQ <- Selfexp_ana$total.APQ[Selfexp_ana$psychologist_vec == 1]
psychiatrist_APQ <- Selfexp_ana$total.APQ[Selfexp_ana$psychiatrist_vec == 1]
research_APQ <- Selfexp_ana$total.APQ[Selfexp_ana$research_vec == 1]
psych_APQ <- Selfexp_ana$total.APQ[Selfexp_ana$psych_vec == 1]
# ANOVA if there is no significant difference in age between professions
anova_3.6_APQ <- aov(total.APQ ~ group, data = data.frame(total.APQ = c(tripsitter_APQ, psychologist_APQ, psychiatrist_APQ, research_APQ),
                                                         group = rep(c("tripsitter", "psychologist", "psychiatrist", "researcher"), 
                                                                     c(length(tripsitter_APQ), length(psychologist_APQ), 
                                                                       length(psychiatrist_APQ), length(research_APQ))))) # Perform one-way ANOVA.
p_value_3.6_APQ <- summary(anova_3.6_APQ)[[1]]$"Pr(>F)"[1] # Check the p-value for significance.
f_statistic_3.6_APQ <- summary(anova_3.6_APQ)[[1]]$"F value"[1] # check the F-statistic
degrees_of_freedom_3.6_APQ <- summary(anova_3.6_APQ)[[1]]$"Df"[1] # check the degrees of freedom
if (p_value_3.6_APQ < 0.05) {
  cat("There is a significant difference in total APQ between the professions.\n")
} else {
  cat("There is no significant difference in total APQ between the professions.\n")
}
# if ANOVA is significant post-hoc tests
# ANCOVA if there is a significant difference in age between professions
library(stats)
library(dplyr)
ancova_3.6_APQ <- data.frame(
  Group = rep(c("Research", "Psychologist", "Psychiatrist", "Tripsitter"), 
              c(length(research_APQ), length(psychologist_APQ), 
                length(psychiatrist_APQ), length(tripsitter_APQ))),
  APQ = c(research_APQ, psychologist_APQ, psychiatrist_APQ, tripsitter_APQ),
  Age = c(age_research, age_psychologist, age_psychiatrist, age_tripsitter)
)
#ancova_3.6_APQ$Group <- factor(ancova_3.6_APQ$Group) # Convert "Group" variable to a factor
#ancova_3.6_APQ$Group <- relevel(ancova_3.6_APQ$Group, ref = "Research") # Change reference category to "Research"
ancova_3.6_APQ_result <- lm(APQ ~ Group + Age, data = ancova_3.6_APQ)
summary(ancova_3.6_APQ_result)
# post-hoc tests
library(multcomp)
ancova_3.6_APQ$Group <- factor(ancova_3.6_APQ$Group) # Convert 'Group' variable to factor
# Create a contrast matrix for all pairwise comparisons
contrasts <- rbind(
  c(-1, 1, 0, 0),   # Research vs. Psychologist
  c(-1, 0, 1, 0),   # Research vs. Psychiatrist
  c(-1, 0, 0, 1),   # Research vs. Tripsitter
  c(0, -1, 1, 0),   # Psychologist vs. Psychiatrist
  c(0, -1, 0, 1),   # Psychologist vs. Tripsitter
  c(0, 0, -1, 1)    # Psychiatrist vs. Tripsitter
)
posthoc_ancova_3.6_APQ <- glht(ancova_3.6_APQ_result, linfct = mcp(Group = contrasts), alternative = "two.sided") # Perform the post hoc tests using Tukey's method
summary(posthoc_ancova_3.6_APQ)

# repeated measures ANCOVA
Participant_nr <- 1:nrow(Selfexp_ana)
Selfexp_ana$Participant_nr <- Participant_nr
research_APQ <- Selfexp_ana[Selfexp_ana$research_vec == 1, c("Participant_nr", "total.APQ")]
psychologist_APQ <- Selfexp_ana[Selfexp_ana$psychologist_vec == 1, c("Participant_nr", "total.APQ")]
psychiatrist_APQ <- Selfexp_ana[Selfexp_ana$psychiatrist_vec == 1, c("Participant_nr", "total.APQ")]
tripsitter_APQ <- Selfexp_ana[Selfexp_ana$tripsitter_vec == 1, c("Participant_nr", "total.APQ")]

install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
ancova_3.6_APQ <- data.frame(
  Participant = c(research_APQ$Participant_nr, psychologist_APQ$Participant_nr, psychiatrist_APQ$Participant_nr, tripsitter_APQ$Participant_nr),
  Group = rep(c("Research", "Psychologist", "Psychiatrist", "Tripsitter"), 
              c(length(research_APQ$total.APQ), length(psychologist_APQ$total.APQ), 
                length(psychiatrist_APQ$total.APQ), length(tripsitter_APQ$total.APQ))),
  APQ = c(research_APQ$total.APQ, psychologist_APQ$total.APQ, psychiatrist_APQ$total.APQ, tripsitter_APQ$total.APQ),
  Age = c(age_research, age_psychologist, age_psychiatrist, age_tripsitter)
)
install.packages("car")
library(car)
ancova_data <- ancova_3.6_APQ
ancova_model <- aov(APQ ~ Group + Age + Error(Participant/Group), data = ancova_data) # Perform the repeated measures ANCOVA
summary(ancova_model)

# Post hoc pairwise comparisons with Tukey adjustment
install.packages("emmeans")
library(emmeans)
ancova_3.6_APQ$Group <- as.factor(ancova_3.6_APQ$Group)
ancova_model <- aov(APQ ~ Age * Group + Error(Participant), data = ancova_3.6_APQ)
em_means <- emmeans(ancova_model, specs = ~Group)
pairwise_comparisons <- pairs(em_means, adjust = "tukey")
print(pairwise_comparisons)


# AOSE
tripsitter_AOSE <- Selfexp_ana$total.AOSE[Selfexp_ana$tripsitter_vec == 1] # Subset the data for each group with 1 in the respective vectors
psychologist_AOSE <- Selfexp_ana$total.AOSE[Selfexp_ana$psychologist_vec == 1]
psychiatrist_AOSE <- Selfexp_ana$total.AOSE[Selfexp_ana$psychiatrist_vec == 1]
research_AOSE <- Selfexp_ana$total.AOSE[Selfexp_ana$research_vec == 1]
psych_AOSE <- Selfexp_ana$total.AOSE[Selfexp_ana$psych_vec == 1]
# ANOVA if there is no significant difference in age between professions
anova_3.6_AOSE <- aov(total.AOSE ~ group, data = data.frame(total.AOSE = c(tripsitter_AOSE, psychologist_AOSE, psychiatrist_AOSE, research_AOSE),
                                                      group = rep(c("tripsitter", "psychologist", "psychiatrist", "researcher"), 
                                                                  c(length(tripsitter_AOSE), length(psychologist_AOSE), 
                                                                    length(psychiatrist_AOSE), length(research_AOSE))))) # Perform one-way ANOVA.
p_value_3.6_AOSE <- summary(anova_3.6_AOSE)[[1]]$"Pr(>F)"[1] # Check the p-value for significance.
f_statistic_3.6_AOSE <- summary(anova_3.6_AOSE)[[1]]$"F value"[1] # check the F-statistic
degrees_of_freedom_3.6_AOSE <- summary(anova_3.6_AOSE)[[1]]$"Df"[1] # check the degrees of freedom
if (p_value_3.6_AOSE < 0.05) {
  cat("There is a significant difference in total APQ between the professions.\n")
} else {
  cat("There is no significant difference in total APQ between the professions.\n")
}
# if ANOVA is significant post-hoc tests
# ANCOVA if there is a significant difference in age between professions
library(stats)
library(dplyr)
ancova_3.6_AOSE <- data.frame(
  Group = rep(c("Research", "Psychologist", "Psychiatrist", "Tripsitter"), 
              c(length(research_AOSE), length(psychologist_AOSE), 
                length(psychiatrist_AOSE), length(tripsitter_AOSE))),
  AOSE = c(research_AOSE, psychologist_AOSE, psychiatrist_AOSE, tripsitter_AOSE),
  Age = c(age_research, age_psychologist, age_psychiatrist, age_tripsitter)
)
#ancova_3.6_AOSE$Group <- factor(ancova_3.6_AOSE$Group) # Convert "Group" variable to a factor
#ancova_3.6_AOSE$Group <- relevel(ancova_3.6_AOSE$Group, ref = "Research") # Change reference category to "Research"
ancova_3.6_AOSE_result <- lm(AOSE ~ Group + Age, data = ancova_3.6_AOSE)
summary(ancova_3.6_AOSE_result)
# post-hoc tests
library(multcomp)
ancova_3.6_AOSE$Group <- factor(ancova_3.6_AOSE$Group) # Convert 'Group' variable to factor
ancova_3.6_AOSE_result <- lm(AOSE ~ Group + Age, data = ancova_3.6_AOSE) # Fit the ANCOVA model
# Create a contrast matrix for all pairwise comparisons
contrasts <- rbind(
  c(-1, 1, 0, 0),   # Research vs. Psychologist
  c(-1, 0, 1, 0),   # Research vs. Psychiatrist
  c(-1, 0, 0, 1),   # Research vs. Tripsitter
  c(0, -1, 1, 0),   # Psychologist vs. Psychiatrist
  c(0, -1, 0, 1),   # Psychologist vs. Tripsitter
  c(0, 0, -1, 1)    # Psychiatrist vs. Tripsitter
)
posthoc_ancova_3.6_AOSE <- glht(ancova_3.6_AOSE_result, linfct = mcp(Group = contrasts), alternative = "two.sided") # Perform the post hoc tests using Tukey's method
summary(posthoc_ancova_3.6_AOSE)

# repeated measures ANCOVA
Participant_nr <- 1:nrow(Selfexp_ana)
Selfexp_ana$Participant_nr <- Participant_nr
research_AOSE <- Selfexp_ana[Selfexp_ana$research_vec == 1, c("Participant_nr", "total.AOSE")]
psychologist_AOSE <- Selfexp_ana[Selfexp_ana$psychologist_vec == 1, c("Participant_nr", "total.AOSE")]
psychiatrist_AOSE <- Selfexp_ana[Selfexp_ana$psychiatrist_vec == 1, c("Participant_nr", "total.AOSE")]
tripsitter_AOSE <- Selfexp_ana[Selfexp_ana$tripsitter_vec == 1, c("Participant_nr", "total.AOSE")]

install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
ancova_3.6_AOSE <- data.frame(
  Participant = c(research_AOSE$Participant_nr, psychologist_AOSE$Participant_nr, psychiatrist_AOSE$Participant_nr, tripsitter_AOSE$Participant_nr),
  Group = rep(c("Research", "Psychologist", "Psychiatrist", "Tripsitter"), 
              c(length(research_AOSE$total.AOSE), length(psychologist_AOSE$total.AOSE), 
                length(psychiatrist_AOSE$total.AOSE), length(tripsitter_AOSE$total.AOSE))),
  AOSE = c(research_AOSE$total.AOSE, psychologist_AOSE$total.AOSE, psychiatrist_AOSE$total.AOSE, tripsitter_AOSE$total.AOSE),
  Age = c(age_research, age_psychologist, age_psychiatrist, age_tripsitter)
)
install.packages("car")
library(car)
ancova_data_AOSE <- ancova_3.6_AOSE
ancova_model_AOSE <- aov(AOSE ~ Group + Age + Error(Participant/Group), data = ancova_data_AOSE) # Perform the repeated measures ANCOVA
summary(ancova_model_AOSE)

# Post hoc pairwise comparisons with Tukey adjustment
install.packages("emmeans")
library(emmeans)
ancova_3.6_AOSE$Group <- as.factor(ancova_3.6_AOSE$Group)
ancova_model_AOSE <- aov(AOSE ~ Age * Group + Error(Participant), data = ancova_3.6_AOSE)
em_means_AOSE <- emmeans(ancova_model_AOSE, specs = ~Group)
pairwise_comparisons_AOSE <- pairs(em_means, adjust = "tukey")
print(pairwise_comparisons_AOSE)