######################################################
## Title: Make shots data script
## Description: Data Preparation for shots chart
## Input: shots_data
## Ouput: csv_document
######################################################

iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
iguodala$name <- "Andre Iguodala"

green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
green$name <- "Draymond Green"

durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
durant$name <- "Kevin Durant"

thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
thompson$name <- "Klay Thompson"

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
curry$name <- "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"

green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"

durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"

thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"

curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"

iguodala$minute <- (iguodala$period)*12-iguodala$minutes_remaining
green$minute <- (green$period)*12-green$minutes_remaining
durant$minute <-(durant$period)*12-durant$minutes_remaining
thompson$minute <- (thompson$period)*12-thompson$minutes_remaining
curry$minute <- (curry$period)*12-curry$minutes_remaining


sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

assembled <- rbind(iguodala,green,durant,thompson,curry)
write.csv(x = assembled, file = '../data/shots-data.csv')

sink(file = '../output/shots-data-summary.txt')
summary(assembled)
sink()
