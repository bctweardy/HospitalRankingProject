getwd()
setwd("C:\\Users\\Brad and Kathleen\\Documents\\Coursera\\Programming\\rprog-data-ProgAssignment3-data")

outcome<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning message about NAs being introduced; that is okay
hist(outcome[, 11])
