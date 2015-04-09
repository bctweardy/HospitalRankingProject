
setwd("C:\\Users\\Brad and Kathleen\\Documents\\Coursera\\Programming\\rprog-data-ProgAssignment3-data")
list.files()


## subfcn function for getting the hospital name
subfcn <- function(cond, col_num, state) {
    statecol <- cond[cond[, 7] == state, ]
    otcm <- statecol[, col_num]
    low <- min(otcm, na.rm=TRUE)
    min_otcm <- which(otcm == low)
    best_hosp <- statecol[min_otcm, 2]
    return(best_hosp)
}


best <- function(state, outcome) {
    ## Read outcome data
    directory <- "C:\\Users\\Brad and Kathleen\\Documents\\Coursera\\Programming\\rprog-data-ProgAssignment3-data"
    cond <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    cond[, 11] <- as.numeric(cond[, 11]) ## Heart Attack column set as numeric
    cond[, 17] <- as.numeric(cond[, 17]) ## Heart Failure column set as numeric
    cond[, 23] <- as.numeric(cond[, 23]) ## Pneumonia column set as numberic
    good_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!state %in% cond$State) {
          stop("invalid state")
    } else if (!outcome %in% good_outcomes) {
          stop("invalid outcome")
    } else {
    
    ## Return hospital name in that state with the lowest 30-day death
    ## rate
        if (outcome == "heart attack") {
                best_hosp <- subfcn(cond, 11, state)
        } else if(outcome == "heart failure") {
                best_hosp <- subfcn(cond, 17, state)
        } else{
            best_hosp <- subfcn(cond, 23, state)
        }
        winner <- best_hosp
        return(winner)
    }
}