rankhospital <- function(state, outcome, num = "best") {
  # read outcome
      data <- read.csv("C:\\Users\\Brad and Kathleen\\Documents\\Coursera\\Programming\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = 'character')
  
          if(!any(state == data$State)) {
              stop('invalid state')
          }
  
          if(outcome == 'heart attack') {
              i <- 11
          }
      
          else if(outcome == 'heart failure') {
              i <- 17
          }
  
          else if(outcome == 'pneumonia') {
              i <- 23
          }
  
          else {
      
              stop('invalid outcome')
          }
  
      data.state <- data[data$State == state, ]
      data.state[, i] <- as.numeric(x = data.state[, i])
      data.state <- data.state[complete.cases(data.state), ]
    
          if(num == "best") {
              num = 1
          }
          
          else if(num == "worst") {
              num = nrow(data.state)
          }
          
          else if(is.numeric(x = num)) {
    
          if(num<1 || num > nrow(data.state)) {
              
              return(NA)
          }
          }
          
          else {
              stop('invalid num')
          }
  
  
    data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
    return.names <- data.state[num, ]$Hospital.Name
    return.names[1]
}
## Test Case
## rankhospital("md", "heart failure", 5)