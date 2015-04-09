rankall <- function(outcome, num = "best") {
      
      ##Read the outcome
      data <- read.csv("C:\\Users\\Brad and Kathleen\\Documents\\Coursera\\Programming\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv")
      
      State <- data[, 7]
      Hospital.Name <- data[, 2]
      
      data[, 11] = as.numeric(data[, 11])
      heart_attack = data[, 11]
      
      data[, 17] = as.numeric(data[, 17])
      heart_failure = data[, 17]
      
      data[, 23] = as.numeric(data[, 23])
      pneumonia = data[, 23]
      
      data.rank <- data.frame(State, heart_attack, heart_failure, pneumonia, Hospital.Name)
      
      if(outcome == "heart attack"){
          ordered.data.rank = data.rank[order(State, heart_attack, Hospital.Name), ]
               
      } else if(outcome == "heart failure"){
            data.rank$heart_attack <- NULL
            ordered.data.rank = data.rank[order(State, heart_failure, Hospital.Name), ]
        
      } else if(outcome == "pneumonia"){
            data.rank$heart_attack <- NULL
            data.rank$heart_failure <- NULL
            ordered.data.rank = data.rank[order(State, pneumonia, Hospital.Name), ]
      } else {
            stop("invalid outcome")
      }
      
      ordered.data.rank <- ordered.data.rank[complete.cases(ordered.data.rank), ]
      
      separated.data = split(ordered.data.rank, ordered.data.rank$state)
      
      hospital = rep(NA, 54)
      state = rep(NA, 54)
      
      for(i in 1:54){
        state[i] = as.character(separated.data[[i]][1, 1])
      }
      
      if(num == "best"){
        for(i in 1:54){
          hospital[i] = as.character(separated.data[[i]][1, "Hospital.Name"])
        }	
      } else if(num == "worst"){
        for(i in 1:54){		
          worstRow = max(separated.data[[i]][ , 2])
          worstHospital = as.character(separated.data[[i]][which(separated.data[[i]][ , 2] == worstRow), "Hospital.Name"])
          hospital[i] = worstHospital
        }
      } else {
            num = as.integer(num)
            for(i in 1:54){
                if(typeof(num) == "integer" && nrow(separated.data[[i]]) >= num){
                hospital[i] = as.character(separated.data[[i]][num, "Hospital.Name"])
          } else {
            NA
          }
        }
      }

rankall.data <- data.frame(hospital, state)
return(rankall.data)
      
}