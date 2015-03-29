setwd(dir="C:/Users/AMT0H04/Documents/AMT/Personal/Kaggle/03-Research/02-R/CourseEra/02-Programming R/Assignment/03/rprog-data-ProgAssignment3-data")

best <- function(state,outcome){
        data.out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        states <- unique(data.out$State)
        outcomes <- c("heart attack","heart failure","pneumonia")
        if (!(state %in% states)) {
                stop("invalid state")
        }
        
        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        data.out.state <- data.out[data.out$State == state,]
        data.out.state[, c(11, 17, 23)] <- sapply(data.out.state[, c(11, 17, 23)], as.numeric)
        data.out.state <- data.out.state[order(data.out.state[, 2]), ]
        
        if (outcome == "heart attack") {
                best <- data.out.state[which.min(data.out.state[, 11]), "Hospital.Name"]
        }
        else if (outcome == "heart failure") {
                best <- data.out.state[which.min(data.out.state[, 17]), "Hospital.Name"]
        }
        else {
                best <- data.out.state[which.min(data.out.state[, 23]), "Hospital.Name"]
        }
        
        best
}