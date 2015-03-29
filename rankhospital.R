setwd(dir="C:/Users/AMT0H04/Documents/AMT/Personal/Kaggle/03-Research/02-R/CourseEra/02-Programming R/Assignment/03/rprog-data-ProgAssignment3-data")

data.out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data.out <- data.out[,c(2, 7, 11, 17, 23)]
data.out[,c(3, 4, 5)] <- sapply(data.out[,c(3, 4, 5)], as.numeric)

rankhospital <- function(state, outcome, num = "best") {
        states <- unique(data.out$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if (!(state %in% states)) {
                stop("invalid state")
        }
        
        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        data.out.state <- data.out[data.out$State == state,]
        
        if (outcome == "heart attack") {
                data.out.state <- data.out.state[order(data.out.state[,3], data.out.state[, 1]), ]
                data.out.state <- data.out.state[!is.na(data.out.state[,3]),]
        }
        else if (outcome == "heart failure") {
                data.out.state <- data.out.state[order(data.out.state[,4], data.out.state[, 1]), ]
                data.out.state <- data.out.state[!is.na(data.out.state[,4]),]
        }
        else {
                data.out.state <- data.out.state[order(data.out.state[,5], data.out.state[, 1]), ]
                data.out.state <- data.out.state[!is.na(data.out.state[,5]),]
        }
        
        if (num == "best") {
                num <- 1L
        }  
        else if (num == "worst") {
                num <- nrow(data.out.state)
        }
        else {
                num <- as.numeric(num)
        }
        
        data.out.state[num, 1]
}