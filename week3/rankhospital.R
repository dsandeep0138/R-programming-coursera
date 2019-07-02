rankhospital <- function(state, outcome, num = "best") {
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  stateout <- out[out$State == state, ]
  
  if (nrow(stateout) == 0) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), stateout$Hospital.Name)), ]
    
  } else if (outcome == "heart failure") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), stateout$Hospital.Name)), ]
    
  } else if (outcome == "pneumonia") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), stateout$Hospital.Name)), ]
    
  } else {
    stop("invalid outcome")
  }
  
  if (num == "best") {
    sorted[1, ]$Hospital.Name
  } else if (num == "worst") {
    sorted[nrow(sorted), ]$Hospital.Name
  } else {
    sorted[num, ]$Hospital.Name
  }
}