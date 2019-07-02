rankall <- function(outcome, num = "best") {
  
  rank_order <- function(x) {
    if (num == "best") {
      num <- as.numeric(1)
    } else if (num == "worst") {
      num <- as.numeric(nrow(x))
    } else {
      num <- as.numeric(num)
    }
    
    x[num, 2]
  }
  
  stateout <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (outcome == "heart attack") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), stateout$Hospital.Name)), ]
    out <- lapply(split(sorted, sorted$State), rank_order)
    
  } else if (outcome == "heart failure") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), stateout$Hospital.Name)), ]
    out <- lapply(split(sorted, sorted$State), rank_order)
    
  } else if (outcome == "pneumonia") {
    stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    stateout <- stateout[complete.cases(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
    sorted <- stateout[with(stateout, order(as.numeric(stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), stateout$Hospital.Name)), ]
    out <- lapply(split(sorted, sorted$State), rank_order)
    
  } else {
    stop("invalid outcome")
  }

  data.frame(hospital=unlist(out), state=names(out))
}