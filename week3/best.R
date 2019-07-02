best <- function(state, outcome) {
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  stateout <- out[out$State == state, ]
  
  if (nrow(stateout) == 0) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    heartattack <- stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    nheartattack <- as.numeric(heartattack)
    fheartattack <- na.omit(nheartattack)
    minval <- toString(min(fheartattack))
  
    ele <- stateout[minval == stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ]
   
  } else if (outcome == "heart failure") {
    heartfailure <- stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    nheartfailure <- as.numeric(heartfailure)
    fheartfailure <- na.omit(nheartfailure)
    minval <- toString(min(fheartfailure))
    
    ele <- stateout[minval == stateout$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, ]

  } else if (outcome == "pneumonia") {
    pneumonia <- stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    npneumonia <- as.numeric(pneumonia)
    fpneumonia <- na.omit(npneumonia)
    minval <- toString(min(fpneumonia))
    
    ele <- stateout[minval == stateout$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, ]

  } else {
    stop("invalid outcome")
  }
  
  ele <- ele[order(ele[ , 'Hospital.Name']), ]
  ele[1, ]$Hospital.Name
}