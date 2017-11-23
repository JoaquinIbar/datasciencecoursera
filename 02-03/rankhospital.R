rankhospital <- function(state, outcome, num = "best"){

  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Check that state and outcome are valid
  ##Checking the state
  #Look for any outcome with that state code
  fstate = outcome_data$State[outcome_data$State == state]
  #If length 0 there is no state with that state code
  if (length(fstate) == 0 ){
    #Send error message and stop execution
    stop("invalid state")
  }
  ##Done checking state
  
  ##Checking the outcome
  #If exist in possible outcomes
  foutcome <- match(outcome, p_outcome)
  if (is.na(foutcome)){
    #Send error message and stop execution
    stop("invalid outcome")
  }
  ##Done checking outcome
  
}
