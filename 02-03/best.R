#Function to finde the best hospital in a state
best <- function(state, outcome){
  ##Possible outcomes
  ##"heart attack" = col 11
  ##"heart failure" = col 17
  ##"pneumonia" = col 23
  p_outcome <- c("heart attack", "heart failure", "pneumonia")
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
  
  #Find the best hospital for each outcome
  if(outcome == "heart attack"){
    
    #Get for heart attack
    outcome_data[,11] <- suppressWarnings(as.numeric(outcome_data[,11]))
    ha_outcome <- outcome_data[outcome_data$State==state & 
                                 complete.cases(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), c(2,11)]
    
  } else if(outcome == "heart failure"){
    
    #Get for heart failure
    outcome_data[,17] <- suppressWarnings(as.numeric(outcome_data[,17])) 
    ha_outcome <- outcome_data[outcome_data$State==state & 
                                 complete.cases(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), c(2,17)]
    
  }else if(outcome == "pneumonia"){
    
    #Get for pneumonia
    outcome_data[,23] <- suppressWarnings(as.numeric(outcome_data[,23]))
    ha_outcome <- outcome_data[outcome_data$State==state & 
                                 complete.cases(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), c(2,23)]
    
  }
  #Sort the data
  ha_sort_outcome <- ha_outcome[order(ha_outcome[,2], ha_outcome[,1]),]
  return(as.character(ha_sort_outcome[1,1])) 
  
}