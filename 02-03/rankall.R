rankall <- function(outcome, num="best"){
  
  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Checking the outcome
  #If exist in possible outcomes
  p_outcome <- c("heart attack", "heart failure", "pneumonia")
  foutcome <- match(outcome, p_outcome)
  if (is.na(foutcome)){
    #Send error message and stop execution
    stop("invalid outcome")
  }
  ##Done checking outcome
  
  states <- unique(outcome_data$State)
  #res <- data.frame(row.names = states)
  #res["state"] <- states
  #res["hospital"] <- c(1:length(states))
  
  #Get the requested hospital per state
  source("rankhospital.R")
  res2 <- sapply(states, rankhospital, num=num, outcome=outcome)
  
  #res <- merge(res, res2, all.x = true)  # merge by row names (by=0 or by="row.names")
  res <- as.data.frame(res2)
  res["state"] <- states  
  colnames(res) <- c("hospital", "state")
  
  res <- res[ order(row.names(res)), ]
  res
}