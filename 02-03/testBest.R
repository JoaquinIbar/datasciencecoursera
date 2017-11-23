test_best <- function(){
  
  source("best.R")
  
  print(best("TX", "heart attack"))
  
  print(best("TX", "heart failure"))
  
  print(best("MD", "heart attack"))
  
  print(best("MD", "pneumonia"))
  
  print(best("BB", "heart attack"))
  
  print(best("NY", "hert attack"))
  
  
}