myFunction <- function(){
  x <- rnorm(100)
  mean(x)
}

myFunction2 <- function(x){
  x + rnorm(length(x))
}