pollutantmean <- function(directory, pollutant, id = 1:332){
  
  my_monitor_data <- data.frame()
  for (f in id){
    
    if(f<10){
      data.list <- read.csv(paste(directory,"\\00",f, ".csv",sep=""))  
    }
    
    if(f>=10 & !f<10 & f<100){
      data.list <- read.csv(paste(directory,"\\0",f, ".csv",sep=""))  
    }
    
    if(f>=100 & !f<10 & !f<100 ){
      data.list <- read.csv(paste(directory,"\\",f, ".csv",sep=""))  
    }
    
    my_monitor_data <- rbind(my_monitor_data, data.list)
    
  }
  
  if(pollutant == 'sulfate'){
    r <- mean(my_monitor_data$sulfate[!is.na(my_monitor_data$sulfate)])
  }
  
  if(pollutant == 'nitrate'){
    r <- mean(my_monitor_data$nitrate[!is.na(my_monitor_data$nitrate)])
  }
  r
}

complete <- function(directory, id=1:332){
  
  my_monitor_data <- data.frame()
  
  resdata <- data.frame("id"=numeric(0), "nobs"=numeric(0))
  

    
    for (f in id){
      
      
      if(f<10){
        data.list <- read.csv(paste(directory,"\\00",f, ".csv",sep=""))  
      }
      
      if(f>=10 & !f<10 & f<100){
        data.list <- read.csv(paste(directory,"\\0",f, ".csv",sep=""))  
      }
      
      if(f>=100 & !f<10 & !f<100 ){
        data.list <- read.csv(paste(directory,"\\",f, ".csv",sep=""))  
      }
      
      
      my_monitor_data <-  data.list
      resdata <- rbind(
        resdata, data.frame(
          "id" = f, "nobs" = length(my_monitor_data$sulfate[!is.na(my_monitor_data$nitrate & my_monitor_data$sulfate)])))
    }

  resdata
}

corr <- function(directory, threshold = 0){
  
  temp = list.files(directory, pattern="*.csv")
  myfiles = lapply(paste(directory,"\\",temp, sep=""), read.csv)
  
  cal <- data.frame("sulfate"=numeric(0), "nitrate"=numeric(0))
  
  r<-vector()
  
  for (file in myfiles) {
    
    nobs = length(file$sulfate[!is.na(file$nitrate & file$sulfate)])
    
    if( nobs >= threshold){
      
      cal <- rbind(cal, 
                   data.frame(
                     "sulfate"=file$sulfate[!is.na(file$nitrate & file$sulfate)],
                     "nitrate"=file$nitrate[!is.na(file$nitrate & file$sulfate)]
                   )
      )
      
      if(nobs != 0){
        
        r <- c(r, 
               if(!is.na(cor(file$sulfate[!is.na(file$nitrate & file$sulfate)], file$nitrate[!is.na(file$nitrate & file$sulfate)]))){
                 cor(file$sulfate[!is.na(file$nitrate & file$sulfate)], file$nitrate[!is.na(file$nitrate & file$sulfate)])
               })
      }
      
    }
    
  }
  
  r
  
}

