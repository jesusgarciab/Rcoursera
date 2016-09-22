

pollutantmean <- function(directory, pollutant, id = 1:332) {
    all <- data.frame()
    address <- paste("C:/Users/JRGARCIA4/Desktop/R coursera/rprog-data-specdata","/", directory, sep = '')
    setwd(address)
    filenames <- list.files(path = address, pattern = ".csv")
    selectedfiles <- filenames[id]
    for (i in selectedfiles) {
      dat <-read.csv(i)
      all <- rbind(all, dat)
    
    }
    round(mean(all[,pollutant], na.rm = TRUE),digits = 3)
} 
  
complete <- function(directory = "specdata", id = 1:332) {
  alldata <- data.frame()
  address <- paste("C:/Users/JRGARCIA4/Desktop/R coursera/rprog-data-specdata","/", directory, sep = '')
  setwd(address)
  filenames <- list.files(path = address, pattern = ".csv")
  selectedfiles <- filenames[id]
  
  id <- c()
  nobs <- c()
  
  for (i in selectedfiles) {
    dat <-read.csv(i)
    id <- c(id,unique(dat$ID))
    
    logical_completes <- sum(!is.na(dat$sulfate + dat$nitrate))
    nobs <- c(nobs, logical_completes)
    }
  round(data.frame(id,nobs),digits = 3)
  
}



corr <- function(directory, threshold = 0) {
  output<- c()
  address <- paste("C:/Users/JRGARCIA4/Desktop/R coursera/rprog-data-specdata","/", directory, sep = '')
  setwd(address)
  filenames <- list.files(path = address, pattern = ".csv")
  
  for (i in filenames) {
    dat <-read.csv(i)
    logical_completes <- sum(!is.na(dat$sulfate + dat$nitrate))
    
    if (logical_completes < threshold)
      next
    else 
      output <- c(output, cor(dat$sulfate, y = dat$nitrate, use = "complete.obs"))
  }
  as.vector(output)
}
