complete <- function(directory, id = 1:332) {
 
  complete <- matrix(0,length(id),2)
  
  file <- list.files(directory)
  
  j <- 1
  
  for(i in id) {
    
    complete[j, 1] <- i
    
    temp <- read.csv(paste(directory,file[i],sep = "/"))
    
    a <- temp[, 2]
    
    b <- temp[, 3]
    
    complete[j, 2] <- length(a[!is.na(a)&!is.na(b)])
    
    j <- j + 1

  }
  colnames(complete) <- c("id", "nobs")
  
  rownames(complete) <- c(1:length(id))
  
  complete <- as.data.frame(complete)

complete
}
