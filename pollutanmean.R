pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutantmean <- 0
  tol = 0
  file <- list.files(directory)
  for(i in id) {
   temp <- read.csv(paste(directory,file[i],sep = "/"))
   if(pollutant == "sulfate") {
     col <- temp[,2]
     pollutantmean <- pollutantmean + sum(col[!is.na(col)])
     tol = tol + length(col[!is.na(col)])
   }
   else {
     col <- temp[,3]
     pollutantmean <- pollutantmean + sum(col[!is.na(col)])
     tol = tol + length(col[!is.na(col)])
   }
   
  }

  pollutantmean <- pollutantmean/tol
  
  pollutantmean
  
}
