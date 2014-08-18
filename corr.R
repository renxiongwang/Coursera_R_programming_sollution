corr <- function(directory, threshold = 0) {

  form <- complete(directory, 1:332)

  nobs <- form[,2]

  id <- form[,1]

  list <- id[nobs > threshold]

  file <- list.files(directory)

  j = 1

  corr = vector(mode = "numeric", length(list))

  #x <- matrix(NA, 1500, length(list))

  #y <- matrix(NA, 1500, length(list))

  for (i in list) {

    temp <- read.csv(paste(directory,file[i],sep = "/"))

    sul <- temp[, 2]

    nig <- temp[, 3]

    x <- sul[!is.na(sul)&!is.na(nig)]

    y <- nig[!is.na(sul)&!is.na(nig)]

    corr[j] <- cor(x, y)

    j <- j + 1

  }

  corr 

}
