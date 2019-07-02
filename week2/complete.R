complete <- function(directory, id = 1:332) {
  output <- data.frame(id=integer(),
                       nobs=integer())
  
  for (i in id) {
    filename <- paste(sprintf("%03d", i), "csv", sep = ".")
    abspath <- file.path(getwd(), directory, filename)
    read <- read.csv(abspath)
    read <- na.omit(read)
    output <- rbind(output, data.frame(id = i, nobs = nrow(read)))
  }
  
  output
}