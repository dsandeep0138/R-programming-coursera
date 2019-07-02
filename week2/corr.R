corr <- function(directory, threshold = 0) {
  output <- numeric()
  
  filelist <- list.files(file.path(getwd(), directory))
  
  for (filename in filelist) {
    abspath <- file.path(getwd(), directory, filename)
    read <- read.csv(abspath)
    filread <- read[complete.cases(read), ]
    if (nrow(filread) > threshold) {
      corv <- cor(filread[["sulfate"]], filread[["nitrate"]], use="complete.obs")
      output <- c(output, corv)
    }
  }
  
  output
}