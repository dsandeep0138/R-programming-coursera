pollutantmean <- function(directory, pollutant, id=1:332) {
  cdf <- data.frame(Date=as.Date(character()),
                    Sulfate=double(),
                    Nitrate=double(),
                    ID=integer())
  
  for (i in id) {
    filename <- paste(sprintf("%03d", i), "csv", sep = ".")
    abspath <- file.path(getwd(), directory, filename)
    read <- read.csv(abspath)
    cdf <- rbind(cdf, read)
  }
  
  
  k <- is.na(cdf[[pollutant]])
  
  mean(cdf[[pollutant]][!k])
}