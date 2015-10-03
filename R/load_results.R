# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




#' Map the keys from a datarun and load them into a dataframe
#'
#' @param zipfile The location of the zipped up output of the run (can be absolute or relative)
#' @param datafile The file within the zip file which conatins the results (output/output2.arff)
#' @param keysheet The location of the keysheet to be used for mapping (can be a url or file location)
#'
#' @return A dataframe of the results with the keys mapped to human-readable labels
#'
#'
mapKeys <- function(zipfile, datafile, keysheet="https://www.dropbox.com/s/o8x5mmhjhtolkkb/feature-ranking-key.xls?dl=1" ){
  if(substr(keysheet,1,4)=="http"){
    temp <- tempfile()
    download.file(keysheet,temp,mode="wb")
    keysheet <- temp
  }
  wb <- XLConnect::loadWorkbook(keysheet)
  keySheet = XLConnect::readWorksheet(wb,sheet = "Sheet1", header=TRUE)
  keySheet$FitDatasetCode <- keySheet[,1]
  keySheet$FitDataset <- keySheet$Dataset
  keySheet$TestDatasetCode <- keySheet[,1]
  keySheet$TestDataset <- keySheet$Dataset

  data <- RWeka::read.arff(unz(zipfile,datafile))


  colIndexes <- seq(2,ncol(keySheet),2)

  for(i in colIndexes){
    colname <- colnames(keySheet)[i]
    #  print(colname)
    #  print(match(colname,colnames(data)))
    foundColumnLoc <- match(colname,colnames(data))
    if(!is.na(foundColumnLoc)){
      keys <- keySheet[,i-1]
      values <- keySheet[,i]
      #    print(colname)
      for(j in 1:nrow(data)){
        #      print(data[j,foundColumnLoc])
        mappedValue <- values[match(data[j,foundColumnLoc],keys)]
        data[j,foundColumnLoc] <- mappedValue
      }
    }
  }
  data$Learner <- gsub(" ","_",data$Learner)
  return(data)
}

