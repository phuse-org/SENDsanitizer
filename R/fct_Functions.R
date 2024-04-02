#' Functions
#'
#' @description getFieldValue function returns the field value for certain variables
#' of SEND datasets.
#'
#' @return Field Value for specific SEND data fields
#'
#' @noRd

#' @importFrom tools file_path_sans_ext
#' @importFrom haven read_xpt

# Function to get the value from field based on category in another field (e.g., check a TXVAL given a TXPARMCD and SETCD)
getFieldValue <- function(dataset,queryField,indexFields,indexValues) {
  for (i in 1:length(indexFields)) {
    indexTmp <- which(dataset[,indexFields[i]]==indexValues[i])
    if (i == 1) {
      index <- indexTmp
    } else {
      index <- intersect(index,indexTmp)
    }
  }
  fieldValue <- dataset[index,queryField]
  if (length(levels(dataset[,queryField])) > 0) {
    return(levels(dataset[,queryField])[fieldValue])
  } else {
    return(fieldValue)
  }
}

# Function to create a list of R dataframes for each .xpt file
# NOTE: this function requries the packages: "Hmisc" and "tools"
load.xpt.files <- function(path=getwd(),domainsOfInterest=NULL) {
  xptFiles <- Sys.glob(paste(path,"*.xpt",sep='/'))
  if (!is.null(domainsOfInterest)) {
    domainsOfInterest <- paste(paste(path,'/',domainsOfInterest,'.xpt',sep=''))
    xptFiles <- xptFiles[which(tolower(xptFiles) %in% tolower(domainsOfInterest))]
  }
  dataFrames <- list()
  count <- 0
  for (xptFile in xptFiles) {
    count <- count + 1
    ## xptData <- sasxport.get(xptFile)
    xptData <- haven::read_xpt(xptFile)
    colnames(xptData) <- toupper(colnames(xptData))
    dataFrames[[count]] <- xptData
  }
  names(dataFrames) <- tolower(tools::file_path_sans_ext(basename(xptFiles)))
  return(dataFrames)
}

