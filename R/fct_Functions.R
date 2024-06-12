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


load_xpt_files <- function(path,domains=NULL) {
  # xptFiles <- Sys.glob(paste(path,"*.xpt",sep='/'))
  xptFiles_all <- fs::dir_ls(path = path, glob = '*.xpt')

  if (!is.null(domains)) {
    ext_name <- tools::file_path_sans_ext(basename(xptFiles_all))
    xptFiles <- xptFiles_all
    if(!all(tolower(domains) %in% tolower(ext_name))){
ind <- which(!tolower(domains) %in% tolower(ext_name))
miss_dom <- domains[ind]
stop('missing xpt file for',': ', paste(miss_dom,collapse = ', '))
    }

    xptFiles <- xptFiles_all[which(tolower(ext_name) %in% tolower(domains))]
  }else {

  xptFiles <- xptFiles_all
  }
  dataFrames <- list()
  for (xptFile in xptFiles) {
    xptData <- haven::read_xpt(xptFile)
    names_domain <- tools::file_path_sans_ext(basename(xptFile))
    dataFrames[[names_domain]] <- xptData
  }
  dataFrames <- lapply(dataFrames,data.table::as.data.table)
  return(dataFrames)
}
