filter_tk_rec <- function(Example=Example,recovery=FALSE){
    get_setcd_tk <- get_trt_group(ExampleStudy1 = Example)
  if(is.null(get_setcd_tk[[1]][['treatment_group']])){
    ## print(get_setcd_tk)
    print(get_setcd_tk)
    stop('there is no treatment group in the study')
  }
  if(!is.null(get_setcd_tk[[1]][['TK_group']])){
    tk_g <- get_setcd_tk[[1]][['TK_group']]
    Example$dm <- Example$dm[!Example$dm$SETCD %in% tk_g,]
    Example$tx <- Example$tx[which(Example$tx$SETCD %in% Example$dm$SETCD),]
    Example$lb <- Example$lb[which(Example$lb$USUBJID %in% Example$dm$USUBJID),]
    Example$om <- Example$om[which(Example$om$USUBJID %in% Example$dm$USUBJID),]
    Example$mi <- Example$mi[which(Example$mi$USUBJID %in% Example$dm$USUBJID),]
    Example$bw <- Example$bw[which(Example$bw$USUBJID %in% Example$dm$USUBJID),]
    Example$ds <- Example$ds[which(Example$ds$USUBJID %in% Example$dm$USUBJID),]
  }
    if(!recovery){
  if(!is.null(get_setcd_tk[[1]][['recovery_group']])){
    tk_rc <- get_setcd_tk[[1]][['recovery_group']]
    Example$dm <- Example$dm[!Example$dm$SETCD %in% tk_rc,]
            Example$tx <- Example$tx[which(Example$tx$SETCD %in% Example$dm$SETCD),]
            Example$lb <- Example$lb[which(Example$lb$USUBJID %in% Example$dm$USUBJID),]
            Example$om <- Example$om[which(Example$om$USUBJID %in% Example$dm$USUBJID),]
            Example$mi <- Example$mi[which(Example$mi$USUBJID %in% Example$dm$USUBJID),]
            Example$bw <- Example$bw[which(Example$bw$USUBJID %in% Example$dm$USUBJID),]
            Example$ds <- Example$ds[which(Example$ds$USUBJID %in% Example$dm$USUBJID),]
    }
    }
  df <- list(data=Example,setcd=get_setcd_tk)
  df
}

#' @title Filter out TK animals and write xpt file.
#' @description Given the directory of a study, function filter out TK animals
#' and write xpt files for bw, dm, lb, mi, om, ts and tx.
#' For ts, its the same as xpt provided.
#' This function does not generate any synthetic data. It is just a function
#' to filter out TK animals.
#' @param path xpt directory path
#' Directory where xpt files localted.
#' @param where_to_save directory
#' Directory where xpt file should be saved.
#' @param recovery whether recovery animals should be included.
#' Default FALSE, which means recovery animal will be filter out.
#' If TRUE, recovery animal will be included when write the xpt file back.
#' @param write_xpt whether write xpt file or return list of datafram
#' default TRUE, which means it will write xpt file in where_to_save directory.
#' @return write xpt file in the where_to_save directory if write_xpt TRUE.
#' If FALSE, it return a list. any domain can be access by df$data$lb and
#' metadata can be access by df$setcd. See examples.
#' @examples
#' \dontrun{
#' filter_tk_write_xpt(xpt_path, where_to_save)
#'
#' df <- filter_tk_write_xpt(path,write_xpt=FALSE)
#' # df$data contain list of domain's dataframe
#' # df$setcd contain list of metadata.
#' # LB domain can be access by df$data$lb
#'
#' }
#'
#' @export



filter_tk_write_xpt <- function(path,where_to_save=NULL,recovery=FALSE,
                                write_xpt=TRUE){

    domains <-  c("bw","dm","ds","lb","mi",

    "ts","tx","om","pc")
Example <- load_xpt_files(path = path,domains = domains)
get_data <- filter_tk_rec(Example = Example,recovery = recovery)


    domains_xpt <-  c("bw","dm","lb","mi","om","ts","tx")

  if(write_xpt){
    if(is.null(where_to_save)){
stop('Provide a directory path where to save xpt file')
    }
                for (domain in domains_xpt){

                    ## printpath <- paste0(path,"/FAKE",studyID,"/",domain,".xpt")
                    printpath <- fs::path(where_to_save, domain, ext= 'xpt')
                    haven::write_xpt(Example[[domain]],path = printpath, version = 5)
                  ## print(paste0('file saved in: ',printpath))

                }
  }else{

return(get_data)


  }


}
