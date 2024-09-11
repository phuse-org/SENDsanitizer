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
