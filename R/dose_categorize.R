

dose_categorize <- function(trt,recovery=FALSE){
  maxdose <- max(trt$dose)
  trt <- trt[dose==maxdose, `:=`(cat='HD')]
  if(length(unique(trt$dose)) > 1){
    if (0 %in% unique(trt$dose)){
      trt <- trt[dose==0, `:=`(cat='Control',dose_order=1)]
    } else{
      trt <- trt[dose %in% min(dose),`:=`(cat='Control',dose_order=1)]
    }
  }else{
    stop('there is only one dose group')
  }
  if(length(unique(trt$dose)) > 2){
    low <- min(trt[!cat %in% c('Control', 'HD'),dose])
    trt <- trt[dose %in% low, `:=`(cat='LD',dose_order=2)]
  } else {
    print('There is only 2 dose group')
  }
  if(length(unique(trt$dose)) > 3){
    mid  <- unique(trt[is.na(cat),dose])
    if(length(mid) > 1) {
      n=3
      for (i in 1:length(mid)){
        mdose <- mid[i]
        trt <- trt[dose %in% mdose, `:=`(cat=paste0('MD','_', as.character(i)),dose_order=n)]
        n <- n+1
      }
    } else{
      trt <- trt[dose %in% mid, `:=`(cat='MD',dose_order=3)]
    }
  }
  df <- trt
  df
}
