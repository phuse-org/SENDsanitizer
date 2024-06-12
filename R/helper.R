
## This code is for getting treatment group, recovery group and TK group

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   Feb-15-2024    Md Yousuf Ali (md.ali@fda.hhs.gov)



get_trt_group <- function(ExampleStudy1) {

  list_return <- list()
  four <- c()
  ## }
    ## print(study)
  tx <- ExampleStudy1$tx
  tx <- data.table::as.data.table(tx)
  tx_df <- tx[,.(STUDYID,SETCD,TXPARMCD,TXVAL)]
  ts <- ExampleStudy1$ts
  ts <- data.table::as.data.table(ts)
  ts_df <- ts[,.(STUDYID,TSPARMCD,TSVAL)]
  ds <- ExampleStudy1$ds
  ds <- data.table::as.data.table(ds)
  ds_df <- ds[, .(STUDYID,USUBJID,DSDECOD)]
  dm <- ExampleStudy1$dm
  dm <- data.table::as.data.table(dm)
  dm_df <- dm[, .(STUDYID,USUBJID,SETCD)]
  pc <- ExampleStudy1$pc
  pc <- data.table::as.data.table(pc)
  # to match with database.
  if(!'POOLID' %in% names(pc)){
pc$POOLID <- ''
  }
  pc_df <- pc[, .(STUDYID,USUBJID,POOLID)]


  pooldef <- ExampleStudy1$pooldef
  pooldef <- data.table::as.data.table(pooldef)
  pooldef_df <- pooldef[, .(STUDYID,USUBJID,POOLID)]
num_study <- unique(dm_df$STUDYID)

  for(i in 1:length(num_study)){


  ## study <- dm[1,STUDYID]
  study <- num_study[i]
  dm <- dm_df[STUDYID==study,]
  ds <- ds_df[STUDYID==study,]
  tx <- tx_df[STUDYID==study,]
  ts <- ts_df[STUDYID==study,]
  pc <- pc_df[STUDYID==study,]
  pooldef <- pooldef_df[STUDYID==study,]
    number_of_setcd <- unique(dm[['SETCD']])
    st_species <- unique(ts[TSPARMCD=='SPECIES'][, TSVAL])

    list_return[[study]][['species']] <- st_species

    list_return[[study]][['setcd']] <- number_of_setcd
    recv_group <- c()
    trtm_group <- c()

if(length(st_species)!= 0) {
    if(tolower(st_species) =="rat") {
      # see if tkdesc in txparmcd
      parmcd <- unique(tx[['TXPARMCD']])
      if('TKDESC' %in% parmcd){
        tkdesc_in_parmcd <- TRUE
      } else {

        tkdesc_in_parmcd <- FALSE
      }

    not_tk_term <-  c("NON-TK", "Non-TK", "non-TK", "NON TK")
      ## tkdesc_in_parmcd
      if(tkdesc_in_parmcd) {
        tk_group <- c()
        unq_tkdesc <- unique(tx[TXPARMCD=='TKDESC',TXVAL])
        if (length(unq_tkdesc) > 0) {
          if('TK' %in% unq_tkdesc) {
            tk_group <- unique(tx[TXPARMCD=='TKDESC' & TXVAL=='TK',  SETCD])
          not_tk <- number_of_setcd[which(!number_of_setcd %in% tk_group)]
          } else if(any(not_tk_term %in% unq_tkdesc)){

            not_tk  <- unique(tx[TXPARMCD=='TKDESC' & TXVAL %in% not_tk_term,  SETCD])
           tk_group <- number_of_setcd[which(!number_of_setcd %in% not_tk)]

          } else {
stop('Value of TKDESC in TXVAL probably not TK or NON-TK')
          }
          ## else{
          ##   not_tk <- number_of_setcd

          ## }
        } else {
stop('Check TKDESC parameter value in TXVAL of TX domain')
        }
      } else {
        tk_group <- c()
        for(i in 1:length(number_of_setcd)){
          set_cd  <- number_of_setcd[i]
          subjid <- unique(dm[SETCD==set_cd, USUBJID])

          if(pc$USUBJID[1]!='') {
            uniq_pc_subj <- unique(pc$USUBJID)
            pc_sub <- 'not_empty'
          } else {
            uniq_pool <- unique(pc$POOLID)
            ## pooldef <- df_domain$pooldef
            pool_sub <- pooldef[POOLID %in% uniq_pool, USUBJID]
            pc_sub <- 'empty'
          }

          if(pc_sub=='not_empty'){
            if(any(subjid %in% uniq_pc_subj)){
              ## print(paste0(set_cd, ' : in TK'))
              tk_group <- c(tk_group, set_cd)
            }
          } else if (pc_sub=='empty'){
            if(any(subjid %in% pool_sub)){
              ## print(paste0(set_cd, ' : in TK and pool'))
              tk_group <- c(tk_group, set_cd)
            }
          }

        }
        not_tk <- number_of_setcd[which(!number_of_setcd %in% tk_group)]
      }
      ## number_of_setcd

      if(length(not_tk) > 0) {
        for (i in 1:length(not_tk)){
          set_cd <- not_tk[i]
          subjid <- unique(dm[SETCD==set_cd, USUBJID])
          dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
          if(tolower("RECOVERY SACRIFICE") %in% dsdecod) {
            recv_group <- c(recv_group, set_cd)
          } else if (tolower("TERMINAL SACRIFICE") %in% dsdecod){
            trtm_group <- c(trtm_group, set_cd)
          } else{
            print(paste0(as.character(set_cd),
                         ' : no recovery or terminally sacrifice',
                         ' animal in this group'))
          }
        }
      }

      list_return[[study]][['TK_group']] <- tk_group
    } else {
      not_tk <- number_of_setcd
      for (i in 1:length(not_tk)){
        set_cd <- not_tk[i]
        subjid <- unique(dm[SETCD==set_cd, USUBJID])
        dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
        if(tolower("RECOVERY SACRIFICE") %in% dsdecod) {
          ## print(paste0(set_cd, ' : in recovery'))
          recv_group <- c(recv_group, set_cd)
        } else if (tolower("TERMINAL SACRIFICE") %in% dsdecod){
          trtm_group <- c(trtm_group, set_cd)
        }
      }
    }
    }

    ## if( length(trtm_group) == 4) {

    ##   four <- c(four,study)
    ##   ## print(four)
    ## }

    ## print(trtm_group)
    list_return[[study]][['treatment_group']] <- trtm_group
    list_return[[study]][['recovery_group']] <- recv_group
  }

## list_return[['four_trtm_group']] <- four
  list_return
}



get_doses <- function(tx){
  tx <- data.table::as.data.table(tx)
  df <- tx[TXPARMCD=='TRTDOS',.(STUDYID,SETCD,TXVAL)]
  df
}


clean_txval_dose <- function(dose) {
    # function to clean doses
  index <- 1:length(dose)
  dose_char <- dose
  dose_num <- as.numeric(dose)
  final_dose <- NA

  for (i in index) {
    if (is.na(dose_num[i])) {
      x <- gsub(";|-|\\/|\\|", ",", dose_char[i])
      x <- max(as.numeric(unlist(strsplit(x,","))))
      final_dose[i] <- x
    } else {
      final_dose[i] <- dose_num[i]
    }
  }
  return(final_dose)
}



