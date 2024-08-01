#' @title function to generate fake data
#' @param path Mandatory\cr
#' path where real data/xpt files located, should be a directory that contains
#' xpt files
#' @param number mandatory, default 1\cr
#'   how many studies to generate. Currently only work with 1
#' @param recovery optional\cr
#' recovery
#' @param where_to_save mandatory\cr
#' where to save generated xpt files. Should be a directory.
#' @export
#' @import data.table
#' @import utils
#' @importFrom dplyr filter count distinct group_by mutate select
#' @importFrom haven write_xpt
#' @importFrom MCMCpack MCMCregress
#' @importFrom stringr str_detect str_replace_all
#' @importFrom tidyr replace_na
#' @importFrom fs dir_create path
#' @importFrom data.table rbindlist
#' @importFrom utils tail
#' @importFrom magrittr  %>%
# test
# what will happen when visitday not present but dsnomdy present
# before NA lbtestcd removed, check if > < in LBSTRESC
# sequence current
#ts,dm,tx
#bw,lb,om,mi
# start here
sanitize <- function(path, number=1, recovery=FALSE,
                     where_to_save=NULL) {
  # whether to show original value in table, this
  # for test only, if true it will not write data
  test_original <- FALSE
  ## test_original <- TRUE
  lb_day_model <- FALSE
  number  <- as.numeric(number)
  PRINT <- FALSE
  Recovery <- recovery
  ExampleStudies <- path
  NumData <- length(ExampleStudies)
  if(NumData > 1){
    multi_study <- TRUE
  } else{
    multi_study <- FALSE
  }
  # check if directory given for where to save data
  if(!is.null(where_to_save)) {
    if(!fs::is_dir(where_to_save)) {
      stop("Provide a correct directory where to save data")
    }
  } else{
    stop("Directory for where_to_save is not given. Provide direcotry.")
  }

  # number of study to generate
  if(number!=1){
stop("Currently only work with 1. Please set number to 1")
  }
  #######################
  ######################
  all_xpt_files <- fs::dir_ls(path=path, glob = '*.xpt')
    avl_domains <- tools::file_path_sans_ext(basename(all_xpt_files))
  if('pooldef' %in% avl_domains){

    domains <-  c("bw","dm","ds","ex","lb","mi",
                  "ta","ts","tx","om","pooldef","pc")
  }else {

    domains <-  c("bw","dm","ds","ex","lb","mi",
                  "ta","ts","tx","om","pc")

  }
  if(!multi_study){
   Example  <- load_xpt_files(path,domains = domains)
    all_setcd <- list()
    get_data  <- filter_tk_rec(Example=Example,recovery=Recovery)
    Example <- get_data$data
    all_setcd[[1]] <- get_data$setcd
    Species <- Example$ts[TSPARMCD=='SPECIES',TSVAL]
# multi_study
  }else{
    all_setcd <- list()
    num_of_study <- length(path)
   Example  <- load_xpt_files(path[1],domains=domains)
    get_data  <- filter_tk_rec(Example=Example,recovery=Recovery)
    Example <- get_data$data
    all_setcd[[1]] <- get_data$setcd
    # remove tk group
    for(i in 2:num_of_study){
   exp_more  <- load_xpt_files(path[i],domains=domains)
    get_clean_data <- filter_tk_rec(Example=exp_more,recovery=Recovery)
    ## all_setcd <- get_clean_data$get_setcd_tk
    all_setcd[[i]] <- get_clean_data$setcd
    bind_to_exp <- get_clean_data$data
   Example$bw <- data.table::rbindlist(list(Example$bw, bind_to_exp$bw),
                                       fill = T,use.names = TRUE)
   Example$lb <- data.table::rbindlist(list(Example$lb, bind_to_exp$lb),
                                       fill = T,use.names = TRUE)
   Example$om <- data.table::rbindlist(list(Example$om, bind_to_exp$om),
                                       fill = T,use.names = TRUE)
   Example$mi <- data.table::rbindlist(list(Example$mi, bind_to_exp$mi),
                                       fill = T,use.names = TRUE)
   Example$ts <- data.table::rbindlist(list(Example$ts, bind_to_exp$ts),
                                       fill = T,use.names = TRUE)
   Example$tx <- data.table::rbindlist(list(Example$tx, bind_to_exp$tx),
                                       fill = T,use.names = TRUE)
   Example$dm <- data.table::rbindlist(list(Example$dm, bind_to_exp$dm),
                                       fill = T,use.names = TRUE)
   Example$ds <- data.table::rbindlist(list(Example$ds, bind_to_exp$ds),
                                       fill = T,use.names = TRUE)
}
    Species <- Example$ts[TSPARMCD=='SPECIES',TSVAL]
    print(Species)
    if (length(unique(Species)) >1){
      tab_pr <- Example$ts[Example$ts$TSPARMCD=="SPECIES",]
      print(tab_pr)
      stop(paste0("ERROR:Species are not the same between",
                  " SEND Example Studies. Pick one Species."))
    }
    #CHeck Study Type is the same
    SSTYP <- Example$ts[TSPARMCD=='SSTYP',TSVAL]
    ## SSTYP <- getFieldValue(Example$ts, "TSVAL", "TSPARMCD", "SSTYP")
    ## SSTYP <- getFieldValue(Example$ts, "TSVAL", "TSPARMCD", "SSTYP")
    if (length(unique(SSTYP)) >1){
      stop(paste0("ERROR:Study Types are not the same between",
                  " SEND Example Studies. Pick one SSTYP."))
    }
    #Check if SEND version is the same
    SNDIGVER <- Example$ts[TSPARMCD=='SNDIGVER',TSVAL]
    ## SNDIGVER <- getFieldValue(Example$ts,"TSVAL", "TSPARMCD", "SNDIGVER")
    if (length(unique(SNDIGVER)) >1){
      stop(paste0("ERROR:SEND versions are not the same between",
                  " SEND Example Studies. Pick one SNDIGVER."))
    }
  }
        # dose categorization
        #######################################################################
        tx_doses <- get_doses(Example$tx)
        ## treatment_doses <- tx_doses[SETCD %in% get_setcd[[1]][['treatment_group']]]
        treatment_doses <- tx_doses
        clean_dose <- SENDsanitizer:::clean_txval_dose(treatment_doses$TXVAL)
        trt <- data.table::copy(treatment_doses)
        trt$dose  <- clean_dose
        ## if(length(trt$dose) < 3) {
        study_numbers <- unique(Example$dm$STUDYID)
        ## }
        if(multi_study){
          first_study <- trt[STUDYID==study_numbers[1],]
          fs_cat <- dose_categorize(first_study)
          for(i in 2:length(study_numbers)){
            dose_cat <- dose_categorize(trt[STUDYID==study_numbers[i]])
            fs_cat  <- data.table::rbindlist(list(first_study,dose_cat))
            fs_cat
          }
          trt <- fs_cat
        }else{
          trt <- dose_categorize(trt)
        }
        # check
        if(!Recovery){
          if(length(unique(trt[cat=='Control',SETCD]))>1){
            print(trt)
            stop(paste0('There are more than one control group.',
                        ' This is probably a combination study.'))
          }
          if(length(unique(trt[cat=='LD',SETCD]))>1){
            stop(paste0('There are more than one LD group.',
                        ' This is probably a combination study.'))
          }
          if(length(unique(trt[cat=='HD',SETCD]))>1){
            stop(paste0('There are more than one HD group.',
                        ' This is probably a combination study.'))
          }
        } else {
          if(length(unique(trt[cat=='Control',SETCD]))>2){
            print(trt)
            stop(paste0('There are more than two control group.',
                        ' This is probably a combination study.'))
          }
          if(length(unique(trt[cat=='LD',SETCD]))>2){
            stop(paste0('There are more than two LD group.',
                        ' This is probably a combination study.'))
          }
          if(length(unique(trt[cat=='HD',SETCD]))>2){
            stop(paste0('There are more than two HD group.',
                  ' This is probably a combination study.'))
          }
        }

        if(Recovery){
          trt$trt_rc <- trt$cat
          trt[SETCD %in% all_setcd[[1]][[1]][['recovery_group']],
              `:=`(trt_rc=paste0(cat,'_Rec'))]
          trt[, `:=`(cat=trt_rc,trt_rc=NULL)]
        }

        if(!Recovery){
          high_num <- length(unique(trt$cat))
          trt <- trt[cat=='HD',`:=`(dose_order=high_num)]
          trt$dose_order <- as.character(trt$dose_order)
        }else {
          max_d <- max(trt$dose_order,na.rm = TRUE) +1
          trt <- trt[cat %in% c('HD','HD_Rec'),`:=`(dose_order=max_d)]
          trt$dose_order <- as.character(trt$dose_order)
          trt[SETCD %in% all_setcd[[1]][[1]][['recovery_group']],
              `:=`(dose_order=paste0(dose_order,'_R'))]
          ## trt[, `:=`(cat=trt_rc,trt_rc=NULL)]
        }
        ######################################################################
        Doses <- trt[,c('SETCD','cat')]
        Doses$ARMCD <- Doses$SETCD
        Doses$Dose <- Doses$cat

        ######################################################################
        ####################################################################
        #Correlate USUBJID with Dose Group
        Doses_m <- Doses[, c('ARMCD','Dose')]
        Doses_m <- Doses_m[!duplicated(Doses_m)]
        trt_m <- trt[,c('SETCD','cat','dose_order')]
        trt_m <- trt_m[!duplicated(trt_m)]

        Subjects <- merge(Example$dm[,c("USUBJID","ARMCD","SEX")], Doses_m,
                          by = "ARMCD")
        Subjects_2 <- merge(Example$dm[,c('USUBJID','SETCD','SEX','ARMCD','ARM')],
                            trt_m, by = 'SETCD')
        Subjects_2 <- Subjects_2[, c('USUBJID','SEX','ARMCD','ARM',
                                     'SETCD','cat','dose_order')]
        ## Subjects_2 <- Subjects_2[,c('USUBJID','SEX','SETCD','cat')]
        Subjects_2$Dose <- Subjects_2$cat
        Subjects_2$cat <- NULL

                                        #Consolidate Severity Methods
  Example$mi$MISEV <- as.character(Example$mi$MISEV)

  Example$mi$MISEV <- gsub("1 OF 5", "1",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("1 OF 4", "1",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("PRESENT", "1",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("MINIMAL", "1",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("2 OF 5", "2",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("MILD", "2",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("3 OF 5", "3",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("2 OF 4", "3",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("MODERATE", "3",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("4 OF 5", "4",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("3 OF 4", "4",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("MARKED", "4",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("5 OF 5", "5",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("4 OF 4", "5",Example$mi$MISEV)
  Example$mi$MISEV <- gsub("SEVERE", "5",Example$mi$MISEV)
ind <- which(Example$mi$MISEV=='')
 Example$mi$MISEV[ind] <- '0'
        Example$mi$MISEV <- ordered(Example$mi$MISEV,
                                    levels= c("0","1",
                                              "2", "3", "4","5"))
        #Set number of subjects to create based on Example(s)
  ## SubjectDet <- all_sub[, ]
  ## SubjectDet <- Subjects %>%
  ##         dplyr::group_by(Dose) %>%
  ##         dplyr::count(USUBJID) %>%
  ##         dplyr::mutate(sum = sum(n)) %>%
  ##         dplyr::select(-n)
  ## SubjectDet <- SubjectDet[!duplicated(SubjectDet$Dose),]
        GeneratedSEND <- list()
  ## print(GeneratedSEND)
  for (j in 1:number ){
    #Make SENDstudy length of one example study
    onestudy <- as.character(Example$dm$STUDYID[1])
    #Generate base for study to fill with proper SEND format
    SENDstudy <- list( 'dm' = data.frame(Example$dm[which(Example$dm$STUDYID == onestudy),]),
                      'bw' = data.frame(Example$bw[which(Example$bw$STUDYID == onestudy),]),
                      'lb' = data.frame(Example$lb[which(Example$lb$STUDYID == onestudy),]),
                      'om' = data.frame(Example$om[which(Example$om$STUDYID == onestudy),]),
                      'mi' = data.frame(Example$mi[which(Example$mi$STUDYID == onestudy),]),
                      'ts' = data.frame(Example$ts[which(Example$ts$STUDYID == onestudy),]),
                      'tx' = data.frame(Example$tx[which(Example$tx$STUDYID == onestudy),]))
    #Create StudyID and Compound Name for generated study
    studyID <- floor(stats::runif(1, min = 10000, max = 100000))
    Compound <- paste0("Fake-Drug ", floor(stats::runif(1, min = 1, max = 100000)))
#1
#ts
# ts is done
#Generate TS Data
#Keeps: Study design, GLP flag and type, duration, species, age, vehicle, dosing
            #duration Replaces: dates, study title, study facility, study
            #compound, primary treatment Removes: Study Director, Animal
            #Purchasing Location, and Test Facility Country

            #Replace StudyID
    SENDstudy$ts$STUDYID <- rep(studyID, nrow(SENDstudy$ts))
    #Find Date TSPARMCDs and replace
    daterows <- grep("DTC", SENDstudy$ts$TSPARMCD)
    SENDstudy$ts[daterows, "TSVAL"] <- rep("XXXX-XX-XX",length(daterows))
    #Replace Study Facility Name and Location
    rows <- grep("TSTF", SENDstudy$ts$TSPARMCD)
    SENDstudy$ts[rows, "TSVAL"] <- rep("FAKE FACILITY", length(rows))
    #Replace Study Compound/Primary Treatment CAS number, name, and unique ingredient ID
    rows <- grep("TRT",SENDstudy$ts$TSPARMCD)
    SENDstudy$ts[rows, "TSVAL"] <- rep(Compound, length(rows))
    if (NumData > 1){
      Vehicles <- Example$ts[grep("TRTV",Example$ts$TSPARMCD),"TSVAL"]
      print(Vehicles)
      str(Vehicles)
      #Remove any N/A or "NOT AVAILABLE"
      ## Vehicles <- Vehicles[grep('NOT AVAILABLE',Vehicles$TSVAL,ignore.case = T,invert = T)]
      ## Vehicles <- Vehicles[grep('NA',Vehicles$TSVAL,invert = T)]
      Vehicles <- Vehicles[which(stringr::str_detect(Vehicles,"NOT AVAILABLE") == FALSE)]
      Vehicles <- Vehicles[which(stringr::str_detect(Vehicles,"NA") == FALSE)]
      #Check if values are the same for vehicle and concatinate if not
      if (length(unique(Vehicles)) == 1){
        SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Vehicles
      } else {
        Vehicles <- paste(Vehicles, collapse = " / ")
        SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Vehicles
      }
    } else {
      SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Example$ts[grep("TRTV",
                                                                                  Example$ts$TSPARMCD),"TSVAL"]
    }

    #Replace Study Title
    rows <- grep("STITLE", SENDstudy$ts$TSPARMCD)
    duration <- getFieldValue(SENDstudy$ts, "TSVAL", "TSPARMCD", "DOSDUR")
    SENDstudy$ts[rows, "TSVAL"] <- paste0(Compound, ": A ",
                                          duration," Fake Study in ",
                                          unique(Species))
    #Clean up Vehicle
    idx <- which(grepl("TRTV",SENDstudy$ts$TSPARMCD)==TRUE)
    SENDstudy$ts[idx,"TSVAL"] <- paste0("VEHICLE")


    #Remove Identifying Information
    RemoveTerms <- c("TFCNTRY","STDIR","SPLRNAM","TFCNTRY",
                     "TRMSAC","SSPONSOR","SPREFID", "SPLRLOC",
                     "PINV","STMON","TSLOC","TSCNTRY","DIET","WATER",
                     "PCLASS","TSNAM")
    for (term in RemoveTerms){
      #Check index for Term
      idx <- which(SENDstudy$ts$TSPARMCD == term)
      #Remove Term
      SENDstudy$ts$TSVAL[idx] <- ""
    }
    print('TS DONE')
    #2
    #dm
    #done
#Generate DM Data
# dm done
    #Keeps: Number of each gender animals in each treatment group
    #Replaces: StudyID, USUBJID, Dates
    SENDstudy$dm <- SENDstudy$dm[which(SENDstudy$dm$USUBJID %in% Subjects_2$USUBJID),]
    #Find number of subjects in each group of each gender
    #attn
    # filter out from control of Subjects
            ## ControlAnimals <- SENDstudy$dm[which(SENDstudy$dm$ARMCD == 1),]
    control_animals <- Subjects_2[Subjects_2$Dose=='Control', c('USUBJID')]
    ControlAnimals <- SENDstudy$dm[SENDstudy$dm$USUBJID %in% control_animals$USUBJID,]
    Gendersplit <- table(ControlAnimals$SEX)
    #Replace StudyID
    SENDstudy$dm$STUDYID <- rep(studyID, nrow(SENDstudy$dm))
    #Generate new USUBJIDs using SBJID
    NEWUSUBJID <- paste0(studyID, "-" ,SENDstudy$dm$SUBJID)
    USUBJIDTable <- data.frame(USUBJID = SENDstudy$dm$USUBJID,
                               NEWUSUBJID = NEWUSUBJID)
    SENDstudy$dm$USUBJID <- NEWUSUBJID
    #Replace Dates
    cols <- grep("DTC", colnames(SENDstudy$dm))
    SENDstudy$dm[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$dm$STUDYID))
    #attn
    SENDstudy$dm$ARM <- as.character(SENDstudy$dm$ARM)
    # only first study
    onestudy <- as.character(Example$dm$STUDYID[1])
    trt_one_st <- trt[STUDYID==onestudy,c('SETCD','cat','dose_order')]
    dm2 <- merge(SENDstudy$dm,trt_one_st, by = 'SETCD')
    dm2 <- data.table::as.data.table(dm2)
    dm2 <- dm2[, `:=`(ARMCD=dose_order,ARM=cat,dose_order=NULL,cat=NULL)]
    SENDstudy$dm <- dm2
            #Make Factors Characters for Correct .xpt creation
            SENDstudy$dm$SEX <- as.character(SENDstudy$dm$SEX)
            SENDstudy$dm$AGEU <- as.character(SENDstudy$dm$AGEU)
    print('DM DONE')

    ## example subject
    ## this is generated arm have control and other group options
    ExampleSubjects <- SENDstudy$dm[,c("USUBJID", "ARM","SUBJID","SEX")]

#3
#tx
# tx done
    #Generate TX data
    #Keeps: SETCD
    #Replaces: SET
    #Replace StudyID
    SENDstudy$tx$STUDYID <- rep(studyID, nrow(SENDstudy$tx))

    tx_f <- data.table::copy(SENDstudy$tx)
    data.table::setDT(tx_f)
    uniq_setcd <- unique(tx_f$SETCD)
    # get only first study
    onestudy <- as.character(Example$dm$STUDYID[1])
    trt_one_study <- trt[STUDYID==onestudy,]
    for (i in 1:length(uniq_setcd)){
      setcd <- uniq_setcd[i]
      val <- trt_one_study[SETCD==setcd,dose_order]
      cat <- trt_one_study[SETCD==setcd,cat]

      tx_f[TXPARMCD=='ARMCD' & SETCD==setcd,`:=`(TXVAL=val)]
      tx_f[TXPARMCD=='GRPLBL' & SETCD==setcd,`:=`(TXVAL= paste0('Group: ',
                                                                val,
                                                                ', ' , cat))]
      tx_f[TXPARMCD=='SPGRPCD' & SETCD==setcd,`:=`(TXVAL=val)]
      tx_f[TXPARMCD=='TRTDOS' & SETCD==setcd,`:=`(TXVAL=cat)]
      ## tx_f[TXPARMCD=='TRTDOSU' & SETCD==setcd,`:=`(TXVAL=cat)]
    }
    # only return these parameter in TXPARMCD
    tx_f <- tx_f[TXPARMCD %in% c('ARMCD','GRPLBL',
                                 'SPGRPCD','TRTDOS','TRTDOSU'),]
    trt_mm <- trt[STUDYID==onestudy,c('SETCD','cat')]
    tx_new <- merge(tx_f,trt_mm, by = 'SETCD')
    tx_new <- data.table::as.data.table(tx_new)
    tx_new <- tx_new[, `:=`(SET=cat,cat=NULL)]
    SENDstudy$tx <- tx_new
    #Replace Factors with Characters
    SENDstudy$tx$SETCD <- as.character(SENDstudy$tx$SETCD)
    SENDstudy$tx$TXPARM <- as.character(SENDstudy$tx$TXPARM)
    SENDstudy$tx$TXPARMCD <- as.character(SENDstudy$tx$TXPARMCD)
    print('TX DONE')

#4
#bw
# bw done
    #Generates BW Data
    #Keeps: BWORRESU, BWTESTCD, BWSTRESU
    #Replaces: STUDYID, USUBJID, BWORRES, BWSTRESC, BWSTRESN, and BWDTC
    #Removes: BWBLFL

    #Account for TK discrepancies possible in BW
    # remove tk animals
    SENDstudy$bw <- SENDstudy$bw[which(SENDstudy$bw$USUBJID %in% Subjects_2$USUBJID),]

    #
    # copied from dm to see how usubjidtable created
    ## NEWUSUBJID <- paste0(studyID, "-" ,SENDstudy$dm$SUBJID)
    ## USUBJIDTable <- data.frame(USUBJID = SENDstudy$dm$USUBJID,
    ##                            NEWUSUBJID = NEWUSUBJID)
    ## SENDstudy$dm$USUBJID <- NEWUSUBJID
    #Add Generated StudyID and USUBJID
    SENDstudy$bw$STUDYID <- rep(studyID, nrow(SENDstudy$bw))
    SENDstudy$bw$USUBJID <- as.character(SENDstudy$bw$USUBJID)
    SENDstudy$bw <- merge( USUBJIDTable,SENDstudy$bw, by = "USUBJID")
    SENDstudy$bw <- SENDstudy$bw[,!(names(SENDstudy$bw) %in% "USUBJID")]
    names(SENDstudy$bw)[names(SENDstudy$bw) == "NEWUSUBJID"] <- "USUBJID"

    #Remove Dates
    cols <- grep("DTC", colnames(SENDstudy$bw))
    SENDstudy$bw[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$bw$STUDYID))

    #Remove BWBLFL

    ## SENDstudy$bw$BWBLFL <- NA
## Subjects <- Subjects_2
    #Find average weight behavior by dose and gender in Example
    ## BWFindings <- merge(Subjects,
    ##                     Example$bw[,c("USUBJID", "BWTESTCD",
    ##                                   "BWSTRESN","BWDY")], by = "USUBJID")

    # this all orginal study info
    # example$bw
    # copied from up just to see what Subjects_2 mean
    ## Subjects_2 <- merge(Example$dm[,c('USUBJID','SETCD','SEX','ARMCD','ARM')],
    ##                     trt_m, by = 'SETCD')
    ## Subjects_2 <- Subjects_2[, c('USUBJID','SEX','ARMCD','ARM',
    ##                              'SETCD','cat','dose_order')]
    ##
    ##
    ## ExampleSubjects <- SENDstudy$dm[,c("USUBJID", "ARM","SUBJID","SEX")]
    ## we are creating new study in SENDstudy$bw
    ## SENDstudy$ARM is from trt cat or control LD MD HD
    ##
    if(test_original){
      SENDstudy$bw$original <- SENDstudy$bw$BWSTRESN

    }
    onestudy <- as.character(Example$dm$STUDYID[1])
    trt_one <- trt[STUDYID==onestudy,]
    BWFindings <- merge(Subjects_2,
                          Example$bw[,c("USUBJID", "BWTESTCD",
                                        "BWSTRESN","BWDY")], by = "USUBJID")
    BWSummary <- BWFindings %>%
      dplyr::group_by(Dose, BWTESTCD,BWDY,SEX) %>%
                dplyr::mutate(ARMavg = mean(BWSTRESN, na.rm = TRUE)) %>%
      dplyr::mutate(ARMstdev = sd(BWSTRESN,na.rm = TRUE))
    # when there is only one value in group
    # sd is NA.
    # which later create warning in generdata and stdev
    #Make Model of weight using MCMCregress

    for (Dose in unique(trt_one$cat)){
      for (gender in unique(ExampleSubjects$SEX)){
        #Limit to proper gender subjects
        #this from SENDstudy$dm, fake studyid and fake subject id
        Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose &
                                               ExampleSubjects$SEX == gender)]

        # this `Sub` from original, Example$dm, if multiple studies then
        # subjects from multiple studies
        Sub <- Subjects_2$USUBJID[which(Subjects_2$Dose == Dose &
                                      Subjects_2$SEX == gender)]
        # original value for group dose and sex
        SubBWFindings <- BWFindings[which(BWFindings$USUBJID %in% Sub),]
        ## browser()
        line <- data.frame(BWSTRESN = SubBWFindings$BWSTRESN,
                           Day= SubBWFindings$BWDY,
                                     Dose = SubBWFindings$Dose)

        #Make model of weight over time per dose group
        uniq_spc <- unique(Species)
        if (uniq_spc %in% c("DOG", "MONKEY")){
          ## print('line 562')
          #Linear fit
          posterior <- MCMCpack::MCMCregress(BWSTRESN~Day,
                                             b0=0, B0 = 0.1, data = line)
        } else {
          #Log fit
          ## print('line 567')
          posterior <- MCMCpack::MCMCregress(log(BWSTRESN)~Day,
                                             b0=0, B0 = 0.1, data = line)
        }
                    #Sample model to fill in Example using Subjs
        #Per individual, sample from postieror and derive line for their response
        Fit <- sample(1:nrow(posterior), size=length(Subjs))
        sn <-1
        #Use that fit to generate new animal data
        for (Subj in Subjs){
                        BWDYs <-SENDstudy$bw$BWDY[which(SENDstudy$bw$USUBJID == Subj)]
                        SubFit <- Fit[sn]
                        #Calculate BWSTRESN per BWDY in Model
                        if (uniq_spc %in% c("DOG", "MONKEY")){
                          #Linear fit
                          GenerData <- data.frame(BWSTRESN = posterior[SubFit,1]+posterior[SubFit,2]*BWDYs,
                                                  BWDY = BWDYs)

                          ## (Intercept)         Day      sigma2
                          ## 2.683688382 0.002356868 0.024676656
                          ##  BWSTRESN <- intercept + Day * BWDYs
                        } else {
                          #Log Fit
                          GenerData <- data.frame(BWSTRESN = exp(posterior[SubFit,1]+posterior[SubFit,2]*BWDYs),
                                                  BWDY = BWDYs)
                        }
                        stdev <- unique(BWSummary$ARMstdev[which(BWSummary$Dose == Dose & BWSummary$SEX == gender)])
                        GenerData$BWSTRESN <- GenerData$BWSTRESN + stats::rnorm(length(GenerData$BWSTRESN),
                                                                                mean = 0, sd = (stdev/2))
                        #Fill into SENDstudy being generated
                        for (day in BWDYs){
                          idx <-which(SENDstudy$bw$USUBJID %in% Subj &
                                      SENDstudy$bw$BWDY %in% day)
                          idx2 <- which(GenerData$BWDY %in% day)
                          SENDstudy$bw$BWSTRESN[idx] <- round(GenerData$BWSTRESN[idx2],2)
                        }
                        sn <- sn+1
        }
      }
    }

    #Testing code to graph fit compared to average of that ARM
    #Convert all BW into data.frame
    Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == "HD" &
                                           ExampleSubjects$SEX == "M")]
    TEST <- SENDstudy$bw[which(SENDstudy$bw$USUBJID %in% Subjs),
                         c("BWDY","BWSTRESN","USUBJID")]

    #Make BWSTRESC and BWORRES match
    SENDstudy$bw$BWSTRESC <- as.character(SENDstudy$bw$BWSTRESN)
    SENDstudy$bw$BWORRES <- SENDstudy$bw$BWSTRESN
    SENDstudy$bw$BWORRESU <- SENDstudy$bw$BWSTRESU

    #Make Factors as Characters
    SENDstudy$bw$BWORRESU <- as.character(SENDstudy$bw$BWORRESU)
    SENDstudy$bw$BWSTRESU <- as.character(SENDstudy$bw$BWSTRESU)
    SENDstudy$bw$BWTESTCD <- as.character(SENDstudy$bw$BWTESTCD)
    SENDstudy$bw$BWTEST <- as.character(SENDstudy$bw$BWTEST)
    SENDstudy$bw$BWDY <- as.character(SENDstudy$bw$BWDY)
            if (any(grepl('VISITDY',colnames(SENDstudy$bw)) == TRUE)){
              SENDstudy$bw$VISITDY <- as.character(SENDstudy$bw$VISITDY)
            }
## View(SENDstudy$bw)
## print(head(Example$bw))
    if(test_original){
      df_bw <- data.table::as.data.table(SENDstudy$bw)
      df_bw <- df_bw[, c(names(df_bw)[!(names(df_bw) %in% c("original","BWSTRESN"))],
                         c( "original","BWSTRESN")), with = FALSE]
      df_bw <- df_bw[,`:=`(BWSTRESN_diff_pct= round((BWSTRESN - original)/original*100,digits = 2))]
      View(df_bw,'bw_test')
    }
    print('BW DONE')

#5
#lb
#
    # this all orginal study info
    # example$bw
    # copied from up just to see what Subjects_2 mean
    ## Subjects_2 <- merge(Example$dm[,c('USUBJID','SETCD','SEX','ARMCD','ARM')],
    ##                     trt_m, by = 'SETCD')
    ## Subjects_2 <- Subjects_2[, c('USUBJID','SEX','ARMCD','ARM',
    ##                              'SETCD','cat','dose_order')]
    ##
    ##
    ## ExampleSubjects <- SENDstudy$dm[,c("USUBJID", "ARM","SUBJID","SEX")]
    ## we are creating new study in SENDstudy$bw
    ## SENDstudy$ARM is from trt cat or control LD MD HD
######### Generates NUMERICAL LB Data #############
    #Keeps: DOMAIN, LBTESTs, LBTESTCD, LBDY, LBDY, LBCAT
    #Replaces: STUDYID, USUBJID, LBORRES, LBSTRESC, LBSTRESN, and LBDTC
    #Removes: LBBLFL
    #Account for TK discrepancies possible in LB
    SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjects_2$USUBJID),]
    #Replace StudyID and USUBJID
    SENDstudy$lb$STUDYID <- rep(studyID, nrow(SENDstudy$lb))
    SENDstudy$lb <- merge( USUBJIDTable,SENDstudy$lb, by = "USUBJID")
    SENDstudy$lb <- SENDstudy$lb[,!(names(SENDstudy$lb) %in% "USUBJID")]
    names(SENDstudy$lb)[names(SENDstudy$lb) == "NEWUSUBJID"] <- "USUBJID"

    #Remove Dates
    cols <- grep("DTC", colnames(SENDstudy$lb))
    SENDstudy$lb[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$lb$STUDYID))
    #Find out value range per treatment group
    LBFindings <- merge(Subjects_2, Example$lb[,c("USUBJID",
                                                "LBTESTCD","LBSPEC",
                                                "LBSTRESN","LBDY",
                                                "LBCAT")], by = "USUBJID")

# check lower case and other terms
    if ('CLINICAL CHEMISTRY' %in% unique(LBFindings$LBCAT)){

      ## LBFindings <- LBFindings %>% dplyr::filter(LBCAT=="CLINICAL CHEMISTRY")
      LBFindings <- LBFindings[LBFindings$LBCAT=="CLINICAL CHEMISTRY",]
      SENDstudy$lb <- SENDstudy$lb[SENDstudy$lb$LBCAT=='CLINICAL CHEMISTRY',]

    } else {
      stop('No observation for Clinical Chemistry in this study')
    }
    # remove test that have NA value for some result.
    # for BILI there is >0.8 or 0.1 value in study. so removed BILI

    na_testcd <- SENDstudy$lb[is.na(SENDstudy$lb$LBSTRESN),]
    na_testcd <- unique(na_testcd$LBTESTCD)
    if(length(na_testcd) > 0){

      for(code in 1:length(na_testcd)){
        sub_lb <- SENDstudy$lb[SENDstudy$lb$LBTESTCD== na_testcd[code],]
        if(any(grepl('<|>', sub_lb$LBSTRESC))){
    SENDstudy$lb <- SENDstudy$lb[!SENDstudy$lb$LBTESTCD %in% na_testcd[code],]
        }

      }

    }


    ## SENDstudy$lb <- SENDstudy$lb[!is.na(SENDstudy$lb$LBSTRESN),]
    uniq_lbtestcd_num <- unique(SENDstudy$lb$LBTESTCD)
    LBFindings <- LBFindings[LBFindings$LBTESTCD %in% uniq_lbtestcd_num,]
    ## LBFindings <- LBFindings %>% dplyr::filter(LBCAT=="CLINICAL CHEMISTRY",
    ##                                            LBTESTCD %in% c('CHOL',
    ##                                                            'GLDH',
    ##                                                            'BICARB'))
    ##
    LBSummary <- LBFindings %>%
      dplyr::group_by(Dose, LBTESTCD,LBDY,SEX) %>%
      dplyr::mutate(ARMavg = mean(LBSTRESN, na.rm = TRUE)) %>%
      dplyr::mutate(ARMstdev = sd(LBSTRESN,na.rm = TRUE))
    #Remove incomplete StudyTests
    LBSummary <- stats::na.omit(LBSummary)
    SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$LBTESTCD %in% LBSummary$LBTESTCD),]
    ## SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$LBSPEC %in% c('WHOLE BLOOD', 'SERUM', 'URINE')),]

    SENDstudy$lb$LBSTRESN_new <- NA
    SENDstudy$lb$LBSTRESN_org <- SENDstudy$lb$LBSTRESN
    #Create a distribution of values using MCMC for LBSTRESN

    if(lb_day_model){

  ##   for (Dose in unique(Doses$Dose)){
  ##     for (gender in unique(ExampleSubjects$SEX)){
  ##       ## print(paste0(Dose, " - ", gender))
  ##       Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose &
  ##                                              ExampleSubjects$SEX == gender)]
  ##       Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose &
  ##                                     Subjects$SEX == gender)]
  ##       GroupTests <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs),
  ##                                  c("LBTESTCD","LBSPEC")]
  ##       ## LBTESTCD LBSPEC
  ##       ##    UREAN  SERUM
  ##       ##     CHOL  SERUM
  ##       ##    CREAT  SERUM
  ##       ##       CL  SERUM
  ##       ##      GGT  SERUM
  ##       ##       CK  SERUM
  ##       for (lbspec in unique(GroupTests$LBSPEC)){
  ##         # SERUM | URINE
  ##         Days <- unique(LBSummary$LBDY[which(LBSummary$USUBJID %in% Sub &
  ##                                             LBSummary$LBSPEC %in% lbspec)])
  ##         ## ind <- which(is.na(LBSummary$LBSTRESN))
  ##         ## LBSummary <- LBSummary[-ind, ]
  ##         LBDATAs <- LBSummary[which(LBSummary$LBSPEC %in% lbspec),]
  ##         LBDATAs <- LBDATAs[which(LBDATAs$USUBJID %in% Sub),]
  ##         #Remove Tests that have a ARMstev of 0 (meaning they likely don't have enough data)
  ##         LBDATAs <- LBDATAs[which(LBDATAs$ARMstdev != 0),]
  ##         #Remove tests that do not have enough data (i.e. all days)
  ##         ## Testspread <- table(droplevels(LBDATAs$LBTESTCD), LBDATAs$LBDY)
  ##         Testspread <- table(LBDATAs$LBTESTCD, LBDATAs$LBDY)
  ##         rowsub <- apply(Testspread,1, function(row) all(row !=0))
  ##         highDataTests <- rownames(Testspread)[rowsub]
  ##         if (length(highDataTests) <= 1){
  ##           ToRemove <- which(GroupTests$LBSPEC %in% c(lbspec))
  ##           SENDstudy$lb <- SENDstudy$lb[-ToRemove,]
  ##           ## str(SENDstudy$lb)
  ##           next #Too little data; SKips loop
  ##         }

  ##         LBDATAs <- LBDATAs[which(LBDATAs$LBTESTCD %in% highDataTests),]
  ##         # how to make line with varying amount of variables
  ##         line <- data.frame(USUBJID= LBDATAs$USUBJID,
  ##                            LBSTRESN = LBDATAs$LBSTRESN,
  ##                            Day= LBDATAs$LBDY, LBTEST = LBDATAs$LBTESTCD)
  ##         line <- dplyr::distinct(line) #check for and remove duplicate rows
  ##         line <- stats::reshape(line, idvar = c("USUBJID","Day"),
  ##                                timevar = 'LBTEST', direction = "wide")
  ##         line <- sapply(line[,2:ncol(line)], as.numeric)
  ##         colnames(line) <- gsub("LBSTRESN.","",colnames(line))
  ##         line <- as.data.frame(line)
  ##         #Remove NA values (fit cannot have them)
  ##         line <- stats::na.omit(line)
  ##         no_col <- length(colnames(line))
  ##         no_row <- nrow(line)

  ##         ll <- unique(LBDATAs$LBTESTCD)
  ##         ll <- ll[which(!ll %in% 'GLOBUL')]
  ##         for (test in ll){

  ##           ## tryCatch({
  ##             Vars <- setdiff(colnames(line),c("Day",test))
  ##             if (length(Vars) > 10){
  ##               #limit Vars to 2 random variables for computation time
  ##               Vars <- sample(Vars, 2)
  ##               ## Vars <- c("SODIUM", "AST")
  ##             }
  ##       #Repeating fit PER test with interaction from other tests in that lbspec
  ##           ## if(lb_day_model){
  ##             equation <- paste0(Vars, sep= '*Day',collapse = " + ")
  ##           ## }else{

  ##           ##   equation <- paste0(Vars, sep= '',collapse = " + ")
  ##           ## }
  ##                                       #Make Fit
  ##             formula_lb <- stats::as.formula(paste0(test, " ~ ", equation))
  ##   ## LBfit <- MCMCpack::MCMCregress(as.formula(paste0(test, " ~ ",equation)),
  ##             ## b0=0, B0 = 0.1, data = line)
  ##             ##   print('761')
  ##             ## print(test)

  ##             LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0,
  ##                                            B0 = 0.1, data = line)
  ## ## LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0, B0 = 0.1, data = line)
  ##             #Sample Model 'Per Individual animal'
  ##             Fit <- sample(1:nrow(LBfit), size=length(Subjs))
  ##             ## print('done')
  ##             sn <-1
  ##             for (Subj in Subjs) {
  ##               LBFit <- LBfit[Fit[sn],]
  ##               #Make LBSTRESN Fit for that variable
  ##               DayVars <- which(grepl("Day",names(LBFit)) == TRUE)
  ##               #Find break between interaction variables and other variables
  ##               InteractionVars <- utils::tail(DayVars,length(DayVars)-1)
  ##               #Original Day Variable will be first found

  ##               #Make Equation Based on Varying length of Variables
  ##               # LBFit[1] is always the intercept
  ##               LBTESTVAR <- LBFit[1]
  ##               #Then it will be individual Variable coeff*their variables (including Day)
  ##               for (num in 2:(InteractionVars[1]-1)){
  ##                 testnm <- names(LBFit[num])
  ##                 LBTESTVAR <- LBTESTVAR + LBFit[num]*line[,testnm]
  ##               }
  ##               #Then interaction variables coeff * their variables will be added
  ##               for (num2 in InteractionVars){
  ##                 testnm <- unlist(strsplit(names(LBFit[num2]),":"))[2]
  ##                 LBTESTVAR <- LBTESTVAR + LBFit[num2]*line[,"Day"]*line[,testnm]
  ##               }
  ##               #Add Variance using stdev/rnorm
  ##               stdev <- unique(LBSummary[which(LBSummary$Dose == Dose &
  ##                                               LBSummary$SEX == gender &
  ##                                               LBSummary$LBTESTCD == test),
  ##                                         c('ARMstdev','LBDY')])
  ##               LBTESTVAR <- abs(LBTESTVAR + stats::rnorm(length(LBTESTVAR),
  ##                                                         mean = 0,
  ##                                                         sd = (stdev$ARMstdev)))
  ##               #Fill DataFrame to allocate to fake individual based on Day once variance is added
  ##               GenerLBData <- data.frame(LBSTRESN = 0,
  ##                                         LBDy = 0,
  ##                                         LBTESTCD = test)
  ##               avrgs <- unique(LBSummary[which(LBSummary$Dose == Dose &
  ##                                               LBSummary$SEX == gender &
  ##                                               LBSummary$LBTESTCD == test),
  ##                                         c('ARMavg','LBDY')])
  ##               #rein in values to the days
  ##               for (Dayz in Days){
  ##                 Val <- LBTESTVAR[which.min(abs(LBTESTVAR - avrgs$ARMavg[which(avrgs$LBDY == Dayz)]))]
  ##                 GenerLBData[nrow(GenerLBData)+1,] <- c(Val ,Dayz, test)
  ##               }
  ##               GenerLBData <- GenerLBData[2:nrow(GenerLBData),]
  ##               # For each day store the value in generated dataset SENDstudy
  ##               ## print('line 843')
  ##               for (day in Days){
  ##                 idx <-which(SENDstudy$lb$USUBJID %in% Subj &
  ##                             SENDstudy$lb$LBTESTCD %in% test &
  ##                             SENDstudy$lb$LBDY %in% day)
  ##                 idx2 <- which(GenerLBData$LBDy %in% day)

  ##                 SENDstudy$lb$LBSTRESN[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
  ##                 SENDstudy$lb$LBSTRESN_org[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
  ##               }
  ##               #add to subject count before new subject done
  ##               sn <- sn+1
  ##             }
  ##           ## }, error=function(e) {
  ##           ##   err_for <- paste0(test, " ~ ", equation)
  ##           ##   ## print(e)
  ##           ##   ## print(test)
  ##           ##   ## print(err_for)
  ##           ##                 ## print(line)
  ##           ## }
  ##           ## )
  ##         }
  ##       }
  ##     }
  ##   }
    } else{
      indx_dm <- unique(SENDstudy$lb$USUBJID)

      dm_dose_gender <- ExampleSubjects[ExampleSubjects$USUBJID %in% unique(SENDstudy$lb$USUBJID),]

            print(length(unique(SENDstudy$lb$USUBJID)))
            print(length(unique(dm_dose_gender$USUBJID)))
      for (Dose in unique(Doses$Dose)){
        for (gender in unique(ExampleSubjects$SEX)){

        ## print(paste0(Dose, " - ", gender))
        ## Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose &
        ##                                        ExampleSubjects$SEX == gender)]
          ## if(Dose == 'HD_Rec' & gender=='M'){
          ##   print('from_ inside____________')
          ##   print(length(unique(SENDstudy$lb$USUBJID)))
          ##   print(length(unique(dm_dose_gender$USUBJID)))

          ## }
          Subjs <- dm_dose_gender[dm_dose_gender$ARM== Dose &
                                  dm_dose_gender$SEX == gender,'USUBJID']
          Subjs <- unique(Subjs$USUBJID)
          Sub <- Subjects_2$USUBJID[which(Subjects_2$Dose == Dose &
                                          Subjects_2$SEX == gender)]
          GroupTests <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs),
                                     c("LBTESTCD","LBSPEC")]

        ## LBTESTCD LBSPEC
        ##    UREAN  SERUM
        ##     CHOL  SERUM
        ##    CREAT  SERUM
        ##       CL  SERUM
        ##      GGT  SERUM
        ##       CK  SERUM
        for (lbspec in unique(GroupTests$LBSPEC)){
          # SERUM | URINE
          Days <- unique(LBSummary$LBDY[which(LBSummary$USUBJID %in% Sub &
                                              LBSummary$LBSPEC %in% lbspec)])
          ## ind <- which(is.na(LBSummary$LBSTRESN))
          ## LBSummary <- LBSummary[-ind, ]
          LBDATAs <- LBSummary[which(LBSummary$LBSPEC %in% lbspec),]
          LBDATAs <- LBDATAs[which(LBDATAs$USUBJID %in% Sub),]
          #Remove Tests that have a ARMstev of 0 (meaning they likely don't have enough data)
          LBDATAs <- LBDATAs[which(LBDATAs$ARMstdev != 0),]
          #Remove tests that do not have enough data (i.e. all days)
          ## Testspread <- table(droplevels(LBDATAs$LBTESTCD), LBDATAs$LBDY)
          Testspread <- table(LBDATAs$LBTESTCD, LBDATAs$LBDY)
          rowsub <- apply(Testspread,1, function(row) all(row !=0))
          highDataTests <- rownames(Testspread)[rowsub]
          if (length(highDataTests) <= 1){

            ToRemove <- which(GroupTests$LBSPEC %in% c(lbspec))
            SENDstudy$lb <- SENDstudy$lb[-ToRemove,]
            ## str(SENDstudy$lb)
            next #Too little data; SKips loop
          }
          LBDATAs <- LBDATAs[which(LBDATAs$LBTESTCD %in% highDataTests),]
          # how to make line with varying amount of variables
          line <- data.frame(USUBJID= LBDATAs$USUBJID,
                             LBSTRESN = LBDATAs$LBSTRESN,
                             Day= LBDATAs$LBDY, LBTEST = LBDATAs$LBTESTCD)
          line <- dplyr::distinct(line) #check for and remove duplicate rows
          line <- stats::reshape(line, idvar = c("USUBJID","Day"),
                                 timevar = 'LBTEST', direction = "wide")
          line <- sapply(line[,2:ncol(line)], as.numeric)
          colnames(line) <- gsub("LBSTRESN.","",colnames(line))
          line <- as.data.frame(line)
          #Remove NA values (fit cannot have them)

          line <- line[, 2:length(colnames(line))]
          line <- stats::na.omit(line)
          line_mean <- colMeans(line)
          no_col <- length(colnames(line))
          no_row <- nrow(line)

          ll <- unique(LBDATAs$LBTESTCD)
          ## ll <- ll[which(!ll %in% 'GLOBUL')]
          for (test in ll){
            close_vars <- setdiff(names(line_mean), test)
            rest_line <- line_mean[names(line_mean) %in% close_vars]
            test_val <- line_mean[names(line_mean) %in% test]
            close_two <- names(sort(abs(rest_line - test_val))[1:2])

              if(length(close_vars)> 1){

                Vars <- close_two

              } else{ stop('Can\'t build MCMC model in LB')}

          ## if( Dose=='Control'&
          ##     gender=='M'&
          ##    test=='ALT'){
          ##  }
            ## tryCatch({
              ## Vars <- setdiff(colnames(line),c("Day",test))
              ## if (length(Vars) > 10){
              ##   Vars <- sample(Vars, 2)
              ## }
        #Repeating fit PER test with interaction from other tests in that lbspec

              equation <- paste0(Vars, sep= '',collapse = " + ")
              formula_lb <- stats::as.formula(paste0(test, " ~ ", equation))

            ## tryCatch({

              LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0,
                                             B0 = 0.1, data = line)
            ## },error=function(e){
            ##   print(e)

            ## }

            ## )
              #Sample Model 'Per Individual animal'
              Fit <- sample(1:nrow(LBfit), size=length(Subjs))
              ## print('done')
              sn <-1
              for (Subj in Subjs) {

                LBFit <- LBfit[Fit[sn],]
                LBTESTVAR <- LBFit[1]
                #Then it will be individual Variable coeff*their variables (including Day)
                #Then interaction variables coeff * their variables will be added
                #Add Variance using stdev/rnorm

                ## om_study <- SENDstudy$om[SENDstudy$om$OMTESTCD=='WEIGHT',
                ##                          c('USUBJID','OMSPEC','OMSTRESN')]
                lb_study <- SENDstudy$lb[SENDstudy$lb$USUBJID==Subj,]
                lb_var1 <- lb_study[lb_study$LBTESTCD == Vars[1],'LBSTRESN']
                lb_var2 <- lb_study[lb_study$LBTESTCD == Vars[2],'LBSTRESN']

                lb_val <- LBTESTVAR + LBFit[2] * lb_var1 + LBFit[3] * lb_var2
                ## if(length(lb_val) < 1){


                ## }
                ##                 if(is.na(lb_val)| lb_val==''){
                ## ##
##                 }
                stdev <- unique(LBSummary[which(LBSummary$Dose == Dose &
                                                LBSummary$SEX == gender &
                                                LBSummary$LBTESTCD == test),
                                          c('ARMstdev','LBDY')])

                ## print(paste0('stdev: ',stdev$ARMstdev))
                noise <- stats::rnorm(1,mean=0,sd=(stdev$ARMstdev))
                ## print(paste0('noise: ', noise))
                ## print(paste0('lb_val:', lb_val))
                ## LBTESTVAR <- abs(LBTESTVAR + stats::rnorm(length(LBTESTVAR),
                ##                                           mean = 0,
                ##                                           sd = (stdev$ARMstdev)))
                #Fill DataFrame to allocate to fake individual based on Day once variance is added
                final_val_lb <- lb_val + noise
                ## if(final_val_lb < 0 | is.na(final_val_lb)){
                ##   ## print(test)
                ##   ## ## print(final_val_lb)
                ##   ## print(noise)

                ## }
              ## print(final_val_lb)
                if(length(final_val_lb) < 1 ){
                  print('here')

                }
## print(final_val_lb)
                if(final_val_lb< 0){

                  get_pos_val <- function(LBfit,Subj,
                                          Vars,line,test,SENDstudy,LBSummary,
                                          Dose,gender){
                    LBFit <- LBfit[sample(1:nrow(LBfit), 1),]

                    LBTESTVAR <- LBFit[1]
                    lb_study <- SENDstudy$lb[SENDstudy$lb$USUBJID==Subj,]
                    lb_var1 <- lb_study[lb_study$LBTESTCD == Vars[1],'LBSTRESN']
                    lb_var2 <- lb_study[lb_study$LBTESTCD == Vars[2],'LBSTRESN']
                    lb_val <- LBTESTVAR + LBFit[2] * lb_var1 + LBFit[3] * lb_var2
                    stdev <- unique(LBSummary[which(LBSummary$Dose == Dose &
                                                    LBSummary$SEX == gender &
                                                    LBSummary$LBTESTCD == test),
                                              c('ARMstdev','LBDY')])

                    noise <- stats::rnorm(1,mean=0,sd=(stdev$ARMstdev))
                    final_val_lb <- lb_val + noise
                    final_val_lb
                  }


                  for (i in 1:10){
                  final_val_lb <- get_pos_val(LBfit,Subj,
                      Vars,line,test,SENDstudy,LBSummary,
                      Dose,gender)
                    ## final_val_lb <- get_pos_val(LBfit,om_study,Subj,Vars,line,test,SENDstudy)
                    if(final_val_lb> 0){
                      break}

                  }

                  if(final_val_lb < 0){
                    final_val_lb <- abs(final_val_lb)
                  }
                }
                ## final_val_lb <- abs(final_val_lb)

                ##                 if(length(final_val_lb==1)){
## print(test)
                ##                 }else{

                ##                 }
                GenerLBData <- data.frame(LBSTRESN = 0,
                                          LBDy = 0,
                                          LBTESTCD = test)
                ## avrgs <- unique(LBSummary[which(LBSummary$Dose == Dose &
                ##                                 LBSummary$SEX == gender &
                ##                                 LBSummary$LBTESTCD == test),
                ##                           c('ARMavg','LBDY')])
                #rein in values to the days
                ## for (Dayz in Days){
                ##   Val <- LBTESTVAR[which.min(abs(LBTESTVAR - avrgs$ARMavg[which(avrgs$LBDY == Dayz)]))]
                ##   GenerLBData[nrow(GenerLBData)+1,] <- c(Val ,Dayz, test)
                ## }
                ## GenerLBData <- GenerLBData[2:nrow(GenerLBData),]
                # For each day store the value in generated dataset SENDstudy
                ## print('line 843')

                for (day in Days){
                  idx <-which(SENDstudy$lb$USUBJID %in% Subj &
                              SENDstudy$lb$LBTESTCD %in% test &
                              SENDstudy$lb$LBDY %in% day)
                  idx2 <- which(GenerLBData$LBDy %in% day)

                  ## SENDstudy$lb$LBSTRESN[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                  ## SENDstudy$lb$LBSTRESN_new[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)

                  if(length(final_val_lb) < 1){

                  SENDstudy$lb$LBSTRESN_new[idx] <- NA
                  } else {
                  SENDstudy$lb$LBSTRESN_new[idx] <- round(as.numeric(final_val_lb), digits = 3)
                  }
                  ## SENDstudy$lb$LBSTRESN_new[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                  ## SENDstudy$lb$LBSTRESN_org[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                }
                #add to subject count before new subject done
                sn <- sn+1
              }
            ## }, error=function(e) {
            ##   err_for <- paste0(test, " ~ ", equation)
            ##   ## print(e)
            ##   ## print(test)
            ##   ## print(err_for)
            ##                 ## print(line)
            ## }
            ## )
          }
        }
      }
    }

    }
    #Testing code to graph fit compared to average of that ARM (HD, M, Selected LB tests)
    Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == "HD" &
                                           ExampleSubjects$SEX == "M")]
    TEST <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs),
                         c("LBDY","LBSTRESN","LBTESTCD","USUBJID")]

    if(test_original){
     df_lb <- data.table::as.data.table(SENDstudy$lb)
     df_lb <- df_lb[,`:=`(LBSTRESN_diff_pct = round(((LBSTRESN_new - LBSTRESN_org)/LBSTRESN_org)*100,digits = 2))]
     View(df_lb, title = 'generated lb for view')
    } else {
      SENDstudy$lb$LBSTRESN  <- SENDstudy$lb$LBSTRESN_new
      SENDstudy$lb$LBSTRESN_org <- NULL
      SENDstudy$lb$LBSTRESN_new <- NULL
    }
    #Coordinate LBORRES and LBSTRESC
    SENDstudy$lb$LBSTRESC <- as.character(SENDstudy$lb$LBSTRESN)
    SENDstudy$lb$LBORRESU <- SENDstudy$lb$LBSTRESU
    SENDstudy$lb$LBORRES <- SENDstudy$lb$LBSTRESN
    #Remove LBBLFL
    ## SENDstudy$lb$LBBLFL <- NA
    #Make Factors as Characters
    SENDstudy$lb$LBTESTCD <- as.character(SENDstudy$lb$LBTESTCD)
    SENDstudy$lb$LBTEST <- as.character(SENDstudy$lb$LBTEST)
    if (any(grepl('LBCAT',colnames(SENDstudy$lb)) == TRUE)){
      SENDstudy$lb$LBCAT <- as.character(SENDstudy$lb$LBCAT)
    }
    if (any(grepl('LBSCAT',colnames(SENDstudy$lb)) == TRUE)){
      SENDstudy$lb$LBSCAT <- as.character(SENDstudy$lb$LBSCAT)
    }
    SENDstudy$lb$LBMETHOD <- as.character(SENDstudy$lb$LBMETHOD)
    SENDstudy$lb$LBSPEC <- as.character(SENDstudy$lb$LBSPEC)
    SENDstudy$lb$LBSTRESU <- as.character(SENDstudy$lb$LBSTRESU)
    print('LB DONE')

#6
#OM
            ######### Generates NUMERICAL OM Data #############
## generate OM data
            SENDstudy$om <- SENDstudy$om[which(SENDstudy$om$USUBJID %in% Subjects$USUBJID),]
            SENDstudy$om$STUDYID <- rep(studyID, nrow(SENDstudy$om))
            SENDstudy$om <- merge( USUBJIDTable,SENDstudy$om, by = "USUBJID")
            SENDstudy$om <- SENDstudy$om[,!(names(SENDstudy$om) %in% "USUBJID")]
            names(SENDstudy$om)[names(SENDstudy$om) == "NEWUSUBJID"] <- "USUBJID"

            #Remove Dates
            cols <- grep("DTC", colnames(SENDstudy$om))
            SENDstudy$om[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$om$STUDYID))
    ## send_study_om <- SENDstudy$om
    ## example_om <- Example$om
## save(send_study_om,USUBJIDTable,example_om, file='om.rda')
    OMFindings <- merge(Subjects, Example$om[, c('USUBJID','OMTESTCD','OMSPEC',
                                             'OMSTRESN','OMDY')],
                    by='USUBJID')

    OMFindings <- OMFindings[OMFindings$OMTESTCD=='WEIGHT',]
    OMSummary <- OMFindings %>% dplyr::group_by(Dose,SEX,OMTESTCD,OMSPEC,OMDY) %>%
      dplyr::mutate(ARMavg=
                      mean(OMSTRESN, na.rm = TRUE)) %>%
      dplyr::mutate(ARMstdev= sd(OMSTRESN, na.rm = TRUE))

   OMSummary  <- stats::na.omit(OMSummary)

    SENDstudy$om$OMSTRESN_new <- NA
    SENDstudy$om$OMSTRESN_org <- SENDstudy$om$OMSTRESN
    om_study <- SENDstudy$om[SENDstudy$om$OMTESTCD=='WEIGHT',
                             c('USUBJID','OMSPEC','OMSTRESN')]
    for(Dose in unique(Doses$Dose)){
      for(gender in unique(ExampleSubjects$SEX)){

        ## ExampleSubjects <- SENDstudy$dm[,c("USUBJID", "ARM","SUBJID","SEX")]
        Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
        Subjs <- Subjs[which(Subjs %in% unique(om_study$USUBJID))]
        Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
        GroupTests <- SENDstudy$om[which(SENDstudy$om$USUBJID %in% Subjs), c("OMTESTCD","OMSPEC")]
        GroupTests <- GroupTests[GroupTests$OMTESTCD=='WEIGHT',]

        for(omspec in unique(GroupTests$OMTESTCD)){

          ## OMDATAs <- OMSummary[which(OMSummary$OMSPEC %in% omspec),]

          OMDATAs <- OMSummary[which(OMSummary$OMTESTCD=='WEIGHT'),]

          OMDATAs <- OMDATAs[which(OMDATAs$USUBJID %in% Sub),]
                                        #Remove Tests that have a ARMstev of 0 (meaning they likely don't have enough data)
          OMDATAs <- OMDATAs[which(OMDATAs$ARMstdev != 0),]
                                        #Remove tests that do not have enough data (i.e. all days)

          line <- data.frame(USUBJID= OMDATAs$USUBJID, OMSTRESN = OMDATAs$OMSTRESN,
                              OMTEST = OMDATAs$OMSPEC)
          line <- dplyr::distinct(line) #check for and remove duplicate rows
          line <- tidyr::pivot_wider(line, values_from = 'OMSTRESN',
                                     names_from = 'OMTEST')

          line <- line[, 2:length(colnames(line))]

          ## line <- sapply(line, as.numeric)
          ## colnames(line) <- gsub("OMSTRESN.","",colnames(line))
          ## line <- as.data.frame(line)
                                        #Remove NA values (fit cannot have them)
          line <- stats::na.omit(line)
          line_mean <- colMeans(line)
          ## no_col <- length(colnames(line))
          ## no_row <- nrow(line)
## browser()
          ll <- unique(OMDATAs$OMSPEC)

          for (test in ll){
            ## test <- 'BRAIN'
## line_mean
            close_vars <- setdiff(names(line_mean), test)
            rest_line <-  line_mean[names(line_mean) %in% close_vars]
            ## print(rest_line)

            test_val <- line_mean[names(line_mean) %in% test]
            ## print(test_val)
            close_two <- names(sort(abs(rest_line - test_val))[1:2])
            ## print(close_two)



            
            tryCatch({

              if(length(close_vars)> 1){

                Vars <- close_two

              } else{ stop('Can\'t build MCMC model in OM')}
              #Repeating fit PER test with interaction from other tests in that omspec
              ## equation <- paste0(Vars, collapse = " + ")
              kl <- paste0('`', paste0(Vars, collapse = '`+`'),'`')
              equation  <- paste0('`', test,'`' , ' ~ ', kl)
              formula_om <- stats::as.formula(equation)

                            OMfit <- MCMCpack::MCMCregress(formula = formula_om, b0=0, B0 = 0.1, data = line)
                            #Sample Model 'Per Individual animal'
              Fit <- sample(1:nrow(OMfit), size=length(Subjs))
              sn <-1
                            for (Subj in Subjs) {
                              OMFit <- OMfit[Fit[sn],]
                                        #Make OMSTRESN Fit for that variable
                              ## DayVars <- which(grepl("Day",names(OMFit)) == TRUE) #Find break between interaction variables and other variables
                              ## InteractionVars <- utils::tail(DayVars,length(DayVars)-1) #Original Day Variable will be first found
                                        #Make Equation Based on Varying length of Variables
                                        # OMFit[1] is always the intercept

                              sub_res <- om_study[om_study$USUBJID %in% Subj,]
                              OMTESTVAR <- OMFit[1]
                              intercept_val <- OMFit[1]
                                #Then it will be individual Variable coeff*their variables (including Day)

                              var1 <- sub_res[sub_res$OMSPEC==Vars[1], 'OMSTRESN']
                              var2 <- sub_res[sub_res$OMSPEC==Vars[2], 'OMSTRESN']
                             val <- intercept_val + OMFit[2] * var1 + OMFit[3] * var2

                              k <- as.data.frame(line[, test])
                              k_sd <- sd(k[,1])
                              noise <- stats::rnorm(1, mean=0, sd=k_sd)
                              final_val <- val + noise
                              indx <- which(SENDstudy$om$USUBJID==Subj &
                                            SENDstudy$om$OMTESTCD==omspec &
                                            SENDstudy$om$OMSPEC==test)

                              indx <- which(SENDstudy$om$USUBJID==Subj &
                                            SENDstudy$om$OMSPEC==test)
                              if(final_val< 0){
                                get_pos_val <- function(OMfit,om_study,Subj,Vars,line,test,SENDstudy){
                                  OMFit <- OMfit[sample(1:nrow(OMfit), 1),]
                                  sub_res <- om_study[om_study$USUBJID %in% Subj,]
                                  OMTESTVAR <- OMFit[1]
                                  intercept_val <- OMFit[1]
                                  var1 <- sub_res[sub_res$OMSPEC==Vars[1], 'OMSTRESN']
                                  var2 <- sub_res[sub_res$OMSPEC==Vars[2], 'OMSTRESN']
                                  val <- intercept_val + OMFit[2] * var1 + OMFit[3] * var2

                                  k <- as.data.frame(line[, test])
                                  k_sd <- sd(k[,1])
                                  noise <- stats::rnorm(1, mean=0, sd=k_sd)
                                  final_val <- val + noise
                                  indx <- which(SENDstudy$om$USUBJID==Subj &
                                                SENDstudy$om$OMTESTCD==omspec &
                                                SENDstudy$om$OMSPEC==test)
                                  indx <- which(SENDstudy$om$USUBJID==Subj &
                                                SENDstudy$om$OMSPEC==test)
                                  final_val
                                }


                                for (i in 1:10){

                                  final_val <- get_pos_val(OMfit,om_study,Subj,Vars,line,test,SENDstudy)
                                  if(final_val> 0){
                                    break}

                                }

                                if(final_val < 0){
                                  final_val <- abs(final_val)
                                }
                              }
                               SENDstudy$om[indx,'OMSTRESN_new'] <- final_val
                              SENDstudy$om[indx,'OMSTRESN_org'] <- final_val
                                #add to subject count before new subject done
                              sn <- sn+1
                            }
              ## }

            }, error=function(e) {

              print(e)
              ##             print(test)
              ## print(formula_om)
              ## print(line)
            }
                          )
          }
        }
      }
    }
  }


  # ratio
## browser()
om_df <- data.table::copy(SENDstudy$om)
data.table::setDT(om_df)

bw_df <- data.table::copy(SENDstudy$bw)
data.table::setDT(bw_df)

  usubs <- unique(om_df$USUBJID)
  df_term <- bw_df[USUBJID %in% usubs & BWTESTCD=='TERMBW']
  usubs <- unique(df_term$USUBJID)
  for (i in 1:length(usubs)){
    sub <- usubs[i]
bw_wgt <- bw_df[USUBJID==sub & BWTESTCD=='TERMBW', .(BWTESTCD,BWSTRESN,BWSTRESU)]
sub_bw <- bw_wgt[['BWSTRESN']]
sub_bw_u <- tolower(as.character(bw_wgt[['BWSTRESU']]))
    ## print(i)
    ## print(sub_bw_u)
    ## if(length(sub_bw_u)==0){
    ## browser()}
if (sub_bw_u == 'kg'){
  sub_bw <- sub_bw * 1000

}


sub_df <- om_df[USUBJID==sub,]
sub_testcd <- unique(sub_df[['OMTESTCD']])
organ_ratio <- grep('^OW|^BW', sub_testcd,value=T, ignore.case = T)
    for(testcd in 1:length(organ_ratio)){
      ratio <- organ_ratio[testcd]
      if(ratio=='OWBR'){
brain <- sub_df[OMSPEC=='BRAIN' & OMTESTCD=='WEIGHT', OMSTRESN]
om_df[USUBJID==sub & OMTESTCD=='OWBR', `:=`(new_c=(OMSTRESN_new/brain)*100)]

      }else if(ratio=='OWHT') {

heart <- sub_df[OMSPEC=='HEART' & OMTESTCD=='WEIGHT', OMSTRESN]
om_df[USUBJID==sub & OMTESTCD=='OWHT', `:=`(new_c=(OMSTRESN_new/heart)*100)]
      } else if(ratio=='OWBROB'){

bulb <- sub_df[OMSPEC=='BRAIN, OLFACTORY BULB' & OMTESTCD=='WEIGHT', OMSTRESN]
om_df[USUBJID==sub & OMTESTCD=='OWBROB', `:=`(new_c=(OMSTRESN_new/bulb)*100)]
      }
##       else if(ratio=='OWRATIO') {

## brain <- sub_df[OMSPEC=='BRAIN' & OMTESTCD=='WEIGHT', OMSTRESN]
## om_df[USUBJID==sub & OMTESTCD=='OWBR', `:=`(new_c=(OMSTRESN_new/brain)*100)]
##       }

      else if(ratio=='OWBW'){
        # what is value is in kg?

om_df[USUBJID==sub & OMTESTCD=='OWBW', `:=`(new_c=(OMSTRESN_new/sub_bw)*100)]

      }else if(ratio=='BWBR'){

brain <- sub_df[OMSPEC=='BRAIN' & OMTESTCD=='WEIGHT', OMSTRESN]
om_df[USUBJID==sub & OMTESTCD=='BWBR', `:=`(new_c=(sub_bw/brain)*100)]
      }
}
  }


  dd <- om_df[, c("STUDYID", "USUBJID","OMTESTCD", "OMSPEC", "OMSTRESN",
                  "OMSTRESN_new",'new_c', "OMSTRESU")]
  dk <- data.table::copy(dd)

  ## dk[, `:=`(dif=round(((abs(OMSTRESN-OMSTRESN_new))/OMSTRESN) * 100,3))]
  dk[, `:=`(dif= round((abs(OMSTRESN - OMSTRESN_new)/OMSTRESN)*100, 3))]

  ## dk[, `:=`(dif=round(((abs(OMSTRESN-OMSTRESN_new))/OMSTRESN) * 100,3))]
dl <-   dk[OMTESTCD=="WEIGHT", c(3,4,5,6,9)][order(OMTESTCD,OMSPEC)]
  kk <- dd[OMTESTCD=='WEIGHT' & OMSPEC %in% c('BRAIN','HEART','LIVER', 'KIDNEY')][order(OMSPEC)]
## have to change om_df to SENDstudy$om
##

# ratio calculation
    ## for(Dose in unique(Doses$Dose)){
    ##   for(gender in unique(ExampleSubjects$SEX)){

    ##     Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
    ##     Sub <- /Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
    ##     GroupTests <- SENDstudy$om[which(SENDstudy$om$USUBJID %in% Subjs), c("OMTESTCD","OMSPEC")]
    ##     GroupTests <- GroupTests[GroupTests$OMTESTCD %in% c('OWBR','OWHT'),]

    ##     for(omspec in unique(GroupTests$OMTESTCD)){


    ##       OMDATAs <- OMSummary[which(OMSummary$OMTESTCD=='WEIGHT'),]

    ##       OMDATAs <- OMDATAs[which(OMDATAs$USUBJID %in% Sub),]
    ##       OMDATAs <- OMDATAs[which(OMDATAs$ARMstdev != 0),]

    ##       line <- data.frame(USUBJID= OMDATAs$USUBJID, OMSTRESN = OMDATAs$OMSTRESN,
    ##                           OMTEST = OMDATAs$OMSPEC)
    ##       line <- dplyr::distinct(line) #check for and remove duplicate rows
    ##       line <- tidyr::pivot_wider(line, values_from = 'OMSTRESN',
    ##                                  names_from = 'OMTEST')

    ##       line <- line[, 2:length(colnames(line))]
    ##       line <- stats::na.omit(line)

    ##       ll <- unique(OMDATAs$OMSPEC)

    ##       for (test in ll){

    ##           Vars <- setdiff(colnames(line),test)
    ##                         if (length(Vars) > 10){ #limit Vars to 2 random variables for computation time
    ##                           Vars <- sample(Vars, 2)
    ##                         }

    ##           kl <- paste0('`', Vars[1],'`',' + ','`',  Vars[2],'`' )
    ##           equation  <- paste0('`', test,'`' , ' ~ ', kl)
    ##           formula_om <- stats::as.formula(equation)

    ##                         OMfit <- MCMCpack::MCMCregress(formula = formula_om, b0=0, B0 = 0.1, data = line)
    ##           Fit <- sample(1:nrow(OMfit), size=length(Subjs))
    ##           sn <-1
    ##                         for (Subj in Subjs) {
    ##                           print(Subj)
    ##                           sn <- sn+1
    ##                         }
    ##           ## }

    ##       }
    ##     }
    ##   }
    ## }

## print('done_loop')
  #Coordinate LBORRES and LBSTRESC
    SENDstudy$om$OMSTRESC <- as.character(SENDstudy$om$OMSTRESN)
    SENDstudy$om$OMORRESU <- SENDstudy$om$OMSTRESU
    SENDstudy$om$OMORRES <- SENDstudy$om$OMSTRESN

                                        #Remove OMBLFL
    ## SENDstudy$om$OMBLFL <- NA

                                        #Make Factors as Characters
    SENDstudy$om$OMTESTCD <- as.character(SENDstudy$om$OMTESTCD)
    SENDstudy$om$OMTEST <- as.character(SENDstudy$om$OMTEST)
    if (any(grepl('OMCAT',colnames(SENDstudy$om)) == TRUE)){
      SENDstudy$om$OMCAT <- as.character(SENDstudy$om$OMCAT)
    }
    if (any(grepl('OMSCAT',colnames(SENDstudy$om)) == TRUE)){
      SENDstudy$om$OMSCAT <- as.character(SENDstudy$om$OMSCAT)
    }
    ## SENDstudy$om$OMMETHOD <- as.character(SENDstudy$om$OMMETHOD)
    SENDstudy$om$OMSPEC <- as.character(SENDstudy$om$OMSPEC)
    SENDstudy$om$OMSTRESU <- as.character(SENDstudy$om$OMSTRESU)

  print('OM DONE')
#7
#MI
  #Generate MI Data
  #Keeps: MISPEC, MIDY, MISTESTCD, MITEST,
  #Replaces: STUDYID, USUBJID, MIDTC, MISTRESC, MIORRES, MISEV
                                        #Removes: MIREASND, MISPCCND,MISPCUFL, MIDTHREL and MIREASND


                                        #Replace StudyID and USUBJID
    SENDstudy$mi$STUDYID <- rep(studyID, nrow(SENDstudy$mi))
    SENDstudy$mi$USUBJID <- as.character(SENDstudy$mi$USUBJID)
            SENDstudy$mi$MIORRES <- as.character(SENDstudy$mi$MIORRES)
    SENDstudy$mi <- merge( USUBJIDTable,SENDstudy$mi, by = "USUBJID")
            SENDstudy$mi <- SENDstudy$mi[,!(names(SENDstudy$mi) %in% "USUBJID")]
            names(SENDstudy$mi)[names(SENDstudy$mi) == "NEWUSUBJID"] <- "USUBJID"
            #Remove Dates
            cols <- grep("DTC", colnames(SENDstudy$mi))
            SENDstudy$mi[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$mi$STUDYID))
            #Consolidate "Normal" Findings
            Example$mi$MISTRESC <- as.character(Example$mi$MISTRESC)
            SENDstudy$mi$MISTRESC <- as.character(SENDstudy$mi$MISTRESC)
            Example$mi$MISTRESC <- toupper(Example$mi$MISTRESC)
            Example$mi$MISTRESC <-  stringr::str_replace_all(Example$mi$MISTRESC, "NORMAL", "UNREMARKABLE")
            Example$mi$MISTRESC <-  stringr::str_replace_all(Example$mi$MISTRESC, "NAD", "UNREMARKABLE")
            Example$mi$MISTRESC <-  stringr::str_replace_all(Example$mi$MISTRESC, "NO ABNORMALITY DETECTED", "UNREMARKABLE")
            Example$mi$MISTRESC <-  stringr::str_replace_all(Example$mi$MISTRESC,"NO ABUNREMARKABLEITY DETECTED","UNREMARKABLE")
            #Find out investigated MISPECS and Frequency of Findings/Severity Range of Findings
            #Calculate percentage of Findings per MISPEC in each ARM

## browser()
            MIFindings <- merge(Subjects, Example$mi[,c("USUBJID", "MISPEC", "MISTRESC","MISEV")], by = "USUBJID")
            FindingsPercen <- MIFindings %>%
                dplyr::group_by(Dose, MISPEC,SEX) %>%
                dplyr::count(MISTRESC) %>%
                dplyr::mutate(percent = n/sum(n)) %>%
                dplyr::select(-n)
            SevPercen <- MIFindings %>%
                dplyr::group_by(Dose, MISPEC,SEX) %>%
                dplyr::count(MISEV) %>%
                dplyr::mutate(percent = n/sum(n)) %>%
                dplyr::select(-n)
            SENDstudy$mi$MIDY <- as.numeric(SENDstudy$mi$MIDY)

            #How to incorporate MCMC in mi....
                # Correspond across which organs... SEverity could be done by regression, but not MISPEC


            #Simulate MISPEC Results to create representative distributions of values
            for(Dose in unique(Doses$Dose)){
                for (gender in unique(ExampleSubjects$SEX)){
                    Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose  & ExampleSubjects$SEX == gender)]
                    GroupTests <- SENDstudy$mi$MISPEC[which(SENDstudy$mi$USUBJID %in% Subjs)]
                    for (Organ in unique(GroupTests)){
                        SevPerc <- SevPercen[which(SevPercen$Dose == Dose & SevPercen$MISPEC == Organ), c('MISEV','percent')]
                        FindPercen <- FindingsPercen[which(FindingsPercen$Dose == Dose & FindingsPercen$MISPEC == Organ), c('MISTRESC','percent')]
                        idx <- which(SENDstudy$mi$USUBJID %in% Subjs & SENDstudy$mi$MISPEC %in% Organ )
                        #Create Appropriate Findings Per MISPEC
                        FindGendata <- sample(FindPercen$MISTRESC,length(idx), replace = TRUE, prob = FindPercen$percent)
                        #Create Appropriate Severity per MISPEC
                        GenData <- data.frame('MISTRESC' = FindGendata,
                                              'MISEV'  = rep(NA, length(FindGendata)),
                                              'MIORRES' = rep(NA, length(FindGendata)))
                        lvls <- levels(Example$mi$MISEV)
                        GenData$MISEV <- factor(GenData$MISEV, levels = lvls)
                        for (i in 1:nrow(GenData)){
                            GenData$MIORRES[i] <- paste0(Organ, ": ", GenData$MISTRESC[i])
                            if (GenData$MISTRESC[i] %in% c("UNREMARKABLE",NA)){
                                GenData$MISEV[i] <- NA
                            } else {
                                Sevs <- as.data.frame(SevPerc)[which(is.na(SevPerc$MISEV) == FALSE),]
                                if (length(Sevs$MISEV)==0){
                                    #Check if all of the Findings have NA Severity
                                    GenData$MISEV[i] <- NA
                                }else{
                                    GenData$MISEV[i] <- sample(Sevs$MISEV,1,replace = TRUE, prob = Sevs$percent)
                                }
                            }
                        }
                        #Store Generated Values
                        SENDstudy$mi[idx,c("MISTRESC","MISEV","MIORRES")] <-GenData
                    }
                    #Make MIDY Appropriate
                    MIDY <- max(SENDstudy$mi[which(SENDstudy$mi$USUBJID %in% Subjs), "MIDY"], na.rm = TRUE)
                    SENDstudy$mi[which(SENDstudy$mi$USUBJID %in% Subjs), "MIDY"] <- MIDY
                }
            }
            #Make MIRESCAT for Non-Normal Findings
            SENDstudy$mi$MIRESCAT <- NA
            SENDstudy$mi$MIRESCAT[which(grepl("UNREMARKABLE",SENDstudy$mi$MISTRESC) == FALSE)] <- "NON-NEOPLASTIC"


            #Make Factors as Characters
            SENDstudy$mi$MITESTCD <- as.character(SENDstudy$mi$MITESTCD)
            SENDstudy$mi$MITEST <- as.character(SENDstudy$mi$MITEST)
            SENDstudy$mi$MISPEC <- as.character(SENDstudy$mi$MISPEC)
            SENDstudy$mi$MISTRESC <- as.character(SENDstudy$mi$MISTRESC)
            SENDstudy$mi$MISEV <- as.character(SENDstudy$mi$MISEV)
print('MI DONE')
  cat('\n \n \n \n ')
            ##### Save Generated Study for Tables ####
            GeneratedSEND[[j]] <- SENDstudy

  ############# Export Folder of Generated Data #################

  if(!test_original){
            if (!is.null(where_to_save)){
                #Create .xpt files if you can
                study_fake <- paste0('FAKE',studyID)
              path_save <- where_to_save
                dir_to_save <- fs::path(path_save,study_fake)
              fs::dir_create(dir_to_save)
                ## dir.create(dir_to_save) #Create Folder to Hold Study

                #Loop through domains created to print them in created folder
                ## browser()
                Domains <- names(SENDstudy)
              Domains <- setdiff(Domains, 'om')
                for (domain in Domains){
                  
                    ## printpath <- paste0(path,"/FAKE",studyID,"/",domain,".xpt")
                    printpath <- fs::path(dir_to_save, domain, ext= 'xpt')
                    ## haven::write_xpt(SENDstudy[[domain]],path = printpath, version = 5)

                  print(paste0('file saved in: ',printpath))
                }
            } else {

              stop(paste0('where to save is not given.',
                          ' Give a directory where to save xpt file and try agian'))


              ##   study_fake <- paste0('FAKE',studyID)
              ##   dir_to_save <- fs::path(path,study_fake)
              ## fs::dir_create(dir_to_save)
              ##   ## dir.create(dir_to_save) #Create Folder to Hold Study

              ##   #Loop through domains created to print them in created folder
              ##   Domains <- names(SENDstudy)
              ##   for (domain in Domains){
              ##       ## printpath <- paste0(path,"/FAKE",studyID,"/",domain,".xpt")
              ##       printpath <- fs::path(dir_to_save, domain, ext= 'xpt')
              ##       haven::write_xpt(SENDstudy[[domain]],path = printpath, version = 5)
              ##     print(paste0('file saved in: ',printpath))
                ## }


            }

  }
        }
