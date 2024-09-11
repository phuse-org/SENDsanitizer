generate_lb <- function(lb_original, dm_fake,Subjects_2) {
SENDstudy$lb <- lb_original
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
    LBFindings <- merge(Subjects, Example$lb[,c("USUBJID",
                                                "LBTESTCD","LBSPEC",
                                                "LBSTRESN","LBDY",
                                                "LBCAT")], by = "USUBJID")
# check lower case and other terms
    if ('CLINICAL CHEMISTRY' %in% unique(LBFindings$LBCAT)){

      LBFindings <- LBFindings %>% dplyr::filter(LBCAT=="CLINICAL CHEMISTRY")
    } else {
      stop('No observation for Clinical Chemistry in this study')
    }
    LBSummary <- LBFindings %>%
      dplyr::group_by(Dose, LBTESTCD,LBDY,SEX) %>%
      dplyr::mutate(ARMavg = mean(LBSTRESN, na.rm = TRUE)) %>%
      dplyr::mutate(ARMstdev = sd(LBSTRESN,na.rm = TRUE))
    #Remove incomplete StudyTests
    LBSummary <- stats::na.omit(LBSummary)
    SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$LBTESTCD %in% LBSummary$LBTESTCD),]

    SENDstudy$lb$LBSTRESN_new <- NA
    SENDstudy$lb$LBSTRESN_org <- SENDstudy$lb$LBSTRESN
    #Create a distribution of values using MCMC for LBSTRESN
    for (Dose in unique(Doses$Dose)){
      for (gender in unique(ExampleSubjects$SEX)){
        ## print(paste0(Dose, " - ", gender))
        Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose &
                                               ExampleSubjects$SEX == gender)]
        Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose &
                                      Subjects$SEX == gender)]
        GroupTests <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs),
                                   c("LBTESTCD","LBSPEC")]
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
          line <- stats::na.omit(line)
          no_col <- length(colnames(line))
          no_row <- nrow(line)

          ll <- unique(LBDATAs$LBTESTCD)
          ll <- ll[which(!ll %in% 'GLOBUL')]
          for (test in ll){

            ## tryCatch({
              Vars <- setdiff(colnames(line),c("Day",test))
              if (length(Vars) > 10){
                #limit Vars to 2 random variables for computation time
                Vars <- sample(Vars, 2)
                ## Vars <- c("SODIUM", "AST")
              }
        #Repeating fit PER test with interaction from other tests in that lbspec
              equation <- paste0(Vars, sep= '*Day',collapse = " + ")
                                        #Make Fit
              formula_lb <- stats::as.formula(paste0(test, " ~ ", equation))
    ## LBfit <- MCMCpack::MCMCregress(as.formula(paste0(test, " ~ ",equation)),
              ## b0=0, B0 = 0.1, data = line)
              ##   print('761')
              ## print(test)

              LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0,
                                             B0 = 0.1, data = line)
  ## LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0, B0 = 0.1, data = line)
              #Sample Model 'Per Individual animal'
              Fit <- sample(1:nrow(LBfit), size=length(Subjs))
              ## print('done')
              sn <-1
              for (Subj in Subjs) {
                LBFit <- LBfit[Fit[sn],]
                #Make LBSTRESN Fit for that variable
                DayVars <- which(grepl("Day",names(LBFit)) == TRUE)
                #Find break between interaction variables and other variables
                InteractionVars <- utils::tail(DayVars,length(DayVars)-1)
                #Original Day Variable will be first found

                #Make Equation Based on Varying length of Variables
                # LBFit[1] is always the intercept
                LBTESTVAR <- LBFit[1]
                #Then it will be individual Variable coeff*their variables (including Day)
                for (num in 2:(InteractionVars[1]-1)){
                  testnm <- names(LBFit[num])
                  LBTESTVAR <- LBTESTVAR + LBFit[num]*line[,testnm]
                }
                #Then interaction variables coeff * their variables will be added
                for (num2 in InteractionVars){
                  testnm <- unlist(strsplit(names(LBFit[num2]),":"))[2]
                  LBTESTVAR <- LBTESTVAR + LBFit[num2]*line[,"Day"]*line[,testnm]
                }
                #Add Variance using stdev/rnorm
                stdev <- unique(LBSummary[which(LBSummary$Dose == Dose &
                                                LBSummary$SEX == gender &
                                                LBSummary$LBTESTCD == test),
                                          c('ARMstdev','LBDY')])
                LBTESTVAR <- abs(LBTESTVAR + stats::rnorm(length(LBTESTVAR),
                                                          mean = 0,
                                                          sd = (stdev$ARMstdev)))
                #Fill DataFrame to allocate to fake individual based on Day once variance is added
                GenerLBData <- data.frame(LBSTRESN = 0,
                                          LBDy = 0,
                                          LBTESTCD = test)
                avrgs <- unique(LBSummary[which(LBSummary$Dose == Dose &
                                                LBSummary$SEX == gender &
                                                LBSummary$LBTESTCD == test),
                                          c('ARMavg','LBDY')])
                #rein in values to the days
                for (Dayz in Days){
                  Val <- LBTESTVAR[which.min(abs(LBTESTVAR - avrgs$ARMavg[which(avrgs$LBDY == Dayz)]))]
                  GenerLBData[nrow(GenerLBData)+1,] <- c(Val ,Dayz, test)
                }
                GenerLBData <- GenerLBData[2:nrow(GenerLBData),]
                # For each day store the value in generated dataset SENDstudy
                ## print('line 843')
                for (day in Days){
                  idx <-which(SENDstudy$lb$USUBJID %in% Subj &
                              SENDstudy$lb$LBTESTCD %in% test &
                              SENDstudy$lb$LBDY %in% day)
                  idx2 <- which(GenerLBData$LBDy %in% day)

                  ## SENDstudy$lb$LBSTRESN[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                  SENDstudy$lb$LBSTRESN_new[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                  SENDstudy$lb$LBSTRESN_org[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                }
                #add to subject count before new subject done
                sn <- sn+1
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
}
