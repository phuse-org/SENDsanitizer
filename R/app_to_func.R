# library(fs)
# library(dplyr)
# library(bayesplot)
# library(haven)
# library(Hmisc)
# library(MCMCpack)
# ## library(shiny)
# ## library(shinyFiles)
# ## library(shinyWidgets)
# library(stringr)
# library(this.path)
# library(tidyr)
# library(tools)

#' @title Run function
#' @param path Mandatory\cr
#' path where real data located, should be directory
#' @param number mandatory, default 1\cr
#'   how many studies to generate
#' @param recovery optional\cr
#' recovery
#' @param where_to_save optional\cr
#' if no directory path given, fake/generate study data will be save
#' directory given in path argument
#' @export

#' @import dplyr
#' @import ggplot2
#' @import bayesplot
#' @import haven
#' @import Hmisc
#' @import MCMCpack
#' @import stringr
#' @import tidyr
#' @import tools
#' @import fs
#' @import RSQLite
#' @import DBI
#' @import data.table
#' @importFrom utils head
# what will happen when visitday not present but dsnomdy present


sanitize <- function(path, number=1, recovery=FALSE,
                     where_to_save=NULL) {

        number  <- as.numeric(number)
        print(paste0("study to generate: ", as.character(number)))
        PRINT <- FALSE
        Recovery <- recovery
        ExampleStudies <- path
        print('study path provided')
        print(ExampleStudies)
        NumData <- length(ExampleStudies)
        print(NumData)



        ## Make Loop for Loading in the SEND Data per Example Study
  for (i in 1:NumData){
            Name <- paste0('ExampleStudy',as.character(i))
            assign(Name,load.xpt.files(ExampleStudies[i]))
            print(Name)
        }
        Domains <- c("bw","dm","ds","ex","lb","mi","ta","ts","tx")


        #Check that Example Studies are Similar and Consolidate
        if (NumData > 1){
            #Generate Names of number of Example Study and concatenate

          Example <- ExampleStudy1
          ## Example$dm <- Example$dm[, dm_col]

            for (j in 2:NumData){
                Name <- paste0('ExampleStudy',as.character(j))
                #Combine BW, DM, DS, EX, LB, MI, TA, TS, and TX

                Example$bw <- data.table::rbindlist(list(Example$bw, get(Name)$bw),fill = T,use.names = TRUE)
                Example$dm <- data.table::rbindlist(list(Example$dm, get(Name)$dm),fill = T,use.names = TRUE)
                Example$ds <- data.table::rbindlist(list(Example$ds, get(Name)$ds),fill = T,use.names = TRUE)
                Example$ex <- data.table::rbindlist(list(Example$ex, get(Name)$ex),fill = T,use.names = TRUE)
                Example$lb <- data.table::rbindlist(list(Example$lb, get(Name)$lb),fill = T,use.names = TRUE)
                Example$mi <- data.table::rbindlist(list(Example$mi, get(Name)$mi),fill = T,use.names = TRUE)
                Example$ta <- data.table::rbindlist(list(Example$ta, get(Name)$ta),fill = T,use.names = TRUE)
                Example$ts <- data.table::rbindlist(list(Example$ts, get(Name)$ts),fill = T,use.names = TRUE)
                Example$tx <- data.table::rbindlist(list(Example$tx, get(Name)$tx),fill = T,use.names = TRUE)

            }
          ## print(Example['bw'])

            #remove unused domains
            Example <- Example[Domains]
            #Check Species are the same
            Species <- getFieldValue(Example$ts, "TSVAL", "TSPARMCD", "SPECIES")
          print(Species)
            if (length(unique(Species)) >1){
              tab_pr <- Example$ts[Example$ts$TSPARMCD=="SPECIES",]
              print(tab_pr)
                stop("ERROR:Species are not the same between SEND Example Studies. Pick one Species.")
            }
            #CHeck Study Type is the same
            SSTYP <- getFieldValue(Example$ts, "TSVAL", "TSPARMCD", "SSTYP")
            if (length(unique(SSTYP)) >1){
                stop("ERROR:Study Types are not the same between SEND Example Studies. Pick one SSTYP.")
            }
            #Check if SEND version is the same
            SNDIGVER <- getFieldValue(Example$ts,"TSVAL", "TSPARMCD", "SNDIGVER")
            if (length(unique(SNDIGVER)) >1){
                stop("ERROR:SEND versions are not the same between SEND Example Studies. Pick one SNDIGVER.")
            }



            #Remove TK SETCDs
            Example$dm <- Example$dm[which(grepl("TK",Example$dm$SETCD) ==FALSE),]
            Example$ta <- Example$ta[which(grepl("Toxicokinetic", Example$ta$ARM) == FALSE),]
            Example$tx <- Example$tx[which(Example$tx$SETCD %in% Example$dm$SETCD),]
            Example$lb <- Example$lb[which(Example$lb$USUBJID %in% Example$dm$USUBJID),]
            Example$mi <- Example$mi[which(Example$mi$USUBJID %in% Example$dm$USUBJID),]
            Example$bw <- Example$bw[which(Example$bw$USUBJID %in% Example$dm$USUBJID),]
            Example$ex <- Example$ex[which(Example$ex$USUBJID %in% Example$dm$USUBJID),]
            Example$ta <- Example$ta[which(Example$ta$ARMCD %in% Example$dm$ARMCD),]

            #Check Dose Levels are equivalent
            if (Recovery == FALSE){
                #Remove Recovery Dose ARMCDs
                doses <- Example$ta[which(grepl("R",Example$ta$ARMCD) == FALSE),c("STUDYID","ARMCD")]
                doses$ARMCD <- as.character(doses$ARMCD)
            }else {
                doses <- Example$ta[,c("STUDYID","ARMCD")]
                doses$ARMCD <- as.character(doses$ARMCD)
            }
            DoseTable <- table(doses)
            if (any(DoseTable == 0)){
              ind <- which(DoseTable==0)
          dd <- data.frame(DoseTable)
          studyid_remove <- unique(dd[which(dd$Freq==0),c('STUDYID')])
            print('please remove following studyid')
              print(studyid_remove)
              print('ERROR:ARMCD line 147 ')
                stop("ERROR:ARMCD for Dosing is not equavalent between SEND Example Studies. Try removing Recovery Animals.")
            }

            ##Create ARMCD and DOSE Correlation
            Doses <-Example$ta[,c("ARMCD","ARM")]
            Doses <- Doses[!duplicated(Doses$ARM),]
            Doses$Dose <- NA


} else {

            #remove unused domains
            Example <- ExampleStudy1 #First study loaded will always be ExampleStudy1
            Example <- Example[Domains]

            #Create ARMCD and DOSE Correlation
            Doses <-data.frame("ARMCD" = as.character(unique(Example$ta$ARMCD)),
                               "Dose" = as.character(unique(Example$ta$ARM)))


}


        #Get SEND Species, LB TESTCDs and MI Tests from Example Study
        Species <- unique(getFieldValue(Example$ts, "TSVAL", "TSPARMCD", "SPECIES"))
      LBTestCDs <- unique(Example$lb$LBTESTCD)
        MITests <- unique(Example$mi$MISPEC)

        #Find out Animal USUBJIDs for Control and Treated Animals
        if (Recovery == FALSE){
            #Remove Recovery Dose ARMCDs/Doses
            Doses <- Doses[which(grepl("R",Doses$ARMCD) == FALSE),]
        }
        #Replace Dose Levels with Control, LD, MD and HD
        ARMS <- unique(Doses$ARMCD) #Find ARMS levels
        if (Recovery == TRUE){
            #Make MaxDose and Account for "R" doses
            NonrecovArm <- ARMS[which(grepl("R",ARMS) == FALSE)]
            Maxdose <- max(as.numeric(as.character(NonrecovArm)))
            Doses$Dose[which(Doses$ARMCD=="1R")] <- "Control R"
            Doses$Dose[which(Doses$ARMCD=="2R")] <- "LD R"
            Doses$Dose[which(Doses$ARMCD== paste0(Maxdose,"R"))] <- "HD R"
        } else {
            Maxdose <- max(as.numeric(as.character(ARMS)))
        }
        Doses$Dose[which(Doses$ARMCD=="1")] <- "Control"
        Doses$Dose[which(Doses$ARMCD==Maxdose)] <- "HD"
        Doses$Dose[which(Doses$ARMCD=="2")] <- "LD"
        if (length(ARMS) > 3){
            for (i in  3:(max(as.numeric(ARMS)) - 1)){
                j <- i-2
                if (j == 1){
                    Doses$Dose[Doses$ARMCD== as.character(i)] <- "MD"
                } else {
                    Doses$Dose[Doses$ARMCD== as.character(i)] <- paste0("MD", j)
                }
            }
        }

        Doses <- Doses[, c("ARMCD","Dose")]
        Doses <- Doses[!duplicated(Doses),]
        #Correlate USUBJID with Dose Group
        Subjects <- merge(Example$dm[,c("USUBJID","ARMCD","SEX")], Doses[, c("ARMCD","Dose")], by = "ARMCD")

        #Double Check Recovery Coding
        RecoveryAnimals <- Example$ds$USUBJID[which(grepl("Recovery",Example$ds$DSTERM) == TRUE)]
        if (Recovery == FALSE){
         if (length(RecoveryAnimals)==0){

         } else if (identical(integer(0),which(Subjects$USUBJID %in% RecoveryAnimals)) == TRUE) {

         }
             else {
             Subjects <- Subjects[-which(Subjects$USUBJID %in% RecoveryAnimals),]
         }
        }
        #Consolidate Severity Methods
        Example$mi$MISEV <- as.character(Example$mi$MISEV)
        Example$mi$MISEV <- str_replace_all(Example$mi$MISEV, "1 OF 5", "1")
        Example$mi$MISEV <- str_replace_all(Example$mi$MISEV, "1 OF 4", "1")
        Example$mi$MISEV <- str_replace_all(Example$mi$MISEV, "PRESENT", "1")
        Example$mi$MISEV <- str_replace_all(Example$mi$MISEV, "MINIMAL", "1")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "2 OF 5", "2")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "MILD", "2")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "3 OF 5", "3")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "2 OF 4", "3")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "MODERATE", "3")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "4 OF 5", "4")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "3 OF 4", "4")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "MARKED", "4")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "5 OF 5", "5")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "4 OF 4", "5")
        Example$mi$MISEV <-  str_replace_all(Example$mi$MISEV, "SEVERE", "5")
        Example$mi$MISEV <- replace_na(Example$mi$MISEV, "0")
        Example$mi$MISEV <- ordered(Example$mi$MISEV, levels= c("0","1", "2", "3", "4","5"))


        #Set number of subjects to create based on Example(s)
        SubjectDet <- Subjects %>%
            group_by(Dose) %>%
            count(USUBJID) %>%
            dplyr::mutate(sum = sum(n)) %>%
            dplyr::select(-n)
        SubjectDet <- SubjectDet[!duplicated(SubjectDet$Dose),]
        GeneratedSEND <- list()

print(GeneratedSEND)

   for (j in 1:number ){
            #Make SENDstudy length of one example study
            onestudy <- as.character(Example$dm$STUDYID[1])
            #Generate base for study to fill with proper SEND format
            SENDstudy <- list( 'dm' = data.frame(Example$dm[which(Example$dm$STUDYID == onestudy),]),
                               'bw' = data.frame(Example$bw[which(Example$bw$STUDYID == onestudy),]),
                               'ds' = data.frame(Example$ds[which(Example$ds$STUDYID == onestudy),]),
                               'ex' = data.frame(Example$ex[which(Example$ex$STUDYID == onestudy),]),
                               'lb' = data.frame(Example$lb[which(Example$lb$STUDYID == onestudy),]),
                               'mi' = data.frame(Example$mi[which(Example$mi$STUDYID == onestudy),]),
                               'ta' = data.frame(Example$ta[which(Example$ta$STUDYID == onestudy),]),
                               'ts' = data.frame(Example$ts[which(Example$ts$STUDYID == onestudy),]),
                               'tx' = data.frame(Example$tx[which(Example$tx$STUDYID == onestudy),]))

            #Create StudyID and Compound Name for generated study
            studyID <- floor(runif(1, min = 10000, max = 100000))
            Compound <- paste0("Fake-Drug ", floor(runif(1, min = 1, max = 100000)))

            #Generate TS Data
            #Keeps: Study design, GLP flag and type, duration, species, age, vehicle, dosing duration
            #Replaces: dates, study title, study facility, study compound, primary treatment
            #Removes: Study Director, Animal Purchasing Location, and Test Facility Country

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
                #Remove any N/A or "NOT AVAILABLE"
                Vehicles <- Vehicles[which(str_detect(Vehicles,"NOT AVAILABLE") == FALSE)]
                Vehicles <- Vehicles[which(str_detect(Vehicles,"NA") == FALSE)]
                #Check if values are the same for vehicle and concatinate if not
                if (length(unique(Vehicles)) == 1){
                    SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Vehicles
                } else {
                    Vehicles <- paste(Vehicles, collapse = " / ")
                    SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Vehicles
                }
            } else {
                SENDstudy$ts[grep("TRTV",SENDstudy$ts$TSPARMCD),"TSVAL"] <- Example$ts[grep("TRTV",Example$ts$TSPARMCD),"TSVAL"]
            }

            #Replace Study Title
            rows <- grep("STITLE", SENDstudy$ts$TSPARMCD)
            duration <- getFieldValue(SENDstudy$ts, "TSVAL", "TSPARMCD", "DOSDUR")
            SENDstudy$ts[rows, "TSVAL"] <- paste0(Compound, ": A ",  duration," Fake Study in ",
                                                  Species)

            #Clean up Vehicle
            idx <- which(grepl("TRTV",SENDstudy$ts$TSPARMCD)==TRUE)
            SENDstudy$ts[idx,"TSVAL"] <- paste0("VEHICLE")

            #Remove Identifying Information
            RemoveTerms <- c("TFCNTRY","STDIR","SPLRNAM","TFCNTRY","TRMSAC","SSPONSOR","SPREFID", "SPLRLOC",
                             "PINV","STMON","TSLOC","TSCNTRY","DIET","WATER", "PCLASS")
            for (term in RemoveTerms){
                #Check index for Term
                idx <- which(SENDstudy$ts$TSPARMCD == term)
                #Remove Term
                SENDstudy$ts$TSVAL[idx] <- ""
            }

            #Generate TA Data
            #Keeps: EPOCH, ELEMEND, ETCD, TAETORD, DOMAIN
            #Replaces: StudyID, ARM

            #Replace StudyID
            SENDstudy$ta$STUDYID <- rep(studyID, nrow(SENDstudy$ta))

            #Replace ARM
            SENDstudy$ta$ARM <- as.character(SENDstudy$ta$ARM)
            if (Recovery == FALSE){
                SENDstudy$ta <- SENDstudy$ta[which(grepl("R",SENDstudy$ta$ARMCD) == FALSE),]
            }
            for (arms in unique(as.character(SENDstudy$ta$ARMCD))){
                row <- which(arms == Doses$ARMCD)
                Dosein <- unique(Doses$Dose[row])
                rows <- which(arms == SENDstudy$ta$ARMCD)
                SENDstudy$ta[rows,"ARM"] <- rep(Dosein, length(rows))
            }

            #Generate DM Data
            #Keeps: Number of each gender animals in each treatment group
            #Replaces: StudyID, USUBJID, Dates

            #Account for discrepancies possible in dm with recovery
            SENDstudy$dm <- SENDstudy$dm[which(SENDstudy$dm$USUBJID %in% Subjects$USUBJID),] ## MAY BE CAUSING PROBLEMS WITH ZYT-779

            #Find number of subjects in each group of each gender
            ControlAnimals <- SENDstudy$dm[which(SENDstudy$dm$ARMCD == 1),]
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

            #Replace ARM
            SENDstudy$dm$ARM <- as.character(SENDstudy$dm$ARM)
            if (Recovery == FALSE){
                SENDstudy$dm <- SENDstudy$dm[which(grepl("R",SENDstudy$dm$ARMCD) == FALSE),]
            }
            for (arms in unique(SENDstudy$dm$ARMCD)){
                row <- which(arms == Doses$ARMCD)
                Dosein <- unique(Doses$Dose[row])
                rows <- which(arms == SENDstudy$dm$ARMCD)
                SENDstudy$dm[rows,"ARM"] <- rep(Dosein, length(rows))
            }
            ExampleSubjects <- SENDstudy$dm[,c("USUBJID", "ARM","SUBJID","SEX")]

            #Make Factors Characters for Correct .xpt creation
            SENDstudy$dm$SEX <- as.character(SENDstudy$dm$SEX)
            SENDstudy$dm$AGEU <- as.character(SENDstudy$dm$AGEU)

            #Generate DS Data
            #Keeps: VISITDY
            #Replaces: StudyID, USUBJID, USUBJID, Dates

            #Account for discrepancies possible in ds
            SENDstudy$ds <- SENDstudy$ds[which(SENDstudy$ds$USUBJID %in% Subjects$USUBJID),]

            #Account for VISITDY for Recovery Animals
            if (Recovery == FALSE){
                SENDstudy$ds <- SENDstudy$ds[which(grepl("Recovery",SENDstudy$ds$DSTERM) == FALSE),]
            }

            #ADD Generated USUJIDs and terminal VISITDY to new DS
            SENDstudy$ds$USUBJID <- SENDstudy$dm$USUBJID

            #Calculate percentage of Terminal Sacrifice in each ARM
            TerminalSac <- merge(Example$ds, Subjects, by = "USUBJID")
            TerminalSac <- TerminalSac %>%
                group_by(Dose) %>%
                count(DSDECOD) %>%
                dplyr::mutate(percent = n/sum(n)) %>%
                dplyr::select(-n)

            #Replace StudyID
            SENDstudy$ds$STUDYID <- rep(studyID, nrow(SENDstudy$ds))

            #Replace Dates
            cols <- grep("DTC", colnames(SENDstudy$ds))
            SENDstudy$ds[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$ds$STUDYID))

            #Make Generated DS Terminal Sacrifice percentage match expectation
            for(Dose in unique(Doses$Dose)){
                Percen <- TerminalSac[which(TerminalSac$Dose == Dose & TerminalSac$DSDECOD == 'TERMINAL SACRIFICE'), 'percent']
                Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose)]
                Gendata <- sample(c("TERMINAL SACRIFICE", "FOUND DEAD"),length(Subjs), replace = TRUE, prob = c(Percen, (1-Percen)))
                #Add in Randomized Data to Subjects in Each Group
                SENDstudy$ds[which(SENDstudy$ds$USUBJID %in% Subjs), "DSDECOD"] <- Gendata
                #Make VISITDY Appropriate
                if('VISITDY' %in% colnames(SENDstudy$ds)){

                VISTDY <- max(SENDstudy$ds[which(SENDstudy$ds$USUBJID %in% Subjs), "VISITDY"], na.rm = TRUE)
                SENDstudy$ds[which(SENDstudy$ds$USUBJID %in% Subjs), "VISITDY"] <- VISTDY

            SENDstudy$ds <-SENDstudy$ds %>%
                mutate(DSTERM = ifelse(DSDECOD == 'FOUND DEAD','Found Dead','Terminal necropsy')) %>%
                mutate(VISITDY = ifelse(DSDECOD == 'FOUND DEAD',NA,VISITDY))
                }
            }

            #Ensure DSTERM/VISITDY is APPROPRIATE

            #Generate TX data
            #Keeps: SETCD
            #Replaces: SET

            #Replace StudyID
            SENDstudy$tx$STUDYID <- rep(studyID, nrow(SENDstudy$tx))

            #Account for Recovery Animals
            if (Recovery == FALSE){
                SENDstudy$tx <- SENDstudy$tx[which(grepl("R",SENDstudy$tx$SETCD) == FALSE),]
            }

            #Replace SET with Blinded Notation
            ARMS <- getFieldValue(SENDstudy$tx,'TXVAL','TXPARM', 'Arm Code')
            SETS <- SENDstudy$tx$SET[which(SENDstudy$tx$TXPARM == 'Arm Code')]
            ARMtoset <- data.frame('Arm' = ARMS, 'Set' = SETS)
            SENDstudy$tx$TXVAL <- as.character(SENDstudy$tx$TXVAL)
            SENDstudy$tx$SET <- as.character(SENDstudy$tx$SET)
            for (i in 1:nrow(ARMtoset)){
                ARMCD <- as.character(ARMtoset$Arm[i])
                SETCD <- as.character(ARMtoset$Set[i])
                DoseCover <- Doses$Dose[which(Doses$ARMCD == ARMCD)]
                if(identical(DoseCover, character(0)) == TRUE){
                DoseCover <- ""
                }
                SENDstudy$tx$TXVAL[which(grepl(SETCD,SENDstudy$tx$TXVAL)== TRUE)] <- unique(DoseCover)
                SENDstudy$tx$SET[which(SENDstudy$tx$SET == SETCD)] <- unique(DoseCover)
            }
            #Replace Factors with Characters
            SENDstudy$tx$SETCD <- as.character(SENDstudy$tx$SETCD)
            SENDstudy$tx$TXPARM <- as.character(SENDstudy$tx$TXPARM)
            SENDstudy$tx$TXPARMCD <- as.character(SENDstudy$tx$TXPARMCD)

            #Remove Identifying Group Names
            idx <- which(grepl("GRPLBL",SENDstudy$tx$TXPARMCD) == TRUE)
            SENDstudy$tx$TXVAL[idx] <- paste0("GROUP: ", SENDstudy$tx$SET[idx])

            #Generate EX data
            #Keeps: ESDOSFRM, EXDOSFRQ, EXROUTE, EXTRTV, EXSTDY, EXDOSEU, EXVATMU, EXSTDY
            #Replaces: EXTRT, EXLOT, EXSTDTC, EXVAMT, EXDOSE, USUBJID, STUDYID

            #ADD Generated USUBJID, EXTRT name, and STUDYID
            SENDstudy$ex$STUDYID <- rep(studyID, nrow(SENDstudy$ex))
            SENDstudy$ex$EXTRT <- rep(Compound, length(SENDstudy$ex$EXTRT))
            #Remove Recovery if Needed
            if (Recovery == FALSE){
                NonRecovSub <- Example$dm$USUBJID[which(grepl("R",Example$dm$ARMCD) == FALSE)]
                SENDstudy$ex <- SENDstudy$ex[which(SENDstudy$ex$USUBJID %in% NonRecovSub),]
            }
            SENDstudy$ex$USUBJID <- as.character(SENDstudy$ex$USUBJID)
            SENDstudy$ex <- merge( USUBJIDTable,SENDstudy$ex, by = "USUBJID")
            SENDstudy$ex <- SENDstudy$ex[,!(names(SENDstudy$ex) %in% "USUBJID")]
            names(SENDstudy$ex)[names(SENDstudy$ex) == "NEWUSUBJID"] <- "USUBJID"

            #Generate Fake EXLOT
            Lotnum <- length(levels(SENDstudy$ex$EXLOT))
            Lot <- paste0("Fake", floor(runif(Lotnum, min = 1, max = 100000)))
            for (i in 1:Lotnum){
                levels(SENDstudy$ex$EXLOT)[i] <-Lot[i]
            }

            #Remove Dates
            cols <- grep("DTC", colnames(SENDstudy$ex))
            SENDstudy$ex[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$ex$STUDYID))

            #Find Distribution of Dose to Vehicle (EXDOSE) to (EXVAMT)
            Dist <- SENDstudy$ex$EXDOSE/SENDstudy$ex$EXVAMT
            #Generate EXDOSE and EXVAMT Numbers based on expectation
            Gen <- round(runif(length(Dist),min=min(SENDstudy$ex$EXVAMT), max = max(SENDstudy$ex$EXVAMT)),2)
            SENDstudy$ex$EXVAMT <- Gen
            SENDstudy$ex$EXDOSE <- Gen*Dist

            #Make Factors as Characters
            SENDstudy$ex$EXDOSU <- as.character(SENDstudy$ex$EXDOSU)
            SENDstudy$ex$EXROUTE <- as.character(SENDstudy$ex$EXROUTE)
            SENDstudy$ex$EXVAMTU <- as.character(SENDstudy$ex$EXVAMTU)
            SENDstudy$ex$EXTRTV <- as.character(SENDstudy$ex$EXTRTV)
            SENDstudy$ex$EXDOSFRQ <- as.character(SENDstudy$ex$EXDOSFRQ)
            SENDstudy$ex$EXDOSFRM <- as.character(SENDstudy$ex$EXDOSFRM)

            #Remove Identifying Terms
            RemoveTerms <- c("SPLRNAM","SSPONSOR","SPREFID", "SPLRLOC")
            for (term in RemoveTerms){
                #Check index for Term
                idx <- which(SENDstudy$tx$TXPARMCD == term)
                #Remove Term
                SENDstudy$tx$TXVAL[idx] <- ""
            }


            #Generates BW Data
            #Keeps: BWORRESU, BWTESTCD, BWSTRESU
            #Replaces: STUDYID, USUBJID, BWORRES, BWSTRESC, BWSTRESN, and BWDTC
            #Removes: BWBLFL

            #Account for TK discrepancies possible in BW
            SENDstudy$bw <- SENDstudy$bw[which(SENDstudy$bw$USUBJID %in% Subjects$USUBJID),]

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

            #Find average weight behavior by dose and gender in Example
            BWFindings <- merge(Subjects, Example$bw[,c("USUBJID", "BWTESTCD", "BWSTRESN","BWDY")], by = "USUBJID")
            BWSummary <- BWFindings %>%
                group_by(Dose, BWTESTCD,BWDY,SEX) %>%
                mutate(ARMavg = mean(BWSTRESN, na.rm = TRUE)) %>%
                mutate(ARMstdev = sd(BWSTRESN,na.rm = TRUE))

            #Make Model of weight using MCMCregress
            for (Dose in unique(Doses$Dose)){
                for (gender in unique(ExampleSubjects$SEX)){
                    #Limit to proper gender subjects
                    Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
                    Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
                    SubBWFindings <- BWFindings[which(BWFindings$USUBJID %in% Sub),]
                    line <- data.frame(BWSTRESN = SubBWFindings$BWSTRESN, Day= SubBWFindings$BWDY, Dose = SubBWFindings$Dose)
                    #Make model of weight over time per dose group
                    if (Species %in% c("DOG", "MONKEY")){
                      ## print('line 562')
                        #Linear fit
                        posterior <- MCMCregress(BWSTRESN~Day, b0=0, B0 = 0.1, data = line)
                    } else {
                        #Log fit
                      ## print('line 567')
                        posterior <- MCMCregress(log(BWSTRESN)~Day, b0=0, B0 = 0.1, data = line)
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
                        if (Species %in% c("DOG", "MONKEY")){
                            #Linear fit
                            GenerData <- data.frame(BWSTRESN = posterior[SubFit,1]+posterior[SubFit,2]*BWDYs,
                                                    BWDY = BWDYs)
                        } else {
                            #Log Fit
                            GenerData <- data.frame(BWSTRESN = exp(posterior[SubFit,1]+posterior[SubFit,2]*BWDYs),
                                                    BWDY = BWDYs)
                        }
                        #Add in noise to fit
                        stdev <- unique(BWSummary$ARMstdev[which(BWSummary$Dose == Dose & BWSummary$SEX == gender)])
                        GenerData$BWSTRESN <- GenerData$BWSTRESN + rnorm(length(GenerData$BWSTRESN), mean = 0, sd = (stdev/2))
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
            Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == "HD" & ExampleSubjects$SEX == "M")]
             TEST <- SENDstudy$bw[which(SENDstudy$bw$USUBJID %in% Subjs), c("BWDY","BWSTRESN","USUBJID")]
            #Add Average of BWSummary
              ## TEST <- merge(TEST, unique(BWSummary[which(BWSummary$Dose == "HD" & BWSummary$SEX == "M"), c("BWDY","ARMavg")]), by = c("BWDY"))
              ## p <- ggplot2::ggplot(data= TEST, aes(x=BWDY,y = BWSTRESN, group=USUBJID, color = "Simulated Animal Data"))+ geom_line()+
              ##       geom_line(aes(y = ARMavg, label="Average of Source Data", color = "Average of Source Data")) +
              ##       ggtitle("HD M Weight Distribution Comparison")+
              ##        labs(x='BWDY (Days)', y="Weight") + scale_color_manual(name = "Legend",
              ##                                                               values = c("darkred","steelblue"),
              ##                                                               breaks = c("Simulated Animal Data",
              ##                                                                          "Average of Source Data"))
              ## print(p)

            #Generate New Data Based on Example >>> rnorm method option, replaces MCMCregress with more averaged values
            # for (Dose in unique(Doses$Dose)){
            #     for (gender in unique(ExampleSubjects$SEX)){
            #         Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
            #         Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
            #         GroupTests <- SENDstudy$bw$BWTESTCD[which(SENDstudy$bw$USUBJID %in% Subjs)]
            #         for (Test in unique(GroupTests)){
            #             Days <- unique(BWSummary$BWDY[which(BWSummary$USUBJID %in% Sub & BWSummary$BWTESTCD %in% Test)])
            #             for (day in Days){
            #                 #make indexs
            #                 idx <-which(SENDstudy$bw$USUBJID %in% Subjs & SENDstudy$bw$BWTESTCD %in% Test &
            #                                 SENDstudy$bw$BWDY %in% day)
            #                 idxs <-which(BWSummary$USUBJID %in% Sub & BWSummary$BWTESTCD %in% Test &
            #                                  BWSummary$BWDY %in% day)
            #                 #Filter Group mean and stdev
            #                 Testavg <- unique(BWSummary$ARMavg[idxs])
            #                 Teststdev <- unique(BWSummary$ARMstdev[idxs])
            #                 #use rnorm() to generate data
            #                 GenerData <- suppressWarnings(round(rnorm(n=length(idx), mean = Testavg, sd = Teststdev),2))
            #                 #Fill tests properly with created values
            #                 SENDstudy$bw$BWSTRESN[idx] <- GenerData
            #             }
            #         }
            #     }
            # }

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

            #Generates NUMERICAL LB Data
            #Keeps: DOMAIN, LBTESTs, LBTESTCD, LBDY, LBDY, LBCAT
            #Replaces: STUDYID, USUBJID, LBORRES, LBSTRESC, LBSTRESN, and LBDTC
            #Removes: LBBLFL

            #Account for TK discrepancies possible in LB
            SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjects$USUBJID),]

            #Replace StudyID and USUBJID
            SENDstudy$lb$STUDYID <- rep(studyID, nrow(SENDstudy$lb))
            SENDstudy$lb <- merge( USUBJIDTable,SENDstudy$lb, by = "USUBJID")
            SENDstudy$lb <- SENDstudy$lb[,!(names(SENDstudy$lb) %in% "USUBJID")]
            names(SENDstudy$lb)[names(SENDstudy$lb) == "NEWUSUBJID"] <- "USUBJID"

            #Remove Dates
            cols <- grep("DTC", colnames(SENDstudy$lb))
            SENDstudy$lb[,cols] <- rep("XXXX-XX-XX",length(SENDstudy$lb$STUDYID))
            #Find out value range per treatment group
            LBFindings <- merge(Subjects, Example$lb[,c("USUBJID", "LBTESTCD", "LBSPEC", "LBSTRESN","LBDY","LBCAT")], by = "USUBJID")
     LBFindings <- LBFindings %>% dplyr::filter(LBCAT=="CLINICAL CHEMISTRY")
     ## LBFindings <- LBFindings %>% dplyr::filter(LBCAT=="CLINICAL CHEMISTRY",
     ##                                            LBTESTCD %in% c('CHOL',
     ##                                                            'GLDH',
     ##                                                            'BICARB'))
            LBSummary <- LBFindings %>%
                group_by(Dose, LBTESTCD,LBDY,SEX) %>%
                mutate(ARMavg = mean(LBSTRESN, na.rm = TRUE)) %>%
                mutate(ARMstdev = sd(LBSTRESN,na.rm = TRUE))
            #Remove incomplete StudyTests
            LBSummary <- na.omit(LBSummary)
            SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$LBTESTCD %in% LBSummary$LBTESTCD),]
            SENDstudy$lb <- SENDstudy$lb[which(SENDstudy$lb$LBSPEC %in% c('WHOLE BLOOD', 'SERUM', 'URINE')),]

            #Create a distribution of values using MCMC for LBSTRESN
     for (Dose in unique(Doses$Dose)){
                for (gender in unique(ExampleSubjects$SEX)){
                  ## print(paste0(Dose, " - ", gender))
                    Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
                    Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
                    GroupTests <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs), c("LBTESTCD","LBSPEC")]
                    for (lbspec in unique(GroupTests$LBSPEC)){
                        Days <- unique(LBSummary$LBDY[which(LBSummary$USUBJID %in% Sub & LBSummary$LBSPEC %in% lbspec)])
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
                           next #Too little data; SKips loop
                        }
                        LBDATAs <- LBDATAs[which(LBDATAs$LBTESTCD %in% highDataTests),]
                        # how to make line with varying amount of variables
                        line <- data.frame(USUBJID= LBDATAs$USUBJID, LBSTRESN = LBDATAs$LBSTRESN, Day= LBDATAs$LBDY, LBTEST = LBDATAs$LBTESTCD)
                        line <- distinct(line) #check for and remove duplicate rows
                        line <- reshape(line, idvar = c("USUBJID","Day"), timevar = 'LBTEST', direction = "wide")
                        line <- sapply(line[,2:ncol(line)], as.numeric)
                        colnames(line) <- gsub("LBSTRESN.","",colnames(line))
                        line <- as.data.frame(line)
                        #Remove NA values (fit cannot have them)
                        line <- na.omit(line)
                        no_col <- length(colnames(line))
                        no_row <- nrow(line)

                        ## print(Dose)
                        ## if (no_col > no_row) {
                        ##   print('__________________________________')

                        ##   print(Dose)
                        ##   print('there are more columns than rows')
                        ##   print('**********************************')


                        ## }
                        ## tryCatch({

                        ll <- unique(LBDATAs$LBTESTCD)
                        ll <- ll[which(!ll %in% 'GLOBUL')]
                        print(ll)

                        for (test in ll){
                          print(test)

                          tryCatch({
                            Vars <- setdiff(colnames(line),c("Day",test))
                            if (length(Vars) > 10){ #limit Vars to 2 random variables for computation time
                                Vars <- sample(Vars, 2)
                            }
                            #Repeating fit PER test with interaction from other tests in that lbspec

                            equation <- paste0(Vars, sep= '*Day',collapse = " + ")
                            #Error test
                            ## print(paste0(lbspec, " - ", test ))
                                        #Make Fit

                        ## if (no_col > no_row) {
                          ## print('__________________________________')

                          formula_lb <- as.formula(paste0(test, " ~ ", equation))

                          ## ## print('there are more columns than rows')
                          ## print('**********************************')
                        ## }
                            ## LBfit <- MCMCpack::MCMCregress(as.formula(paste0(test, " ~ ",equation)), b0=0, B0 = 0.1, data = line)
                          ##   print('761')
                          ## print(test)
                            LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0, B0 = 0.1, data = line)
                            ## LBfit <- MCMCpack::MCMCregress(formula = formula_lb, b0=0, B0 = 0.1, data = line)
                          ## }, error=function(e){
                          ##   print('error')
                          ## print(Dose)
                          ## print(Vars)
                          ## print(equation)
                          ## print(test)
                          ##   print(e)
                          ##   print(line)

                          ## })
                            #Sample Model 'Per Individual animal'
                            Fit <- sample(1:nrow(LBfit), size=length(Subjs))
                          ## print('done')
                            sn <-1
                            for (Subj in Subjs) {
                                LBFit <- LBfit[Fit[sn],]
                                #Make LBSTRESN Fit for that variable
                                DayVars <- which(grepl("Day",names(LBFit)) == TRUE) #Find break between interaction variables and other variables
                                InteractionVars <- tail(DayVars,length(DayVars)-1) #Original Day Variable will be first found
                                #Make Equation Based on Varying length of Variables
                                #LBFit[1] is always the intercept
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
                                stdev <- unique(LBSummary[which(LBSummary$Dose == Dose & LBSummary$SEX == gender & LBSummary$LBTESTCD == test),c('ARMstdev','LBDY')])
                                LBTESTVAR <- abs(LBTESTVAR + rnorm(length(LBTESTVAR), mean = 0, sd = (stdev$ARMstdev)))

                                #Fill DataFrame to allocate to fake individual based on Day once variance is added
                                GenerLBData <- data.frame(LBSTRESN = 0,
                                                          LBDy = 0,
                                                          LBTESTCD = test)
                                avrgs <- unique(LBSummary[which(LBSummary$Dose == Dose & LBSummary$SEX == gender & LBSummary$LBTESTCD == test),c('ARMavg','LBDY')])
                                #rein in values to the days
                                for (Dayz in Days){
                                    Val <- LBTESTVAR[which.min(abs(LBTESTVAR -avrgs$ARMavg[which(avrgs$LBDY == Dayz)]))]
                                    GenerLBData[nrow(GenerLBData)+1,] <- c(Val ,Dayz, test)
                                }
                                GenerLBData <- GenerLBData[2:nrow(GenerLBData),]
                                # For each day store the value in generated dataset SENDstudy
                                for (day in Days){
                                    idx <-which(SENDstudy$lb$USUBJID %in% Subj & SENDstudy$lb$LBTESTCD %in% test &
                                                    SENDstudy$lb$LBDY %in% day)
                                    idx2 <- which(GenerLBData$LBDy %in% day)
                                    SENDstudy$lb$LBSTRESN[idx] <- round(as.numeric(GenerLBData$LBSTRESN[idx2]),3)
                                }
                                #add to subject count before new subject done
                                sn <- sn+1
                            }
                        ## }

                        }, error=function(e) print(e))
                    }
                }
            }
}
            #Testing code to graph fit compared to average of that ARM (HD, M, Selected LB tests)
            Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == "HD" & ExampleSubjects$SEX == "M")]
            TEST <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs), c("LBDY","LBSTRESN","LBTESTCD","USUBJID")]

            for (SampleTests in c("RBC","ALB","SPGRAV") ){
                #Add Average of LBSummary
                ## TEST2 <- TEST[which(TEST$LBTESTCD %in% SampleTests),]
                ## TEST2 <- merge(TEST2, unique(LBSummary[which(LBSummary$Dose == "HD" & LBSummary$LBTESTCD == SampleTests & LBSummary$SEX == "M"), c("LBDY","ARMavg")]), by = c("LBDY"))
                #Make plot per test
                ## p <- ggplot2::ggplot(data= TEST2, aes(x=factor(LBDY),y = LBSTRESN, group=USUBJID, color = "Simulated Animal Data"))+ geom_line()+
                ##     geom_point(aes(x=factor(LBDY),y = LBSTRESN, group=USUBJID, color = "Simulated Animal Data"))+
                ##     geom_line(aes(y = ARMavg, label="Average of Source Data", color = "Average of Source Data")) +
                ##     geom_point(aes(y = ARMavg, label="Average of Source Data", color = "Average of Source Data"))+
                ##     scale_x_discrete()+
                ##     ggtitle(paste0("HD M Distribution Comparison ", SampleTests))+
                ##     labs(x='LBDY (Days)', y=paste0(SampleTests, " Values")) + scale_color_manual(name = "Legend",
                ##                                                            values = c("darkred","steelblue"),
                ##                                                            breaks = c("Simulated Animal Data",
                ##                                                                       "Average of Source Data"))
                ## print(p)
            }

            # #Create distributions of values for LBSTRESN >>> rnorm method option, replaces MCMCregress with more averaged values
            # for (Dose in unique(Doses$Dose)){
            #     for (gender in unique(ExampleSubjects$SEX)){
            #         Subjs <- ExampleSubjects$USUBJID[which(ExampleSubjects$ARM == Dose & ExampleSubjects$SEX == gender)]
            #         Sub <- Subjects$USUBJID[which(Subjects$Dose == Dose & Subjects$SEX == gender)]
            #         GroupTests <- SENDstudy$lb$LBTESTCD[which(SENDstudy$lb$USUBJID %in% Subjs)]
            #         for (lbtest in unique(GroupTests)){
            #             Days <- unique(LBSummary$LBDY[which(LBSummary$USUBJID %in% Sub & LBSummary$LBTESTCD %in% lbtest)])
            #             for (day in Days){
            #                 idx <-which(SENDstudy$lb$USUBJID %in% Subjs & SENDstudy$lb$LBTESTCD %in% lbtest &
            #                                 SENDstudy$lb$LBDY %in% day)
            #                 idxs <-which(LBSummary$USUBJID %in% Sub & LBSummary$LBTESTCD %in% lbtest &
            #                                  LBSummary$LBDY %in% day)
            #                 #Filter Group mean and stdev
            #                 Testavg <- unique(LBSummary$ARMavg[idxs])
            #                 Teststdev <- unique(LBSummary$ARMstdev[idxs])
            #                 #use rnorm to generate new test values based around normal distribution
            #                 GenerData <- suppressWarnings(round(rnorm(n=length(idx), mean = Testavg, sd = Teststdev),2))
            #                 #Fill tests properly with created values
            #                 SENDstudy$lb$LBSTRESN[idx] <- GenerData
            #             }
            #         }
            #     }
            # }

            #Plot to look at correlation between Urine SPGRAV and Urine Volume

            ## Subjs <- ExampleSubjects$USUBJID
            ## TEST <- SENDstudy$lb[which(SENDstudy$lb$USUBJID %in% Subjs), c("LBDY","LBSTRESN","LBTESTCD","USUBJID")]
            ## Test3 <- TEST[which(TEST$LBTESTCD %in% c("SPGRAV","VOLUME")),]
            ## for (day in unique(Test3$LBDY)){
            ##     Test4 <- Test3[which(Test3$LBDY == day), c("USUBJID","LBTESTCD","LBSTRESN")]
            ##     Test4 <- reshape(Test4, idvar = "USUBJID", timevar = "LBTESTCD", direction = "wide")
            ##     #Model of relationship
            ##     newx <- seq(min(Test4$LBSTRESN.SPGRAV), max(Test4$LBSTRESN.SPGRAV), by = 0.001)
            ##     ydata <- seq(min(Test4$LBSTRESN.VOLUME), max(Test4$LBSTRESN.VOLUME),
            ##                  by = (max(Test4$LBSTRESN.VOLUME)-min(Test4$LBSTRESN.VOLUME))/length(newx))
            ##     ydata <- sort(ydata, decreasing = TRUE)
            ##     if (length(ydata)>length(newx)){
            ##         ydata <- ydata[1:length(newx)]
            ##     }
            ##     #Model attempt 1 - linear
            ##     # modelline <- lm(ydata ~ exp(-1/newx))
            ##     # newxy <- list(newx)
            ##     # modeltest <- predict(modelline)
            ##     # modeldata <- data.frame(y = modeltest, x = newx)
            ##     #model attempt 2 - nonlinear
            ##     fo3 <- y ~ 1/(x^c)
            ##     data <- data.frame(y = ydata, x = newx)
            ##     fm3 <- nls(fo3, data=data, start = list(c=1))
            ##     modeltest <- predict(fm3)
            ##     modeldata <- data.frame(y = sort(modeltest,decreasing = TRUE), x = newx)

            ##     #add in gender
            ##     Test4 <- merge(Test4, ExampleSubjects[,c("USUBJID","SEX","ARM")], by = "USUBJID")
            ##     ## q <- ggplot2::ggplot() +
            ##     ##     geom_point(data = Test4, aes(x=LBSTRESN.SPGRAV, y=LBSTRESN.VOLUME, shape = SEX, color = ARM)) +
            ##     ##     geom_line(data = modeldata, aes(x=x, y=y, color = 'Model of Relationship'))+
            ##     ##     labs(x='Urine Specific Gravity (SPGRAV)', y="Urine Volume (mL)") +
            ##     ##     ggtitle(paste0("Generated Volume and SPGRAV for day ", day, " of collection"))
            ##     ## print(q)
            ## }


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
            Example$mi$MISTRESC <-  str_replace_all(Example$mi$MISTRESC, "NORMAL", "UNREMARKABLE")
            Example$mi$MISTRESC <-  str_replace_all(Example$mi$MISTRESC, "NAD", "UNREMARKABLE")
            Example$mi$MISTRESC <-  str_replace_all(Example$mi$MISTRESC, "NO ABNORMALITY DETECTED", "UNREMARKABLE")
            Example$mi$MISTRESC <-  str_replace_all(Example$mi$MISTRESC,"NO ABUNREMARKABLEITY DETECTED","UNREMARKABLE")
            #Find out investigated MISPECS and Frequency of Findings/Severity Range of Findings
            #Calculate percentage of Findings per MISPEC in each ARM
            MIFindings <- merge(Subjects, Example$mi[,c("USUBJID", "MISPEC", "MISTRESC","MISEV")], by = "USUBJID")
            FindingsPercen <- MIFindings %>%
                group_by(Dose, MISPEC,SEX) %>%
                count(MISTRESC) %>%
                dplyr::mutate(percent = n/sum(n)) %>%
                dplyr::select(-n)
            SevPercen <- MIFindings %>%
                group_by(Dose, MISPEC,SEX) %>%
                count(MISEV) %>%
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

            #Remove MISTAT, MIRESCAT, MIDTHREL, and MISPCCND
            ## SENDstudy$mi$MISTAT <- NA
            ## SENDstudy$mi$MIREASND <- NA
            ## SENDstudy$mi$MISPCCND <- NA
            ## SENDstudy$mi$MISPCUFL <- NA
            ## SENDstudy$mi$MIDTHREL <- NA

            #Make Factors as Characters
            SENDstudy$mi$MITESTCD <- as.character(SENDstudy$mi$MITESTCD)
            SENDstudy$mi$MITEST <- as.character(SENDstudy$mi$MITEST)
            SENDstudy$mi$MISPEC <- as.character(SENDstudy$mi$MISPEC)
            SENDstudy$mi$MISTRESC <- as.character(SENDstudy$mi$MISTRESC)
            SENDstudy$mi$MISEV <- as.character(SENDstudy$mi$MISEV)

            ##### Save Generated Study for Tables ####
            GeneratedSEND[[j]] <- SENDstudy

            ############# Export Folder of Generated Data #################

            if (!is.null(where_to_save)){
                #Create .xpt files if you can
                study_fake <- paste0('FAKE',studyID)
              path_save <- where_to_save
                dir_to_save <- fs::path(path_save,study_fake)
              fs::dir_create(dir_to_save)
                ## dir.create(dir_to_save) #Create Folder to Hold Study

                #Loop through domains created to print them in created folder
                Domains <- names(SENDstudy)
                for (domain in Domains){
                    ## printpath <- paste0(path,"/FAKE",studyID,"/",domain,".xpt")
                    printpath <- fs::path(dir_to_save, domain, ext= 'xpt')
                    write_xpt(SENDstudy[[domain]],path = printpath, version = 5)

                  print(paste0('file saved in: ',printpath))
                }
            } else {


                study_fake <- paste0('FAKE',studyID)
                dir_to_save <- fs::path(path,study_fake)
              fs::dir_create(dir_to_save)
                ## dir.create(dir_to_save) #Create Folder to Hold Study

                #Loop through domains created to print them in created folder
                Domains <- names(SENDstudy)
                for (domain in Domains){
                    ## printpath <- paste0(path,"/FAKE",studyID,"/",domain,".xpt")
                    printpath <- fs::path(dir_to_save, domain, ext= 'xpt')
                    write_xpt(SENDstudy[[domain]],path = printpath, version = 5)
                  print(paste0('file saved in: ',printpath))
                }


            }


        }


  }
