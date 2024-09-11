
get_pos_val <- function(OMfit,om_study,Subj,Vars,line,test,SENDStudy ){
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
  indx <- which(SENDstudy$om$USUBJID==Subj & SENDstudy$om$OMTESTCD==omspec &
                SENDstudy$om$OMSPEC==test)
  indx <- which(SENDstudy$om$USUBJID==Subj & SENDstudy$om$OMSPEC==test)
  final_val
}
