## This code is for running in combination

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   Mar-21-2024    Md Yousuf Ali (md.ali@fda.hhs.gov)



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
#' @param study_batch mandatory, fefault 2\cr
#' interger value
#

#' @importFrom utils combn


sanitize_batch <- function(path, number=1, recovery=FALSE,
                           where_to_save=NULL, study_batch=2) {
  save <- where_to_save
dir_comb <- path
taken_m <- as.numeric(study_batch)
kk <- utils::combn(seq(length(dir_comb)), m=taken_m)
df <- as.data.frame(kk)
dir_list <- list()
for (i in 1:length(colnames(df))){

  val <- df[1:taken_m, i]
  dirs <- dir_comb[val]
  dir_list[[i]] <- dirs
}
for(j in 1:length(dir_list)){

tryCatch({
  print(dir_list[[j]])
  sanitize(path = dir_list[[j]], where_to_save = save, recovery = recovery)


}, error=function(e) print(e))

}

 }
