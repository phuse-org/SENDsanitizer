#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Sep-10-2024    Md Yousuf Ali (md.ali@fda.hhs.gov)


#  run when website  branch
path <- "create_articles_R_files"
path_art <- "vignettes/articles/"
dom <- c("bw_documentation.R",
"dm_documentation.R",
"lb_documentation.R",
"mi_documentation.R",
"om_documentation.R",
"ts_documentation.R",
"tx_documentation.R")

for (i in 1:length(dom)){
fn_path <- fs::path(path,dom[i])
knitr::spin(hair = fn_path,knit = F)
rmd <- sub(".R",".Rmd",dom[i])
fs::file_move(fs::path(path,rmd),fs::path(path_art,rmd))
}
