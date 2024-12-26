#' ---
#' title: "Microscopic Findings (MI) domain"
#' output: html_document
#' date: Sep-10-2024
#' author: Md Yousuf Ali
#' toc: false
#' ---

#'
# /* this file will be converted to Rmd file */
# /* commnet written differently */
# /* load packages */
#'
#| include=FALSE
library(DT)
library(data.table)

#'
#| include=FALSE
path <- '~/OneDrive - FDA/yousuf/00_github_projects/SENDsanitizer/'
file_org <- 'no_git_dir/code/phuse/PDS/mi.xpt'
file_fake  <- 'no_git_dir/code/phuse/FAKE15218/mi.xpt'

#'
#| include=FALSE
mi_org <- haven::read_xpt(fs::path(path,file_org))


#' ##  Original Data
#'
#| echo=FALSE
mi_org <- as.data.frame(mi_org)
DT::datatable(mi_org,options = list(pageLength = 10))

#' ##  Columns that changed in original (red background)
#| echo=FALSE
DT::datatable(mi_org,options = list(pageLength=10)) |>
  DT::formatStyle(columns = c('STUDYID','USUBJID','MIORRES','MISTRESC',
                             'MISEV', 'MIDTC'),
                  backgroundColor = '#fbcccf')
#'
#| include=FALSE
mi_fake <- haven::read_xpt(fs::path(path,file_fake))

#' ## Synthetic Data
#'
#| echo=FALSE
mi_fake <- as.data.frame(mi_fake)
data.table::setDT(mi_fake)
data.table::setcolorder(mi_fake,'USUBJID', after = 'DOMAIN')
DT::datatable(mi_fake,options = list(pageLength = 10))

#' ## Synthetic Data with columns that changed (blue background)
#| echo=FALSE
DT::datatable(mi_fake,options = list(pageLength=10)) |>
  DT::formatStyle(columns = c('STUDYID','USUBJID','MIORRES',
                              'MISTRESC','MISEV','MIDTC'),
                          backgroundColor = 'skyblue')
#'
#| include=FALSE
data.table::setDT(mi_org)
data.table::setDT(mi_fake)
# /* MISEQ might not be the way we do this, just for an idea */
# /*  Column by Column Comparison */
#| include=FALSE
mi_merge <- merge(mi_org,mi_fake[,.(STUDYID,USUBJID,
                                    MISEQ,MISTRESC,MIORRES,MISEV,MIDTC)],
                  by = c('MISEQ'),suffixes = c('_original','_fake'))
data.table::setcolorder(mi_merge,
                        c("STUDYID_original", "STUDYID_fake", "DOMAIN",
                          "USUBJID_original","USUBJID_fake",
                          "MIORRES_original","MIORRES_fake",
                          "MISTRESC_original", "MISTRESC_fake",
                          "MISEV_original","MISEV_fake",
                          "MIDTC_original","MIDTC_fake", "MIDY"
                          ))
DT::datatable(mi_merge, options = list(pageLength=20)) |>
  DT::formatStyle(grep('fake',colnames(mi_merge),value = T),
                  backgroundColor = 'skyblue') |>
  DT::formatStyle(grep('original',colnames(mi_merge),value = T),
                  backgroundColor = '#fbcccf')
