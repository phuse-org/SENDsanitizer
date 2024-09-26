#' ---
#' title: "Demographics (DM) domain"
#' output: html_document
#' date: Sep-09-2024
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
file_org <- 'no_git_dir/code/phuse/PDS/dm.xpt'
file_fake  <- 'no_git_dir/code/phuse/FAKE15218/dm.xpt'

#'
#| include=FALSE
dm_org <- haven::read_xpt(fs::path(path,file_org))


#' ##  Original Data
#'
#| echo=FALSE
dm_org <- as.data.frame(dm_org)
DT::datatable(dm_org,options = list(pageLength = 20))

#' ###  Unique setcd in original data
#| echo=FALSE
unique(dm_org$SETCD)


#'
#| include=FALSE
dm_fake <- haven::read_xpt(fs::path(path,file_fake))

#' ### Animals taken only from 01, 04, 06, 08
#| echo=FALSE
unique(dm_fake$SETCD)

#' ## Synthetic Data
#'
#| echo=FALSE
dm_fake <- as.data.frame(dm_fake)
DT::datatable(dm_fake,options = list(pageLength = -1))


#' ## Synthetic Data group by SETCD column
#| echo=FALSE
DT::datatable(dm_fake,options = list(pageLength= 20)) |>
  DT::formatStyle(colnames(dm_fake), valueColumns = 'SETCD',
                  backgroundColor = styleEqual(c('01','04','06','08'),
                                               c('#a8d5ba','#fbcccf','#d2deeb',
                                                 '#d3d8c4')))

# /* data merge */
#| include=FALSE
data.table::setDT(dm_fake)
dm_fake_s <- dm_fake[,.(STUDYID,USUBJID,SUBJID,SETCD,RFSTDTC ,RFENDTC,BRTHDTC,ARMCD,ARM)]
dm_merge_full <- merge(dm_org, dm_fake_s, by = c('SETCD','SUBJID'),
                       all = TRUE, suffixes = c('_original',
                                               '_fake'))
data.table::setcolorder(dm_merge_full,c("STUDYID_original", "STUDYID_fake",
                                        "DOMAIN", "USUBJID_original",
                                        "USUBJID_fake", "SUBJID",
                                        "RFSTDTC_original",
                                        "RFSTDTC_fake",
                                        "RFENDTC_original",  "RFENDTC_fake",
                                        "BRTHDTC_original", "BRTHDTC_fake",
                                        "SITEID", "AGE", "AGETXT", "AGEU",  "SEX",
                                        "SPECIES", "STRAIN", "SBSTRAIN",
                                        "ARMCD_original", "ARMCD_fake",
                                        "ARM_original", "ARM_fake", "SETCD"))



#' ## Columns that changed are in blue background.
#' This is comparison of original data to synthetic data.
#' Since, we just took 4 set code from all the set code availabe, there are lots of
#' empty value if we compare side by side.
#' A column by column comparison.
#| echo=FALSE
DT::datatable(dm_merge_full,options = list(pageLength=20)) |>
  DT::formatStyle(grep('fake',colnames(dm_merge_full),value = T),
                  backgroundColor = 'skyblue')


#' ## Actual original data that we took to work on
#' only those 4 setcd animals
#| echo=FALSE
data.table::setDT(dm_org)
dm_org_setcd <- dm_org[SETCD %in% c('01','04','06','08'), ]
DT::datatable(dm_org_setcd,options = list(pageLength=20))

# /*  filter out 4 setcd */
#| echo=FALSE
data.table::setDT(dm_merge_full)
dm_setcd <- dm_merge_full[SETCD %in% c('01','04','06','08'), ]


#' ## Column by Column Comparison
#'
#| echo=FALSE

DT::datatable(dm_setcd, options = list(pageLength=20)) |>
  DT::formatStyle(grep('fake',colnames(dm_setcd),value = T),
                  backgroundColor = 'skyblue') |>
  DT::formatStyle(grep('original',colnames(dm_setcd),value = T),
                  backgroundColor = '#fbcccf')



