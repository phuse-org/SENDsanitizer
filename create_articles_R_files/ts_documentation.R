#' ---
#' title: "Trial Summary (TS) Domain"
#' output: html_document
#' date: Sep-08-2024
#' author: Md Yousuf Ali
#' toc: false
#' ---



#'
# /* load packages */
# /* this file will be converted to Rmd file */
# /* commnet written differently */
#'
#| include=FALSE
library(DT)
library(data.table)
#'
#| include=FALSE
path <- '~/OneDrive - FDA/yousuf/00_github_projects/SENDsanitizer/'
file_org <- 'no_git_dir/code/phuse/PDS/ts.xpt'
file_fake  <- 'no_git_dir/code/phuse/FAKE15218/ts.xpt'

#'
#| include=FALSE
ts_org <- haven::read_xpt(fs::path(path,file_org))

#' ##  Original Data
#'
#| echo=FALSE
ts_org <- as.data.frame(ts_org)
DT::datatable(ts_org,options = list(pageLength = -1))

#'
#| include=FALSE
ts_fake <- haven::read_xpt(fs::path(path,file_fake))

#' ## Synthetic Data
#'
#| echo=FALSE
ts_fake <- as.data.frame(ts_fake)
DT::datatable(ts_fake,options = list(pageLength = -1))

#' ## Values of the column that changed.
#' STUDYID and TSVAL column have value that differ from original data.
#' Blue background represents all the values that replaced with synthetic value.
#' When it is empty, that's mean value removed
#' (in otherword, replaced with empty string('')).
#' Red text in TSPARMCD is just a visual guide what changes in TSVAL.
#| echo=FALSE
DT::datatable(ts_fake,options = list(pageLength = -1)) |>
  DT::formatStyle('STUDYID',
                  backgroundColor = 'skyblue') |>
  formatStyle('TSVAL', valueColumns = 'TSPARMCD',
              backgroundColor = styleEqual(c(c('TRT','TRTV','TRTCAS',
                                               'TSTFLOC','TSTFNAM',
                                               'EXPENDTC','EXPSTDTC','STITLE',
                                               'STSTDTC',
                                               'SPLRNAM','TFCNTRY','SSPONSOR',
                                               'SPREFID','STDIR','TRMSAC'
                                               )), c('skyblue'))) |>
  formatStyle('TSPARMCD',
               color = styleEqual(
                 c('TRT','TRTV','TRTCAS','TSTFLOC','TSTFNAM',
                   'EXPENDTC','EXPSTDTC','STITLE','STSTDTC',
                   'SPLRNAM','TFCNTRY','SSPONSOR',
                   'SPREFID','STDIR','TRMSAC'), c('red')))

#'

# /*  prepare for side side by comparison */

#| include=FALSE
ll <- merge(ts_org,ts_fake[,c('STUDYID','TSSEQ','TSVAL','TSPARMCD')],
            by = c('TSPARMCD','TSSEQ'),
            suffixes = c('_original','_fake'))
data.table::setDT(ll)
data.table::setcolorder(ll, c('STUDYID_original','STUDYID_fake',
                              'DOMAIN','TSSEQ','TSGRPID','TSPARMCD',
                              'TSPARM','TSVAL_original','TSVAL_fake'))

#' ##  Now column by column comparison
#| echo=FALSE
DT::datatable(ll, options = list(pageLength = -1)) |>
  DT::formatStyle('STUDYID_fake',
                  backgroundColor = 'skyblue') |>
  formatStyle('TSVAL_fake', valueColumns = 'TSPARMCD',
              backgroundColor = styleEqual(c(c('TRT','TRTV','TRTCAS',
                                               'TSTFLOC','TSTFNAM',
                                               'EXPENDTC','EXPSTDTC','STITLE',
                                               'STSTDTC',
                                               'SPLRNAM','TFCNTRY','SSPONSOR',
                                               'SPREFID','STDIR','TRMSAC'
                                               )), c('skyblue'))) |>
  formatStyle('TSPARMCD',
              color = styleEqual(
                c('TRT','TRTV','TRTCAS','TSTFLOC','TSTFNAM',
                  'EXPENDTC','EXPSTDTC','STITLE','STSTDTC',
                  'SPLRNAM','TFCNTRY','SSPONSOR',
                  'SPREFID','STDIR','TRMSAC'), c('red')))
