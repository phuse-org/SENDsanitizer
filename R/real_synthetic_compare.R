#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Dec-30-2024    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)


## library(shiny)

#' @title Compare real and synthetic data
#' @description This is an shiny app that can be used to visually inspect
#' real and synthetic data
#' @param df mandatory, dataframe \cr
#' a dataframe with APPID and STUDYID columns
#' @return App
#' @examples
#' \dontrun{
#' compare_real_synthetic()
#' }
#' @export

#' @import shiny
#' @import shinyFiles




compare_real_synthetic <- function(){

  ui <- shiny::navbarPage(

  shiny::tabsetPanel(
                 shiny::tabPanel("Data Upload",
                                   shinyFiles::shinyDirButton('real_dir',
                                                              'Real Data Directory',
                                                              'Please Select Real Data Directory'),
                                   shinyFiles::shinyDirButton('syn_dir', 'Synthetic Data Directory',
                                                              'Please Select Synthetic Data Directory'),
                          shiny::actionButton('show_table', 'Show Table')
                          ),
          # domain
           shiny::tabPanel('TS',
                           shiny::fluidRow(
                                    shiny::column(width = 5,
                                                  DT::DTOutput('real_ts_df')
                                                  ),
                                    shiny::column(width = 5,offset = 1,

                                                  DT::DTOutput('syn_ts_df')

                                                  )

                                  )
                           ),
#
           shiny::tabPanel('TX',
                           shiny::fluidRow(
                                    shiny::column(width = 5,
                                                  DT::DTOutput('real_tx_df')
                                                  ),
                                    shiny::column(width = 5,offset = 1,

                                                  DT::DTOutput('syn_tx_df')

                                                  )

                                  )
                           ),
          #
           shiny::tabPanel('DM',
                           shiny::fluidRow(
                                    ## shiny::column(width = 5,
                                                  DT::DTOutput('real_dm_df'),
                                                  ## ),
                                    ## shiny::column(width = 5,offset = 1,
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::br(),

                                                  DT::DTOutput('syn_dm_df')

                                                  ## )

                                  )
                           ),
#

           shiny::tabPanel('BW',
                           shiny::fluidRow(
                                    ## shiny::column(width = 5,
                                                  DT::DTOutput('real_bw_df'),

                                    shiny::br(),
                                    shiny::br(),
                                    shiny::br(),
                                                  ## ),
                                    ## shiny::column(width = 5,offset = 1,

                                                  DT::DTOutput('syn_bw_df')

                                                  ## )

                                  )
                           ),
           #

           shiny::tabPanel('LB',
                           shiny::fluidRow(
                                                  DT::DTOutput('real_lb_df'),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::br(),
                                                  DT::DTOutput('syn_lb_df')
                                  )
                           ),

           shiny::tabPanel('OM',
                           shiny::fluidRow(
                                                  DT::DTOutput('real_om_df'),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::br(),
                                                  DT::DTOutput('syn_om_df')
                                  )
                           ),

           shiny::tabPanel('MI',
                           shiny::fluidRow(
                                                  DT::DTOutput('real_mi_df'),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::br(),
                                                  DT::DTOutput('syn_mi_df')
                                  )
                           )
         )
  )



  server <- function(input, output, session) {
    if(test){
      volumes <- c(home = fs::path(fs::path_home(), dir2,dir3))
      volumes_syn <- c(home= fs::path(fs::path_home(),dir2))
    } else{
      volumes <- c(home= fs::path_home())
      volumes_syn <- c(home=fs::path_home())
    }
  shinyFiles::shinyDirChoose(input, 'real_dir',roots=volumes, session = session,
                             allowDirCreate = F,filetypes=c('xpt'))
  shinyFiles::shinyDirChoose(input, 'syn_dir',roots=volumes_syn, session = session,
                             allowDirCreate = F,filetypes=c('xpt'))

# real data
## real data directory
    get_real_dir <- shiny::reactive({
      files <- shinyFiles::parseDirPath(volumes,input$real_dir)
      files

    })

# read all real data xpt files and put in a list
    get_files_real <- shiny::reactive({
      shiny::req(input$show_table)
      files <- get_real_dir()
      xpt_files <- load_xpt_files(files,domains = c('ts','tx',
                                                    'dm','bw','lb','mi','om'))
      xpt_files
    })

# render real data

    output$real_ts_df <- DT::renderDT({
      xpt_list <- get_files_real()
     ts <-  xpt_list$ts
      ts <- DT::datatable(ts)
      DT::formatStyle(ts, c('STUDYID','TSVAL'),backgroundColor='#fbcccf')
    })


    output$real_tx_df <- DT::renderDT({
      xpt_list <- get_files_real()
     tx <-  xpt_list$tx
      tx <- DT::datatable(tx)
      DT::formatStyle(tx,c('STUDYID','SET','TXVAL'),backgroundColor = '#fbcccf')
    })


    output$real_dm_df <- DT::renderDT({
      xpt_list <- get_files_real()
     dm <-  xpt_list$dm
      col_name <- colnames(dm)
      dtc <- grep('DTC',col_name,value = T)
      color_col <- c('USUBJID','STUDYID','ARMCD','ARM', dtc)
      dm <- DT::datatable(dm)
      DT::formatStyle(dm,color_col,
                      backgroundColor = '#fbcccf')
    })

    output$real_bw_df <- DT::renderDT({
      xpt_list <- get_files_real()
      bw <- xpt_list$bw
      column_col <- c('STUDYID','USUBJID','BWORRES','BWSTRESC','BWSTRESN','BWDTC')
      bw <- DT::datatable(bw)
      DT::formatStyle(bw,column_col,backgroundColor = '#fbcccf')
    })

    output$real_lb_df <- DT::renderDT({
      xpt_list <- get_files_real()
      lb <- xpt_list$lb
      dtc <- grep('DTC',colnames(lb),value=T)
      color_col <- c('STUDYID','USUBJID','LBORRES','LBSTRESC','LBSTRESN',dtc)
      lb <- DT::datatable(lb)
      DT::formatStyle(lb, color_col,backgroundColor='#fbcccf')

    })

    output$real_om_df <- DT::renderDT({
      xpt_list <- get_files_real()
      om <- xpt_list$om
      dtc <- grep('DTC',colnames(om),value=T)
      color_col <- c('STUDYID','USUBJID','OMORRES','OMSTRESC','OMSTRESN',dtc)
      om <- DT::datatable(om)
      DT::formatStyle(om, color_col,backgroundColor='#fbcccf')
    })

    output$real_mi_df <- DT::renderDT({
      xpt_list <- get_files_real()
      mi <- xpt_list$mi
      dtc <- grep('DTC',colnames(mi),value=T)
      color_col <- c('STUDYID','USUBJID','MIORRES','MISTRESC','MISEV',dtc)
      mi <- DT::datatable(mi)
      DT::formatStyle(mi, color_col,backgroundColor='#fbcccf')
    })

 # synthetic data

    get_syn_dir <- shiny::reactive({
      files <- shinyFiles::parseDirPath(volumes_syn,input$syn_dir)
      files
    })


# read all syn data xpt files and put in a list
    get_files_syn <- shiny::reactive({
      shiny::req(input$show_table)
      files <- get_syn_dir()
      xpt_files <- load_xpt_files(files,domains = c('ts','tx',
                                                    'dm','bw','lb','mi','om'))
      xpt_files
    })

# render syn data

    output$syn_ts_df <- DT::renderDT({
      xpt_list <- get_files_syn()
     ts <-  xpt_list$ts
      ts <- DT::datatable(ts)
      DT::formatStyle(ts, c('STUDYID','TSVAL'),backgroundColor='skyblue')
    })

    output$syn_tx_df <- DT::renderDT({
      xpt_list <- get_files_syn()
     tx <-  xpt_list$tx
      tx <- DT::datatable(tx)
      DT::formatStyle(tx,c('STUDYID','SET','TXVAL'),backgroundColor = 'skyblue')
    })


    output$syn_dm_df <- DT::renderDT({
      xpt_list <- get_files_syn()
      dm <-  xpt_list$dm
      col_name <- colnames(dm)
      dtc <- grep('DTC',col_name,value = T)
      color_col <- c('USUBJID','STUDYID','ARMCD','ARM', dtc)
      dm <- DT::datatable(dm)
      DT::formatStyle(dm,color_col,
                      backgroundColor = 'skyblue')
    })

    output$syn_bw_df <- DT::renderDT({
      xpt_list <- get_files_syn()
      bw <- xpt_list$bw
      column_col <- c('STUDYID','USUBJID','BWORRES','BWSTRESC','BWSTRESN','BWDTC')
      bw <- DT::datatable(bw)
      DT::formatStyle(bw,column_col,backgroundColor = 'skyblue')
    })

    output$syn_lb_df <- DT::renderDT({
      xpt_list <- get_files_syn()
      lb <- xpt_list$lb
      dtc <- grep('DTC',colnames(lb),value=T)
      color_col <- c('STUDYID','USUBJID','LBORRES','LBSTRESC','LBSTRESN',dtc)
      lb <- DT::datatable(lb)
      DT::formatStyle(lb, color_col,backgroundColor='skyblue')
    })


    output$syn_om_df <- DT::renderDT({
      xpt_list <- get_files_syn()
      om <- xpt_list$om
      dtc <- grep('DTC',colnames(om),value=T)
      color_col <- c('STUDYID','USUBJID','OMORRES','OMSTRESC','OMSTRESN',dtc)
      om <- DT::datatable(om)
      DT::formatStyle(om, color_col,backgroundColor='skyblue')
    })

    output$syn_mi_df <- DT::renderDT({
      xpt_list <- get_files_syn()
      mi <- xpt_list$mi
      dtc <- grep('DTC',colnames(mi),value=T)
      color_col <- c('STUDYID','USUBJID','MIORRES','MISTRESC','MISEV',dtc)
      mi <- DT::datatable(mi)
      DT::formatStyle(mi, color_col,backgroundColor='skyblue')
    })

  }


shinyApp(ui = ui, server = server)

}
