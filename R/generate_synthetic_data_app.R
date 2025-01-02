#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Dec-31-2024    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)


#' @title Generate synthetic data from real data
#' @description Generate synthetic data with shiny app
#' @return app
#' @examples
#' \dontrun{
#' sanitize_shiny_app()
#' }
#' @export

sanitize_shiny_app <- function(){

  where_to_save <- tempdir()

unlink(file.path(where_to_save, "*"), recursive = TRUE)

  ui <- shiny::navbarPage(
                 shiny::tabsetPanel(
                          shiny::tabPanel('Synthetic Data',
                                          shinyFiles::shinyDirButton('real_dir',
                                                                     'Real Data',
                                                                     'Please select data'

                                                                     ),
                                          shiny::actionButton('upload', 'upload'),

                                          shiny::checkboxInput('recovery','Create Recovery?', FALSE),
                                          shiny::actionButton('generate', 'Generate Synthetic'),
                                          shiny::verbatimTextOutput('dir_path'),
                                          DT::DTOutput('real_ts_df')




                                          )

                        )


               )



  server <- function(input,output, session){

    if(test){
      volumes <- c(home = fs::path(fs::path_home(), dir2,dir3))
      ## volumes_syn <- c(home= fs::path(fs::path_home(),dir2))
    } else{
      volumes <- c(home= fs::path_home())
      ## volumes_syn <- c(home=fs::path_home())
    }
  shinyFiles::shinyDirChoose(input, 'real_dir',roots=volumes, session = session,
                             allowDirCreate = F,filetypes=c('xpt'))
  ## shinyFiles::shinyDirChoose(input, 'syn_dir',roots=volumes_syn, session = session,
  ##                            allowDirCreate = F,filetypes=c('xpt'))

# real data
## real data directory
    get_real_dir <- shiny::reactive({
      files <- shinyFiles::parseDirPath(volumes,input$real_dir)
      files

    })

    output$dir_path <- shiny::renderText({
      req(input$generate)
text <- get_real_dir()
      print(text)
      print(where_to_save)
text

    })

    generate_synthetic <- shiny::eventReactive(input$generate,{
      shiny::req(input$generate)
path <- get_real_dir()
print(path)
where_to_save <- where_to_save
print(where_to_save)
df <- sanitize(path = path,where_to_save = where_to_save,recovery = input$recovery,
         number = 1,app=T)

      df

    })


    ## get_files_real <- shiny::reactive({
    ##   shiny::req(input$upload)
    ##   files <- where_to_save
    ##   xpt_files <- load_xpt_files(files,domains = c('ts','tx',
    ##                                                 'dm','bw','lb','mi','om'))
    ##   xpt_files
    ## })


    output$real_ts_df <- DT::renderDT({
      xpt_list <- generate_synthetic()
     ts <-  xpt_list$ts
      ts <- DT::datatable(ts)
      ts
      ## DT::formatStyle(ts, c('STUDYID','TSVAL'),backgroundColor='#fbcccf')
    })












  }

shinyApp(ui = ui, server = server)
}
