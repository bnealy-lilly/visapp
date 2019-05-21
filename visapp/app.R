

## Required libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyFiles)
library(shinyWidgets)
library(shinydashboard)
library(colourpicker)
library(tidyverse) # don't call tidyverse directly, call individual packages - smaller packrat
library(doSNOW)
library(coastr)


# Source files for animations
# General Animation Source Files
source("general_functions/makefootnote.R")
source("general_functions/interpolate.R")

# Rainfall Plot Source Files
source("rainfall_functions/rainfall_data_format.R")
# source("rainfall_functions/rainfall_fun_stephanie_update.R")
source("rainfall_functions/rainfall_fun_v3.R")

# Spider Plot Source Files
source("spider_functions/spider_data_format.R")
source("spider_functions/spider_fun.R")
source("spider_functions/radar4.R")

## Other source files
source("interface_text/intro.R") #introduction paragraph
source("interface_text/descriptions.R") #animation template descriptions


#### UI
###############################################################################################################
ui <- dashboardPage(
  
  dashboardHeader(title = "SCRATCH"),
  
  dashboardSidebar(
    sidebarMenu(
      id="menu_tabs",
      menuItem(tabName = "welcome", "Getting Started", icon=icon('pencil',lib = "glyphicon")),
      menuItem(tabName = "select", "Animation Selection", icon=icon('film',lib = "glyphicon")),
      menuItem(tabName = "upload", "Data Upload and Formatting", icon=icon('table')),
      menuItem(tabName = "customize", "Customization", icon=icon('sliders')),
      menuItem(tabName = "preview", "Generate Frames & Preview", icon=icon('stats',lib = "glyphicon")),
      menuItem(tabName = "save", "Render and Download", icon=icon('download')),
      br(),br(),br(),br(),
      
      menuItem("Want to start over?",icon=icon('undo')),
      useShinyjs(),
      actionButton("square_one","Reset All Settings")
    )
  ),
  
  dashboardBody(
    tabItems(
      ## Welcome tab content
      tabItem(tabName = "welcome", fluidRow(box(uiOutput("intro"),width=8))),
      
      ## Animation selection tab content
      tabItem(tabName = "select",  column(fluidRow(box(selectInput("templates",
                                                                   label = "Animation Templates:",
                                                                   choices=c("Please make a selection:",
                                                                             "Rainfall","Spider")),
                                                       width=12),
                                                   
                                                   conditionalPanel(condition="input.templates != 'Please make a selection:' ",
                                                                    box(h5(strong("Example animation:")),
                                                                        uiOutput("example"), 
                                                                        tags$head(tags$style(
                                                                          type="text/css",
                                                                          "#example img {max-width: 90%; width: 90%; height: auto}")),
                                                                        width=12),br(),
                                                                    
                                                                    box(uiOutput("description"), width=12))),
                                          width=9)),
      
      ## Data upload and formatting tab content
      tabItem(tabName = "upload", conditionalPanel(condition="input.templates == 'Please make a selection:'",
                                                   box(width=6,
                                                       h4("Please select a template on the Animation Selection tab before
                                                          proceeding."))),
              
              conditionalPanel(condition="input.templates != 'Please make a selection:'",
                               fluidRow(column(width=4,br(),box(width=12,
                                                                
                                                                h5(strong("Step 1 - Upload Data:"),align='center'),
                                                                
                                                                uiOutput("data_desc"),tags$hr(),
                                                                
                                                                # column(width=6, 
                                                                #        h5(strong("Upload SDTM Data:")),
                                                                #        shinyFilesButton('upload_sdtm', 
                                                                #                         label='Browse',
                                                                #                         title='Upload SDTM Data:', 
                                                                #                         multiple = TRUE)),
                                                                
                                                                column(width=6, 
                                                                       h5(strong("Upload ADaM Data:")),
                                                                       shinyFilesButton('upload_adam', 
                                                                                        label='Browse', 
                                                                                        title="Upload ADaM Data:", 
                                                                                        multiple = TRUE))),
                                               
                                               box(width=12,
                                                   h5(strong("Step 4 - Create Custom Dataset"),align='center',tags$hr()),
                                                   actionButton('dataformat', strong("Generate Animation Data")), 
                                                   
                                                   actionButton("debug","Debug"), br(),
                                                   
                                                   HTML("<b>Note</b>: After specifying the values for the animation 
                                                           in the tabs to the right, press this button to 
                                                           generate data for your selected animation.
                                                           Only one animation style's data may be generated at one time. 
                                                           To change which animation's data you are 
                                                           producing, select a different template
                                                           on the Animation Selection tab."))), br(),
                                        
                                        column(width=6, box(width=12, h5(strong("Step 2 - Variable Selection:"),align='center'),
                                                            tabsetPanel(id="fun_inputs")),
                                               box(width=12, h5(strong("Step 3 - Select Study Time Points:"),
                                                                align='center',tags$hr()),
                                                   tags$head(tags$style(HTML(".multicol {
                                                                                         height: 150px;
                                                                                         -webkit-column-count: 4;
                                                                                         -moz-column-count: 4;   
                                                                                         column-count: 4;
                                                                                         -moz-column-fill: auto;
                                                                                         -column-fill: auto;
                                                                                         }"))),
                                                   tags$head(tags$style(HTML(".checkbox-inline {
                                                                                          margin-left: 0px;
                                                                                            margin-right: 10px;
                                                                                          }
                                                                                          .checkbox-inline+.checkbox-inline {
                                                                                            margin-left: 0px;
                                                                                            margin-right: 10px;
                                                                                          }"))),
                                                   uiOutput("timepoints"),
                                                   h5("Note: Visits before 'Visit 1' (e.g. 'Visit 0', 'Screening') 
                                                                            are converted to 'Baseline'"),
                                                   actionButton("uncheckall","Uncheck All"),
                                                   actionButton("checkall","Check All"))),
                                        
                                        column(width=2, box(width=12, 
                                                            actionButton("remove_tab","Remove Selected Dataset"),tags$hr(),
                                                            
                                                            h5(strong("Toggle Data Preview")),
                                                            materialSwitch("showdat", value=FALSE, 
                                                                           right=TRUE, status="primary"),
                                                            
                                                            conditionalPanel(condition = "input.showdat",
                                                                             tags$hr(),
                                                                             radioButtons("numrows", "Rows to Display",
                                                                                          choices = c('First Six' = "head",
                                                                                                      'All (slower)' = "all"),
                                                                                          selected = "head"))))),
                               
                               conditionalPanel(condition = "input.showdat",
                                                fluidRow(box(width=12,
                                                             tabsetPanel(id="preview_tabs")))))),
      
      ## Animation customization tab content (content dependent on animation selection)
      tabItem(tabName = "customize",
              uiOutput("customize_ui")),
      
      ## Preview tab content
      tabItem(tabName = "preview", conditionalPanel(condition="input.templates == 'Please make a selection:'",
                                                    box(width=6,
                                                        h4("Please select a template on the Animation Selection tab before
                                                          proceeding."))),
              
              conditionalPanel(condition="input.templates != 'Please make a selection:'", 
                               fluidRow(box(width=7,
                                            column(width=9,
                                                   textInput("selected_path","",value="/lillyce/qa/ly3009104/common/usersanderbox/bnealy")),
                                            column(width=1,
                                                   tags$style(type='text/css', "#dir_select {margin-top: 25px;}"),
                                                   shinyDirButton(id = "dir_select", label = "Browse", title = "Select")))),
                               
                               fluidRow(box(width=2,
                                            actionButton("makeframes","Produce Frames"))),
                               
                               conditionalPanel(condition = "input.makeframes",
                                                fluidRow(box(width=8,
                                                             uiOutput("preview_text"),
                                                             imageOutput("preview")))))),
      
      ## Render and download tab content
      tabItem(tabName = "save", conditionalPanel(condition="input.templates == 'Please make a selection:'",
                                                 box(width=6,
                                                     h4("Please select a template on the Animation Selection tab before
                                                          proceeding."))),
              
              conditionalPanel(condition="input.templates != 'Please make a selection:'",box(width=6,
                                                                                             actionButton("punch_it_chewie","Render Animation"),br(),br(),br(),
                                                                                             radioButtons("delete_frames","Delete individual frames after rendering (optional)?",
                                                                                                          choices = c('Yes' = "Yes",
                                                                                                                      'No' = "No"),
                                                                                                          selected = "Yes")),
                               box(width=6,
                                   h5(strong("Ready to make another animation? Click here:")),
                                   useShinyjs(),
                                   actionButton("restart","Start Over"))))
    )
  )
)
###############################################################################################################

server <- function(input, output, session) {
  
  observeEvent(input$debug,{
    browser()
  })
  
  ## Increase the file upload size limit
  options(shiny.maxRequestSize=10000*1024^2) 
  
  ## Lists to store uploaded and formatted data
  sdtm_dat <- list()
  adam_dat <- list()
  rainfall_adam <- list()
  rainfall_sdtm <- list()
  spider_sdtm <- list()
  spider_adam <- list()
  
  adam_tabnames <- NULL
  sdtm_tabnames <- NULL
  all_tabnames <- NULL
  files_selected <- NULL
  dataset_names <- NULL
  adam_length <- 0
  sdtm_length <- 0
  
  ## Currently hardcoded to specific file for testing, will be /lillyce/ at app launch
  current_path <- "/lillyce/qa/ly3009104/i4v_mc_jahl/prelock/data/analysis/shared"
  current_root <- c(root=current_path)
  
  ## Populate introduction message (stored in intro.R as html formatted text)
  output$intro <- renderText(intro)
  
  ## Populate animation selection page with example images and descriptions
  output$example <- renderUI({
    if(!(input$templates %in% c("Rainfall","Spider"))) return(NULL)
    else if(input$templates == "Rainfall") img(src = 'rainfall_ex.gif',height=6,width=12)
    else if(input$templates == "Spider") img(src = 'spider_ex.gif',height=6,width=12)
  })

  output$description <- renderText({
    if(input$templates == "Rainfall") rainfall_desc
    else if(input$templates == "Spider") spider_desc
    else(no_desc)
  })
  
  ## Populate data upload and formatting page
  output$data_desc <- renderUI({
    if(input$templates == "Rainfall")  h5("For Rainfall plots, please upload ADaM data.", align='center')
    else if(input$templates == "Spider")  h5("For Spider plots, please upload ADaM data.", align='center')
    else(blank)
  })
  
  # Uploading SDTM data triggers automatic tab creation and data preview
  shinyFileChoose(input, "upload_sdtm", session = session, root=current_root, filetypes=c('', 'sas7bdat'))
  
  observeEvent(input$upload_sdtm, {
    filename <- parseFilePaths(current_root, input$upload_sdtm)$datapath
    
    if(length(filename) > 0){
      if(length(sdtm_dat)==0){
        files_selected <<- as.data.frame(parseFilePaths(current_root, input$upload_sdtm))
        
        withProgress(message = 'Uploading data, rendering UI', value = 0.05, {
          
          for(i in 1:length(files_selected$name)) {
            # add a data preview tab for each uploaded dataset
            appendTab("preview_tabs", tabPanel(paste0("SDTM: ", files_selected$name[i]), br(),
                                               dataTableOutput(files_selected$name[i])),
                      select=TRUE)
            
            sdtm_tabnames[i] <<- paste0("SDTM: ", files_selected$name[i])
            
            # pull file paths from shinyFiles, use to read data using coastr
            files_selected$shortpath[i] <- substring(gsub(files_selected$name[i],"",files_selected$datapath[i]),1)
            sdtm_dat[[i]] <<- data.frame(import_cluwe_data(files_selected$shortpath[i], 
                                                           files_selected$name[i], strip_labels = TRUE))
            # minor formatting of AVISITN for animations
            sdtm_dat[[i]]$AVISITN_recod <<- ifelse(sdtm_dat[[i]]$AVISITN < 2, 2, sdtm_dat[[i]]$AVISITN)
            sdtm_dat[[i]]$AVISIT_recod <<- ifelse(sdtm_dat[[i]]$AVISITN_recod == 2, "Baseline", sdtm_dat[[i]]$AVISIT)
            
            # add variable selection tabs for each uploaded dataset (variables based on which template is selected)
            if(input$templates == "Rainfall"){
              
              appendTab("fun_inputs", tabPanel(paste0("SDTM: ", files_selected$name[i]), br(),
                                               selectInput(paste0("rainfall_aperiod_sdtm_",i),
                                                           paste0("Select an analysis period (required):"),
                                                           choices=unique(sdtm_dat[[i]]$APERIOD[!is.na(sdtm_dat[[i]]$APERIOD)])),
                                               selectInput(paste0("rainfall_param_sdtm_",i),
                                                           paste0("Select a parameter (required):"),
                                                           choices=unique(sdtm_dat[[i]]$PARAMCD)),
                                               selectInput(paste0("rainfall_trtarm_sdtm_",i),
                                                           paste0("Select a treatment arm variable (required):"),
                                                           choices=colnames(sdtm_dat[[i]])[which(substring(colnames(sdtm_dat[[i]]),1,3) ==
                                                                                                   "TRT")]),
                                               selectInput(paste0("rainfall_dtype_sdtm_",i),
                                                           paste0("Select a DTYPE in addition to NULL (optional):"),
                                                           choices=unique(sdtm_dat[[i]]$DTYPE[!is.na(sdtm_dat[[i]]$DTYPE)])),
                                               
                                               checkboxGroupInput(paste0("rainfall_flags_sdtm_",i), "Select the population flags:",
                                                                  choices=colnames(sdtm_dat[[i]])[substr(colnames(sdtm_dat[[i]]), 
                                                                                                         nchar(colnames(sdtm_dat[[i]]))-2+1, 
                                                                                                         nchar(colnames(sdtm_dat[[i]])))=="FL" & 
                                                                                                    colnames(sdtm_dat[[i]]) != "ABLFL"])),
                        select=TRUE)
            }
            
            else if(input$templates == "Spider"){
              appendTab("fun_inputs", tabPanel(paste0("SDTM: ", files_selected$name[i]), br(),
                                               selectInput(paste0("spider_parameter_sdtm_",i),
                                                           paste0("Select a Parameter (required):"),
                                                           choices=unique(sdtm_dat[[i]]$PARAMCD)),
                                               selectInput(paste0("spider_flag_sdtm_",i),
                                                           paste0("Select a Flag (required):"),
                                                           choices=colnames(sdtm_dat[[i]])[which(substr(colnames(sdtm_dat[[i]]), 
                                                                                                        nchar(colnames(sdtm_dat[[i]]))-2+1, 
                                                                                                        nchar(colnames(sdtm_dat[[i]])))=="FL")]),
                                               textInput(paste0("spider_colname_sdtm_",i),
                                                         paste0("Specify a Column Name (required):")),
                                               
                                               bsTooltip(paste0("spider_parameter_sdtm_",i), "Variable of interest",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus"),
                                               bsTooltip(paste0("spider_flag_sdtm_",i), "Time period",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus"),
                                               bsTooltip(paste0("spider_colname_sdtm_",i), "Display name.",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus")),
                        select=TRUE)
            }
            incProgress(1/(length(files_selected$name)-.5), detail = files_selected$name[i])
          }
          names(sdtm_dat) <<- as.character(files_selected$name)
        })
        sdtm_length <<- length(sdtm_dat)
      }
      
      
      else({
        filename <- parseFilePaths(current_root, input$upload_sdtm)$datapath
        
        if(length(filename) > 0 & !is.na(files_selected[1,1])){
          files_selected <<- parseFilePaths(current_root, input$upload_sdtm)
          
          withProgress(message = 'Uploading data, rendering UI', value = 0.05, {
            for(i in 1:length(files_selected$name)) {
              appendTab("preview_tabs", tabPanel(paste0("SDTM: ", files_selected$name[i]), br(),
                                                 dataTableOutput(files_selected$name[i])),
                        select=TRUE)
              
              j <- i + sdtm_length
              files_selected$shortpath[i] <- substring(gsub(files_selected$name[i],"",files_selected$datapath[i]),3)
              sdtm_dat[[j]] <<- data.frame(import_cluwe_data(paste0(current_path,files_selected$shortpath[i]),
                                                             files_selected$name[i], strip_labels = TRUE))
              sdtm_dat[[j]]$AVISITN_recod <<- ifelse(sdtm_dat[[j]]$AVISITN < 2, 2, sdtm_dat[[j]]$AVISITN)
              sdtm_dat[[j]]$AVISIT_recod <<- ifelse(sdtm_dat[[j]]$AVISITN_recod == 2, "Baseline", sdtm_dat[[j]]$AVISIT)
              
              if(input$templates == "Spider"){
                appendTab("fun_inputs", tabPanel(paste0("SDTM: ", files_selected$name[i]), br(),
                                                 selectInput(paste0("spider_parameter_sdtm_",j),
                                                             paste0("Select a Parameter:"),
                                                             choices=unique(sdtm_dat[[j]]$PARAMCD)),
                                                 selectInput(paste0("spider_flag_sdtm_",j),
                                                             paste0("Select a Flag:"),
                                                             choices=colnames(sdtm_dat[[j]])[which(substr(colnames(sdtm_dat[[j]]),
                                                                                                          nchar(colnames(sdtm_dat[[j]]))-2+1,
                                                                                                          nchar(colnames(sdtm_dat[[j]])))=="FL")]),
                                                 textInput(paste0("spider_colname_sdtm_", j),
                                                           paste0("Specify a Column Name:")),
                                                 
                                                 bsTooltip(paste0("spider_parameter_sdtm_", j), "Variable of interest",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus"),
                                                 bsTooltip(paste0("spider_flag_sdtm_",j), "Time period",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus"),
                                                 bsTooltip(paste0("spider_colname_sdtm_",j), "Display name.",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus")),
                          select=TRUE)
              }
              incProgress(1/(length(files_selected$name)-0.1), detail = files_selected$name[i])
            }
            names(sdtm_dat)[(sdtm_length+1):(sdtm_length+length(files_selected$name))] <<- as.character(files_selected$name)
          })
          sdtm_length <<- length(sdtm_dat)
        }
      })
      
      
      
      all_times <<- lapply(1:sdtm_length, function(x) sdtm_dat[[x]]$AVISIT_recod)
      
      if(sdtm_length == 1){
        unique_shared_timepoints <<- all_times %>%
          unlist() %>%
          unique()
        unique_shared_timepoints <<- unique_shared_timepoints[!is.na(unique_shared_timepoints)]
      }
      else({
        unique_shared_timepoints <<- unlist(Reduce(intersect, all_times))
      })
      
      output$timepoints <- renderUI({
        tags$div(align = 'left',
                 class = 'multicol',
                 checkboxGroupInput("time_selections", NULL, selected = unique_shared_timepoints,
                                    choices = unique_shared_timepoints))
      })
      
      observeEvent(input$numrows, {
        if(input$numrows=="head"){
          lapply(1:length(sdtm_dat), function(i) {
            output[[as.character(names(sdtm_dat)[i])]] <- renderDataTable(head(sdtm_dat[[names(sdtm_dat)[i]]]),
                                                                          options = list(scrollX = TRUE,
                                                                                         paging = FALSE))
          })
        }
        else {
          lapply(1:length(sdtm_dat), function(i) {
            output[[as.character(names(sdtm_dat)[i])]] <- renderDataTable(sdtm_dat[[names(sdtm_dat)[i]]],
                                                                          options = list(scrollX = TRUE))
          })
        }
      })
    }
  })
  
  # Uploading ADaM data triggers automatic tab creation and data preview
  shinyFileChoose(input, "upload_adam", session = session, root=current_root, filetypes=c('', 'sas7bdat'))
  
  observeEvent(input$upload_adam, {
      filename <- parseFilePaths(current_root, input$upload_adam)$datapath

      if(length(filename) > 0){
        if(length(adam_dat)==0){
          files_selected <<- as.data.frame(parseFilePaths(current_root, input$upload_adam))
        
          withProgress(message = 'Uploading data, rendering UI', value = 0.05, {
            
            for(i in 1:length(files_selected$name)) {
              # add a data preview tab for each uploaded dataset
              appendTab("preview_tabs", tabPanel(paste0("ADaM: ", files_selected$name[i]), br(),
                                                 dataTableOutput(files_selected$name[i])),
                        select=TRUE)
              
              adam_tabnames[i] <<- paste0("ADaM: ", files_selected$name[i])
              
              # pull file paths from shinyFiles, use to read data using coastr
              # browser()
              files_selected$shortpath[i] <- substring(gsub(files_selected$name[i],"",files_selected$datapath[i]),1)
              adam_dat[[i]] <<- data.frame(import_cluwe_data(files_selected$shortpath[i], 
                                                             files_selected$name[i], strip_labels = TRUE))
              # minor formatting of AVISITN for animations
              adam_dat[[i]]$AVISITN_recod <<- ifelse(adam_dat[[i]]$AVISITN < 2, 2, adam_dat[[i]]$AVISITN)
              adam_dat[[i]]$AVISIT_recod <<- ifelse(adam_dat[[i]]$AVISITN_recod == 2, "Baseline", adam_dat[[i]]$AVISIT)
              
              # add variable selection tabs for each uploaded dataset (variables based on which template is selected)
              if(input$templates == "Rainfall"){
                
                appendTab("fun_inputs", tabPanel(paste0("ADaM: ", files_selected$name[i]), br(),
                                                 selectInput(paste0("rainfall_aperiod_adam_",i),
                                                             paste0("Select an analysis period (required):"),
                                                             choices=unique(adam_dat[[i]]$APERIOD[!is.na(adam_dat[[i]]$APERIOD)])),
                                                 selectInput(paste0("rainfall_param_adam_",i),
                                                             paste0("Select a parameter (required):"),
                                                             choices=unique(adam_dat[[i]]$PARAMCD)),
                                                 selectInput(paste0("rainfall_trtarm_adam_",i),
                                                             paste0("Select a treatment arm variable (required):"),
                                                             choices=colnames(adam_dat[[i]])[which(substring(colnames(adam_dat[[i]]),1,3) ==
                                                                                                     "TRT")]),
                                                 selectInput(paste0("rainfall_dtype_adam_",i),
                                                             paste0("Select a DTYPE in addition to NULL (optional):"),
                                                             choices=unique(adam_dat[[i]]$DTYPE[!is.na(adam_dat[[i]]$DTYPE)])),
                                                 
                                                 checkboxGroupInput(paste0("rainfall_flags_adam_",i), "Select the population flags:",
                                                                    choices=colnames(adam_dat[[i]])[substr(colnames(adam_dat[[i]]), 
                                                                                                           nchar(colnames(adam_dat[[i]]))-2+1, 
                                                                                                           nchar(colnames(adam_dat[[i]])))=="FL" & 
                                                                                                      colnames(adam_dat[[i]]) != "ABLFL"])),
                          select=TRUE)
              }
              
              else if(input$templates == "Spider"){
                appendTab("fun_inputs", tabPanel(paste0("ADaM: ", files_selected$name[i]), br(),
                                                 selectInput(paste0("spider_parameter_adam_",i),
                                                             paste0("Select a Parameter (required):"),
                                                             choices=unique(adam_dat[[i]]$PARAMCD)),
                                                 selectInput(paste0("spider_flag_adam_",i),
                                                             paste0("Select a Flag (required):"),
                                                             choices=colnames(adam_dat[[i]])[which(substr(colnames(adam_dat[[i]]), 
                                                                                                          nchar(colnames(adam_dat[[i]]))-2+1, 
                                                                                                          nchar(colnames(adam_dat[[i]])))=="FL")]),
                                                 textInput(paste0("spider_colname_adam_",i),
                                                           paste0("Specify a Column Name (required):")),
                                                 
                                                 bsTooltip(paste0("spider_parameter_adam_",i), "Variable of interest",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus"),
                                                 bsTooltip(paste0("spider_flag_adam_",i), "Time period",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus"),
                                                 bsTooltip(paste0("spider_colname_adam_",i), "Display name.",
                                                           "right", options = list(container = "body"),
                                                           trigger="focus")),
                          select=TRUE)
              }
              incProgress(1/(length(files_selected$name)-.5), detail = files_selected$name[i])
            }
            names(adam_dat) <<- as.character(files_selected$name)
          })
          adam_length <<- length(adam_dat)
        }
      
      
      else({
        filename <- parseFilePaths(current_root, input$upload_adam)$datapath
  
        if(length(filename) > 0 & !is.na(files_selected[1,1])){
          files_selected <<- parseFilePaths(current_root, input$upload_adam)
  
          withProgress(message = 'Uploading data, rendering UI', value = 0.05, {
            for(i in 1:length(files_selected$name)) {
              appendTab("preview_tabs", tabPanel(paste0("ADaM: ", files_selected$name[i]), br(),
                                                 dataTableOutput(files_selected$name[i])),
                        select=TRUE)
  
            j <- i + adam_length
            files_selected$shortpath[i] <- substring(gsub(files_selected$name[i],"",files_selected$datapath[i]),3)
            adam_dat[[j]] <<- data.frame(import_cluwe_data(paste0(current_path,files_selected$shortpath[i]),
                                                           files_selected$name[i], strip_labels = TRUE))
            adam_dat[[j]]$AVISITN_recod <<- ifelse(adam_dat[[j]]$AVISITN < 2, 2, adam_dat[[j]]$AVISITN)
            adam_dat[[j]]$AVISIT_recod <<- ifelse(adam_dat[[j]]$AVISITN_recod == 2, "Baseline", adam_dat[[j]]$AVISIT)
  
            if(input$templates == "Spider"){
              appendTab("fun_inputs", tabPanel(paste0("ADaM: ", files_selected$name[i]), br(),
                                               selectInput(paste0("spider_parameter_adam_",j),
                                                           paste0("Select a Parameter:"),
                                                           choices=unique(adam_dat[[j]]$PARAMCD)),
                                               selectInput(paste0("spider_flag_adam_",j),
                                                           paste0("Select a Flag:"),
                                                           choices=colnames(adam_dat[[j]])[which(substr(colnames(adam_dat[[j]]),
                                                                                                        nchar(colnames(adam_dat[[j]]))-2+1,
                                                                                                        nchar(colnames(adam_dat[[j]])))=="FL")]),
                                               textInput(paste0("spider_colname_adam_", j),
                                                         paste0("Specify a Column Name:")),
  
                                               bsTooltip(paste0("spider_parameter_adam_", j), "Variable of interest",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus"),
                                               bsTooltip(paste0("spider_flag_adam_",j), "Time period",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus"),
                                               bsTooltip(paste0("spider_colname_adam_",j), "Display name.",
                                                         "right", options = list(container = "body"),
                                                         trigger="focus")),
                        select=TRUE)
            }
            incProgress(1/(length(files_selected$name)-0.1), detail = files_selected$name[i])
          }
          names(adam_dat)[(adam_length+1):(adam_length+length(files_selected$name))] <<- as.character(files_selected$name)
        })
        adam_length <<- length(adam_dat)
        }
      })
    
  
  
      all_times <<- lapply(1:adam_length, function(x) adam_dat[[x]]$AVISIT_recod)
  
      if(adam_length == 1){
        unique_shared_timepoints <<- all_times %>%
          unlist() %>%
          unique()
        unique_shared_timepoints <<- unique_shared_timepoints[!is.na(unique_shared_timepoints)]
      }
      else({
        unique_shared_timepoints <<- unlist(Reduce(intersect, all_times))
      })
  
      output$timepoints <- renderUI({
        tags$div(align = 'left',
                 class = 'multicol',
                 checkboxGroupInput("time_selections", NULL, selected = unique_shared_timepoints,
                                    choices = unique_shared_timepoints))
      })
  
      observeEvent(input$numrows, {
        if(input$numrows=="head"){
          lapply(1:length(adam_dat), function(i) {
            output[[as.character(names(adam_dat)[i])]] <- renderDataTable(head(adam_dat[[names(adam_dat)[i]]]),
                                                                          options = list(scrollX = TRUE,
                                                                                         paging = FALSE))
          })
        }
        else {
          lapply(1:length(adam_dat), function(i) {
            output[[as.character(names(adam_dat)[i])]] <- renderDataTable(adam_dat[[names(adam_dat)[i]]],
                                                                          options = list(scrollX = TRUE))
          })
        }
      })
    }
  })
  
  # Button function to remove data tab and associated stored data set
  observeEvent(input$remove_tab, {
    if(length(sdtm_dat) != 0){
      sdtm_dat[[substring(input$fun_inputs,7)]] <<- NULL
    }
    if(length(adam_dat) != 0){
      adam_dat[substring(input$fun_inputs,7)] <<- NULL
    }
    removeTab("preview_tabs", target=input$preview_tabs)
    removeTab("fun_inputs", target=input$fun_inputs)
    
    if(length(adam_dat) == 0 & length(sdtm_dat) == 0){
      output$timepoints <- renderUI({h5(strong("Please re-upload data to view timepoints."))})
    }
  })
  
  # Activates background data formatting based on selected template
  observeEvent(input$dataformat,{
    if(length(sdtm_dat) == 0 & length(adam_dat) == 0) {
      shinyjs::disable("dataformat")
    }
    else{
      if(input$templates == "Rainfall") {
        if(length(sdtm_dat) != 0){
          withProgress(message = 'Formatting data for Rainfall Plot animation', value = 0, {
            lapply(1:length(sdtm_dat), function(r) {
              rainfall_sdtm[[r]] <<- rainfall_data_format(sdtm_dat[[r]],
                                                          eval(parse(text=paste0('input$rainfall_aperiod_sdtm_',r))),
                                                          eval(parse(text=paste0('input$rainfall_param_sdtm_',r))),
                                                          input$rainfall_flags_sdtm,
                                                          input$time_selections,
                                                          eval(parse(text=paste0('input$rainfall_trtarm_sdtm_',r))),
                                                          eval(parse(text=paste0('input$rainfall_dtype_sdtm_',r))))
            })
          })
        }    
        if(length(adam_dat) != 0){
          withProgress(message = 'Formatting data for rainfall Plot animation', value = 0, {
            lapply(1:length(adam_dat), function(r) {
              rainfall_adam[[r]] <<- rainfall_data_format(adam_dat[[r]],
                                                          eval(parse(text=paste0('input$rainfall_aperiod_adam_',r))),
                                                          eval(parse(text=paste0('input$rainfall_param_adam_',r))),
                                                          eval(parse(text=paste0('input$rainfall_flags_adam_',r))),
                                                          input$time_selections,
                                                          eval(parse(text=paste0('input$rainfall_trtarm_adam_',r))),
                                                          eval(parse(text=paste0('input$rainfall_dtype_adam_',r))))
            })
          })
        }
      }
      else if(input$templates == "Spider") {
        if(length(sdtm_dat) != 0){
          withProgress(message = 'Formatting data for Spider Plot animation', value = 0, {
            lapply(1:length(sdtm_dat), function(r) {
              spider_sdtm[[r]] <<- spider_data_format(sdtm_dat[[r]],
                                                      eval(parse(text=paste0('input$spider_parameter_sdtm_',r))),
                                                      eval(parse(text=paste0('input$spider_flag_sdtm_',r))),
                                                      eval(parse(text=paste0('input$spider_colname_sdtm_',r))),
                                                      input$time_selections)
            })
            spider_sdtm_set <<- spider_sdtm %>% purrr::reduce(dplyr::left_join, by=c("TRTP","AVISITN"))
            colnames(spider_sdtm_set)[colnames(spider_sdtm_set)=="TRTP"] <<- "TRT"
            
            output$col_selector <- renderUI({
              unique_trts <- unique(spider_sdtm_set$TRT)
              lapply(1:length(unique_trts), function(i) {
                colourpicker::colourInput(paste0("col",i), paste0("Select a color for ", unique_trts[i],":"))
              })
            })
            
            color_selections <<- NULL
            observe({
              for(i in 1:length(unique(spider_sdtm_set$TRT))){
                color_selections[i] <<- eval(parse(text=paste0('input$col',i)))
              }
            })
          })
        }
        if(length(adam_dat) != 0){
          withProgress(message = 'Formatting data for Spider Plot animation', value = 0, {
            lapply(1:length(adam_dat), function(r) {
              spider_adam[[r]] <<- spider_data_format(adam_dat[[r]],
                                                      eval(parse(text=paste0('input$spider_parameter_adam_',r))),
                                                      eval(parse(text=paste0('input$spider_flag_adam_',r))),
                                                      eval(parse(text=paste0('input$spider_colname_adam_',r))),
                                                      input$time_selections)
            })
            spider_adam_set <<- spider_adam %>% purrr::reduce(dplyr::left_join, by=c("TRTP","AVISITN"))
            colnames(spider_adam_set)[colnames(spider_adam_set)=="TRTP"] <<- "TRT"
            
            output$col_selector <- renderUI({
              unique_trts <- unique(spider_adam_set$TRT)
              lapply(1:length(unique_trts), function(i) {
                colourpicker::colourInput(paste0("col",i), paste0("Select a color for ", unique_trts[i],":"))
              })
            })
            
            color_selections <<- NULL
            observe({
              for(i in 1:length(unique(spider_adam_set$TRT))){
                color_selections[i] <<- eval(parse(text=paste0('input$col',i)))
              }
            })
            
          })
        }
      }
    }
  })
  
  # Button to uncheck all timepoint boxes
  observeEvent(input$uncheckall, {
    updateCheckboxGroupInput(session=session, inputId="time_selections", 
                             choices=unique_shared_timepoints, selected=NULL)
  })
  
  observeEvent(input$checkall, {
    updateCheckboxGroupInput(session=session, inputId="time_selections", 
                             choices=unique_shared_timepoints, selected=unique_shared_timepoints)
  })
  
  ## Populate customization page
  observe({
    if(input$templates == "Please make a selection:"){
      output$customize_ui <- renderUI(box(width=6,
                                          h4("Please select a template on the Animation Selection tab before
                                             proceeding.")))
    }
    else if(input$templates == "Rainfall"){
      output$customize_ui <- renderUI({conditionalPanel(condition = "input.templates == 'Rainfall'", {
        box(width=8,
            column(width=6,
                   textInput("plot_title","Enter a plot title (required):"),br(),
                   textInput("rainfall_studyname","Enter study name:"),br(),
                   textInput("rainfall_xlab","Specify an x-axis label:"),
                   textInput("rainfall_ylab","Specify a y-axis label:"),
                   column(width=6,
                          radioButtons("rainfall_bar_type","Select a bar type:",
                                       choices=c("short", "long")),
                          radioButtons("rainfall_clock_type","Select a clock type:",
                                       choices=c("line", "round")),
                          radioButtons("rainfall_zoom_type","Select a zoom level:",
                                       choices=c("zoomed", "default"))),
                   column(width=6,
                          textInput("speed","Speed in frames per second 
                                                                                    (FPS, default=24):", value=24)),
                   column(width=6,
                          textInput("st_end_pause","Pause duration for 1st and
                                                                                    last periods (seconds):", value=2))),
            column(width=6, 
                   # uiOutput("col_selector")
                   textInput("rainfall_endpoint","Specify a study endpoint (e.g. PASI, EASI):"),
                   
                   selectInput("rainfall_trt1","Select the treatment for the left plot:",
                               choices=unique(rainfall_adam[[1]]$treatment)),
                   
                   # textInput("rainfall_trt1_text","Specify the full treatment name:"),
                   textInput("rainfall_trt1_abr","Specify a treatment abbreviation for 
                                                                             annotations:"),
                   
                   selectInput("rainfall_trt2","Select the treatment for the left plot:",
                               choices=unique(rainfall_adam[[1]]$treatment)),
                   
                   # textInput("rainfall_trt2_text","Specify the full treatment name:"),
                   textInput("rainfall_trt2_abr","Specify a treatment abbreviation for 
                                                                             annotations:")))
      })
      })
    }
    else if(input$templates == "Spider"){
      output$customize_ui <- renderUI(conditionalPanel(condition = "input.templates == 'Spider'", {
        box(width=8,
            column(width=6,
                   textInput("plot_title","Enter a plot title (required):"),br(),
                   textInput("footnote1","Enter a primary footnote (optional):"),br(),
                   textInput("footnote2","Enter a secondary footnote (optional):"),br(),
                   column(width=6,
                          textInput("speed","Speed in frames per second 
                                                                                   (FPS, default=24):", value=24)),
                   column(width=6,
                          textInput("st_end_pause","Pause duration for 1st and
                                                                                    last periods (seconds):",
                                    value=2))),
            column(width=6,
                   uiOutput("col_selector"),
                   radioButtons("opacity","Select a shape fill style:",
                                choiceNames=c("Clear","Filled"),
                                choiceValues=c(0,.8))))
      }))
    }
  })
  
  ## Populate preview page
  # Select output file location (text input section updates to path if brower method used)
  observe({
    shinyDirChoose(input, 'dir_select', roots = c(home = "/lillyce"), filetypes = c('', 'sas7bdat'))
    dir <- reactive(parseDirPath(roots = c(home = "/lillyce"), input$dir_select))
    # updateTextInput(session, "selected_path", label = "Specify an output folder:", value = dir())
    updateTextInput(session, "selected_path", label = "Specify an output folder (required):", value = "/lillyce/qa/ly3009104/common/usersanderbox/bnealy")
  })
  
  # Producing frames runs the plotting function, posts preview image to UI
  observeEvent(input$makeframes,{
    
    if(input$templates == "Rainfall"){
      make_it_rain(rainfall_adam[[1]], input$rainfall_studyname, input$rainfall_endpoint, 
                   input$rainfall_xlab, input$rainfall_ylab, input$selected_path, input$plot_title, 
                   input$rainfall_bar_type, "", "", input$rainfall_clock_type,
                   input$rainfall_zoom_type, input$rainfall_trt1, input$rainfall_trt1_abr,
                   input$rainfall_trt2, input$rainfall_trt2_abr)
      
      output$preview <- renderImage({
        if(file.exists(paste0(input$selected_path,'/RAINFALL0001.JPEG'))) {
          return(list(src = paste0(input$selected_path,'/RAINFALL0001.JPEG'),
                      height = "540px",
                      width = "960px",
                      contentType = "image/jpeg"))
        }
        else if (!file.exists(paste0(input$selected_path,'/RAINFALL0001.JPEG'))) {
          return(NULL)
        }
      })
    }
    
    if(input$templates == "Spider"){
      plot_labels <<- NULL
      
      for(i in 1:length(files_selected$name)) {
        plot_labels[i] <- eval(parse(text=paste0('input$spider_colname_adam_',i)))
      }
      
      eensy_weensy_spider(spider_adam_set, plot_labels, input$selected_path,
                          input$plot_title, input$footnote1, input$footnote2,
                          color_selections, input$opacity, input$speed,
                          input$st_end_pause, input$time_selections)
      
      output$preview <- renderImage({
        if(file.exists(paste0(input$selected_path,'/COARADAR0001.JPEG'))) {
          return(list(src = paste0(input$selected_path,'/COARADAR0001.JPEG'),
                      height = "540px",
                      width = "960px",
                      contentType = "image/jpeg"))
        }
        else if (!file.exists(paste0(input$selected_path,'/COARADAR0001.JPEG'))) {
          return(NULL)
        }
      })
    }
    output$preview_text <- renderUI({h5(strong("Preview image:"))})
  })
  
  ## Populate render and save page
  observeEvent(input$punch_it_chewie,{
    if(input$templates == "Rainfall"){
      rainfall_animator(input$selected_path, delete_choice=input$delete_frames, speed=input$speed)
    }
    else if(input$templates == "Spider"){
      spider_animator(input$selected_path, delete_choice=input$delete_frames, speed=input$speed)
    }
  })
  
  # Reset button to remove all inputs, delete data sets, and return to template selection
  observeEvent(input$square_one,{
    all_inputs <<- names(reactiveValuesToList(input))
    all_tabnames <<- c(adam_tabnames,sdtm_tabnames)
    datsetnum <<- length(all_tabnames)
    
    for(i in 1:datsetnum){
      newtab <- all_tabnames[i]        
      removeTab("fun_inputs", target = newtab)
      removeTab("preview_tabs", target = newtab)
    }
    
    sdtm_dat <<- NULL
    adam_dat <<- NULL
    rainfall_adam <<- NULL
    rainfall_sdtm <<- NULL
    spider_sdtm <<- NULL
    spider_adam <<- NULL
    all_times <<- NULL
    
    if(length(adam_dat) == 0 & length(sdtm_dat) == 0){
      output$timepoints <- renderUI({h5(strong("Please upload data to view timepoints."))})
    }
    
    for(i in 1:length(all_inputs)) reset(paste0(all_inputs[i]))
    
    newtab <- switch(input$menu_tabs,
                     "save" = "welcome",
                     "preview" = "welcome",
                     "customize" = "welcome",
                     "upload" = "welcome",
                     "select" = "welcome"
    )
    
    updateTabItems(session, "menu_tabs", newtab)
  })
  
  observeEvent(input$restart,{
    all_inputs <<- names(reactiveValuesToList(input))
    all_tabnames <<- c(adam_tabnames,sdtm_tabnames)
    datsetnum <<- length(all_tabnames)
    
    for(i in 1:datsetnum){
      newtab <- all_tabnames[i]        
      removeTab("fun_inputs", target = newtab)
      removeTab("preview_tabs", target = newtab)
    }
    
    sdtm_dat <<- NULL
    adam_dat <<- NULL
    rainfall_adam <<- NULL
    rainfall_sdtm <<- NULL
    spider_sdtm <<- NULL
    spider_adam <<- NULL
    all_times <<- NULL
    
    if(length(adam_dat) == 0 & length(sdtm_dat) == 0){
      output$timepoints <- renderUI({h5(strong("Please upload data to view timepoints."))})
    }
    
    for(i in 1:length(all_inputs)) reset(paste0(all_inputs[i]))
    
    newtab <- switch(input$menu_tabs,
                     "save" = "select"
    )
    
    updateTabItems(session, "menu_tabs", newtab)
    
    output$preview <- renderImage({
      return(NULL)
    })
  })
  
  ## Stop the app when browser or tab is closed
  session$onSessionEnded(stopApp)
}


shinyApp(ui = ui, server = server)
