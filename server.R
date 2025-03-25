# TODO:
# updating dropdowns, resetting dropdowns, etc.
# correlation is initially empty and does not say all, once you add specific correlations you need to manually delete all
# integrate new graphs

library(shinyscreenshot)
library(gridExtra)
library(ggplot2)
library(reshape)
library(fields)
library(oro.nifti)
library(neurobase)
library(shinyjs)
library(bslib)
library(BrainEffeX.utils)
library(stringr)

source("helpers.R")

server <- function(input, output, session) {
  ##### Setup #####
  output_dir <- "cns"
  v <- reactiveValues()
  
  #info displays
  createModalNavigationObservers(input, session)
  observeEvent(c(input$apply_filters_btn, input$reset_btn), {
    output$dynamicPanel <- renderUI({
      createDynamicPanel(input, v$study_init)
    })
  })
  
  #clicks button when app opens to display graphs initially with default filters
  observeEvent(input$tab, {
    if (input$tab == "Explorer") {
      click("apply_filters_btn")
    }
  })
  
  #data_list <- load_combo_data(combo = "pooling.none.motion.none") # load just the initial combo's data
  load("data/study.RData") # will load table called 'study'
  v$study_init <- study
  
  v$filters <- reactiveValues(
    dataset = "*",
    map_type = "*",
    task = "*",
    test_type = "*",
    pooling = "none",
    motion = "none",
    correlation = "*"
  )
  observe({
    # Update UI selectInput choices dynamically (moved out of "ui" so only pass data directly to server)
    updateSelectInput(session, "dataset", choices = c("All" = "*", unique(v$study_init$dataset)))
    updateSelectInput(session, "map_type", choices = c("All" = "*", unique(v$study_init$map_type)))
    print(paste0('task choices initially ', unique(v$study_init$test_component_1)))
    updateSelectInput(session, "task", choices = c("All" = "*", unique(v$study_init$test_component_1)))
    updateSelectInput(session, "test_type", choices = c("All" = "*", unique(v$study_init$orig_stat_type)))
    updateSelectInput(session, "correlation", choices = c("All" = "*", unique(v$study_init[v$study_init$orig_stat_type == "r", "test_component_2"])))
  })
  
  observeEvent(input$downloadData, {
    browseURL("https://osf.io/cwnjd/files/osfstorage?view_only=")
  })
  
  ##### Filter files and plot #####
  # Filters files by user input
  filtered_files <- reactive({
    return(filter_files(v$study_init, input$dataset, input$map_type, input$task,
                 input$test_type, input$correlation))
    #print(filtered_files)
  })
  
  #When apply filters button pressed, plot
  observeEvent(input$apply_filters_btn, {
    study_filtered <- filtered_files()
    print("study_filered: ")
    print(study_filtered)
    create_explorer_plots(input, output, study_filtered, v)
  })
  
  observeEvent(input$reset_btn, {
    # Reset input filters to their default values
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    updateSelectInput(session, "pooling", selected = "none")
    updateSelectInput(session, "motion", selected = "none")
    updateSelectInput(session, "correlation", selected = "*")
    #re-plot
    study_filtered <- filter_files(v$study_init, "*", "*", "*", "*", "*")
    create_explorer_plots(input, output, study_filtered, v)
  })
  
  ##### Functions #####
  #Plotting
  create_explorer_plots <- function(input, output, study_filtered, v) {
    if (input$tab == "Explorer") {
      print("Creating placeholders for explorer plots")
      req(study_filtered)
      print("Filtered files loaded")
      
      output$histograms <- renderUI({
        validate(need(nrow(study_filtered) > 0, "No data available for the selected parameters."))
        print("Generating UI elements")
        
        v$plot_output_list <- lapply(1:nrow(study_filtered), function(i) {
          plotname <- paste0("plot", i)
          tagList(
            fluidRow(
              column(6, imageOutput(plotname, height = "200px", width = "100%")),
            )
          )
        })
        
        do.call(tagList, v$plot_output_list)
      })
      print("Done creating placeholders for plots")
      
      print("Filling explorer SimCI and Spatial/Matrix plots")
      for (i in 1:nrow(study_filtered)) {
        print(paste("Processing file", i))  
        local({
          my_i <- i
          plotname<- paste0("plot", my_i)
          
          image_path <- paste0("./cns/", input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", study_filtered[my_i, 3], ".svg")
          print(paste("File path:", image_path))  
          
          output[[plotname]] <- renderImage({
            if (file.exists(image_path)) {
              print("image exists")
              list(src = image_path, width = "100%", height = 200)
            } else {
              print(paste("SimCI file not found:", image_path))  
              list(src = "www/placeholder.png", contentType = 'image/svg+xml', width = "100%", height = 200)
            }
          }, deleteFile = FALSE)
        })
      }
      print("Done filling explorer plots")
    }
  }
  
  # Filter filenames based on desired filters
  filter_files <- function(file_info, dataset, map_type, task_type, test_type, correlation) {
    if (is.null(correlation)) {
      correlation <- "*"
    }
    filtered_table <- file_info[
      (dataset == "*" | file_info$dataset %in% dataset) &
        (map_type == "*" | file_info$map_type %in% map_type) &
        (task_type == "*" | file_info$test_component_1 %in% task_type) &
        (test_type == "*" | file_info$orig_stat_type %in% test_type) &
        ((test_type != "r") | (correlation == "*" | file_info$test_component_2 %in% correlation)),
    ]
    return(filtered_table)
  }
}
