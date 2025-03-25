# TODO:
# updating dropdowns, resetting dropdowns, etc.
# correlation is initially empty and does not say all, once you add specific correlations you need to manually delete all
# integrate new graphs (figure out method to read in and call, matching graphs function no longer needed, update graphing function for 2 graphs merged)

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
  output_dir <- "output"
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
  
  data_list <- load_combo_data(combo = "pooling.none.motion.none") # load just the initial combo's data
  v$study_init <- data_list$study
  
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
  
  
  ##### Extract information from files #####
  
  file_names <- list.files(output_dir) #get filenames
  
  # Extract filter info from each filename
  extract_info <- function(file_name) {
    graph_type <- str_extract(file_name, "^(simci|matrix|brain)")
    dataset <- str_extract(file_name, "(all|abcd|hbn|hcp|hcp_ep|pnc|slim|ukb)")
    map_type <- str_extract(file_name, "(fc|act|all)")
    task_type <- str_extract(file_name, "(rest|mid|nback|sst|rest_nih|rest_srs|rest_diag|emotion|gambling|relational|social|wm)")
    
    test_type <- str_extract(file_name, paste0("(?<=", map_type, ")\\w+(?=", task_type, ")"))
    test_type <- str_replace_all(test_type, "^_|_$", "")
    
    pooling_type <- str_extract(file_name, "(?<=pooling\\.)\\w+")
    motion_type <- str_extract(file_name, "(?<=motion\\.)\\w+")
    
    correlation_name_pattern <- paste0("(?<=", task_type, "_)[^\\-]+(?=-pooling)")
    correlation_name <- str_extract(file_name, correlation_name_pattern)
    
    return(data.frame(
      file_name = file_name,
      graph_type = graph_type,
      dataset = dataset,
      map_type = map_type,
      task_type = task_type,
      test_type = test_type,
      pooling_type = pooling_type,
      motion_type = motion_type,
      correlation_name = correlation_name
    ))
  }
  
  file_info_table <- do.call(rbind, lapply(file_names, extract_info))
  print(file_info_table)
  
  
  ##### Filter files and plot #####
  matched_files <- reactiveVal(NULL)  # Create a reactive value to store results
  
  # Filters files by user input
  filtered_files <- reactive({
    print(input$correlation)
    filter_files(file_info_table, input$dataset, input$map_type, input$task,
                 input$test_type, input$pooling, input$motion, input$correlation)
  })
  
  #When apply filters button pressed, plot
  observeEvent(input$apply_filters_btn, {
    filtered_data <- filtered_files()
    matched_result <- match_simci_with_matrix(filtered_data)
    matched_files(matched_result)
    print(matched_files())
    create_explorer_plots(input, output, matched_files(), v)
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
    matched_files(NULL)
    filtered_files <- filter_files(file_info_table, "*", "*", "*", "*", "none", "none", "*")
    matched_result <- match_simci_with_matrix(filtered_files)
    matched_files(matched_result)
    create_explorer_plots(input, output, matched_files(), v)
  })
  
  ##### Functions #####
  #Plotting
  create_explorer_plots <- function(input, output, matched_files, v) {
    if (input$tab == "Explorer") {
      print("Creating placeholders for explorer plots")
      req(matched_files())
      print("Matched files loaded")
      
      output$histograms <- renderUI({
        validate(need(nrow(matched_files()) > 0, "No data available for the selected parameters."))
        print("Generating UI elements")
        
        v$plot_output_list <- lapply(1:nrow(matched_files()), function(i) {
          plotname_simci <- paste0("plot_simci_", i)
          plotname_spatial <- paste0("plot_spatial_", i)
          
          tagList(
            fluidRow(
              column(6, imageOutput(plotname_simci, height = "200px", width = "100%")),
              column(6, imageOutput(plotname_spatial, height = "200px", width = "100%"))
            )
          )
        })
        
        do.call(tagList, v$plot_output_list)
      })
      print("Done creating placeholders for plots")
      
      print("Filling explorer SimCI and Spatial/Matrix plots")
      for (i in 1:nrow(matched_files())) {
        print(paste("Processing file", i))  
        local({
          my_i <- i
          plotname_simci <- paste0("plot_simci_", my_i)
          plotname_spatial <- paste0("plot_spatial_", my_i)
          
          simci_image_path <- file.path("output", matched_files()[my_i, 1])  
          spatial_image_path <- file.path("output", matched_files()[my_i, 2])  
          
          print(paste("SimCI path:", simci_image_path))  
          print(paste("Spatial path:", spatial_image_path))  
          
          output[[plotname_simci]] <- renderImage({
            if (file.exists(simci_image_path)) {
              list(src = simci_image_path, contentType = 'image/png', width = "100%", height = 200)
            } else {
              print(paste("SimCI file not found:", simci_image_path))  
              list(src = "www/placeholder.png", contentType = 'image/png', width = "100%", height = 200)
            }
          }, deleteFile = FALSE)
          
          output[[plotname_spatial]] <- renderImage({
            if (file.exists(spatial_image_path)) {
              list(src = spatial_image_path, contentType = 'image/png', width = "100%", height = 200)
            } else {
              print(paste("Spatial file not found:", spatial_image_path))  
              list(src = "www/placeholder.png", contentType = 'image/png', width = "100%", height = 200)
            }
          }, deleteFile = FALSE)
        })
      }
      print("Done filling explorer plots")
    }
  }
  
  # Filter filenames based on desired filters
  filter_files <- function(file_info_table, dataset, map_type, task_type, test_type, pooling_type, motion_type, correlation) {
    if (is.null(correlation)) {
      correlation <- "*"
    }
    filtered_table <- file_info_table[
      (dataset == "*" | file_info_table$dataset %in% dataset) &
        (map_type == "*" | file_info_table$map_type %in% map_type) &
        (task_type == "*" | file_info_table$task_type %in% task_type) &
        (test_type == "*" | file_info_table$test_type %in% test_type) &
        ((pooling_type == "none" & file_info_table$pooling_type == "none") |
           (pooling_type != "none" & file_info_table$pooling_type %in% pooling_type)) &
        ((motion_type == "none" & file_info_table$motion_type == "none") |
           (motion_type != "none" & file_info_table$motion_type %in% motion_type))&
        ((test_type != "r") | (correlation == "*" | file_info_table$correlation_name %in% correlation)),
    ]
    return(filtered_table)
    print(filtered_table)
  }
  
  #Pair simci files with matrix/brain files
  match_simci_with_matrix <- function(filtered_files) {
    if (nrow(filtered_files) == 0) return(data.frame())  # Prevent errors if no files exist
    
    simci_files <- filtered_files[filtered_files$graph_type == "simci", c("file_name"), drop = FALSE]
    matrix_brain_files <- filtered_files[filtered_files$graph_type %in% c("matrix", "brain"), c("file_name"), drop = FALSE]
    
    if (nrow(simci_files) == 0 | nrow(matrix_brain_files) == 0) return(data.frame())  # No matches possible
    
    # remove prefix for matching
    simci_files$base_name <- sub("^simci-", "", simci_files$file_name)
    matrix_brain_files$base_name <- sub("^(matrix-|brain-)", "", matrix_brain_files$file_name)
    
    # Match 
    matched_files <- merge(simci_files, matrix_brain_files, by = "base_name", all.x = TRUE, suffixes = c("_simci", "_matrix_brain"))
    
    # Return only the relevant columns
    matched_files <- matched_files[, c("file_name_simci", "file_name_matrix_brain")]
    
    return(matched_files)
  }
  
}
