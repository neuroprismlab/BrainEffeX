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


# Add filters choices limiting based on previous filter selection 
# Make this work with the correlation filter
# Add Download button
# Add Screenshot button
# Fix ordering of plots displayed (after filtering, then the previously displayed graphs stay at the top and don't have the same order as when opened)
# When changing filters, the ui changes the number of spaces for graphs but then does not update graphs till apply filters button is hit


server <- function(input, output, session) {
  ##### Setup #####
  output_dir <- "output"
  v <- reactiveValues()
  
  data_list <- load_combo_data(combo = "pooling.none.motion.none") # load just the initial combo's data
  v$study_init <- data_list$study
  
  v$filters <- reactiveValues(
    dataset = "*",
    map_type = "*",
    task = "*",
    test_type = "*",
    pooling = "none",
    motion = "none"
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
  
  
  ##### Button Logic #####
  #clicks apply filter button when app opens so all graphs with default filters display
  observe({
    click("apply_filters_btn") 
  })
  
  #reset button
   observeEvent(input$reset_btn, {
    # Reset filter values to default
    v$filters$dataset <- "*"
    v$filters$map_type <- "*"
    v$filters$task <- "*"
    v$filters$test_type <- "*"
    v$filters$pooling <- "none"
    v$filters$motion <- "none"
    
    # Reset the UI elements to default values
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    updateSelectInput(session, "pooling", selected = "none")
    updateSelectInput(session, "motion", selected = "none")
  })
  
  ##### Extract information from files #####
  file_names <- list.files(output_dir) #get filenames
 
   #extract filter info from each filename 
  extract_info <- function(file_name) {
    graph_type <- str_extract(file_name, "^(simci|matrix|brain)")
    dataset <- str_extract(file_name, "(all|abcd|hbn|hcp|hcp_ep|pnc|slim|ukb)")
    map_type <- str_extract(file_name, "(fc|act|all)")
    task_type <- str_extract(file_name, "(rest|mid|nback|sst|rest_nih|rest_srs|rest_diag|emotion|gambling|relational|social|wm)")
    test_type <- str_extract(file_name, "(all|r|t|t2)")
    pooling_type <- str_extract(file_name, "(?<=pooling\\.)\\w+")
    motion_type <- str_extract(file_name, "(?<=motion\\.)\\w+") 
    
    return(data.frame(
      file_name = file_name,
      graph_type = graph_type,
      dataset = dataset,
      map_type = map_type,
      task_type = task_type,
      test_type = test_type,
      pooling_type = pooling_type,
      motion_type = motion_type
    ))
  }
  file_info_table <- do.call(rbind, lapply(file_names, extract_info))
  print(file_info_table)
  
  ##### Filter files #####
  matched_files <- reactive({
    filtered_files <- filter_files(file_info_table, input$dataset, input$map_type, input$task, input$test_type, input$pooling, input$motion)
    matched_files <- match_simci_with_matrix(filtered_files)
   # matched_files <- matched_files[match(v$original_files, matched_files$file_name_simci), ] #sorts files in original order 
    return(matched_files)
    print(matched_files)
  })
  
  ##### Plot #####
  observeEvent(input$apply_filters_btn, {
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
        print(paste("Processing file", i))  # Add a print here to track progress
        local({
          my_i <- i
          plotname_simci <- paste0("plot_simci_", my_i)
          plotname_spatial <- paste0("plot_spatial_", my_i)
          
          simci_image_path <- file.path("output", matched_files()[my_i, 1])  # First column is SimCI file
          spatial_image_path <- file.path("output", matched_files()[my_i, 2])  # Second column is Matrix/Brain file
          
          print(paste("SimCI path:", simci_image_path))  # Debugging the file path
          print(paste("Spatial path:", spatial_image_path))  # Debugging the file path
          
          output[[plotname_simci]] <- renderImage({
            if (file.exists(simci_image_path)) {
              list(src = simci_image_path, contentType = 'image/png', width = "100%", height = 200)
            } else {
              print(paste("SimCI file not found:", simci_image_path))  # Debugging missing file
              list(src = "www/placeholder.png", contentType = 'image/png', width = "100%", height = 200)
            }
          }, deleteFile = FALSE)
          
          output[[plotname_spatial]] <- renderImage({
            if (file.exists(spatial_image_path)) {
              list(src = spatial_image_path, contentType = 'image/png', width = "100%", height = 200)
            } else {
              print(paste("Spatial file not found:", spatial_image_path))  # Debugging missing file
              list(src = "www/placeholder.png", contentType = 'image/png', width = "100%", height = 200)
            }
          }, deleteFile = FALSE)
        })
      }
      print("Done filling explorer plots")
    }
  })
  
  ##### Functions #####
  # Filter filenames based on desired filters
  filter_files <- function(file_info_table, dataset, map_type, task_type, test_type, pooling_type, motion_type) {
    filtered_table <- file_info_table[
      (dataset == "*" | file_info_table$dataset %in% dataset) &
        (map_type == "*" | file_info_table$map_type %in% map_type) &
        (task_type == "*" | file_info_table$task_type %in% task_type) &
        (test_type == "*" | file_info_table$test_type %in% test_type) &
        ((pooling_type == "none" & file_info_table$pooling_type == "none") |
           (pooling_type != "none" & file_info_table$pooling_type %in% pooling_type)) &
        ((motion_type == "none" & file_info_table$motion_type == "none") |
           (motion_type != "none" & file_info_table$motion_type %in% motion_type)),
    ]
    return(filtered_table)
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
