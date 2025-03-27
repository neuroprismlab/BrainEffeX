####################################################################
# BrainEffeX Server
# Loading data, filtering, and plotting
####################################################################

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
  v <- reactiveValues()
  
  #Load in explorer tab info
  load("data/study.RData")
  v$study_init <- study
  
  # Load in meta tab info
  load("data/meta_analysis/study_meta_category.RData")
  v$study_meta_category <- study_meta_category
  load("data/meta_analysis/study_meta_statistic.RData")
  v$study_meta_statistic <- study_meta_statistic

  #Load in phen key info
  phen_keys <- read.csv('data/phen_key.csv')
  output$keys <- DT::renderDataTable(phen_keys, options = list(rownames = FALSE, paging = FALSE, scroller = TRUE, scrollY = "400px", scrollX = FALSE, autoWidth = TRUE), rownames = FALSE)
  
  #Calls correct path based on single or meta
  observe({
    if (input$tab == "Meta-Analysis") {
      v$grouped = "meta"
    } else if (input$tab == "Explorer") {
      v$grouped = "single"
    }
  })
  
  # #Info displays
  # createModalNavigationObservers(input, session)
  # observeEvent(c(input$apply_filters_btn, input$reset_btn), {
  #   output$dynamicPanel <- renderUI({
  #     createDynamicPanel(input, v$study_init)
  #   })
  # })
  createModalNavigationObservers(input, session)
  observe({
    output$dynamicPanel <- renderUI({
      createDynamicPanel(input, v$study_init)
    })
  })
  
  # #Clicks button when app opens to display graphs initially with default filters
  # observeEvent(input$tab, {
  #   if (input$tab == "Explorer") {
  #     click("apply_filters_btn")
  #   }
  # })
  
  #Link to find data
  observeEvent(input$downloadData, {
    browseURL("https://osf.io/cwnjd/files/osfstorage?view_only=")
  })
  
  ##### Filter buttons #####
  #Initializing filter values 
  v$filters <- reactiveValues(
    dataset = "*",
    map_type = "*",
    task = "*",
    test_type = "*",
    pooling = "none",
    motion = "none",
    correlation = "*"
  )
  
  # Extract filter options from unfiltered study information
  observe({
    # Update UI selectInput choices dynamically (moved out of "ui" so only pass data directly to server)
    updateSelectInput(session, "dataset", choices = c("All" = "*", unique(v$study_init$dataset)))
    updateSelectInput(session, "map_type", choices = c("All" = "*", unique(v$study_init$map_type)))
    print(paste0('task choices initially ', unique(v$study_init$test_component_1)))
    updateSelectInput(session, "task", choices = c("All" = "*", unique(v$study_init$test_component_1)))
    updateSelectInput(session, "test_type", choices = c("All" = "*", unique(v$study_init$orig_stat_type)))
    updateSelectInput(session, "correlation", choices = c("All" = "*", unique(v$study_init[v$study_init$orig_stat_type == "r", "test_component_2"])))
  })
  
  observeEvent(input$screenshot, {
    screenshot()
  })
  observeEvent(input$screenshot_m, {
    screenshot()
  })
  
  ##### Filter files and plot #####
  #Filter files with every change in filter
  filtered_files <- reactive({
    print(input$correlation)
    return(filter_files(v$study_init, input$dataset, input$map_type, input$task,
                 input$test_type, input$correlation))
  })

  #Update filter options based on previously selected filters (if filter is not all and if filter)
  observe({
    filtered_data <- filtered_files()
    if (input$dataset == "*" || !any(filtered_data$dataset == input$dataset)) {
      updateSelectInput(session, "dataset", choices = c("All" = "*", unique(filtered_data$dataset)))
    }
    if (input$map_type == "*" || !any(filtered_data$map_type == input$map_type)) {
      updateSelectInput(session, "map_type", choices = c("All" = "*", unique(filtered_data$map_type)))
    }
    if (input$task == "*" || !any(filtered_data$test_component_1 == input$task)) {
      updateSelectInput(session, "task", choices = c("All" = "*", unique(filtered_data$test_component_1)))
    }
    if (input$test_type == "*" || !any(filtered_data$orig_stat_type == input$test_type)) {
      updateSelectInput(session, "test_type", choices = c("All" = "*", unique(filtered_data$orig_stat_type)))
    }
    if (input$test_type == "r" &&
        ("*" %in% input$correlation || !any(filtered_data$test_component_2 %in% input$correlation))) {
      updateSelectInput(session, "correlation",
                        choices = c("All" = "*", unique(filtered_data[filtered_data$orig_stat_type == "r", "test_component_2"])))
    }
  })
  
  #When apply filters button pressed, plot
  # observeEvent(input$apply_filters_btn, {
  observe({
    study_filtered <- filtered_files()
    print(study_filtered)
    create_explorer_plots(input, output, study_filtered, v, meta = v$grouped)
  })
  
  observeEvent(input$reset_btn, {
    # Reset input filters to their default values
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    # updateSelectInput(session, "pooling", selected = "none")
    # updateSelectInput(session, "motion", selected = "none")
    updateSelectInput(session, "correlation", selected = "*")
    
    print("re-filtering data with reset inputs")
    print(paste0("input$task: ", input$task))
    print(paste0("input$motion: ", input$motion))
    print(paste0("input$pooling: ", input$pooling))
    print(paste0("input$correlation: ", input$correlation))
    #re-plot
    study_filtered <- filter_files(v$study_init, "*", "*", "*", "*", "*")
    create_explorer_plots(input, output, study_filtered, v, meta = v$grouped)
  })
  

  observeEvent(list(input$tab, input$meta_analysis, input$m_motion, input$m_pooling, input$m_estimate), {
    print("tab clicked")
    if (input$tab == "Meta-Analysis") {
      print("tab is meta-analysis")
      print(input$meta_analysis)
      if (input$meta_analysis == "category") {
        print("meta is category, updating study_meta")
        study_meta <- v$study_meta_category
        print(study_meta)
      } else if (input$meta_analysis == "orig_stat_type") {
        print("meta is statistic, updating study_meta")
        study_meta <- v$study_meta_statistic
        print(study_meta)
      }
      create_explorer_plots(input, output, study_meta, v, meta = v$grouped)
    }
  })

  
  ##### Functions #####
 #Plotting
  create_explorer_plots <- function(input, output, study_filtered, v, meta) {
  if (input$tab == "Explorer") {
    print("Creating placeholders for explorer plots")
    req(study_filtered)
    print("Filtered files loaded")
    
    # Removing files that don't exist
    study_filtered <- study_filtered[sapply(1:nrow(study_filtered), function(i) {
      image_path <- paste0("./cns/", input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "/", study_filtered[i, 3], ".png")
      file.exists(image_path)
    }), ]

    output$histograms <- renderUI({
      validate(need(nrow(study_filtered) > 0, "No data available for the selected parameters."))
      print("Generating UI elements")

      v$plot_output_list <- lapply(1:nrow(study_filtered), function(i) {
        plotname <- paste0("plot", i)
        tagList(
          fluidRow(
            #column(10, imageOutput(plotname, height = "200px", width = "550px"))
            column(10, div(imageOutput(plotname, height = "200px", width = "550px"), style = "margin-bottom: 20px"))
          )
        )
      })

      do.call(tagList, v$plot_output_list)
    })
    print("Done creating placeholders for plots")

    print("Filling explorer SimCI and Spatial/Matrix plots")
    for (i in 1:nrow(study_filtered)) {
      #print(paste("Processing file", i))
      local({
        my_i <- i
        plotname<- paste0("plot", my_i)

        image_path <- paste0("./cns/", input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "/", study_filtered[my_i, 3], ".png")

        output[[plotname]] <- renderImage({
          # No need to check for file existence since we already filtered out missing files
          list(src = image_path, width = "100%", height = 200)
        }, deleteFile = FALSE)
      })
    }
    print("Done filling explorer plots")
  }

  if (input$tab == "Meta-Analysis") {
    print("Creating placeholders for meta plots")
    print(paste0("input$meta_analysis: ", input$meta_analysis))
    print(paste0("input$m_estimate: ", input$m_estimate))
    print(paste0("input$m_motion: ", input$m_motion))
    print(paste0("input$m_pooling: ", input$m_pooling))
    # if (input$meta_analysis == "category") {
    #   study_meta <- v$study_meta_category
    # } else if (input$meta_analysis == "orig_stat_type") {
    #   study_meta <- v$study_meta_statistic
    # }

    # Removing files that don't exist
    study_filtered <- study_filtered[sapply(1:nrow(study_filtered), function(i) {
      image_path <- paste0("./cns/", input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", "meta_", input$meta_analysis, "/", study_filtered[i, "name"], ".png")
      file.exists(image_path)
    }), ] #### TEST THIS

    output$m_plots <- renderUI({
      validate(need(nrow(study_filtered) > 0, "No data available for the selected parameters."))
      print("Generating UI elements")

      v$plot_output_list <- lapply(1:nrow(study_filtered), function(i) {
        plotname <- paste0("m_plot", i)
        tagList(
          fluidRow(
            #column(10, imageOutput(plotname, height = "200px", width = "550px"))
            column(10, div(imageOutput(plotname, height = "200px", width = "550px"), style = "margin-bottom: 20px"))
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
        plotname<- paste0("m_plot", my_i)

        image_path <- paste0("./cns/", input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", "meta_", input$meta_analysis, "/", study_filtered[i, "name"], ".png")

        output[[plotname]] <- renderImage({
          # No need to check for file existence since we already filtered out missing files
          list(src = image_path, width = "100%", height = 200)
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
        ((test_type != "r") | ("*" %in% correlation | file_info$test_component_2 %in% correlation)),  # Fixed condition
    ]
    
    return(filtered_table)
  }
}

  
