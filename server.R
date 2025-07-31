####################################################################
# BrainEffeX Server
# Loading data, filtering, and plotting
####################################################################
library(shinyjs)
library(bslib)
library(shinyscreenshot)
library(DT)

source("helpers.R")

server <- function(input, output, session) {
  ##### Setup #####
  v <- reactiveValues()
  
  #figs_path = "./cns/"
  #testing:
  figs_path = "figures/shiny/"
  
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
  #output$keys <- DT::renderDataTable(phen_keys, options = list(rownames = FALSE, paging = FALSE, scroller = TRUE, scrollY = "400px", scrollX = FALSE, autoWidth = TRUE), rownames = FALSE)
  
  #Calls correct path based on single or meta
  observe({
    if (input$tab == "Meta-Analysis") {
      v$grouped = "meta_simci-spatial"
    } else if (input$tab == "Explorer") {
      v$grouped = "single_simci-spatial"
    }
  })
  
  createModalNavigationObservers(input, session)
  observe({
    output$dynamicPanel <- renderUI({
      createDynamicPanel(input, v$study_init)
    })
  })
  # 
  # #Link to find data
  # observeEvent(input$downloadData, {
  #   browseURL("https://osf.io/cwnjd/files/osfstorage?view_only=")
  # })
  # 
  
  observeEvent(input$downloadData, {
    shinyjs::runjs("window.open('https://osf.io/cwnjd/files/osfstorage?view_only=', '_blank');")
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
    measure = "*"
  )
  
  # Extract filter options from unfiltered study information
  observe({
    # Update UI selectInput choices dynamically (moved out of "ui" so only pass data directly to server)
    updateSelectInput(session, "dataset", choices = c("All" = "*", unique(v$study_init$dataset)))
    updateSelectInput(session, "map_type", choices = c("All" = "*", unique(v$study_init$map_type)))
    print(paste0('task choices initially ', unique(v$study_init$test_component_1)))
    updateSelectInput(session, "task", choices = c("All" = "*", unique(v$study_init$test_component_1)))
    updateSelectInput(session, "test_type", choices = c("All" = "*", unique(v$study_init$orig_stat_type)))
    updateSelectInput(session, "measure", choices = c("All" = "*", unique(v$study_init[,"test_component_2"])[(!is.na(unique(v$study_init[,"test_component_2"]))) & (!grepl("REST", unique(v$study_init[,"test_component_2"])))]))
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
    print(input$measure)
    return(filter_files(v$study_init, input$dataset, input$map_type, input$task,
                        input$test_type, input$measure))
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
        ("*" %in% input$measure || !any(filtered_data$test_component_2 %in% input$measure))) {
      updateSelectInput(session, "measure",
                        choices = c("All" = "*", unique(filtered_data[filtered_data$orig_stat_type == "r", "test_component_2"])))
    }
  })
  
  #When apply filters button pressed, plot
  # observeEvent(input$apply_filters_btn, {
  observe({
    if (input$tab == "Explorer") {
      print("Explorer tab selected")
      study_filtered <- filtered_files()
      print(study_filtered)
      create_explorer_plots(input, output, study_filtered, v, meta = v$grouped)
    }
  })
  
  observeEvent(input$reset_btn, {
    # Reset input filters to their default values
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    # updateSelectInput(session, "pooling", selected = "none")
    # updateSelectInput(session, "motion", selected = "none")
    updateSelectInput(session, "measure", selected = "*")
    
    print("re-filtering data with reset inputs")
    print(paste0("input$task: ", input$task))
    print(paste0("input$motion: ", input$motion))
    print(paste0("input$pooling: ", input$pooling))
    print(paste0("input$measure: ", input$measure))
    #re-plot
    study_filtered <- filter_files(v$study_init, "*", "*", "*", "*", "*")
    create_explorer_plots(input, output, study_filtered, v, meta = v$grouped)
  })
  
  
  observe({
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
  
  output$studyInfoTable <- DT::renderDataTable({
    
    # Read the CSV file
    study_extra <- read.csv("data/study_extra.csv", stringsAsFactors = FALSE)
    study_extra <- study_extra[,c("name", "dataset", "test_component_1", "task_details", "test_component_2", "measure_details", "category")]
    colnames(study_extra) <- c("Study Name", "Dataset", "Task", "Task Details", "Measure", "Measure Details", "Category")
    # Create the data table with all columns from the CSV
    DT::datatable(
      study_extra,
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        searching = TRUE,
        filter = 'none',
        lengthMenu = c(10, 15, 25, 50, 100),
        columnDefs = list(
          list(width = '120px', targets = 0),  # name
          list(width = '70px', targets = 1),   # dataset
          list(width = '70px', targets = 2),  # task
          list(width = '300px', targets = 3),  # task details
          list(width = '150px', targets = 4),   # measure
          list(width = '150px', targets = 5),  # measure details
          list(width = '150px', targets = 6)   # category
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe' 
    )
  }, server = FALSE)
  
  ##### Functions #####
  #Plotting
  create_explorer_plots <- function(input, output, study_filtered, v, meta) {
    if (input$tab == "Explorer") {
      print("Creating placeholders for explorer plots")
      req(study_filtered)
      print("Filtered files loaded")
      
      # Removing files that don't exist
      study_filtered <- study_filtered[sapply(1:nrow(study_filtered), function(i) {
        image_path <- tolower(paste0(figs_path, input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "_", input$estimate, "/", study_filtered[i, 3], ".png"))
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
              column(10, div(imageOutput(plotname, height = "300px", width = "700px"), style = "margin-bottom: 30px"))
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
          
          image_path <- tolower(paste0(figs_path, input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "_", input$estimate, "/", study_filtered[my_i, 3], ".png"))
          
          output[[plotname]] <- renderImage({
            # No need to check for file existence since we already filtered out missing files
            list(src = image_path, width = "100%", height = "300px")
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
        image_path <- tolower(paste0(figs_path, input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", meta, "_", input$m_estimate, "/", study_filtered[i, "name"], ".png"))
        file.exists(image_path)
      }), ] #### TEST THIS
      print(study_filtered)
      
      output$m_plots <- renderUI({
        validate(need(nrow(study_filtered) > 0, "No data available for the selected parameters."))
        print("Generating UI elements")
        
        v$plot_output_list <- lapply(1:nrow(study_filtered), function(i) {
          plotname <- paste0("m_plot", i)
          
          # Get both category and reference space for this meta-analysis
          current_category <- study_filtered[i, "group_level"]
          current_ref <- study_filtered[i, "ref"] 
          
          # Get studies included in this specific category-reference combination
          included_studies <- get_meta_studies(current_category, current_ref, v$study_init)
          
          tagList(
            # Plot on top
            fluidRow(
              column(12, 
                     div(imageOutput(plotname, height = "300px", width = "100%"),
                         style = "margin-bottom: 15px;")
              )
            ),
            # Study list underneath
            fluidRow(
              column(12,
                     div(
                       h5(paste("Studies in", current_category, "meta-analysis:"), 
                          style = "margin-bottom: 5px;"),
                       h6(paste("Reference space:", current_ref), 
                          style = "color: #666; margin-bottom: 10px; font-size: 12px;"),
                       div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 50px; font-size: 13px; columns: 2; column-gap: 20px;",
                           HTML(included_studies)
                       )
                     )
              )
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
          
          image_path <- tolower(paste0(figs_path, input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", meta, "_", input$m_estimate, "/", study_filtered[i, "name"], ".png"))
          
          output[[plotname]] <- renderImage({
            # No need to check for file existence since we already filtered out missing files
            list(src = image_path, width = "700", height = 300)
          }, deleteFile = FALSE)
        })
      }
      print("Done filling explorer plots")
    }
  }
  
  # Filter filenames based on desired filters
  filter_files <- function(file_info, dataset, map_type, task_type, test_type, measure) {
    if (is.null(measure)) {
      measure <- "*"
    }
    
    filtered_table <- file_info[
      (dataset == "*" | file_info$dataset %in% dataset) &
        (map_type == "*" | file_info$map_type %in% map_type) &
        (task_type == "*" | file_info$test_component_1 %in% task_type) &
        (test_type == "*" | file_info$orig_stat_type %in% test_type) &
        ("*" %in% measure | file_info$test_component_2 %in% measure),
    ]
    
    return(filtered_table)
  }
  
  # get list of studies in each meta-analysis
  get_meta_studies <- function(category_name, ref_space, study_init) {
    # Filter studies by both category and reference space
    print(study_init)
    meta_studies <- study_init[study_init$category == category_name & 
                                 grepl(ref_space, study_init$ref, ignore.case = TRUE), ]
    
    if (nrow(meta_studies) == 0) {
      return(paste0("<p><em>No studies found for ", category_name, " (", ref_space, ") meta-analysis.</em></p>"))
    }
    
    # Create a formatted list
    study_list <- paste0(
      "<div style='margin-bottom: 5px; padding: 3px;'>",
      "• ", meta_studies$name,
      "</div>"
    )
    
    return(paste(study_list, collapse = ""))
  }
}


