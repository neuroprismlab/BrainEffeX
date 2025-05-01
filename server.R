####################################################################
# BrainEffeX Server
# Loading figures, filtering, and plotting
####################################################################

library(shinyscreenshot)
library(shinyjs)

source("helpers.R")

server <- function(input, output, session) {
  ##### Setup #####
  images_folder <- "figures" #changes path to call png's
  v <- reactiveValues()
  
  #Load data
  load("data/study.RData")
  v$study_init <- study
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
  
  #Info displays
  createModalNavigationObservers(input, session)
  observe({
    output$dynamicPanel <- renderUI({
      createDynamicPanel(input, v$study_init)
    })
  })
  
  #Extract filter options from unfiltered study information
  observe({
    updateSelectInput(session, "dataset", choices = c("All" = "*", unique(v$study_init$dataset)))
    updateSelectInput(session, "map_type", choices = c("All" = "*", unique(v$study_init$map_type)))
    print(paste0('task choices initially ', unique(v$study_init$test_component_1)))
    updateSelectInput(session, "task", choices = c("All" = "*", unique(v$study_init$test_component_1)))
    updateSelectInput(session, "test_type", choices = c("All" = "*", unique(v$study_init$orig_stat_type)))
    updateSelectInput(session, "correlation", choices = c("All" = "*", unique(v$study_init[v$study_init$orig_stat_type == "r", "test_component_2"])))
  })
  
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
  
  # Screenshot buttons
  observeEvent(input$screenshot, {
    screenshot()
  })
  observeEvent(input$screenshot_m, {
    screenshot()
  })
  
  #Link to find data
  observeEvent(input$downloadData, {
    browseURL("https://osf.io/cwnjd/files/osfstorage?view_only=")
  })
  
  ##### Filter files and plot #####
  #Filter files
  filtered_files <- reactive({
    return(filter_files(v$study_init, input$dataset, input$map_type, input$task,
                 input$test_type, input$correlation))
  })

  #Dynamically update filter options
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
  
  #Plotting
  observe({
    if (input$tab == "Explorer") {
      study_filtered <- filtered_files()
      create_explorer_plots(input, output, study_filtered, v, meta = v$grouped, images_folder)
    }
  })
  
  #Reset button
  observeEvent(input$reset_btn, {
    # Reset input filters to their default values
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    updateSelectInput(session, "correlation", selected = "*")
    
    #re-plot
    study_filtered <- filter_files(v$study_init, "*", "*", "*", "*", "*")
    create_explorer_plots(input, output, study_filtered, v, meta = v$grouped, images_folder)
  })
  
  # Plotting on meta-analysis tab
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
      create_explorer_plots(input, output, study_meta, v, meta = v$grouped, images_folder)
    }
  })
}
