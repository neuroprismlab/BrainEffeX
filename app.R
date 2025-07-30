####################################################################
# BrainEffeX App
####################################################################

# Shiny stuff needed for both ui and server (check)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyscreenshot)
library(shinyjs)
library(shinyBS)
library(bslib)
library(DT) # data tables

# Import custom BrainEffeX functions, ui, and server
#save_plots = FALSE # set to TRUE to save all plots as pngs, MUST BE OFF TO DEPLOY
#library(BrainEffeX.utils) # to run locally, install the package from github with: devtools::install_github("neuroprismlab/BrainEffeX_utils")
source("modals.R")
source("helpers.R")


date_updated = "May-08-2025"



# User interface ---------------------------------------------------------------
ui <- fluidPage(
  
  # theme = shinytheme("spacelab"),
  useShinyjs(),
  
  # Include the custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # JavaScript to trigger the modal on app load
  tags$script(HTML("
    $(document).ready(function(){
      setTimeout(function() {
        $('#instructionsModal1').modal('show');
      }, 500);
    });
  ")),
  
  # titlePanel(
  fluidRow(
    column(8,
           h1("BrainEffeX"),
           #hr(), #space
           h4("A tool for exploring effect sizes in typical neuroimaging study designs"),
           
    ),
    column(4, 
           tags$a(
             href = "https://neuroprismlab.github.io/",  # Replace with your actual lab URL
             target = "_blank",  # Optional: opens in new tab
             tags$div(
               style = "display: flex; flex-direction: column; align-items: flex-end; height: 100%; text-decoration: none;",
               tags$img(src = "nplogo.png", class = "logo", style = "height: 90px; margin-right:10px"),
               h5("The NeuroPrism Lab", style = "margin-top: 5px; color: black;")
             )
           )
           
    ),
    
    actionButton(
      "showInstructions",
      "How to Use This App",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-left:15px"),
    
    # tags$a(href = "https://osf.io/preprints/osf/kryn4_v3", 
    #        #target = "_blank", 
    #        class = "btn btn-primary",
    #        style = "color: #fff; background-color: #5c8a54; border-color: #4d7346; margin-left:15px",
    #        "View Preprint")
    
    actionButton("openLink", 
                 "View Preprint",
                 onclick = "window.open('https://osf.io/preprints/osf/kryn4_v3', '_blank')",
                 style = "color: #fff; background-color: #5c8a54; border-color: #4d7346; margin-left:15px")
  ),
  
  hr(), # space
  
  navset_tab( 
    nav_panel("Explorer", fluidRow( # top row
      column(4, # inputs
             helpText("Select from the following filters to visualize effect sizes:"),
             
             selectInput("dataset",
                         label = tagList("Dataset", icon("info-circle", id = "dataset_icon")),
                         choices = c("All" = "*")),
             bsTooltip("dataset_icon", "Select an dataset to visualize.", "right", options = list(container = "body")),
             
             selectInput("map_type",
                         label = tagList("Map Type", icon("info-circle", id = "map_type_icon")),
                         choices = c("All" = "*")),
             bsTooltip("map_type_icon", "Select the type of map for analysis (e.g., FC or activation).", "right", options = list(container = "body")),
             
             selectInput("task",
                         label = tagList("Task", icon("info-circle", id = "task_icon")),
                         choices = c("All" = "*")),
             bsTooltip("task_icon", "Choose one or more tasks for the analysis. If no tasks are selected, all available options will be displayed by default.", "right", options = list(container = "body")),
             
             selectInput("test_type",
                         label = tagList("Test Type", icon("info-circle", id = "test_type_icon")),
                         choices = c("All" = "*")),
             bsTooltip("test_type_icon", "Select the statistical test type for the analysis: Correlations (r), task vs. rest (t), or between-group (t2) analyses.", "right", options = list(container = "body")),
             
             conditionalPanel(
               condition = "input.test_type.indexOf('r') > -1",
               selectInput("correlation",
                           label = tagList("Correlation", icon("info-circle", id = "correlation_icon")),
                           choices = c("All" = "*"),
                           multiple = TRUE, selected = NULL),
               bsTooltip("correlation_icon", "Select correlation variables for correlation analysis. If no correlation variables are selected, all available options will be displayed by default. See table below for more detailed descriptions of the variable names.", "right", options = list(container = "body"))
             ),
             # # Button to apply filters
             # actionButton("apply_filters_btn", "Apply Filters"),
             
             # Button to reset filters
             actionButton("reset_btn", "Reset Filters"),
             
             
             br(),
             br(),
             helpText("Parameters:"),
             
             selectInput("motion",
                         label = tagList("Motion Method", icon("info-circle", id = "motion_icon")),
                         choices = c("None" = 'none', "Regression" = 'regression', "Threshold" = 'threshold'), 
                         selected = 'none'),
             bsTooltip("motion_icon", "Select the method of motion correction. Regression: the mean framewise displacement (FD) for each subject was regressed from data. Thresholding: TRs with mean FD > 0.1 mm were removed.", "right", options = list(container = "body")),
             
             selectInput("pooling",
                         label = tagList("Pooling", icon("info-circle", id = "pooling_icon")),
                         choices = c("None" = 'none', "Network-level" = 'net')),
             bsTooltip("pooling_icon", "Choose to pool the data.", "right", options = list(container = "body")),
             
             selectInput("estimate",
                         label = tagList("Effect Size Measure", icon("info-circle", id = "effect_size_icon")),
                         choices = c("Cohen's d" = 'd', "R Squared" = 'r_sq'), selected = 'd'),
             bsTooltip("effect_size_icon", "Select the measure of effect size.", "right", options = list(container = "body")),
             # 
             # selectInput("plot_combination_style",
             #             label = tagList("Plot Combination", icon("info-circle", id = "plot_combo_icon")),
             #             choices = c("Single" = 'single', "Overlapping" = 'overlapping', "Meta" = 'meta'), 
             #             selected = 'none'),
             # bsTooltip("motion_icon", "Select the method of motion correction. Regression: the mean framewise displacement (FD) for each subject was regressed from data. Thresholding: TRs with mean FD > 0.1 mm were removed.", "right", options = list(container = "body")),
             # 
             # selectInput("group_by", 
             #             label = tagList("What do you want to group by?", icon("info-circle", id = "group_by_icon")),
             #             choices = c("None" = 'none', "Statistic" = 'orig_stat_type', "Category" = 'category')), 
             # bsTooltip("group_by_icon", "Choose how to group the analysis results.", "right", options = list(container = "body")),
             # 
             h1(" "),
             
             
             # Button to download the plot as PNG
             actionButton("downloadData", "Download Data"),
             # 
             # Button to take a screenshot of the app
             actionButton("screenshot", "Take a screenshot"),
             
             h1(" "),
             # h5("Helpful reminders"),
             # wellPanel(style = "background-color: #ffffff;", 
             #           helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
             #           helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
             #           ),
             h1(" "),
             
             # add a small scrollable table of phenotypic keys and definitions
             
             conditionalPanel(
               condition = "input.test_type.indexOf('r') > -1",
               h4("Variable names"),
               helpText("For correlation studies (r), find more detailed definitions of variable names in this table."),
               DT::dataTableOutput("keys"),
             ),
             
             h6(paste("Version 1.5; Last updated ", date_updated)),
             
             tags$a(href = "https://github.com/neuroprismlab/BrainEffeX", 
                    target = "_blank", 
                    icon("github", class = "fa-2x"), 
                    style = "color: #000; margin-left: 10px; text-decoration: none;")
             
      ),
      
      column(8, align = "centre", # simCI plots
             uiOutput("dynamicPanel"),  # helper menu: dynamic panel in center
             # h5("Helpful reminders"),
             h4("The plots below visualize all edges or voxels in each study."),
             wellPanel(style = "background-color: #ffffff;", 
                       helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
                       helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
             ),
             #downloadButton("downloadPlots", "Download Plots"),
             wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("histograms"), type = 1)),
      ),
    )), # end of fluidRow
    nav_panel("Meta-Analysis", 
              fluidRow( # top row
                column(3, # inputs
                       h1(""),
                       selectInput("meta_analysis", 
                                   label = tagList("What do you want to group by?", icon("info-circle", id = "group_by_icon")),
                                   choices = c("Statistic" = 'orig_stat_type', "Category" = 'category')), 
                       bsTooltip("meta_analysis_icon", "Choose which meta-analysis to visualize.", "right", options = list(container = "body")),
                       selectInput("m_motion",
                                   label = tagList("Motion Method", icon("info-circle", id = "motion_icon")),
                                   choices = c("None" = 'none', "Regression" = 'regression', "Threshold" = 'threshold'), 
                                   selected = 'none'),
                       bsTooltip("motion_icon", "Select the method of motion correction. Regression: the mean framewise displacement (FD) for each subject was regressed from data. Thresholding: TRs with mean FD > 0.1 mm were removed.", "right", options = list(container = "body")),
                       
                       selectInput("m_pooling",
                                   label = tagList("Pooling", icon("info-circle", id = "pooling_icon")),
                                   choices = c("None" = 'none', "Network-level" = 'net')),
                       bsTooltip("pooling_icon", "Pool the data by network.", "right", options = list(container = "body")),
                       
                       selectInput("m_estimate",
                                   label = tagList("Effect Size Measure", icon("info-circle", id = "effect_size_icon")),
                                   choices = c("Cohen's d" = 'd', "R Squared" = 'r_sq'), selected = 'd'),
                       
                       h1(" "),
                       # Button to download the plot as PNG
                       # downloadButton("downloadData", "Download Data"),
                       
                       # Button to take a screenshot of the app
                       actionButton("screenshot_m", "Take a screenshot"),
                       
                       h6(paste("Version 1.5; Last updated ", date_updated)),
                ),
                
                column(9, align = "centre", # plots
                       # simCI plot on the left, accompanying spatial plot on right
                       
                       # h5("Helpful reminders"),
                       h4("The plots below visualize all edges or voxels in each meta-analysis"),
                       wellPanel(style = "background-color: #ffffff;", 
                                 helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
                                 helpText("Simultaneous confidence intervals (95% CI across all edges/voxels)."),
                       ),
                       #downloadButton("downloadPlots_m", "Download Plots"),
                       wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("m_plots"), type = 1)),
                       #plotOutput("m_plots", width = "100%", height = "100%")
                ),
                
                
              )
    ), id = "tab"), 
  
  
  #if you want to create a new panel in the tutorials, you'll have to instiate the modal here
  createGettingStartedModal(),
  createUnderstandingPlotsModal1(),
  createUnderstandingPlotsModal2(),
  createDownloadingEffectMapsModal()
)





#### Server --------------------------------------------------------------------

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
  output$keys <- DT::renderDataTable(phen_keys, options = list(rownames = FALSE, paging = FALSE, scroller = TRUE, scrollY = "400px", scrollX = FALSE, autoWidth = TRUE), rownames = FALSE)
  
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
            list(src = image_path, width = "100%", height = 300)
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
        print(paste("Processing file", i))
        local({
          my_i <- i
          plotname<- paste0("m_plot", my_i)
          
          image_path <- tolower(paste0(figs_path, input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", meta, "_", input$m_estimate, "/", study_filtered[i, "name"], ".png"))
          
          output[[plotname]] <- renderImage({
            # No need to check for file existence since we already filtered out missing files
            list(src = image_path, width = "100%", height = 300)
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


# Run app
shinyApp(ui, server)
