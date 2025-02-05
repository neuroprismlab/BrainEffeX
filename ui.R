####################################################################
# BrainEffeX UI
####################################################################
library(shinyjs)
library(bslib)
library(shinyBS)
library(shinycssloaders)
library(BrainEffeX.utils)

source("modals.R")

date_updated = "Feb-03-2025"

# User interface ----
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
           tags$div(style = "display: flex; flex-direction: column; align-items: flex-end; height: 100%;", 
                    tags$img(src = "nplogo.png", class = "logo", style = "height: 90px; margin-right:10px"),
                    h5("The NeuroPrism Lab", style = "margin-top: 5px;"))
    ),
    
    actionButton(
      "showInstructions",
      "How to Use This App",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-left:15px"),
  ),
  
  hr(), # space
  
  fluidRow( # top row
    column(3, # inputs
           helpText("Select from the following options to visualize effect sizes:"),
           
           selectInput("dataset",
                       label = tagList("Dataset", icon("info-circle", id = "dataset_icon")),
                       choices = c("All" = "*")),
           bsTooltip("dataset_icon", "Select an dataset to visualize.", "right", options = list(container = "body")),
           
           selectInput("measurement_type",
                       label = tagList("Map Type", icon("info-circle", id = "measurement_type_icon")),
                       choices = c("All" = "*")),
           bsTooltip("measurement_type_icon", "Select the type of map for analysis (e.g., FC or activation).", "right", options = list(container = "body")),
           
           selectizeInput("task",
                          label = tagList("Task", icon("info-circle", id = "task_icon")),
                          choices = c("All" = "*"),
                          multiple = TRUE, selected = NULL),
           bsTooltip("task_icon", "Choose one or more tasks for the analysis. If no tasks are selected, all available options will be displayed by default.", "right", options = list(container = "body")),
           
           selectInput("test_type",
                       label = tagList("Test Type", icon("info-circle", id = "test_type_icon")),
                       choices = c("All" = "*")),
           bsTooltip("test_type_icon", "Select the statistical test type for the analysis: Correlations (r), task vs. rest (t), or between-group (t2) analyses.", "right", options = list(container = "body")),
           
           conditionalPanel(
             condition = "input.test_type.indexOf('r') > -1",
             selectInput("behaviour",
                         label = tagList("Correlation", icon("info-circle", id = "behaviour_icon")),
                         choices = c("All" = "*"),
                         multiple = TRUE, selected = NULL),
             bsTooltip("behaviour_icon", "Select behavioural variables for correlation analysis. If no behavioural variables are selected, all available options will be displayed by default. See table below for more detailed descriptions of the variable names.", "right", options = list(container = "body"))
           ),
           
           selectInput("motion",
                       label = tagList("Motion Method", icon("info-circle", id = "motion_icon")),
                       choices = c("None" = 'none', "Regression" = 'regression', "Threshold" = 'threshold'), 
                       selected = 'none'),
           bsTooltip("motion_icon", "Select the method of motion correction. Regression: the mean framewise displacement (FD) for each subject was regressed from data. Thresholding: TRs with mean FD > 0.1 mm were removed.", "right", options = list(container = "body")),
           
           selectInput("spatial_scale",
                       label = tagList("Pooling", icon("info-circle", id = "spatial_scale_icon")),
                       choices = c("None" = 'none', "Network-level" = 'net')),
           bsTooltip("spatial_scale_icon", "Choose to pool the data.", "right", options = list(container = "body")),
           
           selectInput("estimate",
                       label = tagList("Effect Size Measure", icon("info-circle", id = "effect_size_icon")),
                       choices = c("Cohen's d" = 'd', "Pearson's r" = 'r_sq'), selected = 'd'),
           selectInput("group_by", 
                       label = tagList("What do you want to group by?", icon("info-circle", id = "group_by_icon")),
                       choices = c("None" = 'none', "Statistic" = 'orig_stat_type', "Phenotype Category" = 'category')), 
           bsTooltip("group_by_icon", "Choose how to group the analysis results.", "right", options = list(container = "body")),
           
           h1(" "),
           # Button to download the plot as PNG
           downloadButton("downloadData", "Download Data"),
           
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
    ),
    
    column(5, align = "centre", # simCI plots
           uiOutput("dynamicPanel"),  # helper menu: dynamic panel in center
           # h5("Helpful reminders"),
           h4("The plots below visualize all edges or voxels in each study."),
           wellPanel(style = "background-color: #ffffff;", 
                     helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
                     helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
           ),
           downloadButton("downloadPlots", "Download Plots"),
           wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("histograms"), type = 1)),
    ),
    
    column(4, align = "center", # effect size matrices)
           wellPanel(style = "background-color: #ffffff;", h3("Effect size matrices"), helpText("These matrices show the average effect sizes across all studies that fit the selected parameters."),
                     withSpinner(plotOutput("maps", width = "100%", height = "100%"), type = 1),
                     downloadButton("downloadMatrices", "Download Matrices")),
           h1(" "),
           h1(""),
           h1(""),
           wellPanel(style = "background-color: #ffffff;", h3("Activation Maps (Cohen's d)"),
                     h1(""),
                     fluidRow( # second row: plots of activation maps for activation studies 
                       column(4, numericInput("xCoord", "X", 30), numericInput("yCoord", "Y", 30), numericInput("zCoord", "Z", 30)),
                       column(8, withSpinner(plotOutput("brain", width = "90%"), type = 1))
                     ),
                     downloadButton("downloadBrain", "Download Brain Image"),
           )
    ),
    
  ), # end of fluidRow
  
  #if you want to create a new panel in the tutorials, you'll have to instiate the modal here
  createGettingStartedModal(),
  createUnderstandingPlotsModal1(),
  createUnderstandingPlotsModal2(),
  createDownloadingEffectMapsModal()
)
