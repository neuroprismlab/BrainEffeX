####################################################################
# BrainEffeX UI
####################################################################
library(shinyjs)
library(bslib)
library(shinyBS)
library(shinycssloaders)
#library(BrainEffeX.utils)

source("modals.R")

date_updated = "Sept-24-2025"
version = "1.6.3"

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
             bsTooltip("task_icon", "Choose one or more tasks for the analysis. See more information about each task in the Study Info tab. If no tasks are selected, all available options will be displayed by default. Task-based effect sizes may be influenced by dataset-specific modeling strategies and acquisition differences; interpret results with this in mind.", "right", options = list(container = "body")),
             
             selectInput("test_type",
                         label = tagList("Test Type", icon("info-circle", id = "test_type_icon")),
                         choices = c("All" = "*")),
             bsTooltip("test_type_icon", "Select the statistical test type for the analysis: Correlations (r), task vs. rest (t), or between-group (t2) analyses.", "right", options = list(container = "body")),
             
             selectInput("measure",
                         label = tagList("Measure", icon("info-circle", id = "measure_icon")),
                         choices = c("All" = "*"),
                         multiple = TRUE, selected = NULL),
             bsTooltip("measure_icon", "Select measure variables to view. If no variables are selected, all available options will be displayed by default. See study info tab for more detailed descriptions of the measure names.", "right", options = list(container = "body")),
             # # Button to apply filters
             # actionButton("apply_filters_btn", "Apply Filters"),
             
             # Button to reset filters
             actionButton("reset_btn", "Reset Filters"),
             
             
             br(),
             br(),
             helpText("Parameters:"),
             
             selectInput("motion",
                         label = tagList("Motion Method", icon("info-circle", id = "motion_icon")),
                         choices = c("None" = 'none', "Regression" = 'regression', "Threshold 0.1mm" = 'threshold', "Threshold 0.2mm" = "threshold2"), 
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
             
             # conditionalPanel(
             #   condition = "input.test_type.indexOf('r') > -1",
             #   h4("Variable names"),
             #   helpText("For correlation studies (r), find more detailed definitions of variable names in this table."),
             #   DT::dataTableOutput("keys"),
             # ),
             
             h6(paste("Version ", version, "; Last updated ", date_updated)),
             
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
                                   choices = c("Category" = 'category')), 
                       bsTooltip("group_by_icon", "Choose which meta-analysis to visualize.", "right", options = list(container = "body")),
                       selectInput("m_motion",
                                   label = tagList("Motion Method", icon("info-circle", id = "meta_motion_icon")),
                                   choices = c("None" = 'none', "Regression" = 'regression', "Threshold 0.1mm" = 'threshold', "Threshold 0.2mm" = "threshold2"), 
                                   selected = 'none'),
                       bsTooltip("meta_motion_icon", "Select the method of motion correction. Regression: the mean framewise displacement (FD) for each subject was regressed from data. Thresholding: TRs with mean FD > 0.1 mm were removed.", "right", options = list(container = "body")),
                       
                       selectInput("m_pooling",
                                   label = tagList("Pooling", icon("info-circle", id = "meta_pooling_icon")),
                                   choices = c("None" = 'none', "Network-level" = 'net')),
                       bsTooltip("meta_pooling_icon", "Pool the data by network.", "right", options = list(container = "body")),
                       
                       selectInput("m_estimate",
                                   label = tagList("Effect Size Measure", icon("info-circle", id = "meta_effect_size_icon")),
                                   choices = c("Cohen's d" = 'd', "R Squared" = 'r_sq'), selected = 'd'),
                       bsTooltip("meta_effect_size_icon", "Choose effect size estimate to visualize.", "right", options = list(container = "body")),
                       
                       h1(" "),
                       # Button to download the plot as PNG
                       # downloadButton("downloadData", "Download Data"),
                       
                       # Button to take a screenshot of the app
                       actionButton("screenshot_m", "Take a screenshot"),
                       
                       h6(paste("Version: ", version, "; Last updated ", date_updated)),
                ),
                
                column(9, align = "centre", # plots
                       # simCI plot on the left, accompanying spatial plot on right
                       
                       # h5("Helpful reminders"),
                       h3("The plots below visualize all edges or voxels in each meta-analysis"),
                       wellPanel(style = "background-color: #ffffff;", 
                                 helpText("Meta-analyses were conducted for each category of studies. The rma.mv function in the R package metafor was used for mass univariate meta-analysis of effect sizes at each voxel, edge, network, or multivariate measure. Studies were nested by dataset within each category to account for dependence between studies within a dataset."),
                                 helpText("The shaded areas of the plost represent simultaneous confidence intervals (95% CI across all edges/voxels)."),
                                 helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
                                 
                       ),
                       #downloadButton("downloadPlots_m", "Download Plots"),
                       wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("m_plots"), type = 1)),
                       #plotOutput("m_plots", width = "100%", height = "100%")
                ),
                
                
              )
    ), 
  
  nav_panel("Study Info", 
            br(),
            navset_card_pill(
            nav_panel("Study Information",
                      br(),
                     h3("This table provides detailed information about all studies"),
                     downloadButton("downloadStudyInfo", "Download Study Info table with citations (CSV)"),
                     br(),
                       withSpinner(DT::dataTableOutput("studyInfoTable", width = "100%"))),
                     
            nav_panel("Preprocessing",
                      br(),
                      h3("This table provides detailed information about the preprocessing of each dataset by contributors"),
                      br(),
                      withSpinner(DT::dataTableOutput("preprocessingTable", width = "100%")))
  )), id = "tab"),
  
  
  #if you want to create a new panel in the tutorials, you'll have to instiate the modal here
  createGettingStartedModal(),
  createUnderstandingPlotsModal1(),
  createUnderstandingPlotsModal2(),
  createMetaAnalysisModal(), 
  createDownloadingEffectMapsModal()
)


