# load libraries
library(shiny)
library(shinythemes) #TODO: list what we need each package for!
library(ggplot2)
library(oro.nifti)
library(neurobase)
library(ggcorrplot)
library(ggridges)
library(pheatmap)
library(shinycssloaders)
library(shinyjs)
library(fields)
library(sass)
library(bslib)
library(reshape)
library(gridExtra)
library(shinyBS) # For Bootstrap tooltips
library(shinycssloaders)
library(osfr)

# source helper functions
source("helpers.R")

#TODO: this is a temporary fix because we don't have the data for the other studies yet and treat activation maps differently
# effect_maps_available = c("emotion", "gambling", "relational", "social", "wm")

# load data
data_file = "combined_data_2024-09-13.RData"
#load("data/sim_ci.RData") 
load(paste0("data/", data_file))
# load template nifti file
template <- readNIfTI("data/template_nifti")
# load anatomical nifti file
anatomical <- readNIfTI("data/anatomical.nii")

# loads brain_masks as list, sim_ci as list, and study as table

d_clean <- data

# make study all lowercase
study <- data.frame(lapply(study, function(x) {
  if (is.character(x)) {
    return(tolower(x))
  } else {
    return(x)
  }
}))

# also make the names of each list in d_clean all lowercase
names(d_clean) <- tolower(names(d_clean))

# and the names of each list in brain_masks
names(brain_masks) <- tolower(names(brain_masks))

effect_maps_available <- study[study$map_type == "act", "name"]

# d_clean is a list that includes the effect maps, 
# and "study" is a table that contains study information, 
# and "brain_masks" is a list that contains the brain masks

### d_clean is a list of studies, each study contains all combinations of motion and pooling.
# each combination of motion and pooling contains:
# sample size as n
# p-value as p
# effect size as d
# std as std.brain and std.score
# original statistic values as b.standardized
# bound of simultaneous confidence intervals as sim_ci_lb and sim_ci_ub
# pooling method as pooling.method
# motion method as motion.method

#### study is a data frame that contains information about each study, including:
# basefile
# folder
# name
# extension as ext
# dataset
# map_type
# orig_stat_type
# test_component_1
# test_component_2
# category
# ref # TODO: do we need this still? Not currently in study...

# options for spinner
options(spinner.color = "#9ecadb",
        spinner.color.background = "#ffffff", spinner.size = 1)

########################################################################################
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
                       choices = c("All" = "*", unique(study["dataset"]))),
           bsTooltip("dataset_icon", "Select an dataset to visualize.", "right", options = list(container = "body")),
           
           selectInput("measurement_type",
                       label = tagList("Map Type", icon("info-circle", id = "measurement_type_icon")),
                       choices = c("All" = "*", unique(study["map_type"]))),
           bsTooltip("measurement_type_icon", "Select the type of map for analysis (e.g., FC or activation).", "right", options = list(container = "body")),
           
           selectizeInput("task",
                          label = tagList("Task", icon("info-circle", id = "task_icon")),
                          choices = c("All" = "*", unique((study["test_component_1"]))),
                          multiple = TRUE, selected = NULL),
           bsTooltip("task_icon", "Choose one or more tasks for the analysis. If no tasks are selected, all available options will be displayed by default.", "right", options = list(container = "body")),
           
           selectInput("test_type",
                       label = tagList("Test Type", icon("info-circle", id = "test_type_icon")),
                       choices = c("All" = "*", unique(study$orig_stat_type))),
           bsTooltip("test_type_icon", "Select the statistical test type for the analysis: Correlations (r), task vs. rest (t), or between-group (t2) analyses.", "right", options = list(container = "body")),
           
           conditionalPanel(
             condition = "input.test_type.indexOf('r') > -1",
             selectInput("behaviour",
                         label = tagList("Correlation", icon("info-circle", id = "behaviour_icon")),
                         choices = c("All" = "*", unique(study[study$orig_stat_type=="r", "test_component_2"])),
                         multiple = TRUE, selected = NULL),
             bsTooltip("behaviour_icon", "Select behavioural variables for correlation analysis. If no behavioural variables are selected, all available options will be displayed by default.", "right", options = list(container = "body"))
           ),
           
           selectInput("motion",
                       label = tagList("Motion Method", icon("info-circle", id = "motion_icon")),
                       choices = c("None" = 'none', "Regression" = 'regression', "Threshold" = 'threshold'), 
                       selected = 'none'),
           bsTooltip("test_type_icon", "Select the statistical test type for the analysis: Correlations (r), task vs. rest (t), or between-group (t2) analyses.", "right", options = list(container = "body")),
           
           
           selectInput("spatial_scale",
                       label = tagList("Spatial scale", icon("info-circle", id = "spatial_scale_icon")),
                       choices = c("Univariate" = 'none', "Network-level" = 'net')),
           bsTooltip("spatial_scale_icon", "Select the spatial scale for the analysis.", "right", options = list(container = "body")),
           
           selectInput("group_by", 
                       label = tagList("What do you want to group by?", icon("info-circle", id = "group_by_icon")),
                       choices = c("None", "Statistic", "Phenotype Category")), 
           bsTooltip("group_by_icon", "Choose how to group the analysis results.", "right", options = list(container = "body")),
           
           downloadButton("downloadData", "Download Data"),
           h1(" "),
           h5("Helpful reminders"),
           wellPanel(style = "background-color: #ffffff;", 
                     helpText("For correlation studies (r), Var1 is the scanning condition, and Var2 is the behaviour."),
                     helpText("For task vs. rest studies (t), Var1 is the task, and Var2 is rest."),
                     helpText("For between-group studies (t2), Var1 and Var2 are the two groups."),
                     helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals."),
                     helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
                     ),
           h1(" "),
           h6(paste("Version 1.3; Last updated", Sys.Date()))
           
    ),
    
    column(5, align = "centre", # simCI plots
           uiOutput("dynamicPanel"),  # helper menu: dynamic panel in center
           h4("The plots below visualize all edges or voxels in each study."),
        
           wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("histograms"), type = 1))
    ),
    
    
    
    column(4, align = "center", # effect size matrices)
           wellPanel(style = "background-color: #ffffff;", h3("Effect size matrices"), helpText("These matrices show the average effect sizes across all studies that fit the selected parameters."),
                     withSpinner(plotOutput("maps", width = "100%", height = "100%"), type = 1)),
           h1(" "),
           h1(""),
           h1(""),
           wellPanel(style = "background-color: #ffffff;", h3("Activation Maps (Cohen's d)"),
                     h1(""),
                     fluidRow( # second row: plots of activation maps for activation studies 
                       column(4, numericInput("xCoord", "X", 30), numericInput("yCoord", "Y", 30), numericInput("zCoord", "Z", 30)),
                       column(8, withSpinner(plotOutput("brain", width = "100%"), type = 1))
                     )
           )
    )
  ), # end of fluidRow
  
  
  # Modal Dialogs
  bsModal(
    id = "instructionsModal1", title = "Getting Started", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("Welcome to",tags$b("BrainEffeX!"),"Here's how to get started:"),
      tags$p("To facilitate the estimation and exploration of effect sizes for fMRI, we conducted “typical” study designs with large (n > 500) datasets and created a web app to share this data."),
      tags$p("To start, please use",tags$b(tags$i("the menu to the left")),"to filter the available studies by:"),
      tags$ul(
        tags$li("Dataset"),
        tags$li("Map type (FC or activation)"), 
        tags$li("Available Tasks"),
        tags$li("Test type"),
        tags$li("Behavioral correlations (if applicable)"),
        tags$li("Spatial scale"),
       # tags$li("Select a dataset from the 'Dataset' dropdown."),
      #  tags$li("Choose a map type that matches your analysis needs."),
      #  tags$li("Use the 'Task' dropdown to specify tasks you are interested in."),
      #  tags$li("Set the 'Test Type' to define the statistical analysis."),
      #  tags$li("If applicable, select 'Behavioural correlation' variables."),
     #   tags$li("Choose the 'Spatial scale' to determine analysis granularity."),
     #   tags$li("Decide how to group results using 'Group by'."),
     #   tags$li("Visualize results in plots and download data if needed."),
      ),
     tags$p("Refer to the",tags$b(tags$i("tips")),"next to each input for additional guidance!"),
      tags$div(style = "text-align: center;",
               actionButton("nextToPage2", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  ),
  bsModal(
    id = "instructionsModal2", title = "Understanding the Plots", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("Explore the expected effect sizes of the studies that match the provided filters."),
      tags$p(tags$b(tags$i("The plots in the middle and right panels")),"visualize all edges or voxels in each study:"),
      tags$ul(
        tags$li("Simultaneous confidence intervals (95% CI across all edges/voxels)."),
        tags$li(tags$i("Red")," indicates simultaneous CIs overlapping with 0,", tags$i("green"), "indicates no overlap."),
        tags$li("Effect size matrices show the average effect sizes across all studies that fit the selected parameters."),
        tags$li("Activation Maps (Cohen's d) help you to visualize specific brain regions.")
      ),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage1", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("nextToPage3", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  ),
  bsModal(
    id = "instructionsModal3", title = "Downloading Effect Maps", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("How to download effect maps from BrainEffeX:"),
      tags$ul(
        tags$li("Click the", tags$b(tags$i("'Download Data'")), "button after filtering to download effect maps."),
        tags$li("After downloading, you can use the effect maps further, and apply your own masks if needed.")),
        #tags$li("Or, continue to explore different effect sizes!")),
      tags$p("Use the", tags$b(tags$i("'How to Use This App'")), "button at any time to revisit these instructions."),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage2", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("closePage2", "Close", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
               
      )
    )
  )
)
########################################################################################
# Server logic ----
server <- function(input, output, session) {
  
  # Dynamic panel output
  output$dynamicPanel <- renderUI({
    # Create a list of messages for each input
    messages <- c()
    
    # Dataset message
    if (is.null(input$dataset) || input$dataset == "*") {
      messages$dataset <- "• All datasets."
    } else {
      messages$dataset <- paste("• The <b>", input$dataset, "</b> dataset(s).")
    }
    
    # Map type message
    if (is.null(input$measurement_type) || input$measurement_type == "*") {
      messages$measurement_type <- "• All map types."
    } else {
      messages$measurement_type <- paste("• <b>", input$measurement_type, "</b> map type.")
    }
    
    # Task message
    if (is.null(input$task) || length(input$task) == 0) {
      messages$task <- "• No specific tasks are selected."
    } else if (length(input$task) == length(unique(study[["var1"]]))) { #TODO:
      messages$task <- "• All tasks."
    } else {
      messages$task <- paste("• The <b>", paste(input$task, collapse = ", "), "</b> task(s).")
    }
    
    # Test type message
    if (is.null(input$test_type) || input$test_type == "*") {
      messages$test_type <- "• All test types."
    } else {
      messages$test_type <- paste("• The <b>", input$test_type, "</b> test type(s).")
    }
    
    # behaviour message
    if (is.null(input$behaviour) || length(input$behaviour) == 0) {
      messages$behaviour <- "• No specific behaviours are selected."
    } else if (length(input$behaviour) == length(unique(study[["var2"]])) || input$behaviour == "*") {
      messages$behaviour <- "• All correlations"
    } else {
      print(input$behaviour)
      messages$behaviour <- paste("• The <b>", paste(input$behaviour, collapse = ", "), "</b> correlation(s).")
    }
    
    # Group by message
    if (!is.null(input$group_by) && input$group_by != "None") {
      messages$group_by <- paste("• The results are grouped by <b>", input$group_by, "</b>.")
    }
    
    # Debugging print statement
    # print(messages)
    
    # Combine messages into a single string with a heading
    message_text <- paste("<b>You are looking at:</b><br>", paste(messages, collapse = "<br>"))
    
    
    # Combine messages into a single paragraph
    tags$div(
      style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 20px; border-radius: 5px; text-align: center;",
      tags$p(
        HTML(message_text)
      )
    )
  })
  
  
  # Show modal when 'How to Use This App' button is clicked
  observeEvent(input$showInstructions, {
    toggleModal(session, "instructionsModal1", toggle = "open")
  })
  
# Modal navigation
  observeEvent(input$nextToPage2, {
    toggleModal(session, "instructionsModal1", toggle = "close")
    toggleModal(session, "instructionsModal2", toggle = "open")
  })
  
  observeEvent(input$prevToPage1, {
    toggleModal(session, "instructionsModal2", toggle = "close")
    toggleModal(session, "instructionsModal1", toggle = "open")
  })
  
  observeEvent(input$nextToPage3, {
    toggleModal(session, "instructionsModal2", toggle = "close")
    toggleModal(session, "instructionsModal3", toggle = "open")
  })
  
  observeEvent(input$prevToPage2, {
    toggleModal(session, "instructionsModal3", toggle = "close")
    toggleModal(session, "instructionsModal2", toggle = "open")
  })
  
  observeEvent(input$closePage2, {
    toggleModal(session, "instructionsModal3", toggle = "close")
  })
  
  # Observer to handle default task display
  observeEvent(input$task, {
    if (is.null(input$task) || length(input$task) == 0) {
      # Update the selectizeInput to show all tasks if none are selected
      updateSelectizeInput(session, "task", choices = unique(study[["test_component_1"]]))
    }
  }, ignoreInit = TRUE)
  
  # # Observer to handle default behaviour display
  observeEvent(input$behaviour, {
    if (is.null(input$behaviour) || length(input$behaviour) == 0) {
      # Update the selectizeInput to show all tasks if none are selected
      updateSelectizeInput(session, "behaviour", choices = unique(v$beh_choices))
    }
  }, ignoreInit = TRUE)

print(paste("dims of d_clean : ", length(d_clean)))
print(paste("dims of study : ", dim(study)))
  
# set reactive parameters for plotting based on options chosen by user
    v <- reactiveValues()
    observeEvent(list(input$dataset, input$measurement_type, input$task, input$test_type, input$behaviour, input$motion, input$spatial_scale), priority = 1,{
        v$d_clean <- d_clean[(grepl(input$dataset, study$dataset) & 
                             grepl(input$measurement_type, study$map_type) & 
                             (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$test_component_1)) & 
                            (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                             grepl(paste(input$behaviour, collapse="|"), study$test_component_2) &
                             # ensure that the combination of motion and spatial scale is included in d_clean
                             unname(sapply(d_clean, function(sublist) any(grepl(paste0("pooling.", input$spatial_scale, ".motion.", input$motion), names(sublist))))))]
      
        # also filter study by the same parameters
        v$study <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) & 
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$test_component_1)) & 
                         (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                         grepl(paste(input$behaviour, collapse="|"), study$test_component_2) &
                         unname(sapply(d_clean, function(sublist) any(grepl(paste0("pooling.", input$spatial_scale, ".motion.", input$motion), names(sublist)))))),]

        v$task_choices <- unique(v$study$test_component_1)
    })

        observeEvent(ignoreInit = TRUE, input$dataset, {
          v$test_choices <- study[(grepl(input$dataset, study$dataset)),"orig_stat_type"]
          updateSelectInput(session, "test_type", selected = unique(study[["test_type"]]))
        })

        # observeEvent(ignoreInit = TRUE, input$dataset, {
        #   v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
        #                  grepl(input$measurement_type, study$map_type)),"var1"]

        # Observe the dataset input
        observeEvent(input$dataset,ignoreInit = TRUE,{
          # Retrieve the available tasks for the selected dataset
          v$available_tasks <- unique(study[study$dataset == input$dataset, "test_component_1"])
          
          # Update the task selection input with available tasks but do not pre-select any
          updateSelectizeInput(session, "task",choices = v$task_choices, selected = character(0) # Ensure no tasks are selected by default
          )
        # TODO: test above
        
          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$test_component_1))),"test_component_2"]

        #  updateSelectInput(session, "task", selected = unique(study[["var1"]]))
          # Update the beh selection input with available behs but do not pre-select any
          updateSelectInput(session, "behaviour", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
          
        })

       
        # constrain parameters
        # update behaviour selections to only be the available constrained selections... 
        observeEvent(ignoreInit = TRUE, input$measurement_type, priority = 2, {

          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"test_component_1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$test_component_1))),"test_component_2"]

          if (input$test_type == "r") {
            updateSelectInput(session, "behaviour", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
          }
          print("measurement type changed")
          
          
          # Update the task selection input with available tasks but do not pre-select any
          updateSelectizeInput(session, "task",choices = unique(v$task_choices), selected = character(0)) # Ensure no tasks are selected by default
          # Update the beh selection input with available behs but do not pre-select any
          updateSelectInput(session, "behaviour", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
          
         # updateSelectizeInput(session, server = TRUE, "task", selected = "*", choices = c("All" = "*", unique(v$task_choices)))
        }) 



        # when test type is changed from r to another test type, reset behaviour to all 
        observeEvent(ignoreInit = TRUE, list(input$test_type), {
          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"test_component_1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1))),"test_component_2"]

          if (input$test_type != "r") {
            updateSelectInput(session, "behaviour", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
            
          }
        })


      # Download button
      output$downloadData <- downloadHandler(
      filename = function() {
        paste("EffeX_data", ".RData", sep="")
      },
      content = function(file) {
        saveRDS(v$d_clean, file)
      }
    )

  observe({
          v$d_clean_act <- v$d_clean[grepl("_act_", names(v$d_clean))]
          v$d_clean_fc <- v$d_clean[grepl("_fc_", names(v$d_clean))]

          # v$combo_name <- paste0('pooling.', input$spatial_scale, '.motion.', input$motion)

          v$study_fc <- v$study[grepl("_fc_", v$study$name),]
          v$study_act <- v$study[grepl("_act_", v$study$name),]

          # if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (any(grepl(input$task, effect_maps_available, ignore.case = TRUE)))) {
          #   # v$study_name as the name column from study that matches the task input and has map type activation
          #   v$study_name <- v$study[grepl(input$task, v$study$name, ignore.case = TRUE) & grepl("act", v$study$map_type), "name"]
          #   print("creating nifti for: ", v$study_name)
          #   v$nifti <- create_nifti(template, d_clean, v$study_name, brain_masks)
          #   print("nifti created for: ", v$study_name, " with dimensions: ", dim(v$nifti))
          # }
          # else {
          #    print("no nifti created")
          # }
})

  toListen <- reactive({
        list(input$group_by, input$dataset, input$map_type, input$task, input$test_type, input$spatial_scale, input$motion)
      })

  observeEvent(toListen(), {
    print("checking if task is in effect maps available")
    v$combo_name <- paste0('pooling.', input$spatial_scale, '.motion.', input$motion)
    if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (any(grepl(input$task[1], effect_maps_available, ignore.case = TRUE))) && input$spatial_scale == "none") {
            # v$study_name as the name column from study that matches the task input and has map type activation
            print(paste("task: ", input$task, " is in effect maps available"))
            v$study_name <- v$study[grepl(input$task, v$study$name, ignore.case = TRUE) & grepl("act", v$study$map_type), "name"]
            print(paste("creating nifti for: ", v$study_name))
            v$nifti <- create_nifti(template, d_clean, v$study_name, v$combo_name, brain_masks)
            print(paste("nifti created for: ", v$study_name, " with dimensions: ", dim(v$nifti)))
          }
  }, ignoreNULL = TRUE)

    ##### Group_by ######

    observe({
      v$combo_name <- paste0('pooling.', input$spatial_scale, '.motion.', input$motion)
    })

    observeEvent(toListen(), {
      if (input$group_by == "Statistic") {
        # initialize a list to store the data for each stat type and ref type
        v$d_stat <- list()
        # initialize a new study dataframe to store the info for the groupings
        v$study_stat <- data.frame(stat_type = character(0), ref = character(0), name = character(0))
        # for each statistic type
        for (stat in unique(v$study$orig_stat_type)) {
          # for each reference type
          for (ref in unique(v$study$ref)) {
            matching_idx <- which(v$study$orig_stat_type == stat & v$study$ref == ref)
            #print(paste0("length of matching index: ", length(matching_idx)))
            if (length(matching_idx) > 0) {
              matching_names <- v$study$name[matching_idx]
              matching_d_idx <- which(names(v$d_clean) %in% matching_names)
              # matching_d_idx is the idx of the studies in d that match the current stat and ref
              # average across all studies in matching_d_idx
              # initialize an empty vector to store the sum across studies
              d_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]][[v$combo_name]]$d)) # initialize to the size of the largest matrix
              ci_lb_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]][[v$combo_name]]$d)) # TODO: for now just average across CIs, but ask Steph how we should do this!!!
              ci_ub_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]][[v$combo_name]]$d))
              for (i in matching_d_idx) {
                d_total <- d_total + v$d_clean[[i]][[v$combo_name]]$d
                ci_lb_total <- ci_lb_total + v$d_clean[[i]][[v$combo_name]]$sim_ci_lb
                ci_ub_total <- ci_ub_total + v$d_clean[[i]][[v$combo_name]]$sim_ci_ub
              }
              d_avg <- d_total / length(matching_d_idx)
              ci_lb_avg <- ci_lb_total / length(matching_d_idx)
              ci_ub_avg <- ci_ub_total / length(matching_d_idx)
              # store d_avg, ci_lb_avg, and ci_ub_avg in d_stat list as a list
              v$d_stat[[paste0("stat_", stat, "_reference_", ref)]]$d_avg <- d_avg
              v$d_stat[[paste0("stat_", stat, "_reference_", ref)]]$ci_lb_avg <- ci_lb_avg
              v$d_stat[[paste0("stat_", stat, "_reference_", ref)]]$ci_ub_avg <- ci_ub_avg

              # store the study info in the study_stat dataframe
              v$study_stat <- rbind(v$study_stat, data.frame(stat_type = stat, ref = ref, name = paste0("stat_", stat, "_reference_", ref)))

            }
          }
        }
      }

      else if (input$group_by == "Phenotype Category") {
        # initialize a list to store the data for each phenotype category
        v$d_phen <- list()
        # initialize a new study dataframe to store the info for the groupings
        v$study_phen <- data.frame(phen_category = character(0), ref = character(0), name = character(0))
        # for each phenotype category
        for (phen in unique(v$study$category)) {

          for (ref in unique(v$study$ref)) {
            matching_idx <- which(v$study$category == phen & v$study$ref == ref)
            phen_clean <- gsub("\\(", "_", phen)
            phen_clean <- gsub("\\)", "", phen_clean)
            phen_clean <- gsub(" ", "", phen_clean)
            if (length(matching_idx) > 0) {
              matching_names <- v$study$name[matching_idx]
              matching_d_idx <- which(toupper(names(v$d_clean)) %in% toupper(matching_names))
              # matching_d_idx is the idx of the studies in d that match the current phen category
              # average across all studies in matching_d_idx
              # initialize an empty vector to store the sum across studies
              d_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]]$d)) # initialize to the size of the largest matrix
              ci_lb_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]]$d)) # TODO: for now just averages across CIs, but make sure there isn't a diff way we should do this
              ci_ub_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]]$d))
              for (i in matching_d_idx) {
                d_total <- d_total + v$d_clean[[i]]$d
                ci_lb_total <- ci_lb_total + v$d_clean[[i]]$sim_ci_lb
                ci_ub_total <- ci_ub_total + v$d_clean[[i]]$sim_ci_ub
              }
              d_avg <- d_total / length(matching_d_idx)
              ci_lb_avg <- ci_lb_total / length(matching_d_idx)
              ci_ub_avg <- ci_ub_total / length(matching_d_idx)
              # store d_avg, ci_lb_avg, and ci_ub_avg in d_stat list as a list
              v$d_phen[[paste0("phen_", phen_clean, "_ref_", ref)]]$d_avg <- d_avg
              v$d_phen[[paste0("phen_", phen_clean, "_ref_", ref)]]$ci_lb_avg <- ci_lb_avg
              v$d_phen[[paste0("phen_", phen_clean, "_ref_", ref)]]$ci_ub_avg <- ci_ub_avg

              v$study_phen <- rbind(v$study_phen, data.frame(phen_category = phen_clean, ref = ref, name = paste0("phen_", phen_clean, "_reference_", ref)))

            }
          }}}
    
      # load effect map to plot when only one task selected
      # if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (input$task) %in% effect_maps_available) {
      # file_list <- list.files(path = "data/", full.names = TRUE)
      # v$case_task <- toupper(input$task)
      # pattern <- paste0(v$case_task, ".*\\.nii\\.gz")
      # matching_file <- grep(pattern, file_list, value = TRUE)
      # v$effect_map <- readnii(matching_file)
      #} # TODO: update effect map brain plotting for new data format

      # load effect map to plot when only one task selected
      # effect_maps_available is a list of all study names with the map_type act
      
      
})

    ###### plot simCI plots:
    
    # insert the right number of plot output objects into the web page
    # if d_clean is not empty, then plot. Check if d_clean is empty:
    observe({
    output$histograms <- renderUI({
      if (is.null(v$d_clean) || length(v$d_clean) == 0) {
        # if there is no data, display a message
        tagList(
          h3("No data available for the selected parameters.")
        )
      }
      else if (input$group_by == "None") {
        # v$num_plots as the number of 
        # v$num_plots <- sum(sapply(v$d_clean, function(sublist) any(grepl(v$combo_name, names(sublist)))))
        # print(paste0("num plots: round 1:", v$num_plots))
        plot_output_list <- lapply(1:length(v$d_clean), function(i) {
          plotname <- paste0("plot", i)
          #print(plotname)
          plotOutput(plotname, height = "200px", width = "100%")
        })

        # convert the list to a tagList, this is necessary for the list of items to display properly
        do.call(tagList, plot_output_list)
      }
      
      else if (input$group_by == "Statistic") {
        # v$num_plots <- sum(sapply(v$d_clean, function(sublist) any(grepl(v$combo_name, names(sublist)))))

        plot_output_list <- lapply(1:length(v$d_stat), function(i) {
          plotname <- paste0("plot", i)
          plotOutput(plotname, height = "200px", width = "100%")
        })

        # convert the list to a tagList, this is necessary for the list of items to display properly
        do.call(tagList, plot_output_list)
      }
      
      else if (input$group_by == "Phenotype Category") {
        plot_output_list <- lapply(1:length(v$d_phen), function(i) {
          plotname <- paste0("plot", i)
          plotOutput(plotname, height = "200px", width = "100%")
        })

        # convert the list to a tagList, this is necessary for the list of items to display properly
        do.call(tagList, plot_output_list)
      }
    })
    })

    # call renderPlot for ecah one
    # plots are only actually generated when they are visible on the web page
    observe({
      if (input$group_by == "None") {
        # print(paste0("num plots: ", v$num_plots))
        for (i in 1:length(v$d_clean)) {
          # create a local variable to hold the value of i
          print(i)
          local({
            my_i <- i
            plotname <- paste0("plot", my_i, sep="")

            # print(paste0("Processing plot: ", plotname))
            # print(v$d_clean[[my_i]])
            # print(names(v$d_clean[my_i]))
            # print(v$study[my_i,])
            # print(input$spatial_scale)
            # print(input$motion)

            output[[plotname]] <- renderPlot({
              plot_sim_ci(v$d_clean[[my_i]], names(v$d_clean[my_i]), v$study[my_i,], input$spatial_scale, input$motion)
            })
          })
        }
      }
      else if (input$group_by == "Statistic") {
        for (i in 1:length(v$d_stat)) {
          # create a local variable to hold the value of i
          local({
            my_i <- i
            plotname <- paste0("plot", my_i, sep="")

            output[[plotname]] <- renderPlot({
              plot_sim_ci_stat(v$d_stat[[my_i]], names(v$d_stat[my_i]), v$study_stat[my_i,], input$spatial_scale, input$motion)
            })
          })
        }
      }

      else if (input$group_by == "Phenotype Category") {
        for (i in 1:length(v$d_phen)) {
          # create a local variable to hold the value of i
          local({
            my_i <- i
            plotname <- paste0("plot", my_i, sep="")

            output[[plotname]] <- renderPlot({
              plot_sim_ci_phen(v$d_phen[[my_i]], names(v$d_phen)[my_i], v$study_phen[my_i,])
            })
          })
        }
      }
    })

    # create a reactive value to store the height and width of the plot
    # the height should be double the width only when there are two plots (when there are some studies with 268 node parcellation and some with 55 node parcellation),
    # and height should be equal to width when there is only one plot (when all studies are of the same parcellation type)
    
    observe({
      v$num_268_studies <- sum(v$study_fc$ref == "shen_268")
      v$num_55_studies <- sum(v$study_fc$ref == "ukb_55")
      if (v$num_268_studies > 0 & v$num_55_studies > 0) {
        v$h <- 700
        v$w <- 500
      }
      else {
        v$h <- 350
        v$w <- 500
      }
    })

    
    output$maps <- renderPlot({
      validate(
      need((0 < length(v$d_clean_fc)), "We do not have FC data for the selected parameters"))
      
      # create a vector to store the data for if there is more than one study
      t_total_268 <- rep(0, 35778) 
      t_total_268_pooled <- rep(0, 55)
      t_total_55 <-  rep(0 , 1485)

      n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
      n_268_studies_pooled <- 0
      n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation

      for (i in 1:length(v$d_clean_fc)) {
        t <- v$d_clean_fc[[i]][[v$combo_name]]$d

        study_idx <- which(toupper(v$study_fc$name) == toupper(names(v$d_clean_fc)[i]))
        if (v$study_fc$ref[study_idx] == "shen_268"){ # TODO: create a v$study_fc table to store just fc studies
          
          if (input$spatial_scale == "net") {
            t_total_268_pooled <- t_total_268_pooled + t
            n_268_studies_pooled <- n_268_studies_pooled + 1
          }
          else {
            print(c(dim(t), v$study_fc$name[study_idx]))
            
            t_total_268 <- t_total_268 + t
            n_268_studies <- n_268_studies + 1
          }
        }
        
        else if (v$study_fc$ref[study_idx] == "ukb_55") {
          
          # add the data to the total vector
          t_total_55 <- t_total_55 + t

          n_55_studies <- n_55_studies + 1
        }
      }

      # if d_clean_fc is longer than 1, find the average of the matrices
      if (n_268_studies > 1) {
        t_avg_268 <- t_total_268 / n_268_studies
      }

      if (n_268_studies == 1 | n_268_studies == 0) {
        t_avg_268 <- t_total_268
      }

      if (n_268_studies_pooled > 1) {
        t_avg_268_pooled <- t_total_268_pooled / n_268_studies_pooled
      }

      if (n_268_studies_pooled == 1 | n_268_studies_pooled == 0) {
        t_avg_268_pooled <- t_total_268_pooled
      }

      if (n_55_studies > 1) {
        t_avg_55 <- t_total_55 / n_55_studies
      }

      if (n_55_studies == 1 | n_55_studies == 0) {
        t_avg_55 <- t_total_55
      }

      # only plot the 268 plot if n_268_studies > 0
      if (n_268_studies > 0) {
        plot_268 <- plot_full_mat(t_avg_268, "data/map268_subnetwork.csv")
      }

      # only plot the 268 pooled plot if n_268_studies_pooled > 0
      if (n_268_studies_pooled > 0) {
        plot_268_pooled <- plot_full_mat(t_avg_268_pooled, "data/map268_subnetwork.csv")
      }
      
      # only plot the 55 plot if n_55_studies > 0
      if (n_55_studies > 0) {
        plot_55 <- plot_full_mat(t_avg_55) #TODO: need to get the real 55 node map file!
      }

      # if there is only one plot, only plot that one, otherwise plot both
      if (((n_268_studies == 0) & (n_268_studies_pooled == 0)) & (n_55_studies > 0)) {
        grid.arrange(plot_55, ncol = 1)
      }
      else if ((n_55_studies == 0) & ((n_268_studies > 0))) {
        grid.arrange(plot_268, ncol = 1)
      }
      else if ((n_55_studies == 0) & ((n_268_studies_pooled > 0))) {
        grid.arrange(plot_268_pooled, ncol = 1)
      }
      else if ((n_55_studies > 0) & (n_268_studies > 0)) {
        grid.arrange(plot_268, plot_55, ncol = 1)
      }
      else if ((n_55_studies > 0) & (n_268_studies_pooled > 0)) {
        grid.arrange(plot_268_pooled, plot_55, ncol = 1)
      }}
    , height = reactive(v$h))#, width = reactive(v$w))
    

    # plotting brain images:
    ## TODO: ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
    output$brain <- renderPlot({
    # load template brain image: ** TODO: WILL NEED TO CHANGE **
    
      validate(
      need(length(v$d_clean_act) == 1, "Please select exactly one task to visualize the activation map."),
      need(length(v$d_clean_act) > 0, paste0(c("We do not have activation data for the selected parameters. The maps we have available are:", effect_maps_available))),
      need(dim(v$nifti != NA), "")
      )
    
      #print(paste("study name to plot brain of: " , v$study_name))
      #print(paste("dims of nifti: ", dim(v$nifti)))
      #print(paste("attributes of nifti:", attributes(v$nifti)))
      #print(class(v$nifti))

      plot_brain(v$nifti, anatomical)
        # ortho2(
        #     x = anatomical,
        #     y = v$nifti,
        #     crosshairs = FALSE,
        #     bg = 'white',
        #     NA.x = TRUE,
        #     col.y = oro.nifti::hotmetal(),
        #     xyz = c(input$xCoord, input$yCoord, input$zCoord),
        #     text.color = 'black',
        #     ybreaks = seq(min(v$nifti, na.rm = TRUE), max(v$nifti, na.rm = TRUE), length.out = 65),
        #     ycolorbar = TRUE,
        #     mfrow = c(3, 1)
        # )
        # # Add a colorbar with labels
        # min_val <- min(v$nifti, na.rm = TRUE)
        # max_val <- max(v$nifti, na.rm = TRUE)
        # num_breaks <- 65  # Number of breaks for the color scale
        # breaks <- seq(min_val, max_val, length.out = num_breaks)
        # labels <- round(seq(min_val, max_val, length.out = num_breaks - 1), 2)  # Adjust labels length
        
        # colorbar(
        #   breaks = breaks,
        #   col = oro.nifti::hotmetal(),
        #   labels = labels,
        #   text.col = "black"
        # )
   
    
        # Add labels only at specified indices
      #  mtext(text = labels, side = 4, at = breaks[label_indices], las = 1, cex = 0.8)
    })
}

# Run app ----
shinyApp(ui, server)
