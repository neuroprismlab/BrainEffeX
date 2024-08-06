# load libraries
library(shiny)
library(shinythemes)
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

# source helper functions
source("helpers.R")

#TODO: this is a temporary fix because we don't have the data for the other studies yet and treat activation maps differently
effect_maps_available = c("emotion", "gambling", "relational", "social", "wm")

# load data
load("data/sim_ci.RData") 

d_clean <- d

phen_study <- study #TODO: just remove phen_study, it's the same as study now and can be interchangeable

# temporary fix for removing IMAGEN data for now from study, phen_study, and effect_maps #TODO: either remove this or update the data to include IMAGEN data
d_clean <- d_clean[!grepl("IMAGEN", study$dataset)]
phen_study <- phen_study[!grepl("IMAGEN", phen_study$dataset),]
study <- study[!grepl("IMAGEN", study$dataset),]

# d_clean is a list that includes the effect maps, 
# and "study" is a table that contains study information

### d_clean is a list of studies, each study contains: 
# sample size as n, n1, and/or n2
# p-value as p
# effect size as d
# std as std.x and std.y
# original statistic values as orig_stat
# cohen's d values as d
# bound of simultaneous confidence intervals as sim_ci_lb and sim_ci_ub

#### study is a data frame that contains information about each study, including:
# basefile
# folder
# name
# extension as ext
# dataset
# map_type
# orig_stat_type
# var1
# var2
# ref
# code (as the phenotype category)

# options for spinner
options(spinner.color = "#9ecadb",
        spinner.color.background = "#ffffff", spinner.size = 1)

########################################################################################
# User interface ----
ui <- fluidPage(
 # theme = shinytheme("spacelab"),
  useShinyjs(),
  
  # JavaScript to trigger the modal on app load
  tags$script(HTML("
    $(document).ready(function(){
      setTimeout(function() {
        $('#instructionsModal').modal('show');
      }, 500);
    });
  ")),
  
  titlePanel(
    fluidRow(
      column(12,
             h1("BrainEffeX"),
             h4("A tool for exploring effect sizes in typical neuroimaging study designs"),
       
             actionButton(
               "showInstructions",
               "How to Use This App",
               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
             )
      )
    )
  ),
  
  hr(), # space
  
  fluidRow( # top row
    column(3, # inputs
           helpText("Select from the following options to visualize effect sizes:"),
           
           selectInput("dataset",
                       label = tagList("Dataset", icon("info-circle", id = "dataset_icon")),
                       choices = c("All" = "*", unique(study["dataset"]))),
           bsTooltip("dataset_icon", "Choose the dataset to visualize.", "right", options = list(container = "body")),
           
           selectInput("measurement_type",
                       label = tagList("Map Type", icon("info-circle", id = "measurement_type_icon")),
                       choices = c("All" = "*", unique(study["map_type"]))),
           bsTooltip("measurement_type_icon", "Select the type of map for analysis (e.g., brain regions, networks).", "right", options = list(container = "body")),
           
           selectizeInput("task",
                          label = tagList("Task", icon("info-circle", id = "task_icon")),
                          choices = c("All" = "*", unique(study["var1"])),
                          multiple = TRUE, selected = "*"),
           bsTooltip("task_icon", "Choose one or more tasks for the analysis.", "right", options = list(container = "body")),
           
           selectInput("test_type",
                       label = tagList("Test Type", icon("info-circle", id = "test_type_icon")),
                       choices = c("All" = "*", unique(study$orig_stat_type))),
           bsTooltip("test_type_icon", "Select the statistical test type for the analysis.", "right", options = list(container = "body")),
           
           conditionalPanel(
             condition = "input.test_type.indexOf('r') > -1",
             selectInput("behaviour",
                         label = tagList("Behavioural correlation", icon("info-circle", id = "behaviour_icon")),
                         choices = c("All" = "*", unique(study[study$orig_stat_type=="r", "var2"])),
                         multiple = TRUE,
                         selected = c("*")),
             bsTooltip("behaviour_icon", "Select behavioral variables for correlation analysis.", "right", options = list(container = "body"))
           ),
           
           selectInput("spatial_scale",
                       label = tagList("Spatial scale", icon("info-circle", id = "spatial_scale_icon")),
                       choices = c("Univariate", "Network-level", "whole-brain")),
           bsTooltip("spatial_scale_icon", "Select the spatial scale for the analysis.", "right", options = list(container = "body")),
           
           selectInput("group_by", 
                       label = tagList("What do you want to group by?", icon("info-circle", id = "group_by_icon")),
                       choices = c("None", "Statistic", "Phenotype Category")), 
           bsTooltip("group_by_icon", "Choose how to group the analysis results.", "right", options = list(container = "body")),
           
           downloadButton("downloadData", "Download Data"),
           h1(" "),
           wellPanel(style = "background-color: #ffffff;", 
                     helpText("For correlation studies (r), Var1 is the scanning condition, and Var2 is the behaviour."),
                     helpText("For task vs. rest studies (t), Var1 is the task, and Var2 is rest."),
                     helpText("For between-group studies (t2), Var1 and Var2 are the two groups."),
                     helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals.")
           ),
           h1(" "),
           h6("Version 1.3; Last updated 2024-June-03")
    ),
    
    column(5, align = "centre", # simCI plots
           h4("The plots below visualize all edges or voxels in each study."),
           helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
           
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
  
  # Modal Dialog
  bsModal(
    id = "instructionsModal", title = "How to Use This App", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("Welcome to BrainEffeX! Here's how to get started:"),
      tags$ol(
        tags$li("Select a open source dataset from the 'Dataset' dropdown."),
        tags$li("Choose a map type that matches your analysis needs."),
        tags$li("Use the 'Task' dropdown to specify tasks you are interested in."),
        tags$li("Set the 'Test Type' to define the statistical analysis."),
        tags$li("If applicable, select 'Behavioural correlation' variables."),
        tags$li("Choose the 'Spatial scale' to determine analysis granularity."),
        tags$li("Decide how to group results using 'Group by'."),
        tags$li("Visualize results in plots and download data if needed."),
        tags$li("Refer to the tooltips next to each input for additional guidance.")
      ),
      tags$p("Use the 'How to Use This App' button at any time to revisit these instructions.")
    )
  )
)
########################################################################################
# Server logic ----
server <- function(input, output, session) {
    
  # Show modal when 'How to Use This App' button is clicked
    observeEvent(input$showInstructions, {
    toggleModal(session, "instructionsModal", toggle = "open")
  })
  
  
    # set reactive parameters
    v <- reactiveValues()
    observeEvent(list(input$dataset, input$measurement_type, input$task, input$test_type, input$behaviour), priority = 1,{
        v$d_clean <- d_clean[grepl(input$dataset, study$dataset) & 
                             grepl(input$measurement_type, study$map_type) & 
                             (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1)) & 
                            (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                             grepl(paste(input$behaviour, collapse="|"), study$var2)]
      
        # also filter study by the same parameters
        v$study <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) & 
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1)) & 
                         (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                         grepl(paste(input$behaviour, collapse="|"), study$var2)),]

        v$task_choices <- unique(v$study$var1)
            
        # filter phen_study the same way
        v$phen_study <- phen_study[grepl(input$dataset, phen_study$dataset) & 
                  grepl(input$measurement_type, phen_study$map_type) & 
                  (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), phen_study$var1)) & 
                  (input$test_type == "*" | (phen_study$orig_stat_type == input$test_type)) &
                  grepl(paste(input$behaviour, collapse="|"), phen_study$var2),]
    })

        observeEvent(ignoreInit = TRUE, input$dataset, {
          v$test_choices <- study[(grepl(input$dataset, study$dataset)),"orig_stat_type"]
          updateSelectInput(session, "test_type", selected = "*", choices = c("All" = "*", unique(v$test_choices)))
        })

        observeEvent(ignoreInit = TRUE, input$dataset, {
          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"var1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1))),"var2"]

          updateSelectInput(session, "task", selected = "*", choices = c("All" = "*", unique(v$task_choices)))
        })

        observeEvent(ignoreInit = TRUE, input$dataset, {
          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"var1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1))),"var2"]

          updateSelectInput(session, "behaviour", selected = "*", choices = c("All" = "*", unique(v$beh_choices)))
        })
        
        # constrain parameters
        # update behaviour selections to only be the available constrained selections... 
        observeEvent(ignoreInit = TRUE, input$measurement_type, priority = 2, {

          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"var1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1))),"var2"]

          if (input$test_type == "r") {
            updateSelectInput(session, "behaviour", choices = c("All" = "*", unique(v$beh_choices)))
          }
          print("measurement type changed")
          updateSelectizeInput(session, server = TRUE, "task", selected = "*", choices = c("All" = "*", unique(v$task_choices)))
        }) 



        # when test type is changed from r to another test type, reset behaviour to all 
        observeEvent(ignoreInit = TRUE, list(input$test_type), {
          v$task_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type)),"var1"]

          v$beh_choices <- study[(grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) &
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1))),"var2"]

          if (input$test_type != "r") {
            updateSelectInput(session, "behaviour", selected = "*", choices = v$beh_choices)
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

          v$phen_study_fc <- v$phen_study[grepl("_FC_", v$phen_study$name),]
})


    ##### Group_by ######

    toListen <- reactive({
      list(input$group_by, input$dataset, input$map_type, input$task, input$test_type)
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
              matching_d_idx <- which(toupper(names(v$d_clean)) %in% toupper(matching_names))
              # matching_d_idx is the idx of the studies in d that match the current stat and ref
              # average across all studies in matching_d_idx
              # initialize an empty vector to store the sum across studies
              d_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]]$d)) # initialize to the size of the largest matrix
              ci_lb_total <- rep(0, length(v$d_clean[[matching_d_idx[1]]]$d)) # TODO: for now just average across CIs, but ask Steph how we should do this!!!
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
        for (phen in unique(v$study$code)) {

          for (ref in unique(v$study$ref)) {
            matching_idx <- which(v$study$code == phen & v$study$ref == ref)
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
      if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (input$task) %in% effect_maps_available) {
      file_list <- list.files(path = "data/", full.names = TRUE)
      v$case_task <- toupper(input$task)
      pattern <- paste0(v$case_task, ".*\\.nii\\.gz")
      matching_file <- grep(pattern, file_list, value = TRUE)
      v$effect_map <- readnii(matching_file)
      }
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
        plot_output_list <- lapply(1:length(v$d_clean), function(i) {
          plotname <- paste0("plot", i)
          plotOutput(plotname, height = "200px", width = "100%")
        })

        # convert the list to a tagList, this is necessary for the list of items to display properly
        do.call(tagList, plot_output_list)
      }
      
      else if (input$group_by == "Statistic") {
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
        for (i in 1:length(v$d_clean)) {
          # create a local variable to hold the value of i
          local({
            my_i <- i
            plotname <- paste0("plot", my_i, sep="")

            output[[plotname]] <- renderPlot({
              plot_sim_ci(v$d_clean[[my_i]], names(v$d_clean)[my_i], v$study[my_i,])
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
              plot_sim_ci_stat(v$d_stat[[my_i]], names(v$d_stat)[my_i], v$study_stat[my_i,])
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
      v$num_268_studies <- sum(v$phen_study_fc$ref == "Shen_268")
      v$num_55_studies <- sum(v$phen_study_fc$ref == "UKB_55")
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
      t_total_55 <-  rep(0 , 1485)

      n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
      n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation

      for (i in 1:length(v$d_clean_fc)) {
        t <- v$d_clean_fc[[i]]$d

        phen_study_idx <- which(toupper(v$phen_study_fc$name) == toupper(names(v$d_clean_fc)[i]))
        if (v$phen_study_fc$ref[phen_study_idx] == "Shen_268") { # TODO: create a v$study_fc table to store just fc studies
          
          # add t to the total vector as the sum of t_total and t
          t_total_268 <- t_total_268 + t
          n_268_studies <- n_268_studies + 1
        }
        
        else if (v$phen_study_fc$ref[phen_study_idx] == "UKB_55") {
          
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
      
      # only plot the 55 plot if n_55_studies > 0
      if (n_55_studies > 0) {
        plot_55 <- plot_full_mat(t_avg_55) #TODO: need to get the real 55 node map file!
      }

      # if there is only one plot, only plot that one, otherwise plot both
      if ((n_268_studies == 0) & (n_55_studies > 0)) {
        grid.arrange(plot_55, ncol = 1)
      }
      else if ((n_55_studies == 0) & (n_268_studies > 0)) {
        grid.arrange(plot_268, ncol = 1)
      }
      else if ((n_55_studies > 0) & (n_268_studies > 0)) {
        grid.arrange(plot_268, plot_55, ncol = 1)
      }
    }, height = reactive(v$h))#, width = reactive(v$w))
    

    # plotting brain images:
    ## TODO: ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
    output$brain <- renderPlot({
    # load template brain image: ** TODO: WILL NEED TO CHANGE **
    template <- readnii('data/anatomical.nii')
      validate(
      need(length(v$d_clean_act) == 1, "Please select exactly one task to visualize the activation map."),
      need(length(v$d_clean_act) > 0, paste0(c("We do not have activation data for the selected parameters. The maps we have available are:", effect_maps_available)))
    )
    
        ortho2(
            x = template,
            y = v$effect_map,
            crosshairs = FALSE,
            bg = 'white',
            NA.x = TRUE,
            col.y = oro.nifti::hotmetal(),
            xyz = c(input$xCoord, input$yCoord, input$zCoord),
            text.color = 'black',
            ybreaks = seq(min(v$effect_map), max(v$effect_map), length.out = 65),
            ycolorbar = TRUE,
            mfrow = c(3, 1)
        )
        # colorbar(breaks = seq(min(v$effect_map), max(v$effect_map), length.out = 65), col = oro.nifti::hotmetal(), labels = seq(min(v$effect_map), max(v$effect_map), length.out = 64), text.col = "black")
        #TODO: add numbers to legend of brain figure (have tried many times and can't figure it out so far)
    })
}

# Run app ----
shinyApp(ui, server)
