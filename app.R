
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

effect_maps_available = c("emotion", "gambling", "relational", "social", "wm")

# # checking missing packages from list
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()
#                                    [, "Package"])]

# # install missing packages
# if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# load data
load("data/estimate_simci.RData") 

# load phen_study dataframe (dataframe called phen_study)
load("data/phen_study.RData")

# source helper functions
source("helpers.R")

d_clean <- d

# this will load a variable called d_clean that has the effect maps, 
# and a variable called "study" that contains study information

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

# options for spinner
options(spinner.color = "#9ecadb",
        spinner.color.background = "#ffffff", spinner.size = 1)



########################################################################################
# User interface ----
ui <- fluidPage(theme = shinytheme("spacelab"),
  useShinyjs(),

  titlePanel(fluidRow(
    column(12,
           h1("BrainEffeX"),
           h4("A tool for exploring effect sizes in typical neuroimaging study designs")
    )
  )
  ),
  
  hr(), # space

  # mainPanel(
  #   textOutput("selected_vars")
  # ),

  fluidRow( # top row
      column(3, # inputs
      helpText("Select from the following options to visualize effect sizes:"),
                  
      selectInput("dataset",
      			  label = "Dataset",
      			  choices = c("All" = "*", unique(study["dataset"]))),
      
      selectInput("measurement_type",
      			  label = "Map Type",
      			  choices = c("All" = "*", unique(study["map_type"]))),
      
      selectizeInput("task",
      			  label = "Task",
      			  choices = c("All" = "*", unique(study["var1"])),
              multiple = TRUE, selected = "*"),
      
      selectInput("test_type",
      			  label = "Test Type",
      			  choices = c("All" = "*", unique(study$orig_stat_type))), ## TODO: change this is d when data is updated to cohen's d
      			  
      conditionalPanel(
        condition = "input.test_type.indexOf('r') > -1",
            selectInput("behaviour",
      			  label = "Behavioural correlation", #TODO: update choices to include all possible options! For example, missing BMI currently
      			  choices = c("All" = "*", unique(study[study$orig_stat_type=="r","var2"])),
              multiple = TRUE,
              selected = c("*"))),

      selectInput("spatial_scale",
              label = "Spatial scale",
              choices = c("Univariate", "Network-level", "whole-brain")),
              
      selectInput("group_by", 
                  label = "What do you want to group by?",
                  choices = c("None", "Statistic", "Phenotype Category")), 
    


      downloadButton("downloadData", "Download Data"),
      h1(" "),
      wellPanel(style = "background-color: #ffffff;", 
      helpText("For correlation studies (r), Var1 is the scanning condition, and Var2 is the behaviour."),
      helpText("For task vs. rest studies (t), Var1 is the task, and Var2 is rest."),
      helpText("For between-group studies (t2), Var1 and Var2 are the two groups."),
      helpText("The maximum conservative effect size is the largest of: 1) the absolute value of the largest lower bound across confidence intervals, 2) the absolute value of the smallest upper bound across confidence intervals.")
      ),
      # add a box that can print out troubleshooting information
      # wellPanel(style = "background-color: #ffffff;",
      #           h3("Troubleshooting"),
      #           verbatimTextOutput("selected_vars")
      #           )

      ),

      column(5, align = "centre", # simCI plots
        h4("The plots below visualize all edges or voxels in each study."),
        helpText("Simultaneous confidence intervals (95% CI across all edges/voxels). Red indicates simultaneous CIs overlapping with 0, green indicates no overlap."),
        
        wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("histograms"), type = 1))),
      
      column(4, align = "center", # effect size matrices)
        wellPanel(style = "background-color: #ffffff;", h3("Effect size matrices"),
        withSpinner(plotOutput("maps", width = "100%", height = "100%"), type = 1)),
        h1(" "),
        h1(""),
        h1(""),
        wellPanel(style = "background-color: #ffffff;", h3("Activation Maps (Cohen's d)"),
        h1(""),
            fluidRow( # second row: plots of activation maps for activation studies 
              column(4, numericInput("xCoord", "X", 30), numericInput("yCoord", "Y", 30), numericInput("zCoord", "Z", 30)),
              column(8, withSpinner(plotOutput("brain", width = "100%"), type = 1)))
        )))
        
        )
    

    

########################################################################################
# Server logic ----
server <- function(input, output, session) {
    
    
    # set reactive parameters
    v <- reactiveValues()
    observeEvent(list(input$dataset, input$measurement_type, input$task, input$test_type, input$behaviour), priority = 1,{
        v$d_clean <- d_clean[grepl(input$dataset, study$dataset) & 
                             grepl(input$measurement_type, study$map_type) & 
                             (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1)) & 
                            #  grepl(input$test_type, study$orig_stat_type) & 
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

          # create a v$study_fc table to store just fc studies
          v$phen_study_fc <- v$phen_study[grepl("_FC_", v$phen_study$name),]
})

    observe({
      v$this_fill <- "statistic"

      if (input$group_by == "None") { # show each individual study
        v$grouping <- "study"
        v$axis_label <- "Study"
        v$this_density_scale <- 5
        v$this_xlim <- c(-.5,.5)
      }
      else if(input$group_by == "Statistic") { # group by statistic
        v$grouping <- "statistic"
        v$axis_label <- "Statistic"
        v$this_density_scale <- 2.1
        v$this_xlim <- c(-1,1)
      }
    
      if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (input$task) %in% effect_maps_available) {
      file_list <- list.files(path = "data/", full.names = TRUE)
      v$case_task <- toupper(input$task)
      pattern <- paste0(v$case_task, ".*\\.nii\\.gz")
      matching_file <- grep(pattern, file_list, value = TRUE)
      v$effect_map <- readnii(matching_file)
      }
    })
    #   else if (input$group_by == "Phenotype Category") { # group by phenotype category
    #     v$grouping <- "code"
    #     #v$this_fill <- "code"
    #     v$axis_label <- "Phenotype Category"
    #     v$this_density_scale <- 3
    #     v$this_xlim <- c(-0.5, 0.5)
    #   }
    ## TODO: add this again after ^^



    
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
      else {
        plot_output_list <- lapply(1:length(v$d_clean), function(i) {
          plotname <- paste0("plot", i)
          plotOutput(plotname, height = "200px", width = "100%")
        })

        # convert the list to a tagList, this is necessary for the list of items to display properly
        do.call(tagList, plot_output_list)
      }})
    })
    # call renderPlot for ecah one
    # plots are only actually generated when they are visible on the web page
    observe({
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
    })

    # create a reactive value to store the height and width of the plot
    # the height should be double the width only when there are two plots (when there are some studies with 268 node parcellation and some with 55 node parcellation),
    # and height should be equal to width when there is only one plot (when all studies are of the same parcellation type)
    
    observe({
      v$num_268_studies <- sum(v$phen_study_fc$ref == "Shen_268")
      v$num_55_studies <- sum(v$phen_study_fc$ref == "UKB_55")
      if (v$num_268_studies > 0 & v$num_55_studies > 0) {
        v$h <- 1000
        v$w <- 500
      }
      else {
        v$h <- 500
        v$w <- 500
      }
    })

    
    output$maps <- renderPlot({
      validate(
      need((0 < length(v$d_clean_fc)), "We do not have FC data for the selected parameters"))
      
      # create a matrix to store the data for if there is more than one study
      t_total_268 <- matrix(0, nrow = 268, ncol = 268) #TODO: check if this should be a vector instead!
      t_total_55 <-  matrix(0, nrow = 55, ncol = 55)

      n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
      n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation

      for (i in 1:length(v$d_clean_fc)) {
        t <- v$d_clean_fc[[i]]$d

        phen_study_idx <- which(toupper(v$phen_study_fc$name) == toupper(names(v$d_clean_fc)[i]))
        if (v$phen_study_fc$ref[phen_study_idx] == "Shen_268") { # TODO: create a v$study_fc table to store just fc studies
          n_nodes <- sqrt(length(t))
          
          # add t to the total matrix as the sum of t_total and t
          t_total_268 <- t_total_268 + t
          n_268_studies <- n_268_studies + 1
        }
        
        else if (v$phen_study_fc$ref[phen_study_idx] == "UKB_55") {
          n_nodes <- sqrt(length(t))
          
          # add the data to the total matrix
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
        plot_268 <- plot_matrix(t_avg_268, "data/map268_subnetwork.csv", reorder = TRUE)
      }
      
      # only plot the 55 plot if n_55_studies > 0
      if (n_55_studies > 0) {
        plot_55 <- plot_matrix(t_avg_55, "data/map55_FILLER.csv", reorder = TRUE) #TODO: need to get the real 55 node map file!
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
#TODO: add numbers to legend of brain figure

        # orthographic(template, effect_map,
        # xyz = c(input$xCoord, input$yCoord, input$zCoord),
        # bg = 'white', col = "white")
    })
}


# Run app ----
shinyApp(ui, server)
