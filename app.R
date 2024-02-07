# different ways of printing text:
# https://stackoverflow.com/questions/50781653/renderprint-option-in-shinyapp
# after adding shiny library, run with: runApp("app.R")
# install packages with R, not VSCode (VSC sometimes requires extra libraries)

# list of packages required:
list_of_packages <- c("shiny", "ggplot2", "oro.nifti",
                      "neurobase", "ggcorrplot",
                      "ggridges", "pheatmap", "shinycssloaders", "shinyjs", "fields")

# checking missing packages from list
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()
                                   [, "Package"])]

# install missing packages TODO UPDATE WHEN WE ADD MORE MAPS
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

#load packages and data
library(shiny)
library(ggplot2)
library(oro.nifti)
library(neurobase)
library(ggcorrplot)
library(ggridges)
library(pheatmap)
library(shinycssloaders)
library(shinyjs)
library(fields)

# load data
d_clean <- readRDS("data/d_clean_hcp_ukb.rds")
studies <- read.csv("data/studies.csv")
# TODO this will change once I get the proper data structure
# from Steph all in one file.
# (for now using effect_maps data to obtain sample size values, but
# ideally in the future the sample sizes will be a part of d_clean)
# effect_maps <- readRDS("/Users/neuroprism/Library/CloudStorage/
#                        GoogleDrive-halleeninet@gmail.com/
#                        .shortcut-targets-by-id/
#                        1gVHbxOjZcP8uB8Au0lhLpVHCR8RzcdCv/
#                        Effect_Size/data/effect_maps")

# assume that the final data Steph will give me has the following cols:
# study_name, d, original_statistic, ci_lb, ci_ub, n1, n2, phenotype_code

# options for spinner
options(spinner.color = "#9ecadb",
        spinner.color.background = "#ffffff", spinner.size = 1)

# list of available effect maps for visualization
effect_maps_available = c("emotion", "gambling", "relational", "social", "wm")

# User interface ----
ui <- fluidPage(
  useShinyjs(),

  titlePanel("Typical fMRI Effect Size Explorer"),
  
  hr(), # space

  fluidRow( # top row: inputs, probability density plots
      column(4, # inputs
      helpText("Select from the following options to visualize effect sizes:"),
                  
      selectInput("dataset",
      			  label = "Dataset",
      			  choices = c("All" = "*", "ABCD", "SLIM", "HCP" = "hcp", "PNC" = "pnc", "UKB" = "ukb", "HBN" = "hbn", "IMAGEN")),
      
      selectInput("measurement_type",
      			  label = "Measurement Type",
      			  choices = c("All" = "*", "Task-based Activation" = "act", "Functional Connectivity" = "fc")),
      
      selectInput("task",
      			  label = "Task",
      			  choices = c("All" = "*", "Rest" = "rest", "SST", "Emotion" = "emotion", "N-back" = "nback", "Relational" = "relational", "Social" = "social", "Working Memory" = "wm", "Gambling" = "gambling"),
              multiple = TRUE),
      
      selectInput("test_type",
      			  label = "Test Type",
      			  choices = c("All" = "*", "One-sample task-rest" = "\\.t\\.", "Two-sample group contrast" = "\\.t2\\.", "Behavioural correlation" = "\\.r\\.")), ## TODO: change this is d when data is updated to cohen's d
      			  
      conditionalPanel(
        condition = "input.test_type.indexOf('r') > -1",
            selectInput("behaviour",
      			  label = "Behavioural correlation", #TODO: update choices to include all possible options! For example, missing BMI currently
      			  choices = c( "All" = "*","Age" = "\\.age", "IQ" = "\\.iq", "Fluid Intelligence" = "\\.gf", "Peabody Picture Vocab Test" = "\\.ppvt", "Expressive Vocab Test" = "\\.evt", "Stop Signal Task" = "\\.SST", "Letter N-Back Accuracy" = "\\.lnbxacc", "Letter N-Back Response Time" = "\\.lnbxrt", "Penn Face Memory Test Accuracy" = "\\.pfmtxacc", "Penn Face Memory Test Response Time" = "\\.pfmtxrt", "Penn Matrix Reasoning Test Correct Responses" = "\\.pmatxrc", "Penn Verbal Reasoning Test Accuracy" = "\\.pvrtxacc", "Penn Verbal Reasoning Test Response Time" = "\\.pvrtxrt", "Penn Word Memory Test Accuracy" = "\\.pwmtxacc", "Penn Word Memory Test Response Time" = "\\.pwmtxrt", "Wide Range Assessment Test" = "\\.wrat"),
              multiple = TRUE)),
      # selectInput("behaviour",
      # 			  label = "Behavioural correlation",
      # 			  choices = c( "All" = "*","Age" = "\\.age", "IQ" = "\\.iq", "Fluid Intelligence" = "\\.gf", "Peabody Picture Vocab Test" = "\\.ppvt", "Expressive Vocab Test" = "\\.evt", "Stop Signal Task" = "\\.SST", "Letter N-Back Accuracy" = "\\.lnbxacc", "Letter N-Back Response Time" = "\\.lnbxrt", "Penn Face Memory Test Accuracy" = "\\.pfmtxacc", "Penn Face Memory Test Response Time" = "\\.pfmtxrt", "Penn Matrix Reasoning Test Correct Responses" = "\\.pmatxrc", "Penn Verbal Reasoning Test Accuracy" = "\\.pvrtxacc", "Penn Verbal Reasoning Test Response Time" = "\\.pvrtxrt", "Penn Word Memory Test Accuracy" = "\\.pwmtxacc", "Penn Word Memory Test Response Time" = "\\.pwmtxrt", "Wide Range Assessment Test" = "\\.wrat"),
      #         multiple = TRUE), 

      selectInput("spatial_scale",
              label = "Spatial scale",
              choices = c("Univariate", "Network-level", "whole-brain")),
              
      selectInput("group_by", 
                  label = "What do you want to group by?",
                  choices = c("None", "Statistic", "Phenotype Category"))
    
      ),

      column(8, align = "center", # probability density plots
      h2("Effect size probability density"),
      withSpinner(plotOutput("histograms"), type = 1)
      )
      ),

hr() ,
helpText("To visualize an FC effect size matrix, select FC as Measurement Type, and complete other selections."),
helpText("To visualize activation effect sizes on the brain, select Task-Based Activation and select a dataset, task, and test type."),

    fluidRow( # second row: conditional row of additional inputs and plots depending on previous selections
        column(4, # if activation maps are selected, show MRI visualization inputs
        conditionalPanel(
        condition = "input.measurement_type === 'act' && input.dataset !== '*' && input.task !== '*' && input.test_type !== '*' && input.behaviour !== '*'",
        sidebarPanel(
            numericInput("xCoord", "X Coordinate", 30),
            numericInput("yCoord", "Y Coordinate", 30),
            numericInput("zCoord", "Z Coordinate", 30))
    )),

        column(8, align = "center", # if FC is selected, show heatplot of FC effect sizes
        conditionalPanel(
          condition = "input.measurement_type === 'fc' && input.dataset !== '*' && input.task !== '*' && input.test_type !== '*' && input.behaviour !== '*' && input.behaviour.length > 0",
          h2("Effect size matrix"),
          withSpinner(plotOutput("maps", height = "500px", width = "500px"), type = 1)
          ),
        conditionalPanel( # if activation maps selected, show MRI visualization
          condition = "input.measurement_type === 'act' && input.dataset !== '*' && input.task !== '*' && input.test_type !== '*'",
          h2("Effect size maps"),
          withSpinner(plotOutput("brain"), type = 1)
          ))
    )
  )


# Server logic ----
server <- function(input, output, session) {
    # set reactive parameters
    v <- reactiveValues()
    observe({
        # change the data being used to generate plots depending on inputs selected
        # TODO: allow for two task selections, i.e. rest and SST to filter by BOTH tasks
      v$d_clean <- subset(d_clean, grepl(input$dataset, d_clean$study) & 
      							   grepl(input$measurement_type, d_clean$study) &
      							   # grepl(input$task, d_clean$study) &
                       (d_clean$study %in% input$task | grepl(paste(input$task, collapse="|"), d_clean$study, ignore.case = TRUE)) &
      							   grepl(input$test_type, d_clean$study) &
      							   (d_clean$study %in% input$behaviour | grepl(paste(input$behaviour, collapse="|"), d_clean$study, ignore.case = TRUE)))
      
      if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && input$task %in% effect_maps_available) {
        file_list <- list.files(path = "data/", full.names = TRUE)
        v$case_task <- toupper(input$task)
        pattern <- paste0(v$case_task, ".*\\.nii\\.gz")
        matching_file <- grep(pattern, file_list, value = TRUE)
        v$effect_map <- readnii(matching_file)
      }

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
      else if (input$group_by == "Phenotype Category") { # group by phenotype category
        v$grouping <- "code"
        #v$this_fill <- "code"
        v$axis_label <- "Phenotype Category"
        v$this_density_scale <- 3
        v$this_xlim <- c(-0.5, 0.5)
      }

      # reset behavioural correlation selections when test_type is changed from behavioural correlation to something else
      # the behavioural correlation selectInput disappears when test_type is not behavioural correlation, 
      # so need to reset when it disappears
      # observeEvent(input$test_type, {
      #   if (input$test_type != "\\.r\\.") {
      #   updateSelectInput(session, "behaviour", selected = "*")}}, ignoreNULL = TRUE)

      # toListen <- reactive({
      #   list(input$dataset, input$measurement_type, input$task, input$test_type)})
        
      
      # observeEvent(toListen(), {
      #   updateSelectInput(session, "behaviour", choices = c("All" = "*", unique(studies[studies$name %in% unique(v$d_clean$study), ]$var2)))
      # })

      observeEvent(ignoreInit = TRUE, list(input$dataset, input$measurement_type, input$task, input$test_type), {
        selected_studies <- studies$name %in% unique(v$d_clean$study)
        if (!is.null(input$test_type) && input$test_type == "\\.r\\.") {
          updateSelectInput(session, "behaviour", choices = c("All" = "*", unique(studies[selected_studies, ]$var2)))
        }
      })

      ## TODO: with these constraining functions, need to make the resulting choices more human-readable

      observeEvent(ignoreInit = TRUE, list(input$behaviour, input$measurement_type, input$task, input$test_type), {
        selected_studies <- studies$name %in% unique(v$d_clean$study)
        updateSelectInput(session, "dataset", choices = c("All" = "*", unique(studies[selected_studies, ]$dataset)))
      }) # this works, but doesn't look like it's working perfectly because the data used to make studies is not the same as the data used to make d_clean for now

      observeEvent(ignoreInit = TRUE, list(input$behaviour, input$dataset, input$task, input$test_type), {
        selected_studies <- studies$name %in% unique(v$d_clean$study)
        updateSelectInput(session, "measurement_type", choices = c("All" = "*", unique(studies[selected_studies, ]$map_type)))
      })

      observeEvent(ignoreInit = TRUE, list(input$behaviour, input$dataset, input$task, input$measurement_type), {
        selected_studies <- studies$name %in% unique(v$d_clean$study)
        updateSelectInput(session, "test_type", choices = c("All" = "*", unique(studies[selected_studies, ]$stat_type)))
      })

      observeEvent(ignoreInit = TRUE, list(input$behaviour, input$dataset, input$test_type, input$measurement_type), {
        selected_studies <- studies$name %in% unique(v$d_clean$study)
        updateSelectInput(session, "task", choices = c("All" = "*", unique(studies[selected_studies, ]$var1))) ## TODO: be more specific about var1 and var2
      })

      # observeEvent(v$d_clean, {
      #   updateSelectInput(session, "dataset", choices = c("All" = "*", studies[studies$name %in% unique(v$d_clean$study), ]$dataset))
      # })

      # observeEvent(v$d_clean, {
      #   updateSelectInput(session, "measurement_type", choices = studies[studies$name %in% unique(v$d_clean$study), ]$map_type)
      # })

      # observeEvent(v$d_clean, {
      #   updateSelectInput(session, "task", choices = studies[studies$name %in% unique(v$d_clean$study), ]$var1)
      # })

      # observeEvent(v$d_clean, {
      #   updateSelectInput(session, "test_type", choices = studies[studies$name %in% unique(v$d_clean$study), ]$stat_type)
      # })

    })
      


    # plot
    # render UI

    # TODO
    # update this plot to be a CI plot with the sim_ci_calc_plot.R script
    # once I get the proper data structure from Steph

    output$histograms <- renderPlot({
      # d_clean %>% filter(statistic == "r") # TODO: filter by input categories or compare all
      ggplot(v$d_clean,  aes(x = d, y = .data[[v$grouping]], fill = .data[[v$this_fill]])) +
        geom_density_ridges(scale = v$this_density_scale) +
        theme_ridges() +
        #theme(legend.position = "none") +
        labs(y = v$axis_label, x = "Cohen's d") +
        theme(axis.title.y = element_text(size = 20, face = "bold", hjust = 0.5),
        	  axis.title.x = element_text(size = 20, face = "bold", hjust = 0.5)) +
        xlim(v$this_xlim) +
        scale_fill_manual(values = c("t" = '#AABBE9', "r" = "#EBC6D0", "t2" = "#BEDCD5", "t (act)" = "#ECE7BC"))
    })
 

    # output$maps <- renderPlot({
    #     t <- v$d_clean[[1]]
    #     n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
    #     trilmask <- lower.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
    #     t2 <- trilmask
    #     t2[trilmask] <- t
    #     image(100 * t(apply(t2, 2, rev)),
    #           xlab = sprintf("%s Nodes", n_nodes),
    #           ylab = sprintf("%s Nodes", n_nodes),
    #           axes = FALSE)
    #     axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
    #     axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize Y-axis
    #     })

    # try plotting the map with heatmap instead of image
    # output$maps <- renderPlot({
    #   validate(need(length(input$task) < 2, "Please only select one task."),
    #   need(length(input$behaviour) > 0, "Please select one behavioural correlation."),
    #   need(length(input$behaviour) < 2, "Please only select one behavioural correlation."),
    #   need(dim(v$d_clean)[1] > 0, "We do not have data for the selected parameters"),
    #   need((length(unique(v$d_clean$study)) < 2), "Can only plot one study, please select more specific parameters"))
    #     t <- v$d_clean[[1]]
    #     n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
    #     trilmask <- lower.tri(matrix(1, nrow = n_nodes, ncol = n_nodes)) # creates a mask where lower triangle is TRUE
    #     t2 <- trilmask
    #     t2[trilmask] <- t # populates the lower triangle with values from t
    #     # the above line works by populating down columns from top to bottom, left to right
    #     # need to find out if that's the same way that d_clean was populated! TODO
    #     xlabel <- sprintf("%s Nodes", n_nodes)
    #     ylabel <- sprintf("%s Nodes", n_nodes)
    # 
    #     heatmap(t(apply(t2, 2, rev)),
    #       Colv = NA, Rowv = NA,  # Turn off row and column clustering
    #       col = heat.colors(256),
    #       xlab = xlabel, ylab = ylabel,
    #       scale = "none"
    #     )
    # })
    
    # # changing this plot to address the upper/lower triangle problem
    # # the previous way was not accounting for the change in triangle properly
    # output$maps <- renderPlot({
    #   validate(need(length(input$task) < 2, "Please only select one task."),
    #            need(length(input$behaviour) > 0, "Please select one behavioural correlation."),
    #            need(length(input$behaviour) < 2, "Please only select one behavioural correlation."),
    #            need(dim(v$d_clean)[1] > 0, "We do not have data for the selected parameters"),
    #            need((length(unique(v$d_clean$study)) < 2), "Can only plot one study, please select more specific parameters"))
    #   t <- v$d_clean[[1]]
    #   n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
    #   trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes)) # creates a mask where upper triangle is TRUE
    #   t2 <- trilmask
    #   t2[trilmask] <- t # populates the upper triangle with values from t
    #   xlabel <- sprintf("%s Nodes", n_nodes)
    #   ylabel <- sprintf("%s Nodes", n_nodes)
    #   
    #   heatmap(t(apply(t2, 2, rev)),
    #           Colv = NA, Rowv = NA,  # Turn off row and column clustering
    #           col = heat.colors(256),
    #           xlab = xlabel, ylab = ylabel,
    #           scale = "none"
    #   )
    # })
    # 
    # output$maps <- renderPlot({
    #     t <- v$d_clean[[1]]
    #     n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
    #     trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
    #     t2 <- trilmask
    #     t2[trilmask] <- t
    #     image(t2[,nrow(t2):1],
    #           xlab = sprintf("%s Nodes", n_nodes),
    #           ylab = sprintf("%s Nodes", n_nodes),
    #           axes = FALSE)
    #     axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
    #     axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize Y-axis
    #     })
    
    output$maps <- renderPlot({
      validate(need(length(input$task) < 2, "Please only select one task."),
      need(length(input$behaviour) > 0, "Please select one behavioural correlation."),
      need(length(input$behaviour) < 2, "Please only select one behavioural correlation."),
      need(dim(v$d_clean)[1] > 0, "We do not have data for the selected parameters"),
      need((length(unique(v$d_clean$study)) < 2), "Can only plot one study, please select more specific parameters"))
      t <- v$d_clean[[1]]
      if (length(t) == 71824) {
        # if the data includes the whole matrix, not just a triangle:
        n_nodes <- sqrt(length(t))
        trilmask <- matrix(TRUE, nrow = n_nodes, ncol = n_nodes)
        t2 <- trilmask
        t2[trilmask] <- t
        image.plot(t2[,nrow(t2):1],
              # xlab = sprintf("%s Nodes", n_nodes),
              # ylab = sprintf("%s Nodes", n_nodes),
              axes = FALSE, col = hcl.colors(100, palette = "viridis"))
        axis(1, at = seq(0, 1, by = 1), labels = seq(1, n_nodes, by = n_nodes-1), cex.axis = 1.3, lwd = 0)  # Customize X-axis
        axis(2, at = seq(0, 1, by = 1), labels = seq(n_nodes, 1, by = -n_nodes+1), cex.axis = 1.3, lwd = 0)
      }
      else {
        n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
        trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
        t2 <- trilmask
        t2[trilmask] <- t
        image.plot(t2[,nrow(t2):1],
              # xlab = sprintf("%s Nodes", n_nodes),
              # ylab = sprintf("%s Nodes", n_nodes),
              axes = FALSE, col = hcl.colors(100, palette = "viridis"))
        axis(1, at = seq(0, 1, by = 1), labels = seq(1, n_nodes, by = n_nodes-1), cex.axis = 1.3, lwd = 0)  # Customize X-axis
        axis(2, at = seq(0, 1, by = 1), labels = seq(n_nodes, 1, by = -n_nodes+1), cex.axis = 1.3, lwd = 0)
      } 
    })

    # try plotting brain images:
    ## TODO ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
    output$brain <- renderPlot({
        # load template brain image: ** TODO WILL NEED TO CHANGE **
    template <- readnii('data/anatomical.nii')
      validate(
      need(length(input$task) < 2, "Please only select one task."),
      #need(dim(v$d_clean)[1] > 0, "We do not have data for the selected parameters"),
      need(input$test_type == "\\.t\\.", "We currently only have task-based activation maps for one-sample task-rest contrasts")
    )
    # load sample stat map: ** WILL NEED TO CHANGE **
    # effect_map <- readnii('/Users/halleeshearer/Desktop/visualize_effects_app/data/abstract_association-test_z_FDR_0.01.nii')
  
        ortho2(
            x = template,
            y = v$effect_map,
            crosshairs = FALSE,
            bg = 'black',
            NA.x = TRUE,
            col.y = oro.nifti::hotmetal(),
            xyz = c(input$xCoord, input$yCoord, input$zCoord),
            ycolorbar = TRUE,
            ybreaks = seq(min(v$effect_map), max(v$effect_map), length.out = 65),
            mfrow = c(1, 3)
        )
#TODO: add numbers to legend of brain figure

        # orthographic(template, effect_map,
        # xyz = c(input$xCoord, input$yCoord, input$zCoord),
        # bg = 'white', col = "white")
    })
}


# Run app ----
shinyApp(ui, server)
