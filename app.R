
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

# list of packages required:
# list_of_packages <- c("shiny", "ggplot2", "oro.nifti",
#                       "neurobase", "ggcorrplot",
#                       "ggridges", "pheatmap", "shinycssloaders", "shinyjs", "fields", "sass", "bslib",
#                       "shinythemes")
# 
# for (package in list_of_packages) {
#   if (!require(package, character.only = TRUE)) {
#     install.packages(package, dependencies = TRUE)
#     library(package, character.only = TRUE, dependencies = TRUE)
#   }
# }

effect_maps_available = c("emotion", "gambling", "relational", "social", "wm")

# # checking missing packages from list
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()
#                                    [, "Package"])]

# # install missing packages
# if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

# load data
load("data/estimate_simci.RData") 

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
        
# helper function for plotting:
plot_sim_ci <- function(data, name, study_details) {

  # remove na
  na_idx <- is.na(data$d) | is.na(data$sim_ci_lb) | is.na(data$sim_ci_ub)
  data$d <- data$d[!na_idx]
  data$sim_ci_lb <- data$sim_ci_lb[!na_idx]
  data$sim_ci_ub <- data$sim_ci_ub[!na_idx]
  # sort data from smallest to largest d
  sorted_indices <- order(data$d)
  sorted_d <- data$d[sorted_indices]
  # sort confidence intervals by the same order
  sorted_upper_bounds <- data$sim_ci_ub[sorted_indices]
  sorted_lower_bounds <- data$sim_ci_lb[sorted_indices]

  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 500
  if (downsample == 0) {
    downsample = 1
  }
  sorted_d <- sorted_d[seq(1, length(sorted_d), by = downsample)]
  sorted_upper_bounds <- sorted_upper_bounds[seq(1, length(sorted_upper_bounds), by = downsample)]
  sorted_lower_bounds <- sorted_lower_bounds[seq(1, length(sorted_lower_bounds), by = downsample)]

  
  # for coloring of confidence intervals:
  below_zero <- sorted_upper_bounds < 0
  below_cross_idx <- which(diff(below_zero) == -1) # the last TRUE before switch
  
  above_zero <- sorted_lower_bounds > 0
  above_cross_idx <- (which(diff(above_zero) == 1)) + 1 # the last FALSE before switch to true

  # get n for title of plot
  # if the study is a correlation, or one sample t test, the n is n
  # if (grepl("_r_", name) | grepl("_fc_t_", name) | grepl("_act_t_", name) | grepl("_d_", name)) {
  #   n_title <- paste0("n = ", data$n)
  # } 
  
  if (study_details$orig_stat_type == "r" | study_details$orig_stat_type =="t") {
    n_title <- paste0("n = ", data$n)
  } 
  
  # if the study is a two-way t-test, then we need n1 and n2, but we'll make the n variable include both in a string
  if (study_details$orig_stat_type == "t2") {
    n_title <- paste0("n1 = ", data$n1, ", n2 = ", data$n2)
  }

  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  

 
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
 
  # plot a line for d
  par(mar=c(1, 4, 4, 2))
  plot(sorted_d, type = "l", ylim = c(min(sorted_lower_bounds, na.rm = TRUE), max(sorted_upper_bounds, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  legend("topleft", inset = c(-0.1, -0.27),
       legend = c(
         bquote(bold("Dataset:")), 
         paste(study_details$dataset, "  "),
         bquote(bold("Map Type:")), 
         paste(study_details$map_type, "  "),
         bquote(bold("Test type:")), 
         paste(study_details$orig_stat_type, "  "),
         bquote(bold("Var1:")), 
         paste(study_details$var1, "  "),
         bquote(bold("Var2:")), 
         paste(study_details$var2, "  "),
         bquote(bold("Sample Size:")),
         paste(n_title)
       ), 
       bty = "n", ncol = 6, cex = 0.7, text.width = c(70, 80, 80, 90, 110, 70), x.intersp = 0, xpd = TRUE)





  # plot and shade the cofidence intervals:
  # green for intervals that are entirely below zero
  polygon(c(1:below_cross_idx, rev(1:below_cross_idx)), 
          c(sorted_upper_bounds[1:below_cross_idx], rev(sorted_lower_bounds[1:below_cross_idx])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  # red for intervals that include zero
  polygon(c(below_cross_idx:above_cross_idx, rev(below_cross_idx:above_cross_idx)), 
          c(sorted_upper_bounds[below_cross_idx:above_cross_idx], rev(sorted_lower_bounds[below_cross_idx:above_cross_idx])), 
          col = rgb(237/255, 185/255, 185/255, alpha = 0.5), border = NA)
  
  # green for intervals that are entirely above zero
  polygon(c(above_cross_idx:length(above_zero), rev(above_cross_idx:length(above_zero))), 
          c(sorted_upper_bounds[above_cross_idx:length(above_zero)], rev(sorted_lower_bounds[above_cross_idx:length(above_zero)])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
}



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
      
      selectInput("task",
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
      # selectInput("behaviour",
      # 			  label = "Behavioural correlation",
      # 			  choices = c( "All" = "*","Age" = "\\.age", "IQ" = "\\.iq", "Fluid Intelligence" = "\\.gf", "Peabody Picture Vocab Test" = "\\.ppvt", "Expressive Vocab Test" = "\\.evt", "Stop Signal Task" = "\\.SST", "Letter N-Back Accuracy" = "\\.lnbxacc", "Letter N-Back Response Time" = "\\.lnbxrt", "Penn Face Memory Test Accuracy" = "\\.pfmtxacc", "Penn Face Memory Test Response Time" = "\\.pfmtxrt", "Penn Matrix Reasoning Test Correct Responses" = "\\.pmatxrc", "Penn Verbal Reasoning Test Accuracy" = "\\.pvrtxacc", "Penn Verbal Reasoning Test Response Time" = "\\.pvrtxrt", "Penn Word Memory Test Accuracy" = "\\.pwmtxacc", "Penn Word Memory Test Response Time" = "\\.pwmtxrt", "Wide Range Assessment Test" = "\\.wrat"),
      #         multiple = TRUE), 

      selectInput("spatial_scale",
              label = "Spatial scale",
              choices = c("Univariate", "Network-level", "whole-brain")),
              
      selectInput("group_by", 
                  label = "What do you want to group by?",
                  choices = c("None", "Statistic", "Phenotype Category")), 
      
      downloadButton("downloadData", "Download Data")
    
      ),

      column(5, align = "centre", # probability density plots
      
        wellPanel(style = "background-color: #ffffff;", withSpinner(uiOutput("histograms"), type = 1))),
      
      column(4, align = "center", # effect size matrices)
        wellPanel(style = "background-color: #ffffff;", h3("Effect size matrices"),
        withSpinner(plotOutput("maps", width = "100%", height = "800px"), type = 1)),
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
      

 

#       h2("Effect sizes of all edges/voxels"),
#       withSpinner(uiOutput("histograms"), type = 1)
#       )
#       ),

# hr() ,
#     fluidRow( # second row: plots of activation maps for activation studies 
#         column(4, # inputs for activation maps
#             sidebarPanel(
#             numericInput("xCoord", "X Coordinate", 30),
#             numericInput("yCoord", "Y Coordinate", 30),
#             numericInput("zCoord", "Z Coordinate", 30))
#         ),

#         column(8, align = "center", 
#           h2("Activation effect size map"),
#           withSpinner(plotOutput("brain"), type = 1)
#         )),
# hr(),
#     fluidRow( # third row: plots FC effect matrices for FC studies
#         column(12, align = "center", 
#           h2("FC effect size matrix"),
#           withSpinner(plotOutput("maps", height = "500px", width = "1000px"), type = 1)
#           )
#     )
# )

    

########################################################################################
# Server logic ----
server <- function(input, output, session) {
    
    
    # set reactive parameters
    v <- reactiveValues()
    observe({
        # v$matching_studies <- (study$dataset == input$dataset & 
        #                        study$map_type == input$measurement_type & 
        #                        study$var1 == input$task & 
        #                        study$orig_stat_type == input$test_type & 
        #                        study$var2 == input$behaviour)
        # filter d_clean by the input parameters selected
        # v$d_clean <- d_clean[(study$dataset == input$dataset & 
        #                        study$map_type == input$measurement_type & 
        #                        study$var1 == input$task & 
        #                        study$orig_stat_type == input$test_type & 
        #                        # only filter by behaviour if the test type is a behavioural correlation
        #                         ifelse(input$test_type == "\\.r\\.", study$var2 == input$behaviour, TRUE))]


        v$d_clean <- d_clean[grepl(input$dataset, study$dataset) & 
                             grepl(input$measurement_type, study$map_type) & 
                             (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1)) & 
                            #  grepl(input$test_type, study$orig_stat_type) & 
                            (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                             grepl(paste(input$behaviour, collapse="|"), study$var2)]
      

      # Download button
      output$downloadData <- downloadHandler(
      filename = function() {
        paste("EffeX_data", ".RData", sep="")
      },
      content = function(file) {
        saveRDS(v$d_clean, file)
      }
    )


        v$d_clean_act <- v$d_clean[grepl("_act_", names(v$d_clean))]
        v$d_clean_fc <- v$d_clean[grepl("_fc_", names(v$d_clean))]

        # also filter study by the same parameters
        v$study <- study[grepl(input$dataset, study$dataset) & 
                         grepl(input$measurement_type, study$map_type) & 
                         (length(input$task) == 0 | grepl(paste(input$task, collapse="|"), study$var1)) & 
                         (input$test_type == "*" | (study$orig_stat_type == input$test_type)) &
                         grepl(paste(input$behaviour, collapse="|"), study$var2),]

        # v$d_clean_fc <- v$d_clean[grepl("FC", study$map_type)]

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

    #   else if (input$group_by == "Phenotype Category") { # group by phenotype category
    #     v$grouping <- "code"
    #     #v$this_fill <- "code"
    #     v$axis_label <- "Phenotype Category"
    #     v$this_density_scale <- 3
    #     v$this_xlim <- c(-0.5, 0.5)
    #   }
    ## TODO: add this again after ^^



# TODO: add this section back after to constrain parameters!!:
    #   observeEvent(ignoreInit = TRUE, list(input$dataset, input$measurement_type, input$task, input$test_type), {
    #     selected_studies <- studies$name %in% unique(v$d_clean$study)
    #     if (!is.null(input$test_type) && input$test_type == "\\.r\\.") {
    #       updateSelectInput(session, "behaviour", choices = c("All" = "*", unique(studies[selected_studies, ]$var2)))
    #     }
    #   })

    #   ## TODO: with these constraining functions, need to make the resulting choices more human-readable

    #   observeEvent(ignoreInit = TRUE, list(input$measurement_type, input$task, input$test_type), {
    #     selected_studies <- studies$name %in% unique(v$d_clean$study)
    #     updateSelectInput(session, "dataset", selected = input$dataset, choices = c("All" = "*", unique(studies[selected_studies, ]$dataset)))
    #   }) # this works, but doesn't look like it's working perfectly because the data used to make studies is not the same as the data used to make d_clean for now

    #   observeEvent(ignoreInit = TRUE, list(input$dataset, input$task, input$test_type), {
    #     selected_studies <- studies$name %in% unique(v$d_clean$study)
    #     updateSelectInput(session, "measurement_type", selected = input$measurement_type, choices = c("All" = "*", unique(studies[selected_studies, ]$map_type)))
    #   })

    #   observeEvent(ignoreInit = TRUE, list(input$dataset, input$task, input$measurement_type), {
    #     selected_studies <- studies$name %in% unique(v$d_clean$study)
    #     updateSelectInput(session, "test_type", selected = input$test_type, choices = c("All" = "*", unique(studies[selected_studies, ]$stat_type)))
    #   })

    #   observeEvent(ignoreInit = TRUE, list(input$dataset, input$test_type, input$measurement_type), {
    #     selected_studies <- studies$name %in% unique(v$d_clean$study)
    #     updateSelectInput(session, "task", selected = input$task, choices = c("All" = "*", unique(studies[selected_studies, ]$var1))) ## TODO: be more specific about var1 and var2
    #   })

    })
      
    
    # insert the right number of plot output objects into the web page
    # if d_clean is not empty, then plot. Check if d_clean is empty:

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

    
    output$maps <- renderPlot({
      validate(
      need((0 < length(v$d_clean_fc)), "We do not have FC data for the selected parameters"))
      #need((input$measurement_type == "fc"), "To see an effect size matrix, please select functional connectivity as the measurement type"))
      #need((length(unique(v$d_clean)) < 2), "Can only plot one study, please select more specific parameters"))
      # for each study in d_clean, get t as the whole matrix
      
      # create a matrix to store the data for if there is more than one study
      t_total_268 <- matrix(0, nrow = 268, ncol = 268)
      t_total_55 <-  matrix(0, nrow = 55, ncol = 55)

      n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
      n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation

      for (i in 1:length(v$d_clean_fc)) {
        t <- v$d_clean_fc[[i]]$d
        if (length(t) == 71824) {
          # if the data includes the whole matrix, not just a triangle:
          n_nodes <- sqrt(length(t))
          trilmask <- matrix(TRUE, nrow = n_nodes, ncol = n_nodes)
          t2 <- trilmask
          t2[trilmask] <- t

          # set the lower triangle to zero 
          t2[lower.tri(t2, diag = TRUE)] <- 0
          
          # add t2 to the total matrix as the sum of t_total and t2
          t_total_268 <- t_total_268 + t2
          n_268_studies <- n_268_studies + 1
        }
        
        else if (length(t) == 35778) {
          n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
          trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
          t2 <- trilmask
          t2[trilmask] <- t

          # add the data to the total matrix
          t_total_268 <- t_total_268 + t2

          n_268_studies <- n_268_studies + 1
        }

        else if (length(t) == 1485) {
          n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
          trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
          t2 <- trilmask
          t2[trilmask] <- t

          # add the data to the total matrix
          t_total_55 <- t_total_55 + t2

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

      
      par(mfrow = c(2, 1), mar = c(4,2,2,1), mgp = c(3, 1, 0))
      image.plot(t_avg_268[,nrow(t_avg_268):1],
            #xlab = "268 Nodes", cex.lab = 1.5,
            #ylab = sprintf("%s Nodes", n_nodes),
            axes = FALSE, col = hcl.colors(100, palette = "viridis"))
      axis(1, at = seq(0, 1, by = 1), labels = seq(1, n_nodes, by = n_nodes-1), cex.axis = 1.3, lwd = 0)  # Customize X-axis
      axis(2, at = seq(0, 1, by = 1), labels = seq(n_nodes, 1, by = -n_nodes+1), cex.axis = 1.3, lwd = 0)
      title(xlab="268 Nodes", line=2, cex.lab=1.5)

      image.plot(t_avg_55[,nrow(t_avg_55):1],
            #xlab = "55 Nodes", cex.lab = 1.5,
            #ylab = sprintf("%s Nodes", n_nodes),
              axes = FALSE, col = hcl.colors(100, palette = "viridis"))
        axis(1, at = seq(0, 1, by = 1), labels = seq(1, dim(t_avg_55)[1], by = dim(t_avg_55)[1]-1), cex.axis = 1.3, lwd = 0)  # Customize X-axis
        axis(2, at = seq(0, 1, by = 1), labels = seq(dim(t_avg_55)[1], 1, by = -dim(t_avg_55)[1]+1), cex.axis = 1.3, lwd = 0)
        title(xlab="55 Nodes", line=2, cex.lab=1.5)
    })

    ###### an attempt at marking the networks on the axes for 268 parcellation: TODO - check this, and find network names!
      # axis(1, at = c((29/2)/268, 46/268, 73/268, 128/268, 198/268, 232/268, 246/268, 259/268), labels = unique(sort(shen[,2])), cex.axis = 0.5, lwd = 0)  # Customize X-axis
      # axis(1, at = c(0, (29)/268, (29+34)/268, (29+34+20)/268, (29+34+20+90)/268, (29+34+20+90+50)/268, (29+34+20+90+50+18)/268, (29+34+20+90+50+18+9)/268, (29+34+20+90+50+18+9+18)/268), labels = FALSE, tick = TRUE, cex.axis = 0.5, lwd = 1)  # Customize X-axis

      # axis(2, at = c(1-(29/2)/268, 1-46/268, 1-73/268, 1-128/268, 1-198/268, 1-232/268, 1-246/268, 1-259/268), labels = unique(sort(shen[,2])), cex.axis = 0.5, lwd = 0)
      # axis(2, at = c(1-0, 1-(29)/268, 1-(29+34)/268, 1-(29+34+20)/268, 1-(29+34+20+90)/268, 1-(29+34+20+90+50)/268, 1-(29+34+20+90+50+18)/268, 1-(29+34+20+90+50+18+9)/268, 1-(29+34+20+90+50+18+9+18)/268), labels = FALSE, tick = TRUE, cex.axis = 0.5, lwd = 1)  # Customize X-axis

    

    # try plotting brain images:
    ## TODO ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
    output$brain <- renderPlot({
        # load template brain image: ** TODO WILL NEED TO CHANGE **
    template <- readnii('data/anatomical.nii')
      validate(
      #need(length(input$task) < 2, "Please only select one task.")
      need(length(v$d_clean_act) == 1, "Please select exactly one task to visualize the activation map."),
      need(length(v$d_clean_act) > 0, paste0(c("We do not have activation data for the selected parameters. The maps we have available are:", effect_maps_available)))
      #need(input$measurement_type == "act", "To see an activation map, please select task-based activation as the measurement type")
      #need(dim(v$d_clean)[1] > 0, "We do not have data for the selected parameters"),
      #need(input$test_type == "\\.t\\.", "We currently only have task-based activation maps for one-sample task-rest contrasts")
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
