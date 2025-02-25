####################################################################
# BrainEffeX Server
# Loading data, filtering, and plotting
####################################################################

library(shinyscreenshot)
library(gridExtra)
library(ggplot2)
library(reshape)
library(fields)
library(oro.nifti)
library(neurobase)
library(shinyjs)
library(bslib)
library(BrainEffeX.utils)

source("helpers.R")

server <- function(input, output, session) {
  
  ###### Load data ######
  
  # study is a data frame that contains information about each study, including:
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
  # ref 
  
  data_list <- load_data()
  data <- data_list$data # list of effect maps etc
  study <- data_list$study # table of study information
  brain_masks <- data_list$brain_masks # list of brain masks
  template <- data_list$template
  anatomical <- data_list$anatomical
  phen_keys <- data_list$phen_keys

  rm(data_list)
  
  
  ###### Extra setup ######
  
  effect_maps_available <- study[study$map_type == "act", "name"]
  
  # TODO: temporary fix for not including test_component_2 in activation studies
  study$test_component_2[study$map_type == "act"] <- "rest"
  
  # options for spinner
  options(spinner.color = "#9ecadb",
          spinner.color.background = "#ffffff", spinner.size = 1)
  
  
  # Update UI selectInput choices dynamically (moved out of "ui" so only pass data directly to server)
  updateSelectInput(session, "dataset", choices = c("All" = "*", unique(study$dataset)))
  updateSelectInput(session, "map_type", choices = c("All" = "*", unique(study$map_type)))
  updateSelectizeInput(session, "task", choices = c("All" = "*", unique(study$test_component_1)), server = TRUE)
  updateSelectInput(session, "test_type", choices = c("All" = "*", unique(study$orig_stat_type)))
  updateSelectInput(session, "correlation", choices = c("All" = "*", unique(study[study$orig_stat_type == "r", "test_component_2"])))
  
  # Dynamic panel output
  output$dynamicPanel <- createDynamicPanel(input, study)
  createModalNavigationObservers(input, session)
  
  # # Observer to handle default correlation display
  observeEvent(input$correlation, {
    if (is.null(input$correlation) || length(input$correlation) == 0) {
      # Update the selectizeInput to show all tasks if none are selected
      updateSelectizeInput(session, "correlation", choices = unique(v$beh_choices))
    }
  }, ignoreInit = TRUE)
  
  # set reactive parameters for filtering based on options chosen by user
  v <- reactiveValues()
  
  
  # plotting info
  v$plot_info__idx <- list()
  v$plot_info__grouping_var <- list() # each row = grouping variable (same value repeated for each plot)
  v$plot_info__group_level <- list() # each row = level within grouping variable
  v$plot_info__ref <- list() # each row = ref(s) used for a study or grouping variable
  
  # filter data and study by user input selections
  observeEvent(list(input$dataset, input$estimate, input$map_type, input$task, input$test_type, input$correlation, input$motion, input$pooling), priority = 1,{
    # create an index of the studies that fit each input selection, as well as total
    # index of studies that fill all input selections simultaneously (v$filter_index$total)
    v$filter_idx <- get_filter_index(data, input, study)
  
    # filter data and study by matching indices
    v$data <- data[v$filter_idx$total]
    v$study <- study[v$filter_idx$total,]
    
    v$combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
    
    # remove data and study that are NaN
    v$nan_filter <- sapply(v$data, function(study) any(is.nan(study[[v$combo_name]][[input$estimate]])))
    v$data <- v$data[!v$nan_filter]
    v$study <- v$study[!v$nan_filter,]
    
    
    # if (input$plot_combination_style == 'meta') {
    #   if (!("data_group" %in% names(v)) || (previous_meta_grouping_var != input$group_by)) {
    #     v <- meta_analysis(v, v$brain_masks, v$combo_name, grouping_var = input$group_by)
    #     previous_meta_grouping_var <- input$group_by
    #   }
    # }
    
    v$plot_info__idx <- list()
    v$plot_info__grouping_var <- list()
    v$plot_info__group_level <- list()
    v$plot_info__ref <- list()
    
      
    for (i in 1:length(v$data)) {
      v$plot_info__idx[[names(v$data)[[i]]]] <- i
      v$plot_info__grouping_var[[names(v$data)[[i]]]] <- "none"  # overwrite any other grouping var if doing single plots single
      v$plot_info__group_level[[names(v$data)[[i]]]] <- NA
      v$plot_info__ref[[names(v$data)[[i]]]] <- v$study$ref[i]
    }
    
    # clear v$plot_info before re-making
    v$plot_info <- list()
    v$plot_info <- data.frame(
      idx = I(v$plot_info__idx),
      grouping_var = unlist(v$plot_info__grouping_var),
      group_level = unlist(v$plot_info__group_level),
      ref = I(v$plot_info__ref),
      row.names = names(v$plot_info__idx),
      stringsAsFactors = FALSE
    )
    print(v$plot_info)
  })
  
  # update the task selection input with available tasks but do not pre-select any
  observeEvent(input$dataset, {
    v$task_choices <- c("All" = "*", unique((v$study[["test_component_1"]])))
    updateSelectizeInput(session, "task", choices = v$task_choices) # Ensure no tasks are selected by default
  })
  
  # update the correlation selection input with available correlations but do not pre-select any
  observeEvent(list(input$dataset, input$map_type, input$test_type), {
    v$beh_choices <- v$study[, "test_component_2"]
    updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
  })
  
  # reset input$behavior to NULL if input$test_type is not "r"
  observeEvent(input$test_type, {
    if (input$test_type != "r") {
      updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
    }
  })
  
  # update the test type selection input with available test types
  observeEvent(input$dataset, {
    v$test_choices <- study[(grepl(input$dataset, study$dataset)),"orig_stat_type"]
    updateSelectInput(session, "test_type", selected = unique(study[["test_type"]]))
  })
  
  # update correlation selections to only be the available constrained selections... 
  observeEvent(ignoreInit = TRUE, input$map_type, priority = 2, {
    # get filter index for matching studies
    v$filter_idx <- get_filter_index(data, input, study)
    
    # filter by matching datasets and map type
    v$task_choices <- study[(v$filter_idx$dataset & v$filter_idx$map),"test_component_1"]
    
    v$beh_choices <- study[(v$filter_idx$dataset & v$filter_idx$map & v$filter_idx$test_component_1),"test_component_2"]
    
    if (input$test_type == "r") {
      updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
    }
    #print("measurement type changed")
    
    # Update the beh selection input with available behs but do not pre-select any
    updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
    updateSelectizeInput(session, server = TRUE, "task", selected = "*", choices = c("All" = "*", unique(v$task_choices)))
  }) 
  
  observe({
    v$n_act_studies <- length(v$data[grepl("_act_", names(v$data))])
    v$data_fc <- v$data[grepl("_fc_", names(v$data))]
    v$study_fc <- v$study[grepl("_fc_", v$study$name), ]
  })
  
  
  ###### Download & Screenshot buttons ######
  exportDownloadData(output, v)
  exportDownloadBrain(output, v, input, anatomical)
  exportDownloadPlots(output, v, input)
  exportDownloadMatrices(output, v, input)
  observeEvent(input$screenshot, {
    screenshot()
  })
  
  
  toListen <- reactive({
    list(input$dataset, input$map_type, input$task, input$test_type, input$pooling, input$estimate, input$motion)
  })
  
  observeEvent(toListen(), {
    v$combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
    if (!is.null(input$task) && length(input$task) == 1 && input$task != "*" && (any(grepl(input$task[1], effect_maps_available, ignore.case = TRUE))) && input$pooling == "none") {
      # v$study_name as the name column from study that matches the task input and has map type activation
      v$study_name <- v$study[grepl(input$task, v$study$name, ignore.case = TRUE) & grepl("act", v$study$map_type), "name"]
      v$nifti <- create_nifti(template, data, v$study_name, v$combo_name, brain_masks, estimate = input$estimate)
    }
  }, ignoreNULL = TRUE)
  
  
  ##### Group_by / Meta-analysis ######
  
  observe({
    # v$multi_ext <- ifelse(input$dimensionality == "none", "none", paste0("multi.", input$test_type))
    v$combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
    v$mv_combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.multi')
  })
  
  # observeEvent(toListen(), {
  #   if (input$group_by != "none") {
  #     v <- group_data(v, brain_masks, v$combo_name, group_by = input$group_by)
  #   }
  # })
  
  
  ###### Plot Sim CI ######
  
  # insert the right number of plot output objects into the web page
  # if data is not empty, then plot. Check if data is empty:
  observe({
    output$histograms <- renderUI({
      if (length(v$plot_info$idx) == 0) {
        # if there is no data, display a message
        tagList(
          h3("No data available for the selected parameters.")
        )
      } else if (length(v$plot_info$idx) > 0) {
          v$plot_output_list <- lapply(1:length(v$plot_info$idx), function(i) {
            
            plotname <- paste0("plot", i)
            #print(plotname)
            plotOutput(plotname, height = "200px", width = "100%")
          })
          
          # convert the list to a tagList, this is necessary for the list of items to display properly
          do.call(tagList, v$plot_output_list)
        }
      })
    })
  
  # call renderPlot for each one
  # plots are only actually generated when they are visible on the web page
  observe({
    
    #if (input$group_by == "none") {
      # check if v$data is empty
      if (length(v$plot_info$idx) > 0) {
        # print(paste0("num plots: ", v$num_plots))
        print(length(v$plot_info$idx))
        for (i in 1:length(v$plot_info$idx)) {
          print(paste0('plotting study ', rownames(v$plot_info)[i]))
          # create a local variable to hold the value of i
          #print(i)
          local({
            my_i <- i
            plotname <- paste0("plot", my_i, sep="")
            
            this_study_or_group <- rownames(v$plot_info)[my_i]
            this_plot_info <- v$plot_info[this_study_or_group,]
            
            pd_list <- list()
            n_studies_in_pd_list <- 1
            
            for (j in v$plot_info$idx[[i]]) {
                name <- names(v$data[j])
                data <- v$data[[j]]
                study_details <- v$study[j, ]
            }
            
            if (v$combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
              
              # prep
              pd <- prep_data_for_plot(data = data, study_details = study_details, combo_name = v$combo_name, mv_combo_name = v$mv_combo_name, estimate = input$estimate, plot_info = this_plot_info)
              
              pd_list[[n_studies_in_pd_list]] <- pd
              n_studies_in_pd_list <- n_studies_in_pd_list + 1
              
            }
            
            if (length(pd_list) > 0) { # plot only if pd_list isn't empty
              
              # filename
              if (input$pooling == 'net') {
                net_str=" - net"
              } else {
                net_str=""
              }
              out_dir <- paste0('output/', pd_list$extra_study_details[[this_study_or_group]], ' - ', '/', 'simci', net_str)
              fn <- paste0(this_study_or_group, '_', n_studies_in_pd_list, '.png')
              
              # plot
              output[[plotname]] <- renderPlot({
                create_plots(pd_list, plot_type = 'simci', add_description = TRUE)
              })
              
            }
          })
        }
      }
    } 
  )
  
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
      need((0 < length(v$data_fc)), "We do not have FC data for the selected parameters"))
    
    # create a vector to store the data for if there is more than one study
    t_total_268 <- rep(0, 35778) 
    t_total_268_pooled <- rep(0, 55)
    t_total_55 <-  rep(0 , 1485)
    t_total_55_pooled <- rep(0, 55)
    
    n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
    n_268_studies_pooled <- 0
    n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation
    n_55_studies_pooled <- 0
    
    for (i in 1:length(v$data_fc)) {
      t <- v$data_fc[[i]][[v$combo_name]][[input$estimate]]
      
      if (!is.vector(t)) {
        t <- as.vector(t)
      }
      
      study_idx <- which(toupper(v$study_fc$name) == toupper(names(v$data_fc)[i]))
      print(study_idx)
      if (v$study_fc$ref[study_idx] == "shen_268"){ 
        
        if (input$pooling == "net") {
          t_total_268_pooled <- t_total_268_pooled + t
          n_268_studies_pooled <- n_268_studies_pooled + 1
        }
        else {
          #print(c(dim(t), v$study_fc$name[study_idx]))
          
          t_total_268 <- t_total_268 + t
          n_268_studies <- n_268_studies + 1
        }
      }
      
      else if (v$study_fc$ref[study_idx] == "ukb_55") {
        
        if (input$pooling == "net") {
          t_total_55_pooled <- t_total_55_pooled + t
          n_55_studies_pooled <- n_55_studies_pooled + 1
        }
        else {
          # add the data to the total vector
          t_total_55 <- t_total_55 + t
          
          n_55_studies <- n_55_studies + 1
        }
      }
    }
    
    # if data_fc is longer than 1, find the average of the matrices
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
    
    if (n_55_studies_pooled >= 1) {
      t_avg_55_pooled <- t_total_55_pooled / n_55_studies_pooled
    }
    
    # only plot the 268 plot if n_268_studies > 0
    if (n_268_studies > 0) {
      plot_268 <- plot_full_mat(t_avg_268, mapping_path = "data/parcellations/map268_subnetwork.csv", ukb = FALSE, save = FALSE, plot_name = 'Shen_matrix.png')
    }
    
    # only plot the 268 pooled plot if n_268_studies_pooled > 0
    if (n_268_studies_pooled > 0) {
      plot_268_pooled <- plot_full_mat(t_avg_268_pooled, rearrange = FALSE, pooled = TRUE, ukb = FALSE, mapping_path = "data/parcellations/map268_subnetwork.csv", save = FALSE, plot_name = 'Shen_matrix_pooled.png')
    }
    
    # only plot the 55 plot if n_55_studies > 0
    if (n_55_studies > 0) {
      plot_55 <- plot_full_mat(t_avg_55, rearrange = TRUE, mapping_path = "data/parcellations/map55_ukb.csv", save = FALSE, ukb = TRUE, plot_name = 'UKB_matrix.png')
    }
    
    # only plot the 55 pooled plot if n_55_studies_pooled > 0
    if (n_55_studies_pooled > 0) {
      plot_55_pooled <- plot_full_mat(t_avg_55_pooled, rearrange = FALSE, pooled = TRUE, ukb = TRUE, mapping_path = "data/parcellations/map55_ukb.csv", save = FALSE, plot_name = 'UKB_matrix_pooled.png')
    }
    
    # if not pooled, show the 268 and 55 matrices
    if (input$pooling == "none") {
      # if there is only shen or only ukb, only plot one plot
      if ((n_268_studies == 0) & (n_55_studies > 0)) {
        grid.arrange(plot_55, ncol = 1)
      }
      else if ((n_55_studies == 0) & (n_268_studies > 0)) {
        grid.arrange(plot_268, ncol = 1)
      }
      else if ((n_55_studies > 0) & (n_268_studies > 0)) {
        grid.arrange(plot_268, plot_55, ncol = 1)
      }
    }
    
    # if pooled, show the pooled 268 and 55 matrices
    if (input$pooling == "net") {
      # if there is only shen or only ukb, only plot one plot
      if ((n_268_studies_pooled == 0) & (n_55_studies_pooled > 0)) {
        grid.arrange(plot_55_pooled, ncol = 1)
      }
      else if ((n_55_studies_pooled == 0) & (n_268_studies_pooled > 0)) {
        grid.arrange(plot_268_pooled, ncol = 1)
      }
      else if ((n_55_studies_pooled > 0) & (n_268_studies_pooled > 0)) {
        grid.arrange(plot_268_pooled, plot_55_pooled, ncol = 1)
      }
    }}
    , height = reactive(v$h))#, width = reactive(v$w))
  
  
  
  ###### Plot brain images ######
  
  ## TODO: ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
  
  observeEvent(input$estimate, {
    output$brain <- renderPlot({
      par(mar = c(0, 0, 0, 5))
      validate(
        need(v$n_act_studies == 1, "Please select exactly one task to visualize the activation map."),
        need(v$n_act_studies > 0, paste0(c("We do not have activation data for the selected parameters."))),
        need(dim(v$nifti != NA), "")
      )
      
      plot_brain(v$nifti, anatomical, x = input$xCoord, y = input$yCoord, z = input$zCoord)
    })
  })
  
  output$keys <- DT::renderDataTable(phen_keys, 
                                     options = list(rownames = FALSE, 
                                                    paging = FALSE, 
                                                    scroller = TRUE,
                                                    scrollY = "400px",
                                                    scrollX = FALSE,
                                                    autoWidth = TRUE
                                     ),
                                     rownames = FALSE)
  
  observeEvent(input$meta_analysis, {
    output$meta_analysis <- renderPlot({
      par(mar = c(0, 0, 0, 5))
      
    })
  })
  
}