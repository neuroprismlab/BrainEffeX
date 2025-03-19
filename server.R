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
  
  # set reactive parameters for filtering based on options chosen by user
  v <- reactiveValues()
  
  data_list <- load_combo_data(combo = "pooling.none.motion.none") # load just the initial combo's data
  v$data_init <- data_list$data # list of effect maps etc
  v$study_init <- data_list$study # table of study information
  v$brain_masks_init <- data_list$brain_masks # list of brain masks
  template <- data_list$template
  anatomical <- data_list$anatomical
  phen_keys <- data_list$phen_keys
  print("loaded initial data")
  rm(data_list)
  
  
  
  ###### Extra setup ######
  observe({
    effect_maps_available <- v$study_init[v$study_init$map_type == "act", "name"]
    
    # TODO: temporary fix for not including test_component_2 in activation studies
    v$study_init$test_component_2[v$study_init$map_type == "act"] <- "rest"
  })
  
  # options for spinner
  options(spinner.color = "#9ecadb",
          spinner.color.background = "#ffffff", spinner.size = 1)
  
  observe({
  # Update UI selectInput choices dynamically (moved out of "ui" so only pass data directly to server)
  updateSelectInput(session, "dataset", choices = c("All" = "*", unique(v$study_init$dataset)))
  updateSelectInput(session, "map_type", choices = c("All" = "*", unique(v$study_init$map_type)))
  print(paste0('task choices initially ', unique(v$study_init$test_component_1)))
  updateSelectInput(session, "task", choices = c("All" = "*", unique(v$study_init$test_component_1)))
  updateSelectInput(session, "test_type", choices = c("All" = "*", unique(v$study_init$orig_stat_type)))
  updateSelectInput(session, "correlation", choices = c("All" = "*", unique(v$study_init[v$study_init$orig_stat_type == "r", "test_component_2"])))
  })
  observe(print(paste0('initial value of task: ', input$task)))
  # Dynamic panel output
  #output$dynamicPanel <- createDynamicPanel(input, v$study_init)
  observeEvent(c(input$apply_filters_btn, input$reset_btn), {
    output$dynamicPanel <- renderUI({
      createDynamicPanel(input, v$study_init)
    })
  })
  createModalNavigationObservers(input, session)
  
  # # Observer to handle default correlation display
  observeEvent(input$correlation, {
    if (is.null(input$correlation) || length(input$correlation) == 0) {
      # Update the selectizeInput to show all tasks if none are selected
      updateSelectizeInput(session, "correlation", choices = unique(v$beh_choices))
    }
  }, ignoreInit = TRUE)
  
  observe({
    click("apply_filters_btn")
  })
  
 
  # TODO: maybe try assigning v$data here?
  
  # create combo names depending on active tab
  observeEvent(list(input$tab, input$motion, input$pooling, input$m_pooling, input$m_motion, input$meta_analysis), {
    if (input$tab == "Explorer") {
      v$combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
      v$mv_combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.multi')
      print(paste0('current combo name: ', v$combo_name))
    } else if (input$tab == "Meta-Analysis") {
      v$combo_name_m <- paste0('pooling.', input$m_pooling, '.motion.', input$m_motion, '.mv.none')
      v$mv_combo_name_m <- paste0('pooling.', input$m_pooling, '.motion.', input$m_motion, '.mv.multi')
      print(paste0('v$combo_name_m set to: ', v$combo_name_m))
    }
  }, priority = 2)
  
  observeEvent(list(input$tab, input$meta_analysis, input$motion, input$pooling), ignoreInit = TRUE, {
    print(paste0("loading data for tab ", input$tab))
    if (input$tab == "Meta-Analysis") {
      print("loading meta-analysis data")
      load(paste0("data/meta_analysis/meta_", input$meta_analysis, ".RData")) #meta_category.RData") #TMP, loads meta_category variable, need to rename in data files to something generic like data
      #isolate({
        v$data_m_init <- meta$data # list of effect maps etc
        v$study_m_init <- unique(meta$study) # table of study information, TMP: checking if it was an accident to have duplicates in study
        print("head of study info right after loading meta data")
        print(head(v$study_m_init))
        v$brain_masks_m_init <- meta$brain_masks # list of brain masks
      #})
      print("done loading meta-analysis data")
      rm(meta)
      
      v$combo_name_m <- paste0('pooling.', input$m_pooling, '.motion.', input$m_motion, '.mv.none')
      v$mv_combo_name_m <- paste0('pooling.', input$m_pooling, '.motion.', input$m_motion, '.mv.multi')
      print(paste0('v$combo_name_m set to: ', v$combo_name_m))
    } else if (input$tab == "Explorer") {
      v$combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
      v$mv_combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.multi')
      print(paste0('current combo name: ', v$combo_name))
      
      print(paste0("loading data for tab ", input$tab))
      data_list <- load_combo_data(combo = paste0("pooling.", input$pooling, ".motion.", input$motion)) # load just the initial combo's data
      v$data_init <- data_list$data # list of effect maps etc
      v$study_init <- data_list$study # table of study information
      v$brain_masks_init <- data_list$brain_masks # list of brain masks
      print("done loading explorer data")
      rm(data_list)
    }
  }, priority = 3)
  
  # observeEvent(input$apply_filters_btn, {
  #   print('changed map type. resetting task selection to All.')
  #   updateSelectInput(session, "task", selected = "*")
  #   #print(input$task)
  #   
  #   if (input$tab == "Explorer") {
  #     if (input$task == "*") {
  #       print("Task is already *, filtering now")
  #       isolate({
  #         
  #         if (exists("v$data") & (length(v$data) == 0)) {
  #           print("Filtered all data out. No remaining studies.")
  #           return()
  #         }
  #         v$nan_filter <- sapply(v$data_init, function(study) any(is.nan(study[[v$combo_name]][[input$estimate]])))
  #         v$data <- v$data_init[!v$nan_filter]
  #         v$study <- v$study_init[!v$nan_filter,]
  #         
  #         if (exists("v$data") & (length(v$data) == 0)) {
  #           print("Filtered all data out. No remaining studies.")
  #           return()
  #         }
  #         
  #         v$filter_idx <- get_filter_index(v$data, input, v$study)
  #         v$data <- v$data[v$filter_idx$total]
  #         v$study <- v$study[v$filter_idx$total,]
  #         
  #         
  #         
  #         print("Filtered data immediately in map_type observer:")
  #         print(names(v$data))
  #       
  #     })
  #     }
  #   }
  # })
  # 
  # observeEvent(input$apply_filters_btn, {
  #   # print(paste0('task updated to: ', input$task))
  #   # print('Filtering data after task update')
  #   
  #   if (input$tab == "Explorer") {
  #       if (exists("v$data") & (length(v$data) == 0)) {
  #       print("Filtered all data out. No remaining studies.")
  #       return()
  #     }
  #       
  #       v$nan_filter <- sapply(v$data_init, function(study) any(is.nan(study[[v$combo_name]][[input$estimate]])))
  #       v$data <- v$data_init[!v$nan_filter]
  #       v$study <- v$study_init[!v$nan_filter,]
  #       
  #       if (exists("v$data") & (length(v$data) == 0)) {
  #         print("Filtered all data out. No remaining studies.")
  #         return()
  #       }
  #       
  #       v$filter_idx <- get_filter_index(v$data, input, v$study)
  #       v$data <- v$data[v$filter_idx$total]
  #       v$study <- v$study[v$filter_idx$total,]
  #     
  #     
  #     
  #     print("Filtered data after task update:")
  #     print(names(v$data))
  #   }
  # })
  
  # filter meta-analysis data to show just available data
  observeEvent(list(v$combo_name_m, input$m_estimate, input$meta_analysis, input$tab), {
  
    # create an index of the studies that fit each input selection
    # FIRST NEED TO CHECK IF THE COMBO EXISTS IN THE DATA, THEN FILTER FOR NAS
    if (input$tab == 'Meta-Analysis') {
      req(v$data_m_init)
      req(v$combo_name_m)
      # first remove studies that don't include the current combo
      print(paste0('filtering meta data to remove studies without this combo: ', v$combo_name_m))
      v$combo_idx_m <- sapply(v$data_m_init, function(d) v$combo_name_m %in% names(d))
      print(v$combo_idx_m)
      v$data <- v$data_m_init[v$combo_idx_m]
      v$study <- v$study_m_init[v$combo_idx_m,]
      print('v$study after filtering by combo')
      print(head(v$study))
      
      # then remove studies that don't include the selected estimate for the current combo
      print(paste0('filtering meta data to remove studies with NA for this estimate: ', input$m_estimate))
      v$nan_filter_m <- sapply(v$data, function(study) any(is.na(v$study[[v$combo_name_m]][[input$m_estimate]])))
      v$data <- v$data[!v$nan_filter_m]
      v$study <- v$study[!v$nan_filter_m,]
      print('v$study after filtering by NA')
      print(head(v$study))
    }
  }, priority = 1)
  
  # observeEvent(input$tab, priority = 1, {
  #   updateSelectInput(session, "pooling", selected = "none")
  # })
  
  # filter data and study by user input selections
  observeEvent(input$apply_filters_btn, {
    # create an index of the studies that fit each input selection, as well as total
    # index of studies that fill all input selections simultaneously (v$filter_index$total)
    isolate({
    # remove data and study that are NaN
    
    if (exists("v$data") & (length(v$data) == 0)) {
      print("Filtered all data out. No remaining studies.")
      return()
    }
    
    if (input$tab == "Explorer") {
      print('explorer: removing nans from v$data and v$study')
      v$nan_filter <- sapply(v$data_init, function(study) any(is.nan(study[[v$combo_name]][[input$estimate]])))
      v$data <- v$data_init[!v$nan_filter]
      v$study <- v$study_init[!v$nan_filter,]
      #print(which(v$nan_filter == TRUE))
  
      print("names of v$data")
      print(names(v$data))
    }
    
    if (exists("v$data") & (length(v$data) == 0)) {
      print("Filtered all data out. No remaining studies.")
      return()
    }
    
      if (input$tab == "Explorer") {
      print(paste0('about to filter, current task is: ', input$task))
      print('filtering explorer data')
      v$filter_idx <- get_filter_index(v$data, input, v$study)
      v$data <- v$data[v$filter_idx$total]
      v$study <- v$study[v$filter_idx$total,]
      }
    })
    
  })
  
  v$plot_info <- reactive({
    print(paste0('filter button: ', input$apply_filters_btn))
    if ((input$tab == "Explorer") & (length(v$data) > 0)) {
      print('Generating v$plot_info__... for explorer')
      
      plot_info_idx <- list()
      plot_info_grouping_var <- list()
      plot_info_group_level <- list()
      plot_info_ref <- list()
      
      for (i in seq_along(v$data)) {
        study_name <- names(v$data)[i]
        plot_info_idx[[study_name]] <- i
        plot_info_grouping_var[[study_name]] <- "none"
        plot_info_group_level[[study_name]] <- NA
        plot_info_ref[[study_name]] <- v$study$ref[i]
      }
      
      data.frame(
        idx = I(plot_info_idx),
        grouping_var = unlist(plot_info_grouping_var),
        group_level = unlist(plot_info_group_level),
        ref = I(plot_info_ref),
        row.names = names(plot_info_idx),
        stringsAsFactors = FALSE
      )
    } else {
      return(NULL)
    }
  })
  
  v$plot_info_m <- reactive({
    #req(input$meta_analysis)
    req(v$data)
    if (input$tab == "Meta-Analysis") {
      #print('Generating v$plot_info__... for meta-analysis')
      
      if (is.null(v$data) || length(v$data) == 0) {
        #print("Warning: v$data is empty!")
        return(NULL)
      }
      if (is.null(v$study) || nrow(v$study) == 0) {
        #print("Warning: v$study is empty!")
        return(NULL)
      }
      
      plot_info_idx_m <- list()
      plot_info_grouping_var_m <- list()
      plot_info_group_level_m <- list()
      plot_info_ref_m <- list()
      
      for (i in seq_along(v$data)) {
        study_name <- names(v$data)[i]
        plot_info_idx_m[[study_name]] <- i
        plot_info_grouping_var_m[[study_name]] <- input$meta_analysis
        plot_info_group_level_m[[study_name]] <- v$study$group_level[i]
        plot_info_ref_m[[study_name]] <- v$study$ref[i]
      }
      
      data.frame(
        idx = I(plot_info_idx_m),
        grouping_var = unlist(plot_info_grouping_var_m),
        group_level = unlist(plot_info_group_level_m),
        ref = I(plot_info_ref_m),
        row.names = names(plot_info_idx_m),
        stringsAsFactors = FALSE
      )
      #print('data frame of plot_info_m')
      
    } else {
      return(NULL)
    }
  })
  
  # update the task selection input with available tasks but do not pre-select any
  # observeEvent(input$dataset, {
  #   v$task_choices <- c("All" = "*", unique((v$study[["test_component_1"]])))
  #   updateSelectInput(session, "task", choices = v$task_choices) # Ensure no tasks are selected by default
  # })
  
  # update the correlation selection input with available correlations but do not pre-select any
  # observeEvent(list(input$dataset, input$test_type), {
  #   v$beh_choices <- v$study[, "test_component_2"]
  #   print(paste0('updating correlation options to ', v$beh_choices))
  #   updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
  # })
  
  # reset input$behavior to NULL if input$test_type is not "r"
  observeEvent(input$test_type, {
    if (input$test_type != "r") {
      updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
    }
  })
  
  # update the test type selection input with available test types
  observeEvent(input$dataset, {
    v$test_choices <- v$study_init[(grepl(input$dataset, v$study$dataset)),"orig_stat_type"]
    updateSelectInput(session, "test_type", selected = unique(v$study[["test_type"]]))
  })
  
  # update correlation selections to only be the available constrained selections... 
  observeEvent(list(input$dataset, input$test_type, input$map_type), ignoreInit = TRUE, {
    # get filter index for matching studies
    if (input$tab == "Explorer") {
      v$filter_idx <- get_filter_index(v$data_init, input, v$study_init)
      
      # filter by matching datasets and map type
      v$task_choices <- v$study_init[(v$filter_idx$dataset & v$filter_idx$map),"test_component_1"]
      
      v$beh_choices <- v$study_init[(v$filter_idx$dataset & v$filter_idx$map & v$filter_idx$test_component_1),"test_component_2"]
      
      if (input$test_type == "r") {
        updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
      }
      #print("measurement type changed")
      
      # Update the beh selection input with available behs but do not pre-select any
      updateSelectInput(session, "correlation", selected = character(0), choices = unique(v$beh_choices)) # Ensure no behs are selected by default
      updateSelectInput(session, "task", selected = "*", choices = c("All" = "*", unique(v$task_choices)))
    }}) 
  
  observe({
    if ((input$tab == "Explorer") & (length(v$data) > 0)) {
      v$n_act_studies <- length(v$data[grepl("_act_", names(v$data))])
      v$data_fc <- v$data[grepl("_fc_", names(v$data))]
      v$study_fc <- v$study[grepl("_fc_", v$study$name), ]
      print(paste0("number of activation studies: ", v$n_act_studies))
    }
  })
  
  ###### Reset filters & Download & Screenshot buttons ######
  # # Reset all SelectInputs when reset button is clicked
  observeEvent(input$reset_btn, {
    v$data <- v$data_init
    v$study <- v$study_init
    
    updateSelectInput(session, "dataset", selected = "*")
    updateSelectInput(session, "map_type", selected = "*")
    updateSelectInput(session, "task", selected = "*")
    updateSelectInput(session, "test_type", selected = "*")
    updateSelectInput(session, "correlation", selected = character(0)) # Clear selection
    
    updateSelectInput(session, "motion", selected = "none")
    updateSelectInput(session, "pooling", selected = "none")
  })
  
  ###### Download & Screenshot buttons ######
  exportDownloadData(output, v)
  exportDownloadBrain(output, v, input, anatomical)
  exportDownloadPlots(output, v, input)
  exportDownloadMatrices(output, v, input)
  observeEvent(input$screenshot, {
    screenshot()
  })
  observeEvent(input$screenshot_m, {
    screenshot()
  })
  
  
  toListen <- reactive({
    list(input$dataset, input$map_type, input$task, input$test_type, input$pooling, input$estimate, input$motion)
  })
  
  toListen_m <- reactive({
    list(input$meta_analysis, input$m_pooling, input$m_estimate, input$m_motion)
  })
  
  
  ##### Group_by / Meta-analysis ######
  
  ###### Plot Sim CI ######
  
  # insert the right number of plot output objects into the web page
  # if data is not empty, then plot. Check if data is empty:
  observe({
    if (input$tab == "Explorer") {
      print("creating placeholders for explorer simci plots")  
      output$histograms <- renderUI({
        validate(need(length(v$data) > 0, "No data available for the selected parameters."))
        
        if (length(v$plot_info()$idx) == 0) {
          tagList(h3("No data available for the selected parameters."))
        } else {
          v$plot_output_list <- lapply(1:length(v$plot_info()$idx), function(i) {
            plotname_simci <- paste0("plot", i)
            plotname_spatial <- paste0("spatial_plot", i)
            
            tagList(
              fluidRow(
                column(6, plotOutput(plotname_simci, height = "200px", width = "100%")),
                column(6, plotOutput(plotname_spatial, height = "200px", width = "100%"))
              )
            )
          })
          
          do.call(tagList, v$plot_output_list)
        }
      })
    }
  })
  
  # call renderPlot for each one
  # plots are only actually generated when they are visible on the web page
  observe({
    if (input$tab == "Explorer") {
      req(v$plot_info())
      req(input$pooling)
      req(v$combo_name)
      print(paste0('creating explorer plots with pooling : ', input$pooling))
      validate(need(length(v$data) > 0, "no data avilable to plot"))
      print("filling explorer simci and spatial plots")  
      # check if v$data is empty
      if (length(v$plot_info()$idx) > 0) {
        # print(paste0("num plots: ", v$num_plots))
        for (i in 1:length(v$plot_info()$idx)) {
          # print(paste0('plotting study ', rownames(v$plot_info)[i]))
          # create a local variable to hold the value of i
          #print(i)
          local({
            my_i <- i
            plotname_simci <- paste0("plot", my_i, sep="")
            plotname_spatial <- paste0("spatial_plot", my_i)
            
            this_study_or_group <- rownames(v$plot_info())[my_i]
            this_plot_info <- v$plot_info()[this_study_or_group,]
            
            pd_list <- list()
            n_studies_in_pd_list <- 1
            
            for (j in v$plot_info()$idx[[i]]) {
              name <- names(v$data[j])
              data <- v$data[[j]]
              study_details <- v$study[j, ]
            }
            
            if ((v$combo_name %in% names(data)) & (length(data[[v$combo_name]][[input$estimate]]) != 0)) { # if combo_name exists in data (e.g., not all studies have net)
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
              output[[plotname_simci]] <- renderPlot({
                create_plots(pd_list, plot_type = 'simci', add_description = TRUE, estimate = input$estimate)
              }, height = 200, width = 400)
              
            }
            
            if (grepl("_act_", this_study_or_group) & input$pooling == 'none') {
              #print(paste0('creating nifti for ', this_study_or_group))
              v$nifti_list[[this_study_or_group]] <- create_nifti(template, v$data, this_study_or_group, v$combo_name, v$brain_masks_init, estimate = input$estimate)
            }
            
            if (grepl("_fc_", this_study_or_group)) {
              #print(paste0('adding fc data to list for combo :  ', v$combo_name))
              v$fc_combo_data[[this_study_or_group]] <- v$data[[this_study_or_group]][[v$combo_name]][[input$estimate]]
            }
            
            #print(v$fc_combo_data[[this_study_or_group]][1,1:5])
            
            output[[plotname_spatial]] <- NULL
            output[[plotname_spatial]] <- renderPlot({
              if (grepl("_act_", this_study_or_group)) {
                plot_brain(v$nifti_list[[this_study_or_group]], anatomical, x = input$xCoord, y = input$yCoord, z = input$zCoord)
                
              } else if (grepl("_fc_", this_study_or_group)) {
                if (input$pooling != "none") {
                  #len <- length(v$fc_combo_data[[this_study_or_group]])
                  print(paste0('pooling data for ', this_study_or_group))
                  if (grepl("ukb", this_study_or_group)) { #ukb only
                    plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = TRUE, out_path = 'output', plot_name = paste0('matrix-', this_study_or_group, '-', v$combo_name, '.png'), ukb = TRUE, pooled = TRUE, rearrange = FALSE, title = FALSE, estimate = input$estimate)
                    #plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = FALSE, title = FALSE, pooled = ifelse((input$pooling == 'none'), FALSE, TRUE), rearrange = ifelse((input$pooling == 'none'), TRUE, FALSE))
                  } else { # other
                    plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = TRUE, out_path = 'output', plot_name = paste0('matrix-', this_study_or_group, '-', v$combo_name, '.png'), title = FALSE, pooled = TRUE, rearrange = FALSE, estimate = input$estimate)
                    #plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = FALSE, ukb = TRUE, pooled = TRUE, rearrange = FALSE, title = FALSE)
                  }
                } else { # not pooled
                  n_nodes <- (((-1 + sqrt(1 + 8 * length(v$fc_combo_data[[this_study_or_group]]))) / 2) + 1)
                  if (n_nodes == 268) {
                    plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = TRUE, out_path = 'output', plot_name = paste0('matrix-', this_study_or_group, '-', v$combo_name, '.png'), title = FALSE, pooled = FALSE, rearrange = TRUE, estimate = input$estimate)
                  } else if (n_nodes == 55) {
                    plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = TRUE, out_path = 'output', plot_name = paste0('matrix-', this_study_or_group, '-', v$combo_name, '.png'), ukb = TRUE, title = FALSE, estimate = input$estimate)
                  }
                }
                
              }
            }, height = 200, width = 250)
          })
          }
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
  
  
  ###### Plot brain images ######
  
  ## TODO: ## currently we only have one-sample task-act maps, will need to tweak this code when we get other test types
  
  output$keys <- DT::renderDataTable(phen_keys, 
                                     options = list(rownames = FALSE, 
                                                    paging = FALSE, 
                                                    scroller = TRUE,
                                                    scrollY = "400px",
                                                    scrollX = FALSE,
                                                    autoWidth = TRUE
                                     ),
                                     rownames = FALSE)
  
  
  ### Meta-analysis tab
  observe({
    if (input$tab == "Meta-Analysis") {
      req(v$plot_info_m())
      print("creating plot placeholders for meta-analysis simci plots") 
      #validate(need(length(v$plot_info_m()$idx) > 0, "no data avilable to plot"))
      print("Checking v$plot_info_m() in UI rendering:")
      print(v$plot_info_m())
      
      output$m_plots <- renderUI({
        if (length(v$data) == 0 | is.null(v$data))  {
          # if there is no data, display a message
          tagList(
            h3("No data available for the selected parameters.")
          )
        } else if (length(v$plot_info_m()$idx) > 0) {
          v$plot_output_list_m <- lapply(1:length(v$plot_info_m()$idx), function(i) {
            
            plotname_simci_m <- paste0("plot_m", i)
            plotname_spatial_m <- paste0("spatial_plot_m", i)
            #plotOutput(plotname, height = "200px", width = "100%")
            
            tagList(
              fluidRow(
                column(6, plotOutput(plotname_simci_m, height = "200px", width = "100%")),
                column(6, plotOutput(plotname_spatial_m, height = "200px", width = "100%"))
              )
            )
          })
          
          # convert the list to a tagList, this is necessary for the list of items to display properly
          do.call(tagList, v$plot_output_list_m)
        }
      })
    }
  })    
  
  observe({
    if (input$tab == "Meta-Analysis") {
      print('filling placeholders with meta-analysis simci plots')
      print(input$m_pooling)
      #if (input$group_by == "none") {
      # check if v$data is empty
      #print(head(v$plot_info_m()))
      # if (is.null(v$data) || length(v$data) == 0) {
      #   output$m_plots <- renderUI({
      #     tagList(
      #       h3("No data available for the selected parameters."),
      #       p("Please try adjusting your selections.")
      #     )
      #   })
      #   return()  # Exit the observe function early
      # }
      if (length(v$plot_info_m()$idx) > 0) {
        print(paste0("num plots: ", length(v$plot_info_m()$idx)))
        for (i in 1:length(v$plot_info_m()$idx)) {
          # print(paste0('plotting study ', rownames(v$plot_info)[i]))
          # create a local variable to hold the value of i
          #print(i)
          local({
            my_i <- i
            plotname_simci_m <- paste0("plot_m", my_i, sep="")
            plotname_spatial_m <- paste0("spatial_plot_m", my_i)
            
            this_study_or_group <- rownames(v$plot_info_m())[my_i]
            this_plot_info <- v$plot_info_m()[this_study_or_group,]
            
            pd_list_m <- list()
            n_studies_in_pd_list <- 1
            
            for (j in v$plot_info_m()$idx[[my_i]]) {
              name <- names(v$data[j])
              data <- v$data[[j]]
              study_details <- v$study[j, ]
            }
            
            if ((v$combo_name_m %in% names(data)) & !(all(is.na(data[[v$combo_name_m]][[input$m_estimate]])))) { # if combo_name exists in data (e.g., not all studies have net)
              
              # prep
              pd <- prep_data_for_plot(data = data, study_details = study_details, combo_name = v$combo_name_m, mv_combo_name = v$mv_combo_name_m, estimate = input$m_estimate, plot_info = this_plot_info)
              
              pd_list_m[[n_studies_in_pd_list]] <- pd
              n_studies_in_pd_list <- n_studies_in_pd_list + 1
              
            }
            
            if (length(pd_list_m) > 0) { # plot only if pd_list_m isn't empty
              print(paste0('pd_list_m is not empty - the length is ', length(pd_list_m)))
              # filename
              if (input$m_pooling == 'net') {
                net_str=" - net"
              } else {
                net_str=""
              }
              out_dir <- paste0('output/', pd_list_m$extra_study_details[[this_study_or_group]], ' - ', '/', 'simci', net_str)
              fn <- paste0(this_study_or_group, '_', n_studies_in_pd_list, '.png')
              
              # plot
              output[[plotname_simci_m]] <- renderPlot({
                create_plots(pd_list_m, plot_type = 'simci', add_description = TRUE, meta = TRUE, estimate = input$m_estimate)
              }, height = 200, width = 400)
              
            }
            
            # if voxel space
            if (v$plot_info_m()$ref[[my_i]] == "voxel" & input$m_pooling == 'none' & input$m_estimate == "d") {
              v$nifti_list_m[[this_study_or_group]] <- create_nifti(template, v$data, this_study_or_group, v$combo_name_m, v$brain_masks_m_init, estimate = input$m_estimate, meta = TRUE)
            }
            
            # if fc
            if (v$plot_info_m()$ref[[my_i]] != "voxel") {
              v$fc_combo_data_m[[this_study_or_group]] <- v$data[[this_study_or_group]][[v$combo_name_m]][[input$m_estimate]]
            }
            
            output[[plotname_spatial_m]] <- NULL
            output[[plotname_spatial_m]] <- renderPlot({
              # if voxel
              if (v$plot_info_m()$ref[[my_i]] == "voxel" & input$m_pooling == 'none') {
                plot_brain(v$nifti_list_m[[this_study_or_group]], anatomical, x = input$xCoord, y = input$yCoord, z = input$zCoord)
                
              } else {
                if (input$pooling != "none") {
                  #len <- length(v$fc_combo_data[[this_study_or_group]])
                  print(paste0('pooling data for ', this_study_or_group))
                  if (grepl("ukb", this_study_or_group)) { #ukb only
                    plot_full_mat(v$fc_combo_data_m[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = FALSE, ukb = TRUE, pooled = TRUE, rearrange = FALSE, title = FALSE, estimate = input$estimate)
                    #plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = FALSE, title = FALSE, pooled = ifelse((input$pooling == 'none'), FALSE, TRUE), rearrange = ifelse((input$pooling == 'none'), TRUE, FALSE))
                  } else { # other
                    plot_full_mat(v$fc_combo_data_m[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = FALSE, title = FALSE, pooled = TRUE, rearrange = FALSE, estimate = input$estimate)
                    #plot_full_mat(v$fc_combo_data[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = FALSE, ukb = TRUE, pooled = TRUE, rearrange = FALSE, title = FALSE)
                  }
                } else { # not pooled
                  n_nodes <- (((-1 + sqrt(1 + 8 * length(v$fc_combo_data_m[[this_study_or_group]]))) / 2) + 1)
                  if (n_nodes == 268) {
                    plot_full_mat(v$fc_combo_data_m[[this_study_or_group]], mapping_path = 'data/parcellations/map268_subnetwork.csv', save = FALSE, title = FALSE, pooled = FALSE, rearrange = TRUE, estimate = input$estimate)
                  } else if (n_nodes == 55) {
                    plot_full_mat(v$fc_combo_data_m[[this_study_or_group]], mapping_path = 'data/parcellations/map55_ukb.csv', save = FALSE, ukb = TRUE, title = FALSE, estimate = input$estimate)
                  }
                }
                
              }
            }, height = 200, width = 250)
          })
        }
      }
    } 
  }
  )
  
}
