##### Functions #####

#Plotting
create_explorer_plots <- function(input, output, study_filtered, v, meta, images_folder) {
  if (input$tab == "Explorer") {
    print("Creating placeholders for explorer plots")
    req(study_filtered)
    
    # Removing files that don't exist
    study_filtered <- study_filtered[sapply(1:nrow(study_filtered), function(i) {
      image_path <- paste0("./", images_folder, "/", input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "/", study_filtered[i, 3], ".png")
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
            column(10, div(imageOutput(plotname, height = "200px", width = "550px"), style = "margin-bottom: 20px"))
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
        
        image_path <- paste0("./", images_folder, "/", input$estimate, "/motion_", input$motion, "/pooling_", input$pooling, "/", meta, "/", study_filtered[my_i, 3], ".png")
        
        output[[plotname]] <- renderImage({
          # No need to check for file existence since we already filtered out missing files
          list(src = image_path, width = "100%", height = 200)
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
    
    # Removing files that don't exist
    study_filtered <- study_filtered[sapply(1:nrow(study_filtered), function(i) {
      image_path <- paste0("./", images_folder, "/", input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", "meta_", input$meta_analysis, "/", study_filtered[i, "name"], ".png")
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
            column(10, div(imageOutput(plotname, height = "200px", width = "550px"), style = "margin-bottom: 20px"))
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
        
        image_path <- paste0("./", images_folder, "/", input$m_estimate, "/motion_", input$m_motion, "/pooling_", input$m_pooling, "/", "meta_", input$meta_analysis, "/", study_filtered[i, "name"], ".png")
        
        output[[plotname]] <- renderImage({
          # No need to check for file existence since we already filtered out missing files
          list(src = image_path, width = "100%", height = 200)
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
