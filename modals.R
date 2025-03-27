# Modal Dialogs

# Modal 1: Getting started and explain left panel

createGettingStartedModal <- function() {
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
        tags$li("Motion Method"),
        tags$li("Pooling Method"),
      ),
     tags$p("Refer to the",tags$b(tags$i("tips")),"next to each input for additional guidance!"),
      tags$div(style = "text-align: center;",
               actionButton("nextToPage2", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}

# Modal 2: Understanding the Plots

createUnderstandingPlotsModal1 <- function() {
  bsModal(
    id = "instructionsModal2", title = "Understanding the Plots", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("Explore the expected effect sizes of the studies that match the provided filters."),
      tags$p(tags$b(tags$i("The plots in the middle(on the left?)")),"visualize all edges or voxels in each study:"),
      tags$ul(
        tags$li("Simultaneous confidence intervals (95% CI across all edges/voxels)."),
        tags$li(tags$i("Red")," indicates simultaneous CIs overlapping with 0,", tags$i("green"), "indicates no overlap."),
        tags$li(tags$i("Multivariate Effect sizes")," are displayed under the plots."),
      ),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage1", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("nextToPage3", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}

# Modal 3: Understanding the Plots

createUnderstandingPlotsModal2 <- function() {
  bsModal(
    id = "instructionsModal3", title = "Understanding the Plots", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("Explore the expected effect sizes of the studies that match the provided filters."),
      tags$p(tags$b(tags$i("The plots in the right panels(on the right?)")),"show the effect maps spatially:"),
      tags$ul(
        tags$li("Effect size matrices show the average effect sizes across all studies that fit the selected parameters (this changed right once each simci had a corresponding graph?)."),
        tags$li("Activation Maps help you to visualize the effect maps from activation studies on the brain.")
      ),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage2", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("nextToPage4", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}

# Modal 4: Downloading Effect Maps button to access the raw data files stored on OSF

createDownloadingEffectMapsModal <- function() {
  bsModal(
    #id = "instructionsModal4", title = "Downloading Effect Maps", trigger = NULL,
    id = "instructionsModal4", title = "Downloading Data", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("How to download data from BrainEffeX:"),
      tags$ul(
        tags$li("Click the", tags$b(tags$i("'Download Data'")), "button to access the raw data files stored on OSF.")),
        #tags$li("Click the", tags$b(tags$i("'Download Data'")), "button after filtering to download effect maps."),
        #tags$li("After downloading, you can use the effect maps further, and apply your own masks if needed.")),
        #tags$p("Use the", tags$b(tags$i("'Download Matrices'")), "button or", tags$b(tags$i("'Download Brain Image'")),"to download the matrices or brain image separately."),
      tags$p("Use the", tags$b(tags$i("'How to Use This App'")), "button at any time to revisit these instructions."),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage3", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("closePage4", "Close", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}


############################################

# # Function to create the dynamic panel content
createDynamicPanel <- function(input, study) {
  # Combine the reactive expression to update when either the Apply Filters or Reset Filters button is clicked
  renderUI({
    event <- eventReactive(
      list(input$apply_filters_btn, input$reset_btn), {  # Trigger on either button click
        messages <- c()
        
        # Dataset message
        if (is.null(input$dataset) || input$dataset == "*") {
          messages$dataset <- "• All datasets."
        } else {
          messages$dataset <- paste("• The <b>", input$dataset, "</b> dataset(s).")
        }
        
        # Map type message
        if (is.null(input$map_type) || input$map_type == "*") {
          messages$map_type <- "• All map types."
        } else {
          messages$map_type <- paste("• <b>", input$map_type, "</b> map type.")
        }
        
        # Task message
        if (is.null(input$task) || length(input$task) == 0) {
          messages$task <- "• No specific tasks are selected."
        } else if ("*" %in% input$task) {
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
        
        # Correlation message
        # if (is.null(input$correlation) || length(input$correlation) == 0) {
        #   messages$correlation <- "• No specific correlations are selected."
        # } else if (length(input$correlation) == length(unique(study[["var2"]])) || input$correlation == "*") {
        #   messages$correlation <- "• All correlations"
        # } else {
        #   messages$correlation <- paste("• The <b>", paste(input$correlation, collapse = ", "), "</b> correlation(s).")
        # }
        if (is.null(input$correlation) || length(input$correlation) == 0) {
          messages$correlation <- "• No specific correlations are selected."
        } else if (length(input$correlation) == length(unique(study[["var2"]])) || "*" %in% input$correlation) {
          messages$correlation <- "• All correlations"
        } else {
          messages$correlation <- paste("• The <b>", paste(input$correlation, collapse = ", "), "</b> correlation(s).")
        }
        
        
        # Group by message
        if (!is.null(input$group_by) && input$group_by != "none") {
          messages$group_by <- paste("• The results are grouped by <b>", input$group_by, "</b>.")
        }
        
        # Combine the messages into one text
        message_text <- paste("<b>You are looking at:</b><br>", paste(messages, collapse = "<br>"))
        
        tags$div(
          style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 20px; border-radius: 5px; text-align: center;",
          tags$p(HTML(message_text))
        )
      }
    )
    
    # Render the dynamic content based on the eventReactive output
    event()
  })
}

# Modal event observers for navigation
createModalNavigationObservers <- function(input, session) {
  observeEvent(input$showInstructions, {
    toggleModal(session, "instructionsModal1", toggle = 'open')
  })
  
  observeEvent(input$nextToPage2, {
    toggleModal(session, "instructionsModal1", toggle = 'close')
    toggleModal(session, "instructionsModal2", toggle = 'open')
  })
  
  observeEvent(input$prevToPage1, {
    toggleModal(session, "instructionsModal2", toggle = 'close')
    toggleModal(session, "instructionsModal1", toggle = 'open')
  })
  
  observeEvent(input$nextToPage3, {
    toggleModal(session, "instructionsModal2", toggle = 'close')
    toggleModal(session, "instructionsModal3", toggle = 'open')
  })
  
  observeEvent(input$prevToPage2, {
    toggleModal(session, "instructionsModal3", toggle = "close")
    toggleModal(session, "instructionsModal2", toggle = "open")
  })
  
  observeEvent(input$nextToPage4, {
    toggleModal(session, "instructionsModal3", toggle = "close")
    toggleModal(session, "instructionsModal4", toggle = "open")
  })
  observeEvent(input$prevToPage3, {
    toggleModal(session, "instructionsModal4", toggle = "close")
    toggleModal(session, "instructionsModal3", toggle = "open")
  })

  observeEvent(input$closePage4, {
    toggleModal(session, "instructionsModal4", toggle = "close")
  })
}
