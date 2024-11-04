# Modal Dialogs

# Modal 1: Getting started

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
        tags$li("Spatial scale"),
      ),
     tags$p("Refer to the",tags$b(tags$i("tips")),"next to each input for additional guidance!"),
      tags$div(style = "text-align: center;",
               actionButton("nextToPage2", "Next", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}

# Modal 2: Understanding the Plots

createUnderstandingPlotsModal <- function() {
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
  )
}

# Modal 3: Downloading Effect Maps

createDownloadingEffectMapsModal <- function() {
  bsModal(
    id = "instructionsModal3", title = "Downloading Effect Maps", trigger = NULL,
    size = "large",
    tags$div(
      tags$p("How to download effect maps from BrainEffeX:"),
      tags$ul(
        tags$li("Click the", tags$b(tags$i("'Download Data'")), "button after filtering to download effect maps."),
        tags$li("After downloading, you can use the effect maps further, and apply your own masks if needed.")),
      tags$p("Use the", tags$b(tags$i("'How to Use This App'")), "button at any time to revisit these instructions."),
      tags$div(style = "text-align: center;",
               actionButton("prevToPage2", "Previous", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;"),
               actionButton("closePage2", "Close", style = "margin-top: 10px; background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px;")
      )
    )
  )
}
