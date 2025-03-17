####################################################################
# BrainEffeX App
####################################################################

# Shiny stuff needed for both ui and server (check)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(bslib)
library(DT) # data tables

# Import custom BrainEffeX functions, ui, and server
save_plots = FALSE # set to TRUE to save all plots as pngs, MUST BE OFF TO DEPLOY
library(BrainEffeX.utils) # to run locally, install the package from github with: devtools::install_github("neuroprismlab/BrainEffeX_utils")
source("ui.R")
source("server.R")
source("helpers.R")

# Run app
shinyApp(ui, server)
