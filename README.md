# Brain EffeX
This repository contains the code for the [BrainEffeX web app](https://neuroprismlab.shinyapps.io/BrainEffeX/) for exploring fMRI effect sizes. 

---
## Purpose
Effect size estimation is crucial for power analysis and experiment deisgn, but poses unique challenges in fMRI research due to the complexity of the data and analysis techniques. Here, we utalized large fMRI datasets to obtain precise univariate and multivariate effect size estimates from "typical" fMRI study designs: brain-behavior correlation, task vs. rest, and between-group analyses of functional connecitivity and task-based activation maps. This is an interactive web application for exploring these effect maps. 
> The preprint for this web app is avilable [here](https://osf.io/preprints/osf/kryn4_v1).
---

## Preparing Inputs
The inputs for this app are prepared in the [Calculate_EffeX Repository](https://github.com/neuroprismlab/calculate_effeX). First, effect map data is calculated and properly formatted. 
> Formated effect map data is hosted [here](https://osf.io/cwnjd/files/osfstorage), as the file exceeds GitHub's size limits.

The script 'generate_figures.R' inputs this effect map data and exports the visualizations. These visualizations are stored in a folder called 'figures', and are inputted into the web app. 

The R package containing useful functions used in this app (and in related publications) can be found in the [BrainEffeX_utils Repository](https://github.com/neuroprismlab/BrainEffeX_utils).

---
## Data Contributions
To support BrainEffeX as a growing resource, we welcome contributions of large sample fMRI datasets (n>500). Data should be provided at the subject level, after preproccessing and computation of functional connectivity or task-based activiation. 
If you have data to contribute to the app, please see the README.md file in the 'for_contributors' folder within this repository. 

---
## To run the BrainEffeX app locally on your computer with RStudio:
1. Clone this repository to your computer
2. Install the Shiny package in RStudio
3. Install the BrainEffeX Utils package in R or RStudio (for instructions on adding this package, refer to the README.md file in the [BrainEffeX_utils Repository](https://github.com/neuroprismlab/BrainEffeX_utils))
4. Open the cloned repo directory in RStudio
5. Open the app.R file
6. Click 'run app' button in the top-right corner of the script window to launch the app
