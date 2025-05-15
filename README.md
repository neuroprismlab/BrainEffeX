# BrainEffeX <img src="www/nplogo.png" alt="NP Logo" style="width:50px; height:50px; vertical-align: middle;"/>
This repository contains the code for the [BrainEffeX web app](https://neuroprismlab.shinyapps.io/BrainEffeX/) for exploring fMRI effect sizes. 

---
## Purpose
Effect size estimation is crucial for power analysis and experiment deisgn, but poses unique challenges in fMRI research due to the complexity of the data and analysis techniques. Here, we utalized large fMRI datasets to obtain precise univariate and multivariate effect size estimates from "typical" fMRI study designs: brain-behavior correlation, task vs. rest, and between-group analyses of functional connecitivity and task-based activation maps. This is an interactive web application for exploring these effect maps. 
> The preprint for this web app is available on [OSF](https://osf.io/preprints/osf/kryn4_v3).

---
## Repository Structure
Relavent files:
```
BrainEffeX/                 
└── data                     # Data structures 
└── figures                  # Visualizations of effect size estimates displayed on app      
└── for_contributors         # Intructions for contributors             
├── README.md                # Project overview and usage
├── app.R                    # Main script that launches the Shiny app
├── helpers.R                # Functions called in server.R
├── modals.R                 # Functions to generate modal dialogs
├── server.R                 # Contains the logic for the app
├── ui.R                     # Defines the layout and apperance of the app
```

---
## Data and Inputs
Group level statistical maps are computed and converted to effect size estimates using scripts within the [calculate_effeX repository](https://github.com/neuroprismlab/calculate_effeX). 
> The resulting effect size maps are avaliable on [OSF](https://osf.io/cwnjd/files/osfstorage), as they exceed GitHub's file size limitations. 

The script 'generate_figures.R' uses these maps to create the visualizations, which are saved in a figures folder and then loaded into **BrainEffeX**.

The R package containing useful functions used in this app (and in related publications) can be found in the [BrainEffeX_utils Repository](https://github.com/neuroprismlab/BrainEffeX_utils).

---
## Contributions
To support **BrainEffeX** as a growing resource, we welcome contributions of large sample fMRI datasets (n>500). Data should be provided at the subject level, after preproccessing and computation of functional connectivity or task-based activiation. 

### 📊 Data Contributions
- If you have data to contribute to the app, please see the README.md file in the 'for_contributors' folder within this repository. 

### 💻 Developer contributions
- If you'd like to contribute features to the BrainEffeX app, please submit a pull request.

For any questions or feedback, please submit an [issue](https://github.com/neuroprismlab/BrainEffeX/issues) or contact us by email: shearer.h@northeastern.edu

---
## Run BrainEffeX Locally
1. Clone this repository to your computer
2. Install the Shiny package in RStudio
3. Open the cloned repo directory in RStudio
4. Open the app.R file
5. Click 'run app' button in the top-right corner of the script window to launch the app

---
## Citation
If you use this pipeline in your work, please cite:

>Shearer, et al. (2025). BrainEffeX: A Web App for Exploring fMRI Effect Sizes. OSF. doi.org/10.31219/osf.io/kryn4_v3

[![DOI](https://img.shields.io/badge/DOI-10.31219/osf.io/kryn4.v3-blue.svg)](https://doi.org/10.31219/osf.io/kryn4_v3)
