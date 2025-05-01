# BrainEffeX
This repository contains the code for the [BrainEffeX web app](https://neuroprismlab.shinyapps.io/BrainEffeX/) for exploring fMRI effect sizes. 

---
## Purpose
Effect size estimation is crucial for power analysis and experiment deisgn, but poses unique challenges in fMRI research due to the complexity of the data and analysis techniques. Here, we utalized large fMRI datasets to obtain precise univariate and multivariate effect size estimates from "typical" fMRI study designs: brain-behavior correlation, task vs. rest, and between-group analyses of functional connecitivity and task-based activation maps. This is an interactive web application for exploring these effect maps. 
> The preprint for this web app is avilable [here](https://osf.io/preprints/osf/kryn4_v2).

---
## Repository Structure
```
BrainEffeX/
└── .github/ISSUE_TEMPLATE/   # Data contribution form? FIX                   
└── data                      # Data structures 
└── figures                   # Visualizations of effect size estimates displayed on app      
└── for_contributors          # Intructions for contributors             
└── www                    
├── .gitignore   
├── README.md                # Project overview and usage
├── app.R                    # Main script that launches the Shiny app
├── helpers.R                # Functions called in server.R
├── modals.R                 # Functions to generate modal dialogs
├── server.R                 # Contains the logic for the app
├── ui.R                     # Defines the layout and apperance of the app
```

---
## Preparing Inputs
The inputs for this app are prepared in the [Calculate_EffeX Repository](https://github.com/neuroprismlab/calculate_effeX). Group-level statistical maps are computed, then converted to effect size estimates.
> These effect size maps can be found [here](https://osf.io/cwnjd/files/osfstorage), as the file exceeds GitHub's size limits.

The script 'generate_figures.R' inputs this effect map data and exports the visualizations. These visualizations are stored in a folder called 'figures', and are inputted into **BrainEffeX**. 

The R package containing useful functions used in this app (and in related publications) can be found in the [BrainEffeX_utils Repository](https://github.com/neuroprismlab/BrainEffeX_utils).

---
## Contributions
To support **BrainEffeX** as a growing resource, we welcome contributions of large sample fMRI datasets (n>500). Data should be provided at the subject level, after preproccessing and computation of functional connectivity or task-based activiation. 

### 📊 Data Contributions
- If you have data to contribute to the app, please see the README.md file in the 'for_contributors' folder within this repository. 

### 💻 Developer contributions
- If you'd like to contribute features to the BrainEffeX app, please submit a pull request.

If you have suggestions, please submit an issue or contact us by email: shearer.h@northeastern.edu

---
## To run the BrainEffeX app locally on your computer with RStudio:
1. Clone this repository to your computer
2. Install the Shiny package in RStudio
3. Open the cloned repo directory in RStudio
4. Open the app.R file
5. Click 'run app' button in the top-right corner of the script window to launch the app

---
## Citation
If you use this pipeline in your work, please cite:

CITATION
DOI tag

