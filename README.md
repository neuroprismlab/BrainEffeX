# Brain EffeX
This repository contains the code for the [BrainEffeX web app](https://neuroprismlab.shinyapps.io/BrainEffeX/) for exploring fMRI effect sizes. 

---
## Purpose
Effect size estimation is crucial for power analysis and experiment deisgn, but poses unique challenges in fMRI research due to the complexity of the data and analysis techniques. Here, we utalized large fMRI datasets to obtain precise univariate and multivariate effect size estimates from "typical" fMRI study designs: brain-behavior correlation, task vs. rest, and between-group analyses of functional connecitivity and task-based activation maps. This is an interactive web application for exploring these effect maps. 
> The preprint for this web app is avilable [here](https://osf.io/preprints/osf/kryn4_v1).
> 
> Effect map data is hosted [here](https://osf.io/cwnjd/files/osfstorage), as the file exceeds GitHub's size limits. 

The code for the analysis that produced the data fed into the app can be found at https://github.com/neuroprismlab/calculate_effeX. 

An R package containing useful functions used in this app (and in related publications) can be found at https://github.com/neuroprismlab/BrainEffeX_utils.

If you have data to contribute to the app, please see the README.md file in the for_contributors/ directory. 

### To run the BrainEffeX app locally on your computer with RStudio:
1. Clone this repository to your computer
2. Install the Shiny package in RStudio
3. Open the cloned repo directory in RStudio
4. Open the app.R file
5. Click 'run app' near the top right of the script window (button has a green play button)
