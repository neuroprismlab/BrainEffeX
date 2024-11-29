### helper functions for activation effect maps
# 1. create_nifti_template()
# template <- readNIfTI(paste0(out_path, 'template_nifti'))
# 2. nifti <- create_nifti(template, study_name, brain_masks)
# 3. plot_brain(nifti, anatomical)

# set-up
load('combined_data_2024-09-10.RData')
study_name <- '10-Sep-2024_hcp_act_t_SOCIAL'
anatomical <- readNIfTI('anatomical.nii')

# create nifti template file (for hcp studies specifically)
create_nifti_template <- function(sample_nifti_path = '/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz',
                                  out_path = '/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/') {
  # INPUTS: 
  # - sample_nifti_path: the path to a nifti to use for the template
  # - out_path: the path to save the nifti file
  
  # OUTPUTS:
  # - a nifti file named template_nifti at out_path
  
  template <- readNIfTI(sample_nifti_path, read_data = FALSE)
  
  writeNIfTI(template, paste0(out_path, 'template_nifti'))
}

## create a nifti file from a template and study name
create_nifti <- function(nifti_template, study_name, brain_masks, out_path = '/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/', export = FALSE) {
  # INPUTS:
  # - study_name: string, the name of the study to use the mask from
  # - brain_masks: list of brain masks (from combined_gl output)
  # - out_path: path to save the nifti template to
  
  # OUTPUTS:
  # - template: nifti file that contains the mask for the given study

  # template <- readNIfTI('/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz', read_data = FALSE)
  
  structured <- brain_masks[[study_name]]$mask
  
  new_data <- data[[study_name]]$pooling.none.motion.none$d
  
  structured[structured==1] <- new_data[1,]
  
  nifti_template@.Data <- structured
  
  if (export) {
    writeNIfTI(nifti_template, paste0(out_path, study_name))
  }
  return(nifti_template)
}


# plot the nifti
plot_brain <- function(nifti, anatomical) {
  nifti[nifti == 0] <- NA
  ortho2(
    x = anatomical,
    y = test_nifti,
    crosshairs = FALSE,
    bg = 'white',
    NA.x = TRUE,
    col.y = colorRamps::blue2red(30),
    #xyz = c(input$xCoord, input$yCoord, input$zCoord),
    text.color = 'black',
    ybreaks = seq(-5, 5, length.out = 31),
    ycolorbar = TRUE,
    mfrow = c(3, 1)
  )
}





# gravyard:

# fill the template with a given study's data
# study_name <- '10-Sep-2024_hcp_act_t_EMOTION'
# motion <- 'none'
# pooling <- 'none'
# 
# # load template nifti
# template_nifti <- readNIfTI(paste0(out_path, 'template_nifti'))
# 
# # grab data to fill with
# new_data <- data[[study_name]]$pooling.none.motion.none$d
# 
# data_nifti <- template_nifti
# 
# structured_data <- brain_masks[[study_name]]$mask
# 
# structured_data[structured_data==1] <- new_data[1,]
# 
# data_nifti@.Data <- structured_data
# 
# 

