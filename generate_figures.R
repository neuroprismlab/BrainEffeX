## generate_figures.R

####################################################################
#
# Run meta-analysis & create plots
#
####################################################################

## Load libraries

library(metafor)
library(ggpubr) # requires svglite
library(devtools) # for installing/loading utils
library(dplyr)
library(devtools)
library(png)
library(reshape2)
#library(BrainEffeX.utils)
library(neurobase)
library(colorspace)


## Load utils

# Option 1. simple add local scripts to path (for dev):
#utils_package_local <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/misc/BrainEffeX_utils/"
#load_all(utils_package_local)

# Option 2. full install of local package (for dev):
# utils_package_local <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/misc/BrainEffeX_utils/"
# install(utils_package_local)
# library(BrainEffeX.utils)

# Option 3. install package from github:
# utils_github_path <- "neuroprismlab/BrainEffeX_utils"
# install_github(utils_github_path)
# library(BrainEffeX.utils)

# set working directory to the BrainEffeX local cloned repo
repo_location = "/Users/shearer.h/Google Drive/My Drive/Github/BrainEffeX/"
setwd(repo_location)

testing = FALSE

## User-defined paths & parameters
meta_data_file_name <- 'meta_analysis_2025-05-01.RData'
combined_data_file_name <- 'combined_data_2025-02-24.RData'
data_dir <- "data/"
out_basename_basename <- "figures/"

# if testing, save the outputs to 
if (testing) {
  out_basename_basename <- paste0(repo_location, "testing/")
}


# plot params - USER-DEFINED

plot_output_style <- c('shiny') # c('shiny', 'manuscript') # 'shiny': save plots for one study at a time, simci-spatial type, add descrip; 'manuscript': save plots concatenated across all studies, add descrip

all_effect_size_types <- c('d','r_sq')              # c('d', 'r_sq', 'd.full_res')

all_motion <- c('none', 'regression', 'threshold')  # c('none', 'regression', 'threshold') # TODO: stat_control -> "...regression...$d", full_residualization -> "...regression...$d.full_res"
all_pooling <- c('none', 'net') #  # c('none','net')
do_multi <- c(FALSE)

all_plot_combination_styles <- c('meta')   # c('single','meta','overlapping') # note: overlapping can only be used for manuscript
all_grouping_var <- c('orig_stat_type', 'category')  # c('none', 'category', 'orig_stat_type') # used only for meta & overlap plots - TODO: separate out?
all_manuscript_plot_types <- c('simci', 'spatial')  # c('simci', 'spatial', 'density', 'power') # only used for plot_output_style = 'manuscript'

make_plots <- TRUE
save_plots <- TRUE
# save_logs <- FALSE
rearrange_by_stat_type <- TRUE # for single plots


# check if the data directory contains the data files
# if not, download them from OSF
if (!file.exists(paste0(data_dir, combined_data_file_name))) {
  library(osfr)
  print("Data is not downloaded. Downloading now from OSF.")
  project <- osf_retrieve_node("https://osf.io/cwnjd")
  files <- osf_ls_files(project)
  file_idx <- files$name == combined_data_file_name
  file <- files[file_idx,]
  osf_download(file, path = file.path(data_dir))
  if (combined_data_file_name %in% list.files(data_dir)) {
    # file already exists
    print("Main data file succesfully downloaded from OSF")
  } else {
    # file does not exist
    print("Data could not be downloaded from OSF. Try manually downloading and moving the file to the data directory from https://osf.io/cwnjd.")
  }
}

if (!file.exists(paste0(data_dir, meta_data_file_name))) {
  library(osfr)
  print("Data is not downloaded. Downloading now from OSF.")
  project <- osf_retrieve_node("https://osf.io/cwnjd")
  files <- osf_ls_files(project)
  file_idx <- files$name == meta_data_file_name
  file <- files[file_idx,]
  osf_download(file, path = file.path(data_dir))
  if (meta_data_file_name %in% list.files(data_dir)) {
    # file already exists
    print("Meta-analysis data file succesfully downloaded from OSF")
  } else {
    # file does not exist
    print("Data could not be downloaded from OSF. Try manually downloading and moving the file to the data directory from https://osf.io/cwnjd.")
  }
}

# special settings for shiny vs. manuscript

if (plot_output_style == 'shiny') {
  all_plot_types <- c('simci-spatial')
  if ('overlapping' %in% all_plot_combination_styles) {
    warning("Overlapping plots not supported in shiny mode. Removing.")
    all_plot_combination_styles <- all_plot_combination_styles[all_plot_combination_styles != 'overlapping']
  }
  # if (save_logs == TRUE) {
  # warning("Logs not used in shiny. Setting save_logs to FALSE.")
  save_logs <- FALSE
  # }
  add_plt_description <- TRUE # text at bottom of screen
  use_minimal_title <- FALSE
  
} else if (plot_output_style == 'manuscript') {
  all_plot_types <- all_manuscript_plot_types
  all_plot_combination_styles <- all_plot_combination_styles
  save_logs = TRUE
  add_plt_description <- FALSE # text at bottom of screen
  use_minimal_title <- TRUE # cleaner title for manuscript
}

# input and output directories



## Loop over plot types and styles

for (pooling in all_pooling) {
  for (motion in all_motion) {
    for (grouping_var in all_grouping_var) {
      for (effect_size_type in all_effect_size_types) {
        for (plot_combination_style in all_plot_combination_styles) {
          for (plot_type in all_plot_types) {
            
            print(paste0('Doing plot_combination_style: ', plot_combination_style, ' | plot_type: ', plot_type, ' | pooling: ', pooling, ' | motion: ', motion, ' | grouping_var: ', grouping_var))
            
            
            ## Set up strings
            
            combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.none')
            mv_combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.multi')
            
            # Correct args
            
            if (plot_combination_style == 'single' & grouping_var != 'none') {
              grouping_var <- 'none'
              cat("Warning: grouping_var set to 'none' for single plots\n")
            }
            
            ## Load data
            
            if (!exists("v")) {
              v <- load_data(data_dir)
            }
            
            
            ## Run meta-analysis, if specified
            
            # TODO: move to combine_gl with other more intensive processing / stat estimates & run all relevantmeta beforehand
            run_meta <- FALSE
            try_meta_file <- FALSE
            
            if (plot_combination_style == 'meta') {
              
              # setup string and filename
              meta_str <- paste0('meta_',grouping_var)
              meta_fn_dir <- system.file("meta/", package = "BrainEffeX.utils") # TODO: set this somewhere else
              meta_fn <- file.path(meta_fn_dir, "v.RData")
              
              # load meta_analysis data file
              #meta_str <- paste0('meta_', grouping_var)
              meta_path <- paste0(data_dir, meta_data_file_name)
              load(meta_path)
              # this loads v that includes meta data
              
              # check if this var contains the meta for this grouping var / combo
              if (!(meta_str %in% names(v))) { # check if this grouping var already exists in data
                try_meta_file <- TRUE
              } else if (!(combo_name %in% names(v[[meta_str]]$data[[1]]))) { # check if this combo_name has been run
                try_meta_file <- TRUE
              }
              
              # TODO: will also have to catch the case where d is defined but r_sq is not
              
              # check if saved meta file contains the meta for this grouping var / combo
              if (try_meta_file) {
                if (file.exists(meta_fn)) { # try to read pre-saved meta-analysis
                  load(meta_fn)
                  if (!(meta_str %in% names(v))) { # check again for grouping var
                    run_meta <- TRUE
                  } else if (!(combo_name %in% names(v[[meta_str]]$data[[1]]))) { # check again for combo
                    run_meta <- TRUE
                  }
                } else { # no file
                  run_meta <- TRUE
                }
                
              }
            }
            
            if (run_meta) {
              v <- meta_analysis(v, v$brain_masks, combo_name, grouping_var = grouping_var)
              save(v, file = meta_fn)
            }
            
            
            
            
            
            
            
            ## Set up unique identifiers for each plot
            
            plot_info__idx <- list() # each row = list of study(s) in data to include in each plot
            # for single plots: each row = 1 entry per study to index into v$data
            # for meta-analysis plots: each row = 1 entry per category to index v[[meta_str]]$data
            # for overlapping plots: each row = list of indices per group (x map type) to index into v$data
            
            plot_info__grouping_var <- list() # each row = grouping variable (same value repeated for each plot)
            plot_info__group_level <- list() # each row = level within grouping variable
            plot_info__ref <- list() # each row = ref(s) used for a study or grouping variable
            
            if (plot_combination_style == 'single') {  # name by study
              
              all_study_names <- names(v$data)
              
              for (i in 1:length(v$data)) {
                plot_info__idx[[all_study_names[[i]]]] <- i
                plot_info__grouping_var[[all_study_names[[i]]]] <- "none"  # overwrite any other grouping var if doing single plots
                plot_info__group_level[[all_study_names[[i]]]] <- NA
                plot_info__ref[[all_study_names[[i]]]] <- v$study$ref[i]
              }
              
              
              # sort all rows of plot_info__idx by orig_stat_type, with studies sharing same stat type next to each other
              if (rearrange_by_stat_type) {
                orig_stat_type_order <- c(which(v$study$orig_stat_type == 'r'), which(v$study$orig_stat_type == 't2'), which(v$study$orig_stat_type == 't'))
                plot_info__idx <- plot_info__idx[orig_stat_type_order]
                plot_info__grouping_var <- plot_info__grouping_var[orig_stat_type_order]
                plot_info__group_level <- plot_info__group_level[orig_stat_type_order]
                plot_info__ref <- plot_info__ref[orig_stat_type_order]
              }
              
              
            } else if (plot_combination_style == 'meta') { # name by average of grouping var
              
              for (i in 1:length(v[[meta_str]]$data)) {
                plot_info__idx[[names(v[[meta_str]]$data)[[i]]]] <- i
                plot_info__grouping_var[[names(v[[meta_str]]$data)[[i]]]] <- grouping_var
                plot_info__group_level[[names(v[[meta_str]]$data)[[i]]]] <- v[[meta_str]]$study$group_level[i]
                plot_info__ref[[names(v[[meta_str]]$data)[[i]]]] <- v[[meta_str]]$study$ref[i]
              }
              
            } else if (plot_combination_style == 'overlapping') { # overlapping individual plots
              
              if (grouping_var == 'category') {
                study_group_name <- v$study$category
              } else if (grouping_var == 'orig_stat_type') {
                study_group_name <- v$study$orig_stat_type
              }
              
              all_group_names <- unique(study_group_name)
              all_map_types <- unique(v$study$map_type)
              
              for (this_map_type in all_map_types) {
                for (this_group_name in all_group_names) {
                  idx <- which(study_group_name == this_group_name & v$study$map_type == this_map_type)
                  plot_info__idx[[paste0(this_group_name, '.', this_map_type)]] <- idx
                  plot_info__grouping_var[[paste0(this_group_name, '.', this_map_type)]] <- grouping_var
                  plot_info__group_level[[paste0(this_group_name, '.', this_map_type)]] <- this_group_name
                  plot_info__ref[[paste0(this_group_name, '.', this_map_type)]] <- unique(v$study$ref[idx])
                }
              }
            }
            
            plot_info <- data.frame(
              idx = I(plot_info__idx),
              grouping_var = unlist(plot_info__grouping_var),
              group_level = unlist(plot_info__group_level),
              ref = I(plot_info__ref),
              row.names = names(plot_info__idx),
              stringsAsFactors = FALSE
            )
            rm(plot_info__idx, plot_info__grouping_var, plot_info__group_level, plot_info__ref)
            
            
            
            ## Make Plots
            
            panel_list <- list() # list of panels
            panel_list_2 <- list() # list of panels
            
            panel_list <-list()
            log_list <- list() # list of logs
            
            for (i in 1:length(plot_info$idx)) { # loop over panels - this_study_or_group is the name of the group or study
              
              this_study_or_group <- rownames(plot_info)[i]
              this_plot_info <- plot_info[this_study_or_group,]
              
              pd_list <- list() # list of plot info for single panel
              pd_list_2 <- list() # add'l list of plot info for plot type to add to pd_list
              ld_list <- list() # list of log info for single panel
              
              n_studies_in_pd_list <- 1
              
              # 1. Prep
              
              for (j in plot_info$idx[[i]]) {
                print(plot_info$idx[[i]])
                # change metadata based on whether using meta-analysis
                
                if (plot_combination_style == 'meta') {
                  
                  data <- v[[meta_str]]$data[[j]]
                  study_details <- list()
                  brain_masks <- v[[meta_str]]$brain_masks[[j]]$pooling.none.motion.none.mv.none # TODO: this is because we explicitly set this for meta but not for single studies - assuming motion type shouldn't affect the mask and always using an external mask for pooling
                  
                } else {
                  
                  data <- v$data[[j]]
                  study_details <- v$study[j, ]
                  # if (pooling == "net" && study_details$map_type == "act") {
                  #   next
                  # } #testing
                  brain_masks <- v$brain_masks[[j]]
                }
                
                if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
                  if (any(!is.na(data[[combo_name]][[effect_size_type]])) > 0) {  # data is not just NA
                    
                    # prep
                    
                    if (plot_type == 'simci-spatial') { # shiny
                      
                      pd_list[[n_studies_in_pd_list]] <- prep_data_for_plot(data = data, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
                      pd_list_2[[n_studies_in_pd_list]] <- prep_data_for_spatial_plot(data = data, brain_masks = brain_masks, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
                      
                    } else if (plot_type == 'spatial') {
                      
                      # TODO: we probably don't even need a dedicated function for the spatial plots, just pass the relevant info
                      pd_list[[n_studies_in_pd_list]] <- prep_data_for_spatial_plot(data = data, brain_masks = brain_masks, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
                      
                    } else {
                      
                      pd_list[[n_studies_in_pd_list]] <- prep_data_for_plot(data = data, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
                      
                    }
                    
                    ld_list[[n_studies_in_pd_list]] <- get_summary_info(pd_list[[n_studies_in_pd_list]]$study_details, pd_list[[n_studies_in_pd_list]]$extra_study_details)
                    n_studies_in_pd_list <- n_studies_in_pd_list + 1
                    
                  }
                }
              }
              
              
              # 2. Plot & Log
              
              if (make_plots) {
                if (length(pd_list) > 0) { # plot only if pd_list isn't empty
                  
                  # set up plot
                  if (length(ld_list) > 1) {
                    log_list[[i]] <- combine_summary_info(ld_list)
                  } else {
                    log_list[[i]] <- ld_list[[1]]
                  }
                  
                  if (plot_type == 'simci-spatial') {
                    # if (pooling == "net" && study_details$map_type == "act") {
                    #   next
                    # } #testing
                    panel_list[[i]] <- create_plots(pd_list, plot_type = 'simci', effect_type = effect_size_type, do_multivariate = FALSE, add_description = add_plt_description, do_minimal_title = use_minimal_title, log_list[[i]], meta = plot_combination_style == "meta")
                    panel_list_2[[i]] <- create_plots(pd_list_2, plot_type = 'spatial', effect_type = effect_size_type, do_multivariate = FALSE, add_description = add_plt_description, do_minimal_title = use_minimal_title, log_list[[i]], meta = plot_combination_style == "meta")
                    
                  } else {
                    
                    panel_list[[i]] <- create_plots(pd_list, plot_type = plot_type, effect_type = effect_size_type, do_multivariate = FALSE, add_description = add_plt_description, do_minimal_title = use_minimal_title, log_list[[i]], meta = plot_combination_style == "meta")
                    
                  }
                  
                }
              }
              
            }
            
            
            if (make_plots) {
              
              # General plot parameters
              # TODO: figure out what we want to set up here vs. to pass or set up in
              # create_plots, which gets passed to plot_sim_ci, etc.
              # Should at least set all panel / canvas dimensions here
              
              pp <- list()
              pp$width_per_panel <- 7 
              pp$height_per_panel <- 6 
              pp$res <- 100
              pp$units <- "in"
              pp$title_size <- 20
              if (plot_type == 'simci-spatial') {
                pp$ncol <- 2
                pp$nrow <- 1
              } else if (plot_type == 'power') {
                pp$ncol <- length(panel_list)
                pp$nrow <- 1
              } else {
                pp$ncol <- 1
                pp$nrow <- length(panel_list)
              }
              if (plot_output_style == 'manuscript') {
                do_minimal_title <- TRUE
              } else {
                do_minimal_title <- FALSE
              }
              
              # prep: set up dir and file names
              
              if (save_plots) {
                if (plot_combination_style == 'meta') {
                  grouping_var_str <- paste0('_', grouping_var)
                } else {
                  grouping_var_str <- ''
                }
                
                if (plot_output_style == 'manuscript') {
                  grouping_var_str <- paste0('_', plot_type)
                } else { 
                  grouping_var_str <- paste0('_', plot_type, '_', effect_size_type)
                }
                
                out_basename <- paste0(out_basename_basename, plot_output_style, '/', effect_size_type, '/motion_', motion, '/pooling_', pooling, '/', plot_combination_style, grouping_var_str)
                if (plot_type == 'simci-spatial') { # use out_basename as dir, otherwise use as basename for concat plots
                  out_basename <- paste0(out_basename,'/')
                }
                actual_dir <- sub("/[^/]*$", "", out_basename)
                if (!dir.exists(actual_dir)) {
                  dir.create(actual_dir, recursive = TRUE)
                }
                cat("Saving plots to...\n", out_basename, "\n", sep = "")
              }
              
              
              # plot multi panels
              
              if (plot_type == 'simci-spatial') {
                
                for (i in 1:length(panel_list)) {
                  
                  if (is.null(panel_list[[i]]) || is.null(panel_list_2[[i]])) {
                    next
                  }
                  
                  t <- list(panel_list[[i]], panel_list_2[[i]])
                  t_master_title <- t[[1]]$labels$title
                  t[[1]]$labels$title <- ""
                  t[[2]]$labels$title <- ""
                  
                  multi_plot <- ggarrange(plotlist = t, ncol = pp$ncol, nrow = pp$nrow)
                  multi_plot <- annotate_figure(multi_plot,
                                                top = text_grob(t_master_title, face = "bold", size = pp$title_size))
                  
                  if (save_plots) {
                    
                    study_name <- names(plot_info$idx)[[i]]
                    
                    plot_fn <- paste0(out_basename, study_name, '.png')
                    ggsave(plot_fn, plot = multi_plot, width = pp$width_per_panel * pp$ncol, height = pp$height_per_panel * pp$nrow, units = pp$units, dpi = pp$res, bg = "white", device = "png")
                    
                    if (save_logs) { # TODO: some logs are identical - see about saving only single
                      log_fn <- paste0(out_basename, study_name,'.txt')
                      writeLines(unlist(lapply(log_list, function(x) c(x$title_text, x$bottom_text, ""))), log_fn)
                    }
                    
                  }
                  
                }
                
              } else {
                
                multi_plot <- ggarrange(plotlist = panel_list, ncol=pp$ncol, nrow=pp$row)
                
                if (save_plots) {
                  
                  plot_fn <- paste0(out_basename, '.png')
                  ggsave(plot_fn, plot = multi_plot, width = pp$width_per_panel * pp$ncol, height = pp$height_per_panel * pp$nrow, units = pp$units, dpi = pp$res, bg = "white", device = "png", limitsize = FALSE)
                  
                  if (save_logs) { # TODO: some logs are identical - see about saving only single
                    log_fn <- paste0(out_basename, '.txt')
                    writeLines(unlist(lapply(log_list, function(x) c(x$title_text, x$bottom_text, ""))), log_fn)
                  }
                  
                }
                
              }
              
            }
            
            
            ## close loop over plot types and styles
          } # effect_size_type
        } # pooling
      } # plot_combination_style
    } # plot_type
  } # motion
} # grouping_var

