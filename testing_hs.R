# testing inputs

this_data_dir <- 'data/'
out_dir_basename <- 'output/'

input <- list()

input$dataset <- '*'
input$map_type <- '*'
input$task <- '*'
input$test_type <- '*'
input$correlation <- '*'
input$pooling <- 'none'
input$motion <- 'none'
input$estimate <- 'd'
input$group_by <- 'none'

combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.none')
mv_combo_name <- paste0('pooling.', input$pooling, '.motion.', input$motion, '.mv.multi')

make_plots <- TRUE
save_plots <- FALSE

plot_type <- 'simci'
add_plt_description <- TRUE

plot_combination_style <- 'single' #TODO: turn into input var
grouping_var <- input$group_by
effect_size_type <- input$estimate

if (input$pooling == 'net') {
  net_str=" - net"
} else {
  net_str=""
}

v <- load_data(this_data_dir)

plot_info__idx <- list()

plot_info__grouping_var <- list() # each row = grouping variable (same value repeated for each plot)
plot_info__group_level <- list() # each row = level within grouping variable
plot_info__ref <- list() # each row = ref(s) used for a study or grouping variable

if (plot_combination_style == 'single') {  # name by study
  
  for (i in 1:length(v$data)) {
    plot_info__idx[[names(v$data)[[i]]]] <- i
    plot_info__grouping_var[[names(v$data)[[i]]]] <- "none"  # overwrite any other grouping var if doing single plots single
    plot_info__group_level[[names(v$data)[[i]]]] <- NA
    plot_info__ref[[names(v$data)[[i]]]] <- v$study$ref[i]
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

for (i in 1:length(plot_info$idx)) { # this_study_or_group is the name of the group or study
  
  this_study_or_group <- rownames(plot_info)[i]
  this_plot_info <- plot_info[this_study_or_group,]
  
  pd_list <- list()
  n_studies_in_pd_list <- 1
  
  # 1. Prep
  
  for (j in plot_info$idx[[i]]) {
    
    # change metadata based on whether using meta-analysis
    
    if (plot_combination_style == 'meta') {
      
      name <- names(v$d_group[j])
      data <- v$d_group[[j]]
      study_details <- list()
      
    } else {
      
      name <- names(v$data[j])
      data <- v$data[[j]]
      study_details <- v$study[j, ]
      
    }
    
    if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
      
      # prep
      
      pd <- prep_data_for_plot(data = data, name = name, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
      
      pd_list[[n_studies_in_pd_list]] <- pd
      n_studies_in_pd_list <- n_studies_in_pd_list + 1
      
    }
  }
  
  # 2. Plot
  
  if (make_plots) {
    if (length(pd_list) > 0) { # plot only if pd_list isn't empty
      
      # filename
      out_dir <- paste0(out_dir_basename, pd_list$extra_study_details[[this_study_or_group]], ' - ', plot_combination_style, '/', plot_type, net_str)
      fn <- paste0(this_study_or_group, '_', n_studies_in_pd_list, '.png')
      
      # plot
      create_plots(pd_list, plot_type = plot_type, add_description = add_plt_description, save = save_plots, out_path = out_dir, file_name = fn)
      
    }
  }
  
}













#' Group Studies by Factor
#'
#' This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs)
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#' @param v A list containing effect size data
#' @param brain_masks A list containing the brain masks
#' @param combo_name A string specifying the combo to plot - # TODO: note: in app, this is saved directly in v
#' @param group_by A string to specify grouping: "orig_stat_type" or "category"
#'
#' @return An updated list with the grouped_by data
#' @export
#'
#' @examples
#' # Example usage
#' # meta_analysis(v,v$brain_masks, "pooling.none.motion.none.mv.none")
meta_analysis <- function(v, brain_masks, combo_name, grouping_var = "category") {
  
  testing <- 1
  
  # libraries & functions
  
  library(metafor)
  
  # helpers
  
  which_triangle <- function(mat) {
    if (!is.matrix(mat)) stop("Input must be a matrix")
    
    is_upper <- all(mat[lower.tri(mat)] == 0)
    is_lower <- all(mat[upper.tri(mat)] == 0)
    # note: we do not care about checking for diagonal (this function does not include diagonal)
    
    if (is_upper && is_lower) { return("both")
    } else if (is_upper) { return("upper")
    } else if (is_lower) { return("lower")
    } else { return("no_data")
    }
  }
  
  d_se <- function(d, n1, n2 = NULL) {
    if (is.null(n2)) { # one-sample
      se <- sqrt(1 / n1 + (d^2 / (2 * n1)))
    } else { # two-sample
      se <- sqrt((n1 + n2) / (n1 * n2) + (d^2 / (2 * (n1 + n2))))
    }
    return(se)
  }
  
  r_sq_se <- function(r_sq, n) {
    r <- sqrt(r_sq)
    se_r <- sqrt((1 - r^2) / (n - 2));
    se <- se_r^2
    return(se)
  }
  
  
  # initialize vars for storing grouping results
  v$data_group <- list() # store data for each stat + ref type
  v$study_group <- data.frame(group = character(0), ref = character(0), name = character(0)) # store grouping info
  v$brain_masks_group <- list()
  
  # combo_name <- names(data)[grepl(combo_name, names(v$data[[1]]))] # because some are "multi" and some "multi.r" - TODO: this isn't great - somehow it changes mv.none to mv.multi
  
  # for each level of the grouping var (e.g., statistic type, category)
  for (level in unique(v$study[[grouping_var]])) {
    print(level)
    # for each reference type
    for (ref in unique(v$study$ref)) {
      print(ref)
      matching_idx__study <- which(v$study[[grouping_var]] == level & v$study$ref == ref) # TODO: maybe explicitly add map type here
      
      if (length(matching_idx__study) > 0) {
        
        matching_names <- v$study$name[matching_idx__study]
        matching_idx__data <- which(toupper(names(v$data)) %in% toupper(matching_names))
        # idx of the studies in d that match the current stat and ref
        
        # get intersection of all masks
        
        intersection_mask <- brain_masks[[v$study$name[matching_idx__study[1]]]]$mask
        # for (this_study in matching_idx__data) {
        #   
        #   if (v$study$map_type[matching_idx__study[1]] == "fc") {
        #     
        #     # make FC triangles all upper so they can be combined
        #     
        #     # TODO: edit the below to actually fix non-upper conditions, namely: structure data and apply operation for every variable in v$data (d, r_sq, ci's, stats, etc)
        #     # TODO: do we already have a checker like this elsewhere or in the combine functions?
        #     
        #     # triangle_type <- which_triangle(brain_masks[[v$study$name[this_study]]]$mask)
        #     # switch(triangle_type,
        #     #        # "upper" = leave as is
        #     #        "lower" = { # transpose
        #     #          warning("Data should be upper triangular but is lower triangular.")
        #     #          # v$data[[this_study]][[combo_name]]$d <- t(v$data[[this_study]][[combo_name]]$d)
        #     #          # brain_masks[[v$study$name[this_study]]]$mask <- t(brain_masks[[v$study$name[this_study]]]$mask)
        #     #        },
        #     #        "both" = { # remove lower
        #     #          warning("Data should be upper triangular but contains entries on both sides of diagonal.")
        #     #          # v$data[[this_study]][[combo_name]]$d[lower.tri(v$data[[this_study]][[combo_name]]$d)] <- 0
        #     #          # brain_masks[[v$study$name[this_study]]]$mask[lower.tri(brain_masks[[v$study$name[this_study]]]$mask)] <- 0
        #     #        },
        #     #        "no_data" = { # remove this data
        #     #          warning("Mask suggests no data exists.")
        #     #          # v$data[[this_study]][[combo_name]]$d <- NULL
        #     #          # v$study$name[this_study]]]$mask[lower.tri(brain_masks[[v$study$name[this_study]]]$mask <- NULL
        #     #        }
        #     #        # TODO: check if there are too few entries in mask (e.g., <75%)
        #     # )
        #   }
        #   intersection_mask <- intersection_mask & brain_masks[[v$study$name[this_study]]]$mask
        # }
        
        
        # Combine data
        
        # TODO: should actually save all results so data_group fields mirror data_group (e.g., v$data_group$d, v$data_group$r_sq, etc)
        
        # initialize
        d__group <- NULL
        d_se__group <- NULL
        r_sq__group <- NULL
        r_sq_se__group <- NULL
        
        d_sim_ci_lb__group <- NULL
        d_sim_ci_ub__group <- NULL
        r_sq_sim_ci_lb__group <- NULL
        r_sq_sim_ci_ub__group <- NULL
        
        if (length(matching_idx__data) == 1) { # NO META-ANALYSIS:
          
          # get individual study effect size, sample size, & ci's
          
          this_study <- matching_idx__data[1]
          
          # set up n's for se calc
          
          this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
          if (!is.null(this_n_total)) { # correlation, so undefined
            this_n1 <- this_n_total/2 # TODO: check
            this_n2 <- this_n_total/2
            this_n_total <- this_n_total
          } else {
            this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
            this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
            this_n_total <- this_n1 + this_n2
          }
          
          d__group <- as.numeric(v$data[[this_study]][[combo_name]]$d) # TODO: for r2, $d -> $R2
          d_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)  # TODO: remove all references to CI if we end up using se's
          d_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)
          d_se__group <- d_se(d__group, n1 = this_n1, n2 = this_n2)
          
          r_sq__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq) # TODO: for r2, $d -> $R2
          r_sq_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
          r_sq_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)
          r_sq_se__group <- r_sq_se(r_sq__group, n = this_n_total)
          
          print("- single")
          
          
        } else { # META-ANALYSIS:
          
          # 1. Get individual study effect sizes + sample sizes
          
          # preallocate to store data across studies
          
          if (v$study$map_type[matching_idx__data[1]] == "act") {
            n_vars_intersection <- sum(intersection_mask)
          } else {
            n_vars_intersection <- sum(intersection_mask)
          }
          
          d__all <- matrix(0, nrow = n_vars_intersection, ncol = length(matching_idx__data))
          d_sim_ci_ub__all <- d__all
          d_sim_ci_lb__all <- d__all
          d_se__all <- d__all
          
          r_sq__all <- d__all
          r_sq_sim_ci_ub__all <- d__all
          r_sq_sim_ci_lb__all <- d__all
          r_sq_se__all <- d__all
          
          # get data
          
          it <- 1
          for (this_study in matching_idx__data) {
            
            # get n's
            
            this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
            if (!is.null(this_n_total)) { # correlation, so undefined
              this_n1 <- this_n_total/2
              this_n2 <- this_n_total/2
              this_n_total <- this_n_total
            } else {
              this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
              this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
              this_n_total <- this_n1 + this_n2
            }
            
            this_d <- as.numeric(v$data[[this_study]][[combo_name]]$d)
            this_d_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)
            this_d_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)
            
            this_r_sq <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq)
            this_r_sq_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
            this_r_sq_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)
            
            this_d_se <- d_se(this_d, n1 = this_n1, n2 = this_n2)
            this_r_sq_se <- r_sq_se(this_r_sq, n = this_n_total)
            
            # make vector for indexing that combines d_mask (1D) and intersection_mask (2D/3D)
            #   mask_of_masks is a 1D that is as long as d_mask,
            #   but only has 1's where effects exist across studies (i.e., where there are 1's in intersection_mask)
            #   -> this lets us grab effects only where they exist across studies
            
            d_mask <- brain_masks[[v$study$name[this_study]]]$mask
            mask_of_masks <- intersection_mask[d_mask == 1]
            
            this_d <- this_d[mask_of_masks]
            this_d_sim_ci_lb <- this_d_sim_ci_lb[mask_of_masks]
            this_d_sim_ci_ub <- this_d_sim_ci_ub[mask_of_masks]
            
            this_r_sq <- this_r_sq[mask_of_masks]
            this_r_sq_sim_ci_lb <- this_r_sq_sim_ci_lb[mask_of_masks]
            this_r_sq_sim_ci_ub <- this_r_sq_sim_ci_ub[mask_of_masks]
            
            this_d_se <- this_d_se[mask_of_masks]
            this_r_sq_se <- this_r_sq_se[mask_of_masks]
            
            # append to total
            
            d__all[, it] <- this_d
            d_sim_ci_lb__all[, it] <- this_d_sim_ci_lb
            d_sim_ci_ub__all[, it] <- this_d_sim_ci_ub
            d_se__all[, it] <- this_d_se
            
            r_sq__all[, it] <- this_r_sq
            r_sq_sim_ci_lb__all[, it] <- this_r_sq_sim_ci_lb
            r_sq_sim_ci_ub__all[, it] <- this_r_sq_sim_ci_ub
            r_sq_se__all[, it] <- this_r_sq_se
            
            it <- it + 1
            
          }
          
          
          
          # 2. Meta analysis
          
          # preallocate to store results
          
          d__group <- numeric(dim(d__all)[1])
          d_sim_ci_lb__group <- d__group
          d_sim_ci_ub__group <- d__group
          d_se__group <- d__group
          
          r_sq__group <- d__group
          r_sq_sim_ci_lb__group <- d__group
          r_sq_sim_ci_ub__group <- d__group
          r_sq_se__group <- d__group
          
          start_time <- Sys.time()
          for (this_variable in 1:dim(d__all)[1]) {
            
            if (testing) { # simple mean
              
              d__group[this_variable] <- mean(d__all[this_variable,])
              d_sim_ci_lb__group[this_variable] <- mean(d_sim_ci_lb__all[this_variable,])
              d_sim_ci_ub__group[this_variable] <- mean(d_sim_ci_ub__all[this_variable,])
              
              r_sq__group[this_variable] <- mean(r_sq__all[this_variable,])
              r_sq_sim_ci_lb__group[this_variable] <- mean(r_sq_sim_ci_lb__all[this_variable,])
              r_sq_sim_ci_ub__group[this_variable] <- mean(r_sq_sim_ci_ub__all[this_variable,])
              
            } else { # meta-analysis
              
              d_meta_analysis <- NULL
              d_meta_analysis <- rma.uni(yi = d__all[this_variable,], se = d_se__all[this_variable,], method = "REML")
              d__group[this_variable] <- d_meta_analysis$b
              d_sim_ci_lb__group[this_variable] <- d_meta_analysis$ci.lb # TODO: here and below - re-specify alpha/n_vars for corrected CI
              d_sim_ci_ub__group[this_variable] <- d_meta_analysis$ci.ub
              
              r_sq_meta_analysis <- NULL
              r_sq_meta_analysis <- rma.uni(yi = r_sq__all[this_variable,], se = r_sq_se__all[this_variable,], method = "REML")
              r_sq__group[this_variable] <- r_sq_meta_analysis$b
              r_sq_sim_ci_lb__group[this_variable] <- r_sq_meta_analysis$ci.lb
              r_sq_sim_ci_ub__group[this_variable] <- r_sq_meta_analysis$ci.ub
              
            }
            
          }
          elapsed_time <- Sys.time() - start_time
          print(elapsed_time)
        }
        
        
        # remove values from indices that are zero in d_total, sim_ci_lb_total, and sim_ci_ub_total
        
        
        # TODO: necessary? we already do the intersection mask. (do we get enough 0's to worry, esp in activation?)
        zero_idx <- which((d__group == 0) & (d_sim_ci_lb__group == 0) & (d_sim_ci_ub__group == 0))
        r_sq_zero_idx <- which((r_sq__group == 0) & (r_sq_sim_ci_lb__group == 0) & (r_sq_sim_ci_ub__group == 0))
        
        # TODO: not sure we want this, after all the masking - if so, keep a new mask
        
        if (length(zero_idx) > 0) {
          d__group <- d__group[-zero_idx]
          d_se__group <- d_se__group[-zero_idx]
          
          if (testing) {
            d_sim_ci_lb__group <- d_sim_ci_lb__group[-zero_idx]
            d_sim_ci_ub__group <- d_sim_ci_ub__group[-zero_idx]
          }
          
        }
        if (length(r_sq_zero_idx) > 0) {
          r_sq__group <- r_sq__group[-zero_idx]
          r_sq_se__group <- r_sq_se__group[-zero_idx]
          
          if (testing) {
            r_sq_sim_ci_lb__group <- r_sq_sim_ci_lb__group[-zero_idx]
            r_sq_sim_ci_ub__group <- r_sq_sim_ci_ub__group[-zero_idx]
          }
        }
        
        # store d_avg, sim_ci_lb_avg, and sim_ci_ub_avg in data_group list as a list
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$d <- d__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$se <- d_se__group
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq <- r_sq__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_se <- r_sq_se__group
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_lb <- d_sim_ci_lb__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_ub <- d_sim_ci_ub__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_lb <- r_sq_sim_ci_lb__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_ub <- r_sq_sim_ci_ub__group
        
        # store the study info in the study_stat dataframe
        
        v$study_group <- rbind(v$study_group, data.frame(group_level = level, ref = ref, name = paste0(grouping_var, "_", level, "_reference_", ref)))
        
        # store intersection masks
        
        # TODO: pass this up through plotter for visualization
        v$brain_masks_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$mask <- intersection_mask
        
      }
    }
  }
  
  return(v)
  
  
}








#' Group Studies by Factor
#'
#' This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs)
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#' @param v A list containing effect size data
#' @param brain_masks A list containing the brain masks
#' @param combo_name A string specifying the combo to plot - # TODO: note: in app, this is saved directly in v
#' @param group_by A string to specify grouping: "orig_stat_type" or "category"
#'
#' @return An updated list with the grouped_by data
#' @export
#'
#' @examples
#' # Example usage
#' # meta_analysis(v,v$brain_masks, "pooling.none.motion.none.mv.none")
meta_analysis <- function(v, brain_masks, combo_name, grouping_var = "category") {
  
  testing <- 1
  
  # libraries & functions
  
  library(metafor)
  
  # helpers
  
  which_triangle <- function(mat) {
    if (!is.matrix(mat)) stop("Input must be a matrix")
    
    is_upper <- all(mat[lower.tri(mat)] == 0)
    is_lower <- all(mat[upper.tri(mat)] == 0)
    # note: we do not care about checking for diagonal (this function does not include diagonal)
    
    if (is_upper && is_lower) { return("both")
    } else if (is_upper) { return("upper")
    } else if (is_lower) { return("lower")
    } else { return("no_data")
    }
  }
  
  d_se <- function(d, n1, n2 = NULL) {
    if (is.null(n2)) { # one-sample
      se <- sqrt(1 / n1 + (d^2 / (2 * n1)))
    } else { # two-sample
      se <- sqrt((n1 + n2) / (n1 * n2) + (d^2 / (2 * (n1 + n2))))
    }
    return(se)
  }
  
  r_sq_se <- function(r_sq, n) {
    r <- sqrt(r_sq)
    se_r <- sqrt((1 - r^2) / (n - 2));
    se <- se_r^2
    return(se)
  }
  
  
  # initialize vars for storing grouping results
  v$data_group <- list() # store data for each stat + ref type
  v$study_group <- data.frame(group = character(0), ref = character(0), name = character(0)) # store grouping info
  v$brain_masks_group <- list()
  
  # combo_name <- names(data)[grepl(combo_name, names(v$data[[1]]))] # because some are "multi" and some "multi.r" - TODO: this isn't great - somehow it changes mv.none to mv.multi
  
  # for each level of the grouping var (e.g., statistic type, category)
  for (level in unique(v$study[[grouping_var]])) {
    
    # for each reference type
    for (ref in unique(v$study$ref)) {
      print(ref)
      matching_idx__study <- which(v$study[[grouping_var]] == level & v$study$ref == ref) # TODO: maybe explicitly add map type here
      
      if (length(matching_idx__study) > 0) {
        
        
        
        matching_names <- v$study$name[matching_idx__study]
        matching_idx__data <- which(toupper(names(v$data)) %in% toupper(matching_names))
        # idx of the studies in d that match the current stat and ref
        
        
        
        # get intersection of all masks
        
        intersection_mask <- brain_masks[[v$study$name[matching_idx__study[1]]]]$mask

        if (v$study$map_type[matching_idx__study[1]] == "act") {
          for (this_study in matching_idx__data) {
            intersection_mask <- intersection_mask & brain_masks[[v$study$name[this_study]]]$mask
          }
        }
        
        # Combine data
        
        # TODO: should actually save all results so data_group fields mirror data_group (e.g., v$data_group$d, v$data_group$r_sq, etc)
        
        # initialize
        d__group <- NULL
        d_se__group <- NULL
        r_sq__group <- NULL
        r_sq_se__group <- NULL
        
        d_sim_ci_lb__group <- NULL
        d_sim_ci_ub__group <- NULL
        r_sq_sim_ci_lb__group <- NULL
        r_sq_sim_ci_ub__group <- NULL
        
        if (length(matching_idx__data) == 1) { # NO META-ANALYSIS:
          print("not doing a meta-analysis")
          # get individual study effect size, sample size, & ci's
          
          this_study <- matching_idx__data[1]
          
          # set up n's for se calc
          
          this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
          if (!is.null(this_n_total)) { # correlation, so undefined
            this_n1 <- this_n_total/2 # TODO: check
            this_n2 <- this_n_total/2
            this_n_total <- this_n_total
          } else {
            this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
            this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
            this_n_total <- this_n1 + this_n2
          }
          
          d__group <- as.numeric(v$data[[this_study]][[combo_name]]$d) # TODO: for r2, $d -> $R2
          d_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)  # TODO: remove all references to CI if we end up using se's
          d_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)
          d_se__group <- d_se(d__group, n1 = this_n1, n2 = this_n2)
          
          r_sq__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq) # TODO: for r2, $d -> $R2
          r_sq_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
          r_sq_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)
          r_sq_se__group <- r_sq_se(r_sq__group, n = this_n_total)
          
          print("- single")
          
          
        } else { # META-ANALYSIS:
          print("doing a meta-analysis")
          # 1. Get individual study effect sizes + sample sizes
          
          # preallocate to store data across studies
          
          if (v$study$map_type[matching_idx__data[1]] == "act") {
            n_vars_intersection <- sum(intersection_mask)
          } else {
            n_vars_intersection <- sum(intersection_mask)
          }
          
          print(paste0('n_vars_intersection: ', n_vars_intersection))
          
          d__all <- matrix(0, nrow = n_vars_intersection, ncol = length(matching_idx__data))
          print(paste0("dim of d__all before allocating: ", dim(d__all)))
          d_sim_ci_ub__all <- d__all
          d_sim_ci_lb__all <- d__all
          d_se__all <- d__all
          
          r_sq__all <- d__all
          r_sq_sim_ci_ub__all <- d__all
          r_sq_sim_ci_lb__all <- d__all
          r_sq_se__all <- d__all
          
          # get data
          
          it <- 1
          for (this_study in matching_idx__data) {
            print(v$study$name[this_study])
            # get n's
            
            this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
            if (!is.null(this_n_total)) { # correlation, so undefined
              this_n1 <- this_n_total/2
              this_n2 <- this_n_total/2
              this_n_total <- this_n_total
            } else {
              this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
              this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
              this_n_total <- this_n1 + this_n2
            }
            
            this_d <- as.numeric(v$data[[this_study]][[combo_name]]$d)
            print(paste0("length of this_d: ", length(this_d)))
            this_d_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)
            this_d_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)
            
            this_r_sq <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq)
            this_r_sq_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
            this_r_sq_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)
            
            this_d_se <- d_se(this_d, n1 = this_n1, n2 = this_n2)
            this_r_sq_se <- r_sq_se(this_r_sq, n = this_n_total)
            
            # make vector for indexing that combines d_mask (1D) and intersection_mask (2D/3D)
            #   mask_of_masks is a 1D that is as long as d_mask,
            #   but only has 1's where effects exist across studies (i.e., where there are 1's in intersection_mask)
            #   -> this lets us grab effects only where they exist across studies
            
            d_mask <- brain_masks[[v$study$name[this_study]]]$mask
            mask_of_masks <- intersection_mask[d_mask == 1]
            
            this_d <- this_d[mask_of_masks]
            this_d_sim_ci_lb <- this_d_sim_ci_lb[mask_of_masks]
            this_d_sim_ci_ub <- this_d_sim_ci_ub[mask_of_masks]
            
            this_r_sq <- this_r_sq[mask_of_masks]
            this_r_sq_sim_ci_lb <- this_r_sq_sim_ci_lb[mask_of_masks]
            this_r_sq_sim_ci_ub <- this_r_sq_sim_ci_ub[mask_of_masks]
            
            this_d_se <- this_d_se[mask_of_masks]
            this_r_sq_se <- this_r_sq_se[mask_of_masks]
            
            # append to total
            
            d__all[, it] <- this_d
            d_sim_ci_lb__all[, it] <- this_d_sim_ci_lb
            d_sim_ci_ub__all[, it] <- this_d_sim_ci_ub
            d_se__all[, it] <- this_d_se
            
            r_sq__all[, it] <- this_r_sq
            r_sq_sim_ci_lb__all[, it] <- this_r_sq_sim_ci_lb
            r_sq_sim_ci_ub__all[, it] <- this_r_sq_sim_ci_ub
            r_sq_se__all[, it] <- this_r_sq_se
            
            it <- it + 1
            
          }
          
          print("done looping through studies")
          
          
          # 2. Meta analysis
          
          # preallocate to store results
          
          d__group <- numeric(dim(d__all)[1])
          d_sim_ci_lb__group <- d__group
          d_sim_ci_ub__group <- d__group
          d_se__group <- d__group
          
          r_sq__group <- d__group
          r_sq_sim_ci_lb__group <- d__group
          r_sq_sim_ci_ub__group <- d__group
          r_sq_se__group <- d__group
          
          start_time <- Sys.time()
          print(paste0("dim of d__all: ", dim(d__all)))
          for (this_variable in 1:dim(d__all)[1]) {
            if (testing) { # simple mean
              #print(d__all[this_variable,])
              #print(paste0('dims of d__group[this_variable]', dim(d__group[this_variable])))
              d__group[this_variable] <- mean(d__all[this_variable,])
              d_sim_ci_lb__group[this_variable] <- mean(d_sim_ci_lb__all[this_variable,])
              d_sim_ci_ub__group[this_variable] <- mean(d_sim_ci_ub__all[this_variable,])
              
              r_sq__group[this_variable] <- mean(r_sq__all[this_variable,])
              r_sq_sim_ci_lb__group[this_variable] <- mean(r_sq_sim_ci_lb__all[this_variable,])
              r_sq_sim_ci_ub__group[this_variable] <- mean(r_sq_sim_ci_ub__all[this_variable,])
              
            } else { # meta-analysis
              
              d_meta_analysis <- NULL
              d_meta_analysis <- rma.uni(yi = d__all[this_variable,], se = d_se__all[this_variable,], method = "REML")
              d__group[this_variable] <- d_meta_analysis$b
              d_sim_ci_lb__group[this_variable] <- d_meta_analysis$ci.lb # TODO: here and below - re-specify alpha/n_vars for corrected CI
              d_sim_ci_ub__group[this_variable] <- d_meta_analysis$ci.ub
              
              r_sq_meta_analysis <- NULL
              r_sq_meta_analysis <- rma.uni(yi = r_sq__all[this_variable,], se = r_sq_se__all[this_variable,], method = "REML")
              r_sq__group[this_variable] <- r_sq_meta_analysis$b
              r_sq_sim_ci_lb__group[this_variable] <- r_sq_meta_analysis$ci.lb
              r_sq_sim_ci_ub__group[this_variable] <- r_sq_meta_analysis$ci.ub
              
            }
            
          }
          elapsed_time <- Sys.time() - start_time
          print(elapsed_time)
        }
        
        
        # remove values from indices that are zero in d_total, sim_ci_lb_total, and sim_ci_ub_total
        
        
        # TODO: necessary? we already do the intersection mask. (do we get enough 0's to worry, esp in activation?)
        zero_idx <- which((d__group == 0) & (d_sim_ci_lb__group == 0) & (d_sim_ci_ub__group == 0))
        r_sq_zero_idx <- which((r_sq__group == 0) & (r_sq_sim_ci_lb__group == 0) & (r_sq_sim_ci_ub__group == 0))
        
        # TODO: not sure we want this, after all the masking - if so, keep a new mask
        
        if (length(zero_idx) > 0) {
          d__group <- d__group[-zero_idx]
          d_se__group <- d_se__group[-zero_idx]
          
          if (testing) {
            d_sim_ci_lb__group <- d_sim_ci_lb__group[-zero_idx]
            d_sim_ci_ub__group <- d_sim_ci_ub__group[-zero_idx]
          }
          
        }
        if (length(r_sq_zero_idx) > 0) {
          r_sq__group <- r_sq__group[-zero_idx]
          r_sq_se__group <- r_sq_se__group[-zero_idx]
          
          if (testing) {
            r_sq_sim_ci_lb__group <- r_sq_sim_ci_lb__group[-zero_idx]
            r_sq_sim_ci_ub__group <- r_sq_sim_ci_ub__group[-zero_idx]
          }
        }
        
        # store d_avg, sim_ci_lb_avg, and sim_ci_ub_avg in data_group list as a list
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$d <- d__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$se <- d_se__group
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq <- r_sq__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_se <- r_sq_se__group
        
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_lb <- d_sim_ci_lb__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_ub <- d_sim_ci_ub__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_lb <- r_sq_sim_ci_lb__group
        v$data_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_ub <- r_sq_sim_ci_ub__group
        
        # store the study info in the study_stat dataframe
        
        v$study_group <- rbind(v$study_group, data.frame(group_level = level, ref = ref, name = paste0(grouping_var, "_", level, "_reference_", ref)))
        
        # store intersection masks
        
        # TODO: pass this up through plotter for visualization
        v$brain_masks_group[[paste0(grouping_var, "_", level, "_reference_", ref)]][[combo_name]]$mask <- intersection_mask
        
      }
    }
  }
  
  return(v)
  
  
}
