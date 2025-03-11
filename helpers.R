#########################################################################
# helper function for plotting simultaneous confidence intervals: (when group_by is None)
plot_sim_ci <- function(data, name, study_details, combo_name, mv_combo_name, group_by = 'none', save = FALSE, out_path = 'output', file_name = 'plot') {
  # input:
  # data: the effect size data in the format v$d_clean[[i]] where i is an index number
  # name: the name of the study in the format names(v$d_clean[i])
  # study_details: a list of study details in the format v$study[i,]
  # combo_name: the name of the combo to plot as a string, e.g., paste0('pooling.', input$spatial_scale, '.motion.', input$motion, '.mv.none')
  # mv_combo_name: the name of the multivariate combo to plot as a string, e.g., paste0('pooling.', input$spatial_scale, '.motion.', input$motion, '.mv.multi') #TODO: this could be simplified
  # group_by: the variable to group by, either 'none', 'orig_stat_type', or 'category'
  
  if (save) {
    out_name = paste0(out_path, '/', file_name)
    png(out_name)
  }
  
  # find the full combo name for this multivariate test #TODO: fix the code that creates the data to assign the rest test statistic to the combos
  full_mv_combo_name <- names(data)[grepl(mv_combo_name, names(data))]

  # remove na
  na_idx <- is.na(data[[combo_name]]$d) | is.na(data[[combo_name]]$sim_ci_lb) | is.na(data[[combo_name]]$sim_ci_ub)
  data[[combo_name]]$d <- data[[combo_name]]$d[!na_idx]
  data[[combo_name]]$sim_ci_lb <- data[[combo_name]]$sim_ci_lb[!na_idx]
  data[[combo_name]]$sim_ci_ub <- data[[combo_name]]$sim_ci_ub[!na_idx]

  # unlist sim CIs if list
  if (is.list(data[[combo_name]]$sim_ci_lb)) {
    data[[combo_name]]$sim_ci_lb <- unlist(data[[combo_name]]$sim_ci_lb)
    data[[full_mv_combo_name]]$sim_ci_lb <- unlist(data[[full_mv_combo_name]]$sim_ci_lb)
  }
  if (is.list(data[[combo_name]]$sim_ci_ub)) {
    data[[combo_name]]$sim_ci_ub <- unlist(data[[combo_name]]$sim_ci_ub)
    data[[full_mv_combo_name]]$sim_ci_ub <- unlist(data[[full_mv_combo_name]]$sim_ci_ub)
  }

  # sort data from smallest to largest d
  sorted_indices <- order(data[[combo_name]]$d)
  sorted_d <- data[[combo_name]]$d[sorted_indices]
  # sort confidence intervals by the same order
  sorted_upper_bounds <- data[[combo_name]]$sim_ci_ub[sorted_indices]
  sorted_lower_bounds <- data[[combo_name]]$sim_ci_lb[sorted_indices]
  
  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 100
  if (downsample < 1) {
    downsample = 1
  }
  sorted_d <- sorted_d[seq(1, length(sorted_d), by = downsample)]
  sorted_upper_bounds <- sorted_upper_bounds[seq(1, length(sorted_upper_bounds), by = downsample)]
  sorted_lower_bounds <- sorted_lower_bounds[seq(1, length(sorted_lower_bounds), by = downsample)]
  
  
  # for coloring of confidence intervals:
  below_zero <- sorted_upper_bounds < 0
  below_cross_idx <- which(diff(below_zero) == -1) + 1# the last TRUE before switch
  
  above_zero <- sorted_lower_bounds > 0
  above_cross_idx <- (which(diff(above_zero) == 1)) + 1 # the last FALSE before switch to true
  
  if (group_by == 'none') {
    n_title <- paste0("n = ", data[[combo_name]]$n)
  }
  
  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  percent_not_zero = percent_below_zero + percent_above_zero
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
  
  # plot a line for d
  par(mar=c(3, 4, 5, 2))
  plot(sorted_d, type = "l", ylim = c(min(sorted_lower_bounds, na.rm = TRUE), max(sorted_upper_bounds, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  if (group_by == 'none') {
          # legend = c(
          #   bquote(bold("Dataset:")), 
          #   paste(study_details$dataset, "  "),
          #   bquote(bold("Map:")), 
          #   paste(study_details$map_type, "  "),
          #   bquote(bold("Sample Size:")),
          #   paste(n_title),
          #   bquote(bold("Test:")), 
          #   paste(study_details$orig_stat_type, ": ", study_details$test_component_1, ", ", study_details$test_component_2)
          # ),

          legend("topleft", inset = c(-0.1, -0.5), 
            legend = c(bquote(bold("Test: ") ~ .(study_details$orig_stat_type) ~ ": " ~ .(study_details$test_component_1) ~ ", " ~ .(study_details$test_component_2)),
                      bquote(bold("Dataset: ") ~ .(study_details$dataset)), 
                      "",
                      bquote(bold("Map: ") ~ .(study_details$map_type)),
                      "",
                      bquote(bold("Sample Size: ") ~ .(n_title))),
                      col = 2, bty = "n", cex = 1, text.width = 25, xpd = TRUE, ncol = 3)

  } else if (group_by == "orig_stat_type") {
    legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Statistic:")), 
         paste(study_details$group, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  } else if (group_by == "category") {
    legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Phenotype Category:")), 
         paste(study_details$group, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  }
  max_cons_effect = ifelse((abs(max(data[[combo_name]]$sim_ci_lb, na.rm = TRUE)) > abs(min(data[[combo_name]]$sim_ci_ub, na.rm = TRUE))), 
                                                              ifelse((max(data[[combo_name]]$sim_ci_lb, na.rm = TRUE) > 0),
                                                                     round(abs(max(data[[combo_name]]$sim_ci_lb, na.rm = TRUE)), 2), 0),
                                                              ifelse((min(data[[combo_name]]$sim_ci_ub, na.rm = TRUE) < 0), round(abs(min(data[[combo_name]]$sim_ci_ub, na.rm = TRUE)), 2), 0))

  if (group_by == 'none') {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(percent_not_zero * 100, 1)) ~ "%"),
                                                          bquote(bold("Multivariate effect size: ") ~.(round(data[[full_mv_combo_name]]$d, 2)) ~ "  [" ~.(round(data[[full_mv_combo_name]]$sim_ci_lb, 2)) ~ ", " ~.(round(data[[full_mv_combo_name]]$sim_ci_ub, 2)) ~ "]")), col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  } else {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(percent_not_zero * 100, 1)) ~ "%")),
                                                          col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  }
 
  
  # plot and shade the cofidence intervals:
  # green for intervals that are entirely below zero
  polygon(c(1:below_cross_idx, rev(1:below_cross_idx)), 
          c(sorted_upper_bounds[1:below_cross_idx], rev(sorted_lower_bounds[1:below_cross_idx])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  # red for intervals that include zero
  polygon(c(below_cross_idx:above_cross_idx, rev(below_cross_idx:above_cross_idx)), 
          c(sorted_upper_bounds[below_cross_idx:above_cross_idx], rev(sorted_lower_bounds[below_cross_idx:above_cross_idx])), 
          col = rgb(237/255, 185/255, 185/255, alpha = 0.5), border = NA)
  
  # green for intervals that are entirely above zero
  polygon(c(above_cross_idx:length(above_zero), rev(above_cross_idx:length(above_zero))), 
          c(sorted_upper_bounds[above_cross_idx:length(above_zero)], rev(sorted_lower_bounds[above_cross_idx:length(above_zero)])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  if (save) {
    dev.off()
  }
}

#########################################################################

#### Plot full FC matrix given a triangle:

plot_full_mat <- function(triangle_ordered, pooled = FALSE, mapping_path = NA, rearrange = TRUE, save = FALSE, out_path = 'output', plot_name = 'matrix.png') {
    # takes an ordered triangle vector (without NAs) and plots the full matrix
    
    #TODO: look into heatmaply package for plotly interactive heatmap!
    # https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html
  
    if (save) {
      out_name = paste0(out_path, "/", plot_name)
    }
    
    if (!is.na(mapping_path)) {
    # load mapping
        mapping <- read.csv(mapping_path, header = TRUE)
        
    }

    # if the data is pooled, the number of nodes is determined from the map
    if (pooled) {
      nrow = length(unique(mapping$category))
    } else {
      nrow = (((-1 + sqrt(1 + 8 * length(triangle_ordered))) / 2) + 1)
    }

    # mirror the triangle across the x = y line to get full matrix
    # first fill in half the matrix with the triangle data
    mat <- matrix(0, nrow = nrow, ncol = nrow)
    mat[upper.tri(mat, diag = ifelse(pooled, TRUE, FALSE))] <- triangle_ordered
    full_mat <- mat + t(mat) #- diag(diag(triangle_ordered))

    # rearrange if necessary
    if (rearrange) {
      full_mat <- full_mat[mapping$oldroi, mapping$oldroi]
    }

    # melt the matrix for ggplot
    melted <- melt(full_mat)
    colnames(melted) <- c("Var1", "Var2", "value")
    
    # determine the title of the plot based on the number of nodes
    plot_title = ifelse((nrow == 268 | nrow == 10), "Studies with Shen 268 node atlas", ifelse(nrow == 55, "Studies with UKB 55 nodes", "Studies with unknown parcellation"))

    heatmap_plot <- ggplot(melted, aes(Var1, Var2, fill = value)) +

    labs(fill = "Cohen's d", 
          title = plot_title,
           x = "", y = "") +
      
      geom_tile() +
      scale_fill_gradient2(limits = c(min(melted$value), max(melted$value)),
        low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.margin = margin(.5, .5, .5, .5, "lines"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

    if (!is.na(mapping_path)) {
      if (!pooled) {
        
        for (i in 1:(nrow(mapping) - 1)) {
            if (mapping$category[i] != mapping$category[i + 1]) {
            heatmap_plot <- heatmap_plot + geom_vline(xintercept = i + 0.5, color = "black", size = 0.3) +
                geom_hline(yintercept = i+0.5, color = "black")
            }
        }
        
        # Calculate the positions of the labels
        label_positions <- c(1, which(mapping$category[-1] != mapping$category[-length(mapping$category)]) + 1, length(mapping$category) + 1)
        label_positions <- (label_positions[-1] + label_positions[-length(label_positions)]) / 2
        label_strings <- mapping$label[label_positions]
        
        # Add labels to each mapping category
        heatmap_plot <- heatmap_plot + annotate("text", x = label_positions, y = -6, label = label_strings, angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
        heatmap_plot <- heatmap_plot + annotate("text", x = -10, y = label_positions, label = label_strings, angle = 0, hjust = 0.5, vjust=1, size=3.5)
      }

      if (pooled) {
        # for pooled data, add black lines to separate every cell of the matrix
        # label each row and column as the networks
        for (i in 1:(nrow)) {
          heatmap_plot <- heatmap_plot + geom_vline(xintercept = i+0.5, color = "black", size = 0.3) +
            geom_hline(yintercept = i+0.5, color = "black")

          heatmap_plot <- heatmap_plot + annotate("text", x = i, y = -1, label = unique(mapping$label)[i], angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
          heatmap_plot <- heatmap_plot + annotate("text", x = -1, y = i, label = unique(mapping$label)[i], angle = 0, hjust = 0.5, vjust=1, size=3.5)
        }
      }
    }
    
        
        # Add axis labels to the heatmap
        if (!is.na(mapping_path)) {
          heatmap_plot <- heatmap_plot + labs(x = "Network", y = "Network")
        } else if (is.na(mapping_path)) {
          heatmap_plot <- heatmap_plot + labs(x = "UKB 55 Node", y = "UKB 55 Node")
        }
    
    if (save) {
      ggsave(out_name)
    }
    
    return(heatmap_plot)
    
    
}


#########################################################################
# Plotting brains
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
create_nifti <- function(nifti_template, data, study_name, combo_name, brain_masks, out_path = '/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/', export = FALSE) {
  # INPUTS:
  # - study_name: string, the name of the study to use the mask from
  # - brain_masks: list of brain masks (from combined_gl output)
  # - out_path: path to save the nifti template to
  
  # OUTPUTS:
  # - template: nifti file that contains the mask for the given study

  # template <- readNIfTI('/Users/neuroprism/Desktop/effect_size_shiny_neurohack/data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz', read_data = FALSE)
  
  structured <- brain_masks[[study_name]]$mask
  
  new_data <- data[[study_name]][[combo_name]]$d
  
  structured[structured==1] <- new_data[1,]
  
  nifti_template@.Data <- structured
  
  if (export) {
    writeNIfTI(nifti_template, paste0(out_path, study_name))
  }
  return(nifti_template)
}


# plot the nifti
plot_brain <- function(nifti, anatomical, x, y, z) {
  nifti[nifti == 0] <- NA
  nifti[nifti > 0.1] <- 0.1
  nifti[nifti < -0.1] <- -0.1
  
  par(mar = c(1, 1, 1, 4))
  ortho2(
    x = anatomical,
    y = nifti,
    crosshairs = TRUE,
    bg = 'white',
    NA.x = TRUE,
    col.y = colorspace::diverge_hsv(30),
    xyz = c(x, y, z),
    text.color = 'black',
    #clabels = seq(-0.1, 0.1, length.out = 30),
    ybreaks = seq(-0.1, 0.1, length.out = 31),
    ycolorbar = TRUE,
    mfrow = c(3, 1)
  )

  min_val = -0.1
  max_val = 0.1
  num_breaks = 31
  breaks = seq(min_val, max_val, length.out = num_breaks)
  labels = round(seq(min_val, max_val, length.out = num_breaks-1), 2)
  
  colorbar(
    breaks = breaks,
    col = colorspace::diverge_hsv(30),
    labels = labels,
    text.col = 'black'
  )
}



### IN PROGRESS:

plotly_full_mat <- function(triangle_ordered, pooled = FALSE, mapping_path = NA) {

    if (!is.na(mapping_path)) {
    # load mapping
        mapping <- read.csv(mapping_path, header = TRUE)
    }

    # if the data is pooled, the number of nodes is determined from the map
    if (pooled) {
      nrow = length(unique(mapping$category))
    } else {
      nrow = (((-1 + sqrt(1 + 8 * length(triangle_ordered))) / 2) + 1)
    }

    # mirror the triangle across the x = y line to get full matrix
    # first fill in half the matrix with the triangle data
    mat <- matrix(0, nrow = nrow, ncol = nrow)
    mat[upper.tri(mat, diag = ifelse(pooled, TRUE, FALSE))] <- triangle_ordered
    full_mat <- mat + t(mat) #- diag(diag(triangle_ordered))

    # transpose then rotate the matrix counterclockwise 90 degrees
    # rotate <- function(x) t(apply(x, 2, rev))
    # full_mat <- rotate(rotate(rotate(full_mat)))
    # full_mat <- t(full_mat)
    # # reflect along the y = 0 line
    # full_mat <- full_mat[, nrow(full_mat):1]
    

    # Reverse the category labels for rows and columns to match the transposed matrix
    transposed_row_colors <- rev(mapping$category)
    transposed_col_colors <- rev(mapping$category)

    # # melt the matrix for ggplot
    # melted <- melt(full_mat)
    # colnames(melted) <- c("Var1", "Var2", "value")
    
    # determine the title of the plot based on the number of nodes
    plot_title = ifelse((nrow == 268 | nrow == 10), "Studies with Shen 268 node atlas", ifelse(nrow == 55, "Studies with UKB 55 nodes", "Studies with unknown parcellation"))

    category_boundaries <- which(diff(as.numeric(as.factor(mapping$category))) != 0)

    # takes a full matrix
    heatmap_plot <- heatmaply(full_mat, xlab = "Node", ylab = "Node", main = plot_title, 
            dendrogram = "none",
            show_dendrogram = c(FALSE,FALSE),
            showticklabels = c(FALSE,FALSE),
            row_side_colors = rev(mapping$category),
            col_side_colors = rev(mapping$category),
            #colorbar_thickness = 10,
            limits = c(min(full_mat), max(full_mat)),
            col = colorRampPalette(c("blue", "white", "red"))(100),
            plot_method = 'ggplot',
            subplot_widths = c(0.9, 0.1),
            symm = TRUE,
            colorbar_xanchor = "left",
            colorbar_yanchor = "bottom"
            #col_side_colors=data.frame(a=seq_len(length(category_boundaries))),
            #heatmap_layers = list(geom_vline(xintercept = category_boundaries[1] - 0.5, color = "black", size = 20)))
                #lapply(seq_len(length(category_boundaries)+1), function(i) geom_hline(yintercept = category_boundaries[i] - 0.5, color = "black", size = 3))
    )
    # heatmap_plot <- heatmap_plot %>%
    #     add_segments(x = 5, xend = 5, y = 0.5, yend = 260, line = list(color = "black")) %>%
    #     add_lines(x = c(0, nrow), y = category_boundaries[1], line = list(color = "black", width = 2))

    # heatmap_plot$x$layout$shapes <- list(
    #     if (is.null(p$x$layout$shapes)) list() else p$x$layout$shapes,
    #     list(
    #         type = "line",
    #         x0 = 0.5,
    #         x1 = nrow + 0.5,
    #         y0 = category_boundaries[1] - 0.5,
    #         y1 = category_boundaries[1] - 0.5,
    #         line = list(color = "black", width = 2)
    #     )
    # )

shapes_list <- if (is.null(heatmap_plot$x$layout$shapes)) list() else heatmap_plot$x$layout$shapes

# Loop through condition_boundaries to add lines for each boundary
for (boundary in category_boundaries) {
  boundary_pos <- boundary + 0.5  # Adjust boundary position for the lines

  # Add the horizontal and vertical lines at the boundary
  shapes_list <- append(shapes_list, list(
    list(
      type = "line",
      x0 = 0.5,
      x1 = nrow(full_mat) + 0.5,
      y0 = boundary_pos,
      y1 = boundary_pos,
      xref = "y1",
      yref = "y2",
      line = list(color = "black", width = 1)
    ),
    list(
      type = "line",
      x0 = boundary_pos,
      x1 = boundary_pos,
      y0 = 0.5,
      y1 = ncol(full_mat) + 0.5,
      xref = "y1",
      yref = "y2",
      line = list(color = "black", width = 1)
    )
  ))
}

# Update the plot layout with the new shapes
heatmap_plot$x$layout$shapes <- shapes_list

return(heatmap_plot)

}


#########################################################################
# helper functions for saving brain plots

# export_handlers.R

exportDownloadData <- function(output, v) {
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("EffeX_data", ".RData", sep="")
    },
    content = function(file) {
      saveRDS(v$d_clean, file)
    }
  )
}

exportDownloadBrain <- function(output, v, input, anatomical) {
  output$downloadBrain <- downloadHandler(
    filename = function() {
      paste("Effex_brain", ".png", sep="")
    },
    content = function(file) {
      png(file)
      plot_brain(v$nifti, anatomical, input$xCoord, input$yCoord, input$zCoord)
      dev.off()
    }
  )
}

exportDownloadPlots <- function(output, v, input) {
  output$downloadPlots <- downloadHandler(
    filename = function() {
      paste("Effex_plots", ".zip", sep="")
    },
    content = function(file) {
      tmpdir <- tempdir()
      v$plot_files <- c()
      
      # Save plots as PNGs
      if (input$group_by == "none" && length(v$d_clean) > 0) {
        for (i in 1:length(v$d_clean)) {
          plotname <- paste0(names(v$d_clean[i]), '.png')
          plotpath <- file.path(tmpdir, plotname)
          
          plot_sim_ci(
            v$d_clean[[i]], names(v$d_clean[i]), v$study[i,],
            combo_name = v$combo_name, mv_combo_name = v$mv_combo_name,
            group_by = input$group_by, save = TRUE, out_path = tmpdir, file_name = plotname
          )
          
          if (file.exists(plotpath)) {
            v$plot_files <- c(v$plot_files, plotpath)
          }
        }
      } else if (length(v$d_group) > 0) {
        for (i in 1:length(v$d_group)) {
          plotname <- paste0(names(v$d_group[i]), '.png')
          plotpath <- file.path(tmpdir, plotname)
          
          plot_sim_ci(v$d_group[[i]], names(v$d_group[i]), v$study_group[i,],
                      combo_name = v$combo_name, mv_combo_name = v$mv_combo_name,
                      group_by = input$group_by, save = TRUE, out_path = tmpdir, file_name = plotname
          )
          
          if (file.exists(plotpath)) {
            v$plot_files <- c(v$plot_files, plotpath)
          }
        }
      }
      
      zip(file, v$plot_files, flags = "-j")
    },
    contentType = "application/zip"
  )
}

exportDownloadMatrices <- function(output, v, input) {
  output$downloadMatrices <- downloadHandler(
      filename = function() {
        paste("Effex_matrices", ".zip", sep="")
      },
      content = function(file) {
        tmpdir <- tempdir()
         message("Temporary directory: ", tmpdir)

        t_total_268 <- rep(0, 35778) 
        t_total_268_pooled <- rep(0, 55)
        t_total_55 <-  rep(0 , 1485)

      n_268_studies <- 0 # initialize count of studies that use the 268 node parcellation
      n_268_studies_pooled <- 0
      n_55_studies <- 0 # initialize count of studies that use the 55 node parcellation
 
      for (i in 1:length(v$d_clean_fc)) {
        t <- v$d_clean_fc[[i]][[v$combo_name]]$d

        study_idx <- which(toupper(v$study_fc$name) == toupper(names(v$d_clean_fc)[i]))
        if (v$study_fc$ref[study_idx] == "shen_268"){ 
          
          if (input$spatial_scale == "net") {
            t_total_268_pooled <- t_total_268_pooled + t
            n_268_studies_pooled <- n_268_studies_pooled + 1
          }
          else {
            #print(c(dim(t), v$study_fc$name[study_idx]))
            
            t_total_268 <- t_total_268 + t
            n_268_studies <- n_268_studies + 1
          }
        }
        
        else if (v$study_fc$ref[study_idx] == "ukb_55") {
          
          # add the data to the total vector
          t_total_55 <- t_total_55 + t

          n_55_studies <- n_55_studies + 1
        }
      }

      # if d_clean_fc is longer than 1, find the average of the matrices
      if (n_268_studies > 1) {
        t_avg_268 <- t_total_268 / n_268_studies
      }

      if (n_268_studies == 1 | n_268_studies == 0) {
        t_avg_268 <- t_total_268
      }

      if (n_268_studies_pooled > 1) {
        t_avg_268_pooled <- t_total_268_pooled / n_268_studies_pooled
      }

      if (n_268_studies_pooled == 1 | n_268_studies_pooled == 0) {
        t_avg_268_pooled <- t_total_268_pooled
      }

      if (n_55_studies > 1) {
        t_avg_55 <- t_total_55 / n_55_studies
      }

      if (n_55_studies == 1 | n_55_studies == 0) {
        t_avg_55 <- t_total_55
      }

        plot_files <- c()

        if (n_268_studies > 0) {
          plotpath <- file.path(tmpdir, 'Shen_matrix.png')
          plot_full_mat(t_avg_268, rearrange = TRUE, mapping_path = "data/parcellations/map268_subnetwork.csv", save = TRUE, out_path = tmpdir, plot_name = 'Shen_matrix.png')
          if (file.exists(plotpath)) {
            plot_files <- c(plot_files, plotpath)
          } else {
            message("Plot not found: ", plotpath)  # Debugging: Check if the plot was saved
        }
        }

        # only plot the 268 pooled plot if n_268_studies_pooled > 0
        if (n_268_studies_pooled > 0) {
          plotpath <- file.path(tmpdir, 'Shen_matrix_pooled.png')
          plot_full_mat(t_avg_268_pooled, rearrange = FALSE, pooled = TRUE, mapping_path = "data/parcellations/map268_subnetwork.csv", save = TRUE, out_path = tmpdir, plot_name = 'Shen_matrix_pooled.png')
          if (file.exists(plotpath)) {
            plot_files <- c(plot_files, plotpath)
          } else {
            message("Plot not found: ", plotpath)  # Debugging: Check if the plot was saved
          }
        }
       
        # only plot the 55 plot if n_55_studies > 0
        if (n_55_studies > 0) {
          plotpath <- file.path(tmpdir, 'UKB_matrix.png')
          plot_55 <- plot_full_mat(t_avg_55, rearrange = TRUE, mapping_path = "data/parcellations/map55_ukb.csv", save = TRUE, out_path = tmpdir, plot_name = 'UKB_matrix.png')
          if (file.exists(plotpath)) {
            plot_files <- c(plot_files, plotpath)
          } else {
            message("Plot not found: ", plotpath)  # Debugging: Check if the plot was saved
          }
        }
        
        
        # Zip all the saved plot files
        if (length(plot_files) > 0) {
          zip(file, plot_files, flags = "-j")  # -j flag to ignore folder structure
      } else {
        message("No plots to zip")
      }
      },
      contentType = "application/zip"
    )
}
