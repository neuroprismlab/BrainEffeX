#########################################################################
# helper function for plotting simultaneous confidence intervals:
plot_sim_ci <- function(data, name, study_details) {

  # remove na
  na_idx <- is.na(data$d) | is.na(data$sim_ci_lb) | is.na(data$sim_ci_ub)
  data$d <- data$d[!na_idx]
  data$sim_ci_lb <- data$sim_ci_lb[!na_idx]
  data$sim_ci_ub <- data$sim_ci_ub[!na_idx]
  # sort data from smallest to largest d
  sorted_indices <- order(data$d)
  sorted_d <- data$d[sorted_indices]
  # sort confidence intervals by the same order
  sorted_upper_bounds <- data$sim_ci_ub[sorted_indices]
  sorted_lower_bounds <- data$sim_ci_lb[sorted_indices]

  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 100
  if (downsample == 0) {
    downsample = 1
  }
  sorted_d <- sorted_d[seq(1, length(sorted_d), by = downsample)]
  sorted_upper_bounds <- sorted_upper_bounds[seq(1, length(sorted_upper_bounds), by = downsample)]
  sorted_lower_bounds <- sorted_lower_bounds[seq(1, length(sorted_lower_bounds), by = downsample)]

  
  # for coloring of confidence intervals:
  below_zero <- sorted_upper_bounds < 0
  below_cross_idx <- which(diff(below_zero) == -1) # the last TRUE before switch
  
  above_zero <- sorted_lower_bounds > 0
  above_cross_idx <- (which(diff(above_zero) == 1)) + 1 # the last FALSE before switch to true

  # get n for title of plot
  # if the study is a correlation, or one sample t test, the n is n
  # if (grepl("_r_", name) | grepl("_fc_t_", name) | grepl("_act_t_", name) | grepl("_d_", name)) {
  #   n_title <- paste0("n = ", data$n)
  # } 
  
  if (study_details$orig_stat_type == "r" | study_details$orig_stat_type =="t" | study_details$orig_stat_type == "d") {
    n_title <- paste0("n = ", data$n)
  } 
  
  # if the study is a two-way t-test, then we need n1 and n2, but we'll make the n variable include both in a string
  if (study_details$orig_stat_type == "t2") {
    n_title <- paste0("n1 = ", data$n1, ", n2 = ", data$n2)
  }

  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  

 
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
 
  # plot a line for d
  par(mar=c(2, 4, 5, 2))
  plot(sorted_d, type = "l", ylim = c(min(sorted_lower_bounds, na.rm = TRUE), max(sorted_upper_bounds, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Dataset:")), 
         paste(study_details$dataset, "  "),
         bquote(bold("Map Type:")), 
         paste(study_details$map_type, "  "),
         bquote(bold("Test type:")), 
         paste(study_details$orig_stat_type, "  "),
         bquote(bold("Var1:")), 
         paste(study_details$var1, "  "),
         bquote(bold("Var2:")), 
         paste(study_details$var2, "  "),
         bquote(bold("Sample Size:")),
         paste(n_title)
       ), 
       bty = "n", ncol = 6, cex = 1, text.width = c(15, 15, 15, 15, 25, 20), x.intersp = 0.0, xpd = TRUE)
  legend("bottomright", inset = c(0, -0.2), legend = c(bquote(bold("Maximum conservative effect size: ")), 
                                   ifelse((abs(max(data$sim_ci_lb, na.rm = TRUE)) > abs(min(data$sim_ci_ub, na.rm = TRUE))), 
                                          ifelse((max(data$sim_ci_lb, na.rm = TRUE) > 0),
                                          round(abs(max(data$sim_ci_lb, na.rm = TRUE)), 2), 0),
                                          ifelse((min(data$sim_ci_ub, na.rm = TRUE) < 0), round(abs(min(data$sim_ci_ub, na.rm = TRUE)), 2), 0))), xjust = 1, yjust = 1, col = 2, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)





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
}


##### plotting the confidence intervals for group_by == statistic:
plot_sim_ci_stat <- function(data, name, study_details) {

  # remove na
  na_idx <- is.na(data$d_avg) | is.na(data$ci_lb_avg) | is.na(data$ci_ub_avg)
  data$d_avg <- data$d_avg[!na_idx]
  data$ci_lb_avg <- data$ci_lb_avg[!na_idx]
  data$ci_ub_avg <- data$ci_ub_avg[!na_idx]
  # sort data from smallest to largest d
  sorted_indices <- order(data$d_avg)
  sorted_d <- data$d_avg[sorted_indices]
  # sort confidence intervals by the same order
  sorted_upper_bounds <- data$ci_ub_avg[sorted_indices]
  sorted_lower_bounds <- data$ci_lb_avg[sorted_indices]

  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 100
  if (downsample == 0) {
    downsample = 1
  }
  sorted_d <- sorted_d[seq(1, length(sorted_d), by = downsample)]
  sorted_upper_bounds <- sorted_upper_bounds[seq(1, length(sorted_upper_bounds), by = downsample)]
  sorted_lower_bounds <- sorted_lower_bounds[seq(1, length(sorted_lower_bounds), by = downsample)]

  
  # for coloring of confidence intervals:
  below_zero <- sorted_upper_bounds < 0
  below_cross_idx <- which(diff(below_zero) == -1) # the last TRUE before switch
  
  above_zero <- sorted_lower_bounds > 0
  above_cross_idx <- (which(diff(above_zero) == 1)) + 1 # the last FALSE before switch to true

  # get n for title of plot
  # if the study is a correlation, or one sample t test, the n is n
  # if (grepl("_r_", name) | grepl("_fc_t_", name) | grepl("_act_t_", name) | grepl("_d_", name)) {
  #   n_title <- paste0("n = ", data$n)
  # } 
  
  # if (study_details$orig_stat_type == "r" | study_details$orig_stat_type =="t" | study_details$orig_stat_type == "d") {
  #   n_title <- paste0("n = ", data$n)
  # } 
  
  # if the study is a two-way t-test, then we need n1 and n2, but we'll make the n variable include both in a string
  # if (study_details$orig_stat_type == "t2") {
  #   n_title <- paste0("n1 = ", data$n1, ", n2 = ", data$n2)
  # }

  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  

 
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
 
  # plot a line for d
  par(mar=c(2, 4, 5, 2))
  plot(sorted_d, type = "l", ylim = c(min(sorted_lower_bounds, na.rm = TRUE), max(sorted_upper_bounds, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  legend("topleft", #inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Statistic:")), 
         paste(study_details$stat_type, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  legend("bottomright", inset = c(0, -0.2), legend = c(bquote(bold("Maximum conservative effect size: ")), 
                                   ifelse((abs(max(data$ci_lb_avg, na.rm = TRUE)) > abs(min(data$ci_ub_avg, na.rm = TRUE))), 
                                          ifelse((max(data$ci_lb_avg, na.rm = TRUE) > 0),
                                          round(abs(max(data$ci_lb_avg, na.rm = TRUE)), 2), 0),
                                          ifelse((min(data$ci_ub_avg, na.rm = TRUE) < 0), round(abs(min(data$ci_ub_avg, na.rm = TRUE)), 2), 0))), xjust = 1, yjust = 1, col = 2, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)





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
}

#########################################################################

#### Plot full FC matrix given a triangle:


plot_full_mat <- function(triangle_ordered, mapping_path = NA) {
    # takes an ordered triangle vector (without NAs) and plots the full matrix
    
    nrow = (((-1 + sqrt(1 + 8 * length(triangle_ordered))) / 2) + 1)

    # mirror the triangle across the x = y line to get full matrix
    # first fill in half the matrix with the triangle data
    mat <- matrix(0, nrow = nrow, ncol = nrow)
    mat[upper.tri(mat)] <- triangle_ordered
    full_mat <- mat + t(mat) #- diag(diag(triangle_ordered))

    # melt the matrix for ggplot
    melted <- melt(full_mat)
    colnames(melted) <- c("Var1", "Var2", "value")

    heatmap_plot <- ggplot(melted, aes(Var1, Var2, fill = value)) +
      
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
        # load mapping
        mapping <- read.csv(mapping_path, header = TRUE)

        
        for (i in 1:(nrow(mapping) - 1)) {
            if (mapping$category[i] != mapping$category[i + 1]) {
            heatmap_plot <- heatmap_plot + geom_vline(xintercept = i, color = "black", size = 0.3) +
                geom_hline(yintercept = i, color = "black")
            }
        }
        
        # Calculate the positions of the labels
        label_positions <- c(1, which(mapping$category[-1] != mapping$category[-length(mapping$category)]) + 1, length(mapping$category) + 1)
        label_positions <- (label_positions[-1] + label_positions[-length(label_positions)]) / 2
        label_strings <- mapping$label[label_positions]
        
        # Add labels to each mapping category
        heatmap_plot <- heatmap_plot + annotate("text", x = label_positions, y = -6, label = label_strings, angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
        heatmap_plot <- heatmap_plot + annotate("text", x = -10, y = label_positions, label = label_strings, angle = 0, hjust = 0.5, vjust=1, size=3.5)

        # Add axis labels to the heatmap
        heatmap_plot <- heatmap_plot + labs(x = "Network", y = "Network")
    }
    return(heatmap_plot)
}


#### Plot FC Matrix:

# input: a square map (as a numeric vector) (the output from triangle_to_matrix function)
# output: a plot of the square map

plot_matrix <- function(square_map, mapping_file_path = NA, reorder = TRUE) {
  
  # read in the mapping file if mapping_file_path is not FALSE
  if (!is.na(mapping_file_path)) {
    mapping <- read.csv(mapping_file_path, header = TRUE)
  }

  # check that the map is a full matrix (square):
  if (sqrt(length(square_map)) %% 1 == 0) {
    # convert to a matrix
    n_nodes <- sqrt(length(square_map))
    mat <- matrix(data = square_map, nrow = n_nodes, ncol = n_nodes)

    # Reorder data if needed (typically not reordered) - Order the rows and columns of the connectivity matrix according to the mapping
    if (reorder & (!is.na(mapping_file_path))) {
      ordered_matrix <- mat[mapping$oldroi, mapping$oldroi]
    } else {
      ordered_matrix <- mat
    }

    # melt ordered_matrix for ggplot
    ordered_matrix <- melt(ordered_matrix)
    colnames(ordered_matrix) <- c("Var1", "Var2", "value")

    # Create a heatmap of the connectivity matrix
    # heatmap_plot <- ggcorrplot(ordered_matrix) + # an alternative to the below line
    heatmap_plot <- ggplot(ordered_matrix, aes(Var1, Var2, fill = value)) +
      # set maximum and mimimum values for the color scale as max and min values of the matrix
      labs(fill = "Cohen's d", 
           title = ifelse(n_nodes == 268, "Studies with Shen 268 node atlas", ifelse(n_nodes == 55, "Studies with UKB 55 nodes", "Studies with unknown parcellation")),
           x = "", y = "") +
      
      geom_tile() +
      scale_fill_gradient2(limits = c(min(ordered_matrix$value), max(ordered_matrix$value)),
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
    
    # if mapping is not false, add the labels to the heatmap
    if (!is.na(mapping_file_path)) {
      # Draw lines on the heatmap to represent the boundaries
      for (i in 1:(nrow(mapping) - 1)) {
        if (mapping$category[i] != mapping$category[i + 1]) {
          heatmap_plot <- heatmap_plot + geom_vline(xintercept = i, color = "black", size = 0.3) +
            geom_hline(yintercept = i, color = "black")
        }
      }
      
      # Calculate the positions of the labels
      label_positions <- c(1, which(mapping$category[-1] != mapping$category[-length(mapping$category)]) + 1, length(mapping$category) + 1)
      label_positions <- (label_positions[-1] + label_positions[-length(label_positions)]) / 2
      label_strings <- mapping$label[label_positions]
      
      # Add labels to each mapping category
      heatmap_plot <- heatmap_plot + annotate("text", x = label_positions, y = -6, label = label_strings, angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
      heatmap_plot <- heatmap_plot + annotate("text", x = -10, y = label_positions, label = label_strings, angle = 0, hjust = 0.5, vjust=1, size=3.5)

      # Add axis labels to the heatmap
      heatmap_plot <- heatmap_plot + labs(x = "Network", y = "Network")
      # adjust the location of the x and y axis labels

    }

    # if mapping file is not provided, add labels to the heatmap as numbers of nodes, only labeling the first node, and every 10th node, and the last node
    if (is.na(mapping_file_path)) {
      heatmap_plot <- heatmap_plot + annotate("text", x = seq(1, n_nodes, by = 10), y = -1, label = seq(1, n_nodes, by = 10), angle = 90, hjust = 1, vjust=0.5, size=3.5)
      heatmap_plot <- heatmap_plot + annotate("text", x = -1, y = seq(1, n_nodes, by = 10), label = seq(1, n_nodes, by = 10), angle = 0, hjust = 0.5, vjust=1, size=3.5)
      heatmap_plot <- heatmap_plot + annotate("text", x = n_nodes, y = -1, label = n_nodes, angle = 90, hjust = 1, vjust=0.5, size=3.5)
      heatmap_plot <- heatmap_plot + annotate("text", x = -1, y = n_nodes, label = n_nodes, angle = 0, hjust = 0.5, vjust=1, size=3.5)
    
    # also add axis labels if mapping file is not provided saying "Node number"
    heatmap_plot <- heatmap_plot + labs(x = "Node number", y = "Node number")
    }

    # Return the heatmap plot
    return(heatmap_plot)

  } else if ((((-1 + sqrt(1 + 8 * length(square_map))) / 2) + 1) %% 1 == 0) {
    # else if the map is half a matrix (triangle): 
    stop("The map is not a square matrix.")
  }
}