### Calculate simultaneous confidence intervals from Cohen's d

# load data
d_clean <- readRDS('/Users/neuroprism/Library/CloudStorage/
                              GoogleDrive-halleeninet@gmail.com/My\ Drive/
                              NeuroPRISM/effect_size_shiny_git/
                              effect_size_shiny/data/d_clean_hcp_ukb.rds')

# (for now using effect_maps data to obtain sample size values, but 
# ideally in the future the sample sizes will be a part of d_clean)
effect_maps <- readRDS('/Users/neuroprism/Library/CloudStorage/
                       GoogleDrive-halleeninet@gmail.com/
                       .shortcut-targets-by-id/
                       1gVHbxOjZcP8uB8Au0lhLpVHCR8RzcdCv/
                       Effect_Size/data/effect_maps')

# as an example, calculate simultaneous CIs for one study:
# !! data contains d values !!
study_name <- "hcp_fc_d_emotion_rest"
alpha <- 0.05 # the alpha level before correction
n_edges <- length(effect_maps$d[[study_name]]) # no of comparisons/edges
d <- effect_maps$d[[study_name]][,1]
n1 <- effect_maps$n[[study_name]]

# use Bonferroni correction to obtain corrected alpha level:
this_alpha_corr <- alpha/n_edges

# calculate simultaneous CI:
s_ci <- sapply(d, function(x) d.ci(x, n1 = n1, alpha = this_alpha_corr))

# to plot the data:

# rearrange the CI data into a dataframe with columns: d, ci_lb, ci_ub
ci_data <- data.frame(d = d, 
                      ci_lb = s_ci[1,], ci_ub = s_ci[3,])

# sort ci_data by d values from smallest to largest
ci_data <- ci_data[order(ci_data$d),]

# add index column to ci_data
ci_data$idx = 1:length(d)

# plot
ci_data %>%
  ggplot(aes(x = idx, y = d)) +
  geom_line(aes(y = d), lty = 1) +
  geom_line(aes(y = ci_lb), color = "lightblue") +
  geom_line(aes(y = ci_ub), color = "lightblue") +
  geom_line(aes(y = 0), color = "red")
  labs(x = "Edge", y = "Cohen's d", color = NULL) +
  theme_test()
  
  
##### Functions to calculate and plot sim CIs:
  
##### One-sample:
  
# make a function that will calculate and plot a given study d and sim CI
# must be a study that has d values, and is only one sample!
  
ci_plot_one_sample <- function(study_name, alpha) {
  n_edges <- length(effect_maps$d[[study_name]]) # no of comparisons/edges
  d <- effect_maps$d[[study_name]][,1]
  n1 <- effect_maps$n[[study_name]][1,1]
  this_alpha_corr <- alpha/n_edges
  
  s_ci <- sapply(d, function(x) d.ci(x, n1 = n1, alpha = this_alpha_corr))
  
  # rearrange the CI data into a dataframe with columns: d, ci_lb, ci_ub
  ci_data <- data.frame(d = d, 
                        ci_lb = s_ci[1,], ci_ub = s_ci[3,])
  
  # sort ci_data by d values from smallest to largest
  ci_data <- ci_data[order(ci_data$d),]
  
  # add index column to ci_data
  ci_data$idx = 1:length(d)
  
  # add column to represent whether the interval includes zero
  ci_data$intercepts_zero <- ifelse(ci_data$ci_lb <=0 & ci_data$ci_ub >= 0, TRUE, FALSE)
  
  # create color vector depending on intercepts_zero
  colors <- ifelse(ci_data$intercepts_zero, "red", "lightblue")
  
  #plot
  ci_data %>%
    ggplot(aes(x = idx, y = d)) +
    geom_line(aes(y = d), lty = 1) +
    geom_line(aes(y = 0), color = "red") +
    labs(x = "Edge", y = "Cohen's d", color = NULL) +
    geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = intercepts_zero), alpha = 0.2) +
    theme_test() +
    theme(legend.position = "none")
}



####### Two-sample:


# this currently doesn't plot properly, likely because of NAs

ci_plot_two_sample <- function(study_name, alpha) {
  n_edges <- length(effect_maps$d[[study_name]]) # no of comparisons/edges
  d <- effect_maps$d[[study_name]][1,]
  n1 <- effect_maps$n1[[study_name]]
  n2 <- effect_maps$n2[[study_name]]
  this_alpha_corr <- alpha/n_edges
  
  s_ci <- sapply(d, function(x) d.ci(x, n1 = n1, n2 = n2, alpha = this_alpha_corr))
  
  # rearrange the CI data into a dataframe with columns: d, ci_lb, ci_ub
  ci_data <- data.frame(d = d, 
                        ci_lb = s_ci[1,], ci_ub = s_ci[3,])
  
  # sort ci_data by d values from smallest to largest
  ci_data <- ci_data[order(ci_data$d),]
  
  # add index column to ci_data
  ci_data$idx = 1:length(d)
  
  # add column to represent whether the interval includes zero
  ci_data$intercepts_zero <- ifelse(ci_data$ci_lb <=0 & ci_data$ci_ub >= 0, TRUE, FALSE)
  
  # create color vector depending on intercepts_zero
  colors <- ifelse(ci_data$intercepts_zero, "red", "lightblue")
  
  #plot
  ci_data %>%
    ggplot(aes(x = idx, y = d)) +
    geom_line(aes(y = d), lty = 1) +
    geom_line(aes(y = 0), color = "red") +
    labs(x = "Edge", y = "Cohen's d", color = NULL) +
    geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub, fill = intercepts_zero), alpha = 0.2) +
    theme_test() +
    theme(legend.position = "none")
}