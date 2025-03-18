# make 12 separate RData files, one for each combo

# set parameters
filename = 'combined_data_2025-02-24.RData'
out_path = ''


# load combined data
load(filename)
data2 <- data

# get list of combo names
combo_names = unique(names(data2$abcd_fc_r_REST_age_months))
# make multivariate names generalizable to all tests with wildcard
combo_names = sub("multi.r", "multi.*",x = combo_names)

combo_names <- c("pooling.none.motion.none", 
                 "pooling.none.motion.regression",
                 "pooling.none.motion.threshold",
                 "pooling.net.motion.none", 
                 "pooling.net.motion.regression",
                 "pooling.net.motion.threshold")


# loop through combo_names
for (combo in combo_names) {
  # for each study, grab that combo's data
  this_combo_data = list()
  
  uv_combo_name <- paste0(combo, ".mv.none")
  
  for (name in names(data2)) {
    d <- list()
    
    this_combo = names(data2[[name]])[grepl(uv_combo_name, names(data2[[name]]))]
    combo_data = data2[[name]][[this_combo]]
    mv_combo_name <- names(data2[[name]])[grepl(paste0(combo, ".mv.multi.*"), names(data2[[name]]))]
    mv_combo_data = data2[[name]][[mv_combo_name]]
    
    # remove unnecessary information from data
    if ('stat' %in% names(combo_data)) {
      combo_data$stat <- NULL
    }
    if ('p' %in% names(combo_data)) {
      combo_data$p <- NULL
    }
    if ('std.brain' %in% names(combo_data)) {
      combo_data$std.brain <- NULL
    }
    if ('std.score' %in% names(combo_data)) {
      combo_data$std.score <- NULL
    }
    if ('p.fullres' %in% names(combo_data)) {
      combo_data$p.fullres <- NULL
    }
    if ('stat.fullres' %in% names(combo_data)) {
      combo_data$stat.fullres <- NULL
    }
    
    # remove unnecessary information from mv data
    if ('stat' %in% names(mv_combo_data)) {
      mv_combo_data$stat <- NULL
    }
    if ('p' %in% names(mv_combo_data)) {
      mv_combo_data$p <- NULL
    }
    if ('std.brain' %in% names(mv_combo_data)) {
      mv_combo_data$std.brain <- NULL
    }
    if ('std.score' %in% names(mv_combo_data)) {
      mv_combo_data$std.score <- NULL
    }
    if ('p.fullres' %in% names(mv_combo_data)) {
      mv_combo_data$p.fullres <- NULL
    }
    if ('stat.fullres' %in% names(mv_combo_data)) {
      mv_combo_data$stat.fullres <- NULL
    }
    
    d[[uv_combo_name]] <- combo_data
    d[[mv_combo_name]] <- mv_combo_data
    
    # save this study's data to the list
    this_combo_data[[name]] = d
  }
  
  data <- this_combo_data
  
  # save this_combo_data as RData file
  save(data, study, brain_masks, file = paste0(out_path, 'combo_', combo, '.RData'))
}