# make 12 separate RData files, one for each combo

# load combined data
load('combined_data_2025-02-13.RData')

# get list of combo names
combo_names = unique(names(data$abcd_fc_r_REST_age_months))
# make multivariate names generalizable to all tests with wildcard
combo_names = sub("multi.r", "multi.*",x = combo_names)


# loop through combo_names
for (combo in combo_names) {
  # for each study, grab that combo's data
  this_combo_data = list()
  for (name in names(data)) {
    this_combo = names(data[[name]])[grepl(combo, names(data[[name]]))]
    combo_data = data[[name]][[this_combo]]
    this_combo_data[[name]] = combo_data
  }
  # save this_combo_data as RData file
  save(this_combo_data, file = paste0('combo_', combo, '.RData'))
}

d = data
for (name in names(data)) {
  for (combo in names(data[[name]])) {
    data[[name]][[combo]]$sim_ci_lb = unlist(data[[name]][[combo]]$sim_ci_lb)
    data[[name]][[combo]]$sim_ci_ub = unlist(data[[name]][[combo]]$sim_ci_ub)
  }
}