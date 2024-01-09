## add HCP task-activation data to d_clean

# load d_clean
d_clean <- readRDS('~/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph share/visualize_effects_app/data/d_clean_cats.rds')

# remove previous hcp emotion task activation data
d_clean <- d_clean[d_clean$study != hcp.act.d.emotion.rest, ]

# TODO put this into a for loop:

task_l = "emotion" #c("gambling", "relational", "social", "wm")
task_u = toupper(task_l)

for (i in 1:length(task_l)) {

    # load nifti
    data_files <- list.files(path = "/Users/halleeshearer/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph share/visualize_effects_app/data/", full.names = TRUE)
    pattern <- paste0(task_u[i], ".*\\.nii\\.gz")
    matching_file <- grep(pattern, file_list, value = TRUE)
    nifti <- readnii(matching_file)

    nifti_data <- img_data(nifti)

    list <- as.list(nifti_data)

    # remove zero values (likely those outside of the brain, but could theoretically include some in the brain)
    nonzero_list <- list[list!=0]

    # convert to numeric
    nonzero_numeric <- unlist(nonzero_list)

    # create new dataframe to add to d_clean
    new_data <- data.frame(d = nonzero_numeric, 
        study = rep(paste0("hcp.act.t.",task_l[i],".rest"), length(nonzero_list)), 
        statistic = rep("t (act)", length(nonzero_list)), 
        code = rep("cognitive (task)", length(nonzero_list)))


    # combine new dataframe with d_clean
    d_clean <- rbind(d_clean, new_data)
}


# export d_clean result
saveRDS(d_clean, file = '~/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph share/visualize_effects_app/data/d_clean_whcp.rds')
