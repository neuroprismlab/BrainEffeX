# Script to load d_clean and merge with phenotype categories file

# path to data
data_path <- '~/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph share/visualize_effects_app/data'

# load d_clean
load(paste0(data_path,'/d_clean.rds'))

# load phenotype categorization file
phen_cat <- read.csv(paste0(data_path, '/non_brain_measure_code.csv'))

# phen_cat is formatted with _'s instead of .'s in the study names
# change phen_cat to use .'s as well so that we can join the dataframes:
phen_cat$study <- gsub("_", ".", phen_cat$study)

# join d_clean and phen_cat by the study column
d_clean <- left_join(d_clean, phen_cat, by = "study")

# save the results
saveRDS(d_clean, file = paste0(data_path, '/d_clean_cats.rds'))

# note: the resulting .rds file must be loaded to the shiny app with
# the readRDS function. Make sure the shiny code reflects this.