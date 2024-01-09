# load UKB (fMRI Atlas) data, transform to Cohen's d, export

# set paths
short_path = '/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My\ Drive/NeuroPRISM/My\ Files/UKB_data/'
data_path = '/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My\ Drive/NeuroPRISM/My\ Files/UKB_data/ukbfmriatlas_data/'
out_path = '/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph\ share/visualize_effects_app/data/'

# set constants
conditions = c("resting", "task")
parcellation = "glasser360" # could change to schaefer200

# load data
this_filename <- paste(data_path,parcellation, conditions[2],".txt",sep="")
df <- read.csv(this_filename, skip = 1, strip.white=TRUE, na.strings=c("null,"), sep="[")
df <- df$X
df <- df[!grepl("]", df)]
df <- gsub(",","",df)
df <- df[df!=""]
df <- as.numeric(df)

# convert from r to d
# formula from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7906439/pdf/nihms-1671121.pdf
d <- (df*2)/((1-(df^2))^(1/2))


# create new dataframe to add to d_clean
new_data <- data.frame(d = d, 
                       study = rep(paste0("ukb.fc.d.task"), length(d)), 
                       statistic = rep("r", length(d)), 
                       code = rep("NA", length(d)))

# load d_clean (change to most recent d_clean file)
d_clean <- readRDS("/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/.shortcut-targets-by-id/17uYR-Ubbo9n0459awrNyRct0CL4MwAXl/Hallee-Steph\ share/visualize_effects_app/data/d_clean_hcp_ukb.rds")

# combine new dataframe with d_clean
d_clean <- rbind(d_clean, new_data)

# export
saveRDS(d_clean, file = paste0(out_path, 'd_clean_hcp_ukb.rds'))
