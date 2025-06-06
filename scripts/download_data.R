# this script downloads the RData file that contains the effect maps
# this file is too large to store on Github, so it is stored on OSF
# the link to the data is https://osf.io/cwnjd, the file is effect_maps_public.RData


library(osfr)

current_dir <- getwd()
data_dir <- file.path(paste0(current_dir, "/data/"))

# check if data file already exists
if ("effect_maps_public.RData" %in% list.files(data_dir)) {
    # file already exists
    print("Data already downloaded")
} else {
    # file does not exist
    print("Data is not downloaded. Downloading now from OSF.")
    project <- osf_retrieve_node("https://osf.io/cwnjd")
    files <- osf_ls_files(project)
    data_file <- subset(files, name == "effect_maps_public.RData")
    current_dir <- getwd()
    osf_download(data_file, path = file.path(paste0(current_dir, "/data/")))
    if ("effect_maps_public.RData" %in% list.files(data_dir)) {
        # file already exists
        print("Data successfully downloaded")
    } else {
        # file does not exist
        print("Data could not be downloaded from OSF. Try manually downloading and moving the file to the data directory from https://osf.io/cwnjd.")
}
}


