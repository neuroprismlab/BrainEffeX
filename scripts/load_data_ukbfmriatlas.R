## Exploring functional connectivity in the UK Biobank
# http://fmriatlas.org/group_mean_g360_s200_resting.html

n_nodes <- 360
dirname <- '/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My\ Drive/NeuroPRISM/My\ Files/UKB\ Data/ukbfmriatlas_data/'
basenames <- c("glasser360resting","glasser360task")

m <- data.frame(matrix(ncol = length(basenames), nrow = n_nodes*n_nodes))
names(m) <- basenames

for (this_basename in basenames) {
  this_filename <- paste(dirname,this_basename,".txt",sep="")
  df <- read.csv(this_filename, skip = 1, strip.white=TRUE, na.strings=c("null,"), sep="[")
  df <- df$X
  df <- df[!grepl("]", df)]
  df <- gsub(",","",df)
  df <- df[df!=""]
  df <- as.numeric(df)
  # df <- matrix(df, nrow = n_nodes)
  
  m[[this_basename]] <- df
}

atlas_filename <- paste(dirname,"glasser_atlas/misc_ukbfmriatlas/glasser360atlas_nodes.txt",sep="")
map <- read.csv(atlas_filename, strip.white=TRUE, na.strings=c("null,"), sep="[")
map <- map$X
map <- map[!grepl("]", map)]
map <- gsub(",","",map)


# for downloading the network assignments (hallee added)
# atlas_filename <- '/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My\ Drive/NeuroPRISM/My\ Files/UKB\ Data/Glasser360_ID.csv'
# map <- read.csv(atlas_filename, skip = 1, header = TRUE, col.names = c("parcel_index", "area_name", "area_description", "network", "key_studies", "location", "alt_area_name"))

nets <- c(1,8,61,82,157,179,202,252,268,345,353,357,360)
nets_names <- c("Visual1", "Visual2", "Somatomotor", "Cingulo−Opercular",
                "Dorsal−Attention", "Language", "Frontoparietal", "Auditory", "Default",
                "Posterior−Multimodal","Ventral−Multimodal", "Orbito−Affective")
map2 <- data.frame(net = rep("", max(nets)))
for(i in seq_along(nets[-length(nets)])) {
  map2[nets[i]:(nets[i+1]-1), "net"] <- nets_names[i]
}
map2[nets[length(nets)]:nrow(map2), "net"] <- nets_names[length(nets_names)]

idx_vec <- seq(1:(n_nodes*n_nodes))

vec2mat_idx_mapping <- data.frame(idx_vec = idx_vec, 
                                  idx_mat_row = (idx_vec-1) %% n_nodes + 1, 
                                  idx_mat_col = (idx_vec-1) %/% n_nodes + 1)

m$node1 <- map2$net[vec2mat_idx_mapping$idx_mat_row]
m$node2 <- map2$net[vec2mat_idx_mapping$idx_mat_col]
