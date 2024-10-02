# load packages
library(networkD3)

# load study
path_to_data = "~/Desktop/effect_size_shiny/data/combined_data_2024-09-25.RData"
load(path_to_data)


# add sample size category
study$n_cat <- NA
study$n_cat[study$dataset == "abcd"] <- ">1500"
study$n_cat[study$dataset == "hbn"] <- ">400"
study$n_cat[study$dataset == "pnc"] <- ">400"
study$n_cat[study$dataset == "ukb"] <- ">10000"
study$n_cat[study$dataset == "hcp"] <- ">1000"

datasets = unique(study$dataset)

# Create an empty dataframe for the Sankey plot data
df <- data.frame(dataset = character(), sample_size= character(), map_type= character(), stat= character())

# Fill the dataframe with all rows from `study`, not just unique datasets
for (dataset in datasets) {
  # Get the indices for all rows corresponding to the current dataset
  dataset_rows <- which(study$dataset == dataset)
  
  # Loop through all rows for the current dataset
  for (i in dataset_rows) {
    sample_size <- study$n_cat[i]        # Get the corresponding sample_size for this row
    map_type <- study$map_type[i]        # Get the corresponding map_type for this row
    stat <- study$orig_stat_type[i]      # Get the corresponding orig_stat_type for this row
    
    # Create a new row with the values for this combination
    new_row <- data.frame(dataset = dataset, sample_size = sample_size, map_type = map_type, stat = stat)
    
    # Append the new row to the dataframe
    df <- rbind(df, new_row)
  }
}

df <- unique(df)

# Create nodes (unique values from the dataset, sample size, map type, and stat columns)
nodes <- data.frame(name = unique(c(df$dataset, df$sample_size, df$map_type, df$stat)))
nodes$group <- as.factor(c("abcd", "hbn", "pnc", "hcp", "ukb", "n1", "n2", "n3", "n4", "fc", "act", "r", "t2", "t"))

# Calculate the number of occurrences for each connection
# For dataset -> sample_size
link1 <- as.data.frame(table(study$dataset, study$n_cat))
link1 <- link1[link1$Freq > 0, ]  # Remove zero-frequency rows
link1$source <- match(link1$Var1, nodes$name) - 1
link1$target <- match(link1$Var2, nodes$name) - 1
link1 <- link1[, c("source", "target", "Freq")]  # Retain only necessary columns
names(link1) <- c("source", "target", "value")

# For sample_size -> map_type
link2 <- as.data.frame(table(study$n_cat, study$map_type))
link2 <- link2[link2$Freq > 0, ]  # Remove zero-frequency rows
link2$source <- match(link2$Var1, nodes$name) - 1
link2$target <- match(link2$Var2, nodes$name) - 1
link2 <- link2[, c("source", "target", "Freq")]
names(link2) <- c("source", "target", "value")

# For map_type -> stat
link3 <- as.data.frame(table(study$map_type, study$orig_stat_type))
link3 <- link3[link3$Freq > 0, ]  # Remove zero-frequency rows
link3$source <- match(link3$Var1, nodes$name) - 1
link3$target <- match(link3$Var2, nodes$name) - 1
link3 <- link3[, c("source", "target", "Freq")]
names(link3) <- c("source", "target", "value")

# Combine all links
links <- rbind(link1, link2, link3)
links$group <- as.factor(rep("l", dim(links)[1]))


# Use a palette from RColorBrewer (you can try different palettes like "Set1", "Set2", etc.)
#colors <- brewer.pal(n = 20, name = "Set2")
my_color <- 'd3.scaleOrdinal() .domain(["abcd", "hbn", "pnc", "hcp", "ukb", "n1", "n2", "n3", "n4", "fc", "act", "r", "t2", "t", "l"]) .range(["#A3CEF1", "#ADCED9", "#B7CDC1", "#C1CCA9", "#CBCB91", "#FFBA49", "#F6A050", "#F19353", "#EC8556", "#91C9D8", "#E1B4E1", "#F7EAA6", "#FFD6B0", "#FBE0AB", "#DDDDDD"])'

# Assign the colors to the nodes
color_scale <- sprintf('d3.scaleOrdinal().domain(%s).range(%s)', 
                       jsonlite::toJSON(nodes$name), 
                       jsonlite::toJSON(colors))


# Now create the Sankey plot
sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
              Value = "value", LinkGroup = "group", NodeGroup = "group", colourScale = my_color, NodeID = "name", units = "T", fontSize = 12, nodeWidth = 30)
