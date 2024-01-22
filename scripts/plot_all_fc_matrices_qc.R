names <- attributes(d_master)
names <- unname(unlist(names))
path <- '/Users/neuroprism/Desktop/testing/matrices'

for (i in names) {
  filename <- paste0(i, "_plot.jpg")
  if (grepl("fc", i)) {
    if (dim(d_master[[i]])[1] == 71824) {
      # if the data includes the whole matrix, not just a triangle:
      jpeg(file = filename)
      t <- d_master[[i]][,1]
      n_nodes <- sqrt(dim(d_master[[i]])[1])
      trilmask <- matrix(TRUE, nrow = n_nodes, ncol = n_nodes)
      t2 <- trilmask
      t2[trilmask] <- t
      image(t2[,nrow(t2):1],
            xlab = paste0("dims: ", dim(d_master[[i]])),
            ylab = sprintf("%s Nodes", n_nodes),
            axes = FALSE)
      axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
      axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))
    }
    else if (dim(d_master[[i]])[2] == 71824) {
      # if the data includes the whole matrix, not just a triangle:
      jpeg(file = filename)
      t <- d_master[[i]][,1]
      n_nodes <- sqrt(dim(d_master[[i]])[1])
      trilmask <- matrix(TRUE, nrow = n_nodes, ncol = n_nodes)
      t2 <- trilmask
      t2[trilmask] <- t
      image(t2[,nrow(t2):1],
            xlab = paste0("dims: ", dim(d_master[[i]])),
            ylab = sprintf("%s Nodes", n_nodes),
            axes = FALSE)
      axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
      axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))
    }
    else if (dim(d_master[[i]])[1] > 1) {
      jpeg(file = filename)
      t <- d_master[[i]][,1]
      n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
      trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
      t2 <- trilmask
      t2[trilmask] <- t
      image(t2[,nrow(t2):1],
            xlab = paste0("dims: ", dim(d_master[[i]])),
            ylab = sprintf("%s Nodes", n_nodes),
            axes = FALSE)
      axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
      axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))
    }
    else if (dim(d_master[[i]])[1] == 1) {
      jpeg(file = filename)
      t <- d_master[[i]][1,]
      n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
      trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
      t2 <- trilmask
      t2[trilmask] <- t
      image(t2[,nrow(t2):1],
            xlab = paste0("dims: ", dim(d_master[[i]])),
            ylab = sprintf("%s Nodes", n_nodes),
            axes = FALSE)
      axis(1, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))  # Customize X-axis
      axis(2, at = seq(0, n_nodes, by = 20), labels = seq(0, n_nodes, by = 20))
    }
  dev.off()
  }
  }



# plot one FC matrix for testing purposes:

d_clean <- readRDS("/Users/neuroprism/Library/CloudStorage/GoogleDrive-halleeninet@gmail.com/My Drive/NeuroPRISM/effect_size_shiny_git/effect_size_shiny/data/d_clean_hcp_ukb.rds")

t <- filter(d_clean, study == "ABCD.fc.r.rest.age")
t <- t[[1]]

n_nodes <- ((-1 + sqrt(1 + 8 * length(t))) / 2) + 1
trilmask <- upper.tri(matrix(1, nrow = n_nodes, ncol = n_nodes))
t2 <- trilmask
t2[trilmask] <- t
image(t2[,nrow(t2):1],
      #xlab = sprintf("%s Nodes", n_nodes),
      #ylab = sprintf("%s Nodes", n_nodes),
      axes = FALSE)
axis(1, at = seq(0, 1, by = 1), labels = seq(1, n_nodes, by = n_nodes-1), cex.axis = 1.5)  # Customize X-axis
axis(2, at = seq(0, 1, by = 1), labels = seq(1, n_nodes, by = n_nodes-1), cex.axis = 1.5)

# try with ggplot

ggplot(data = t2[,nrow(t2):1])