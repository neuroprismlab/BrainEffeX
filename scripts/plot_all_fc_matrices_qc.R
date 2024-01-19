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