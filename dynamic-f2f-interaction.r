
# Load necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(igraph, network, statnet, btergm, texreg, tidyverse, readr)


# Load the edge list
edge_list_net0 <- read_csv('edge_list_net0.csv')

# Create a list of unique timestamps
timestamps <- unique(edge_list_net0$timestamp)

# Function to convert edge list to network objects for each timestamp
create_network_objects <- function(edge_list, timestamps) {
  network_objects <- list()
  for (timestamp in timestamps) {
    edges_at_time <- edge_list %>%
      filter(timestamp == !!timestamp) %>%
      select(from, to, weight)
    
    
    net <- suppressWarnings(
      network(edges_at_time[, c("from", "to")], directed = TRUE, loops = TRUE, matrix.type = "edgelist")
    )
    
    set.edge.attribute(net, "weight", edges_at_time$weight)
    network_objects[[as.character(timestamp)]] <- net
  }
  return(network_objects)
}

# Convert edge list to network objects
network_objects <- create_network_objects(edge_list_net0, timestamps)

# Use all time steps for TERGM analysis

tergm_model <- suppressWarnings(
  btergm(network_objects[1:500] ~ edges + mutual, R = 2000)
)


print(summary(tergm_model))

# Goodness of Fit evaluation
# Use the last time step as the target for GoF evaluation
gof_targets <- list(network_objects[[501]], network_objects[[502]], network_objects[[503]], network_objects[[504]], network_objects[[505]], network_objects[[506]], network_objects[[507]], network_objects[[508]], network_objects[[509]], network_objects[[510]])

gof_results <- btergm::gof(tergm_model, target = gof_targets, nsim = 100)

dev.new()
plot(gof_results, roc.rgraph = TRUE, pr.rgraph = TRUE)
title(main = "~ edges + mutual")
