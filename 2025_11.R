library(here)
library(data.table)
library(tictoc)

filepath <- here("data", "2025_11", "input")

# Read the data
tic("Total execution time")
dt <- fread(filepath, sep = ":", header = FALSE, col.names = c("device", "connections"))

# Clean up the connections column and create edge list
dt[, connections := trimws(connections)]

# Create a list structure for the graph
graph <- list()
for(i in 1:nrow(dt)) {
  device <- dt$device[i]
  targets <- trimws(strsplit(dt$connections[i], " ")[[1]])
  graph[[device]] <- targets
}

# Function to find all paths using depth-first search
find_all_paths <- function(graph, start, end, path = c()) {
  # Add current node to path
  path <- c(path, start)

  # If we've reached the end, return this path
  if(start == end) {
    return(list(path))
  }

  # If this node has no outgoing connections, return empty
  if(is.null(graph[[start]])) {
    return(list())
  }

  # Explore all neighbors
  all_paths <- list()
  for(node in graph[[start]]) {
    # Avoid cycles - don't revisit nodes already in path
    if(!(node %in% path)) {
      new_paths <- find_all_paths(graph, node, end, path)
      all_paths <- c(all_paths, new_paths)
    }
  }

  return(all_paths)
}

# Find all paths from "you" to "out"
paths <- find_all_paths(graph, "you", "out")

# Store the answer
answer <- length(paths)

# Print the results
cat("\n=== ANSWER ===\n")
cat("Number of unique paths from 'you' to 'out':", answer, "\n")
cat("==============\n\n")

# Optional: print all paths (set to FALSE for large datasets)
verbose <- TRUE
if(verbose && answer <= 20) {
  cat("All paths:\n")
  for(i in 1:length(paths)) {
    cat("  Path", i, ":", paste(paths[[i]], collapse = " -> "), "\n")
  }
  cat("\n")
}

toc()
