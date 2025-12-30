library(here)
library(data.table)
library(igraph)
library(tictoc)
filepath <- here("data", "2025_11", "input")

# Read the data
dt <- fread(filepath, sep = ":", header = FALSE, col.names = c("device", "connections"))

# Clean up the connections column
dt[, connections := trimws(connections)]

# Create edge list for igraph
edge_list <- rbindlist(lapply(1:nrow(dt), function(i) {
  device <- dt$device[i]
  targets <- trimws(strsplit(dt$connections[i], " ")[[1]])
  data.table(from = device, to = targets)
}))

# Function to count paths
count_paths <- function(edge_list, start, end) {
  # Create amended edge list
  edge_list_amended <- copy(edge_list)

  # For rows where from == end, set to == end
  edge_list_amended[from == end, to := end]

  edge_list_amended <- rbind(edge_list_amended, list("out", "out"))

  # Remove duplicate rows
  edge_list_amended <- unique(edge_list_amended)

  # Create path_counter - start with rows where from == start
  path_counter <- data.table(from = start, rows = 1)

  # Iteratively join until all 'to' values are 'end'
  iteration <- 0
  while(!all(path_counter$from == end)) {
  # while(iteration < 34) {
      iteration <- iteration + 1

    # Join edge_list_amended to path_counter
    joined_counter <- edge_list_amended[path_counter, on = .(from = from)]

    # Remove the "from" column
    joined_counter[, from := NULL]

    # Rename "to" to "from"
    setnames(joined_counter, "to", "from")

    path_counter <- joined_counter[, rows := sum(rows), by = from] |> unique()

    if (end != "out") {
      path_counter <- path_counter[from != "out"]
    }

    # cat("Iteration", iteration, ": path_counter rows is ", nrow(path_counter),
        # " and path_counter paths is ", sum(path_counter$rows), "\n")

    # print(head(path_counter))

  }

  return(path_counter)
}

tic("Test 12_2 time")

# Test the function
cat("\n=== Testing count_paths ===\n")
svr_dac <- count_paths(edge_list, "svr", "dac")$rows |> sum()
svr_fft <- count_paths(edge_list, "svr", "fft")$rows |> sum()
dac_fft <- count_paths(edge_list, "dac", "fft")$rows |> sum()
fft_dac <- count_paths(edge_list, "fft", "dac")$rows |> sum()
dac_out <- count_paths(edge_list, "dac", "out")$rows |> sum()
fft_out <- count_paths(edge_list, "fft", "out")$rows |> sum()

total_paths <- svr_dac * dac_fft * fft_out + svr_fft * fft_dac * dac_out

cat("\nNumber of paths:", as.character(total_paths), "\n")

toc()
