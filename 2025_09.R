library(here)
library(data.table)
library(tictoc)

tic()

filepath <- here("data", "2025_09", "input")

input <-
  fread(
    file = filepath,
    header = FALSE,
    colClasses = c("integer"),
    col.names = c("X", "Y")
  )

unique_X <- input$X |> sort() |> unique()
unique_Y <- input$Y |> sort() |> unique()

input[, c("X_index", "Y_index") := .(match(X, unique_X) + 1, match(Y, unique_Y) + 1)]

tile_grid <- matrix(".", nrow = length(unique_X) + 2, ncol = length(unique_Y) + 2)

flood_fill <- function(mat, start_row, start_col) {
  rows <- nrow(mat)
  cols <- ncol(mat)

  # Queue for BFS (using a list of coordinates)
  queue <- list(c(start_row, start_col))
  visited <- matrix(FALSE, rows, cols)

  while (length(queue) > 0) {
    # Dequeue
    current <- queue[[1]]
    queue <- queue[-1]

    r <- current[1]
    c <- current[2]

    # Skip if out of bounds, already visited, or a wall
    if (r < 1 || r > rows || c < 1 || c > cols ||
        visited[r, c] || mat[r, c] == "#" || mat[r, c] == "X") {
      next
    }

    # Mark as visited
    visited[r, c] <- TRUE
    mat[r, c] <- "*"

    # Add neighbors to queue (up, down, left, right)
    queue <- c(queue, list(c(r-1, c), c(r+1, c), c(r, c-1), c(r, c+1)))
  }

  return(mat)
}

for (tile in 1:nrow(input)) {

  current_tile <- tile
  next_tile <- tile + 1
  if (tile == nrow(input)) {next_tile <- 1}

  for (green_X in input$X_index[current_tile]:input$X_index[next_tile]) {
    for (green_Y in input$Y_index[current_tile]:input$Y_index[next_tile]) {
      tile_grid[green_X, green_Y] <- "X"
    }
  }

  tile_grid[input$X_index[current_tile], input$Y_index[current_tile]] <- "#"
  tile_grid[input$X_index[next_tile], input$Y_index[next_tile]] <- "#"

}

tile_grid <- flood_fill(tile_grid, length(unique_X) + 2, length(unique_Y) + 2)

mat <- matrix(NA_integer_, nrow = nrow(input), ncol = nrow(input))
valid_mat <- matrix(NA_integer_, nrow = nrow(input), ncol = nrow(input))

for (tile_1 in 1:nrow(input)) {
  for (tile_2 in 1:nrow(input)) {
    if (tile_2 > tile_1) {
      valid_mat[tile_1, tile_2] <- 1
      mat[tile_1, tile_2] <-
        (abs(input$X[tile_2] - input$X[tile_1]) + 1) * (abs(input$Y[tile_2] - input$Y[tile_1]) + 1)

      for (X in input$X_index[tile_1]:input$X_index[tile_2]) {
        if (tile_grid[X, input$Y_index[tile_1]] == "*") {valid_mat[tile_1, tile_2] <- 0; break()}
        if (tile_grid[X, input$Y_index[tile_2]] == "*") {valid_mat[tile_1, tile_2] <- 0; break()}
      }

      for (Y in input$Y_index[tile_1]:input$Y_index[tile_2]) {
        if (tile_grid[input$X_index[tile_1], Y] == "*") {valid_mat[tile_1, tile_2] <- 0; break()}
        if (tile_grid[input$X_index[tile_2], Y] == "*") {valid_mat[tile_1, tile_2] <- 0; break()}
      }

    }
  }
}

# Get indices of N lowest values
idx <- order(mat, decreasing = TRUE)
idx_valid <- order(mat*valid_mat, decreasing = TRUE)

# Convert to row/column coordinates
coords <- arrayInd(idx, dim(mat))
coords_valid <- arrayInd(idx_valid, dim(mat))


# Get the values
values <- mat[idx]
values_valid <- mat[idx_valid]

# Combine into a data frame
shortest_connections <- data.table(
  row = coords[, 1],
  col = coords[, 2],
  value = values
)

result_1 <- values[1]
result_1 |> as.character() |> print()

result_2 <- values_valid[1]
result_2 |> as.character() |> print()

toc()
