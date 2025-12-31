filepath <- here("data", "2025_08", "input")
num_connections <- 10

input <-
  fread(
    file = filepath,
    header = FALSE,
    colClasses = c("integer"),
    col.names = c("X", "Y", "Z")
  )

num_connections <- 0.5 * nrow(input) * (nrow(input) - 1)

mat <- matrix(NA_integer_, nrow = nrow(input), ncol = nrow(input))

for (j_box_1 in 1:nrow(input)) {
  for (j_box_2 in 1:nrow(input)) {
    if (j_box_2 > j_box_1) {
      mat[j_box_1, j_box_2] <-
        (((input$X[j_box_2] - input$X[j_box_1]) ^ 2) +
           ((input$Y[j_box_2] - input$Y[j_box_1]) ^ 2) +
           ((input$Z[j_box_2] - input$Z[j_box_1]) ^ 2) ) |>
        sqrt()
    }
  }
}

# Get indices of N lowest values
idx <- order(mat)[1:num_connections]

# Convert to row/column coordinates
coords <- arrayInd(idx, dim(mat))

# Get the values
values <- mat[idx]

# Combine into a data frame
shortest_connections <- data.table(
  row = coords[, 1],
  col = coords[, 2],
  value = values
)

# Initialize disjoint set for 20 items
make_set <- function(n) {
  list(
    parent = 1:n,
    rank = rep(0, n)
  )
}

# Find with path compression
find <- function(ds, x) {
  if (ds$parent[x] != x) {
    ds$parent[x] <- find(ds, ds$parent[x])
  }
  ds$parent[x]
}

# Union by rank
union <- function(ds, x, y) {
  root_x <- find(ds, x)
  root_y <- find(ds, y)

  if (root_x != root_y) {
    if (ds$rank[root_x] < ds$rank[root_y]) {
      ds$parent[root_x] <- root_y
    } else if (ds$rank[root_x] > ds$rank[root_y]) {
      ds$parent[root_y] <- root_x
    } else {
      ds$parent[root_y] <- root_x
      ds$rank[root_x] <- ds$rank[root_x] + 1
    }
  }
  ds
}

# Initialize for 20 items
ds <- make_set(nrow(input))

# Process all rows in coords
for (i in 1:num_connections) {
  ds <- union(ds, coords[i, 1], coords[i, 2])

}

# Check which set each item belongs to
# for (i in 1:nrow(input)) {
#   cat("Item", i, "is in set with root", find(ds, i), "\n")
# }

# Or get all unique sets
roots <- sapply(1:nrow(input), function(i) find(ds, i))
unique_sets <- split(1:nrow(input), roots)

# Sort by size (descending - largest sets first)
unique_sets_sorted <- unique_sets[order(sapply(unique_sets, length), decreasing = TRUE)]

# result_1 <-
#   length(unique_sets_sorted[[1]]) *
#   length(unique_sets_sorted[[2]]) *
#   length(unique_sets_sorted[[3]])


# Initialize disjoint set for 20 items
ds <- make_set(nrow(input))

last_row <- NULL

# Process rows one at a time
for (i in 1:nrow(coords)) {
  ds <- union(ds, coords[i, 1], coords[i, 2])

  # Check how many unique sets we have
  roots <- sapply(1:nrow(input), function(j) find(ds, j))
  num_sets <- length(unique(roots))

  # If we have only one set, record this row
  if (num_sets == 1) {
    last_row <- i
    break  # Stop as soon as we reach one set
  }
}

if (!is.null(last_row)) {
  cat("Row", last_row, "results in one unique set\n")
  cat("This row connects items:", coords[last_row, 1], "and", coords[last_row, 2], "\n")
} else {
  cat("No single set formed after processing all rows\n")
}

(input$X[coords[last_row, 1]] * input$X[coords[last_row, 2]]) |> as.character() |> print()

