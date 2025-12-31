filepath <- here("data", "2025_07", "input")

input <-
  fread(
    file = filepath,
    header = FALSE,
    colClasses = c("character")
  )


calcs <- input |> copy()
calcs[, paste0("char", 1:max(nchar(input$V1))) := tstrsplit(input$V1, "", fixed = TRUE)]
calcs[, V1 := NULL]

mat <- calcs |> as.matrix()

rows <- nrow(mat)
cols <- ncol(mat)
num_splits <- 0

path_counter <- matrix(0, nrow = rows, ncol = cols)

for (row in 2:rows) {
  for (col in 1:cols) {
    # If the cell above is S then put a laser
    if(mat[row - 1, col] == "S") {
      mat[row, col] = "|"
      path_counter[row, col] = 1
      }

    # If the cell above is a laser, and this is not a splitter, then put a laser
    if(mat[row - 1, col] == "|" &&  mat[row, col] != "^") {
      mat[row, col] = "|"
      path_counter[row, col] = path_counter[row, col] + path_counter[row - 1, col]
      }

    # If the cell above is a laser, and this is a splitter, then split and increment counter
    if(mat[row - 1, col] == "|" &&  mat[row, col] == "^")
      {
        mat[row, col - 1] = "|"
        mat[row, col + 1] = "|"
        num_splits = num_splits + 1

        path_counter[row, col - 1] = path_counter[row, col - 1] + path_counter[row - 1, col]
        path_counter[row, col + 1] = path_counter[row, col + 1] + path_counter[row - 1, col]
      }

  }
}

num_splits |> as.character() |> print()

path_counter[rows,] |> sum() |> as.character() |> print()
