input <-
  fread(
    file = here("data", "2025_04", "input"),
    header = FALSE,
    colClasses = c("character")
  )


# Convert to 0s and 1s
calcs <- as.data.table(
  do.call(rbind,
          lapply(strsplit(input$V1, ""), function(x) {
            as.numeric(ifelse(x == "@", 1, 0))
          })
  )
)

parameters <- list(
  counter = 0,
  mat = as.matrix(calcs)
)


forklift <- function(parameters) {

  N <- nrow(parameters$mat)
  M <- ncol(parameters$mat)

  output <- matrix(0, nrow = N, ncol = M)
  new_mat <- matrix(0, nrow = N, ncol = M)
  old_mat <- parameters$mat

  for (i in 1:N) {
    for (j in 1:M) {
      if (parameters$mat[i, j] == 1) {
        # Define the range of surrounding cells (handling edges)
        row_range <- max(1, i-1):min(N, i+1)
        col_range <- max(1, j-1):min(M, j+1)

        # Sum the cell plus its neighbors
        output[i, j] <- sum(parameters$mat[row_range, col_range])
        if (output[i, j] <= 4) {new_mat[i, j] <- 0}
        if (output[i, j] > 4) {new_mat[i, j] <- 1}
      }
    }
  }

  parameters$counter <- parameters$counter + sum(output > 0 & output <= 4)
  parameters$mat <- new_mat

  if (identical(old_mat,new_mat) == FALSE) {parameters <- forklift(parameters)}

  return(parameters)


}

result <- parameters |>
  forklift()

result$counter |> print()
