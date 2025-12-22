library(here)
library(data.table)

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








# Assuming calcs is your data.table with columns of 0s and 1s
# Get dimensions
N <- nrow(calcs)
M <- ncol(calcs)

# Convert to matrix for easier neighbor access
mat <- as.matrix(calcs)

# Initialize output matrix
output <- matrix(0, nrow = N, ncol = M)

# Loop through each cell
for (i in 1:N) {
  for (j in 1:M) {
    if (mat[i, j] == 1) {
      # Define the range of surrounding cells (handling edges)
      row_range <- max(1, i-1):min(N, i+1)
      col_range <- max(1, j-1):min(M, j+1)

      # Sum the cell plus its neighbors
      output[i, j] <- sum(mat[row_range, col_range])
    }
  }
}

# Convert output back to data.table if needed
output_dt <- as.data.table(output)
# Keep original column names if desired
setnames(output_dt, names(calcs))

print(output_dt)


result_1 <- sum(output > 0 & output <= 4)
