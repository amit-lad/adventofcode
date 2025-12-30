library(here)
library(data.table)

input <-
  fread(
    file = here("data", "2025_05", "input"),
    header = FALSE,
    colClasses = c("character")
  )


# Find the blank line
blank_line <- which(input$V1 == "")

# Split into two data.tables
ranges_raw <- input[1:(blank_line - 1)]
numbers <- input[(blank_line + 1):.N]

# Expand the ranges into full list of numbers
ranges_split <- ranges_raw[, tstrsplit(V1, "-", fixed = TRUE)]
setnames(ranges_split, c("start", "end"))
ranges_split[, `:=`(start = as.numeric(start), end = as.numeric(end))]

# Create full list by expanding each range
# full_numbers <- ranges_split[, .(number = seq(start, end)), by = .(start, end)]
# full_numbers <- full_numbers[, .(number)]  # Keep only the number column
# full_numbers <- unique(full_numbers)

# Clean up the numbers data.table
numbers[, V1 := as.numeric(V1)]
setnames(numbers, "V1", "number")

fresh_ingredients <- numbers[sapply(numbers$number, function(n) {
  any(n >= ranges_split$start & n <= ranges_split$end)
})]

result_1 <- fresh_ingredients |> nrow()

result_1 |> print()


# Sort ranges by start position
setorder(ranges_split, start, end)

# Merge overlapping ranges and count unique numbers
merged_ranges <- ranges_split[, {
  if (.N == 1) {
    .(start = start, end = end)
  } else {
    current_start <- start[1]
    current_end <- end[1]
    result <- list()

    for (i in 2:.N) {
      if (start[i] <= current_end + 1) {
        # Overlapping or adjacent - merge
        current_end <- max(current_end, end[i])
      } else {
        # Gap - save current range and start new one
        result[[length(result) + 1]] <- list(start = current_start, end = current_end)
        current_start <- start[i]
        current_end <- end[i]
      }
    }
    # Add final range
    result[[length(result) + 1]] <- list(start = current_start, end = current_end)
    rbindlist(result)
  }
}]

# Count total unique numbers
total_unique <- merged_ranges[, sum(end - start + 1)]
total_unique |> as.character() |> print()

