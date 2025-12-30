library(here)
library(data.table)
library(tictoc)

filepath <- here("data", "2025_12", "input")

# Load your input (replace 'input.txt' with your actual filename)
raw_input <- readLines(filepath)
full_text <- paste(raw_input, collapse = "\n")

# Separate the Shapes section from the Regions section (split by double newline)
parts <- strsplit(full_text, "\n\n")[[1]]

dt_shapes <- data.table(raw = unlist(strsplit(parts[1:(length(parts)-1)] |> paste(collapse = ","), ",")))

dt_shapes[, `:=`(
  shape = as.integer(sub("^(\\d+):.*", "\\1", raw)) + 1,
  num_rows = lengths(regmatches(raw, gregexpr("\n", raw))),
  num_cols = sapply(strsplit(raw, "\n"), function(x) nchar(x[2])),
  filled_area = lengths(regmatches(raw, gregexpr("#", raw))),
  unfilled_area = lengths(regmatches(raw, gregexpr("\\.", raw))),
  footprint = lengths(regmatches(raw, gregexpr("#", raw))) + lengths(regmatches(raw, gregexpr("\\.", raw)))
)]

#Extract the regions part at the bottom of file
dt_regions <- parts[length(parts)] |> fread(sep = "", col.names = "raw", header = FALSE)

# Get length and width of region.
dt_regions[, c("width", "length") := tstrsplit(raw, "[x:]", type.convert = TRUE, keep = 1:2)]

# We take the part after the first colon, split by space, and convert to integer
dt_regions[, shapes := lapply(strsplit(sub(".*: ", "", raw), " "), as.integer)]

dt_regions[, `:=` (
  area_total = width * length,
  area_multiple_9 = floor(width * length / 9),
  area_squares = floor(width / 3) * floor(length / 3)
)]

dt_regions[, `:=` (
  shape_area_total = sapply(shapes, function(counts) {sum(counts * dt_shapes$filled_area)}),
  shape_area_multiple_9 = sapply(shapes, function(counts) {sum(counts)}),
  shape_area_squares = sapply(shapes, function(counts) {sum(counts)})
)]


dt_regions[, area_filled_total := sapply(shapes, function(counts) {
  sum(counts * dt_shapes$filled_area)
})]


result <- dt_regions[area_total >= shape_area_total, .N]
result |> print()
