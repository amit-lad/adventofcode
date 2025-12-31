filepath <- here("data", "2025_06", "input")

input <-
  fread(
    file = filepath,
    header = FALSE,
    colClasses = c("character")
  )

calcs <- input |> copy() |> t() |> as.data.table()

# Get the last column name
last_col <- names(calcs)[ncol(calcs)]

# Get all column names except the last one
value_cols <- names(calcs)[1:(ncol(calcs) - 1)]

# Calculate sum for rows with "+"
calcs_sum <- calcs[get(last_col) == "+", Reduce(`+`, lapply(.SD, as.integer)), .SDcols = value_cols] |> sum()

# Calculate product for rows with "*"
calcs_mult <- calcs[get(last_col) == "*", Reduce(`*`, lapply(.SD, as.numeric)), .SDcols = value_cols] |> sum()

result_1 <- calcs_mult + calcs_sum
result_1 |> as.character() |> print()



operations <- tail(input, 1) |> t() |> as.data.table()
setnames(operations, "V1", "operation")

values <-
  read_lines(
    file = filepath,
    skip = 0,
    n_max = nrow(input) - 1
    ) |>
  as.data.table()


  fread(
    file = filepath,
    header = FALSE,
    sep = NULL,
    colClasses = c("character"),
    nrows = nrow(input) - 1
  )

max(nchar(values$V1))


# Split into char1, char2, char3, etc.
values[, paste0("char", 1:max(nchar(values$V1))) := tstrsplit(V1, "", fixed = TRUE)]
values[, V1 := NULL]

transposed_values <- values |> t() |> as.data.table()
transposed_values[is.na(transposed_values)] <- ""
numbers <- transposed_values[, .(numbers = as.numeric(do.call(paste0, .SD)))]

numbers[, group := cumsum(is.na(numbers)) + 1]
numbers <- numbers[!is.na(numbers)]
numbers[, operation := operations$operation[group]]
result_datatable <- numbers[, .(
  group = .GRP,
  operation = first(operation),
  result = if(first(operation) == "+") sum(numbers) else prod(numbers)
), by = group]

result_2 <- result_datatable$result |> sum()
result_2 |> as.character() |> print()
