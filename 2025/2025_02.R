input <-
  fread(
    file = here("data", "2025_02", "input"),
    header = FALSE,
    sep2 = ","
  )

input <- input |> t() |> as.data.table()


calcs <- input[
  , c("start", "end") := tstrsplit(V1, "-", type.convert = TRUE)][
  , .(value = seq(start, end, by = 1L)), by = .I][
  , .(value)]

calcs[, length := nchar(value)]

calcs_even <- calcs[(length %% 2) == 0]
calcs_even[, num_left := substr(value, 1, length / 2)]
calcs_even[, num_right := substr(value, 1+(length / 2), length)]
calcs_same <- calcs_even[num_left == num_right]

result_1 <- sum(calcs_same$value)

calcs_2 <- calcs[, matches := grepl("^(.+)\\1+$", value)] |> unique()

result_2 <- calcs_2[matches == TRUE, value] |> sum()
