library(here)
library(data.table)

input <-
  fread(
    file = here("data", "2025_01", "input"),
    header = FALSE,
    col.names = ("input")
  )

calcs <- copy(input)

calcs[, `:=`(
  direction = fcase(
    substr(input, 1, 1) == "L", -1,
    substr(input, 1, 1) == "R", 1
  ),
  magnitude = as.integer(gsub("^[LR]", "", input))
)]

calcs2 <- calcs[rep(1:.N, magnitude)]

calcs[, result := direction * magnitude]
calcs[, running_total := (50 + cumsum(result))]
calcs[, running_total_mod := (50 + cumsum(result)) %% 100]

calcs2[, result := direction]
calcs2[, running_total := (50 + cumsum(result))]
calcs2[, running_total_mod := (50 + cumsum(result)) %% 100]


result_1 <- calcs[running_total_mod == 0, .N]

result_2 <- calcs2[running_total_mod == 0, .N]



