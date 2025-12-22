library(here)
library(data.table)

input <-
  fread(
    file = here("data", "2025_03", "input"),
    header = FALSE,
    colClasses = c("character"),
    col.names = ("banks")
  )

calcs <- input |> data.table::copy()

calcs[, c("joltage") := {

  batteries <- as.integer(strsplit(banks, "")[[1]])
  num_batteries <- length(batteries)

  possible_batteries_01 <- batteries[1:(num_batteries)-1]
  battery_01 <- possible_batteries_01 |> max()
  battery_01_position <- which(possible_batteries_01 == battery_01)[1]

  possible_batteries_02 <- batteries[(battery_01_position+1):(num_batteries)-0]
  battery_02 <- possible_batteries_02 |> max()
  battery_02_position <- which(possible_batteries_02 == battery_01)[1]

  joltage = 10 * battery_01 + battery_02

  list(joltage)

}, by = banks]


result_1 <- calcs$joltage |> sum()


calcs_2 <- input |> data.table::copy()

calcs_2[, c("joltage") := {

  batteries <- as.integer(strsplit(banks, "")[[1]])
  num_batteries <- length(batteries)

  possible_batteries_01 <- batteries[1:(num_batteries - 11)]
  battery_01 <- possible_batteries_01 |> max()
  battery_01_position <- which(possible_batteries_01 == battery_01)[1]

  possible_batteries_02 <- batteries[(battery_01_position + 1):(num_batteries - 10)]
  battery_02 <- possible_batteries_02 |> max()
  battery_02_position <- which(possible_batteries_02 == battery_02)[1] + battery_01_position

  possible_batteries_03 <- batteries[(battery_02_position + 1):(num_batteries - 9)]
  battery_03 <- possible_batteries_03 |> max()
  battery_03_position <- which(possible_batteries_03 == battery_03)[1] + battery_02_position

  possible_batteries_04 <- batteries[(battery_03_position + 1):(num_batteries - 8)]
  battery_04 <- possible_batteries_04 |> max()
  battery_04_position <- which(possible_batteries_04 == battery_04)[1] + battery_03_position

  possible_batteries_05 <- batteries[(battery_04_position + 1):(num_batteries - 7)]
  battery_05 <- possible_batteries_05 |> max()
  battery_05_position <- which(possible_batteries_05 == battery_05)[1] + battery_04_position

  possible_batteries_06 <- batteries[(battery_05_position + 1):(num_batteries - 6)]
  battery_06 <- possible_batteries_06 |> max()
  battery_06_position <- which(possible_batteries_06 == battery_06)[1] + battery_05_position

  possible_batteries_07 <- batteries[(battery_06_position + 1):(num_batteries - 5)]
  battery_07 <- possible_batteries_07 |> max()
  battery_07_position <- which(possible_batteries_07 == battery_07)[1] + battery_06_position

  possible_batteries_08 <- batteries[(battery_07_position + 1):(num_batteries - 4)]
  battery_08 <- possible_batteries_08 |> max()
  battery_08_position <- which(possible_batteries_08 == battery_08)[1] + battery_07_position

  possible_batteries_09 <- batteries[(battery_08_position + 1):(num_batteries - 3)]
  battery_09 <- possible_batteries_09 |> max()
  battery_09_position <- which(possible_batteries_09 == battery_09)[1] + battery_08_position

  possible_batteries_10 <- batteries[(battery_09_position + 1):(num_batteries - 2)]
  battery_10 <- possible_batteries_10 |> max()
  battery_10_position <- which(possible_batteries_10 == battery_10)[1] + battery_09_position

  possible_batteries_11 <- batteries[(battery_10_position + 1):(num_batteries - 1)]
  battery_11 <- possible_batteries_11 |> max()
  battery_11_position <- which(possible_batteries_11 == battery_11)[1] + battery_10_position

  possible_batteries_12 <- batteries[(battery_11_position + 1):(num_batteries - 0)]
  battery_12 <- possible_batteries_12 |> max()
  battery_12_position <- which(possible_batteries_12 == battery_12)[1]+ battery_11_position

  joltage =
      100000000000 * battery_01 +
      10000000000 * battery_02 +
      1000000000 * battery_03 +
      100000000 * battery_04 +
      10000000 * battery_05 +
      1000000 * battery_06 +
      100000 * battery_07 +
      10000 * battery_08 +
      1000 * battery_09 +
      100 * battery_10 +
      10 * battery_11 +
      1 * battery_12

  list(joltage)

}, by = banks]

result_2 <- calcs_2$joltage |> sum() |> as.character()

result_2 |> print()
