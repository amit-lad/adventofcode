filepath <- here("data", "2025_10", "input")

# Read with fread, treating the entire line as one column
dt <- fread(file = filepath, header = FALSE, sep = "\n", col.names = "raw")









# Parse the raw column into the three desired columns
dt[, `:=`(
  light = sub(".*\\[(.*)\\].*", "\\1", raw),
  switches = lapply(raw, function(line) {
    # Extract all parentheses groups
    switches_matches <- regmatches(line, gregexpr("\\([^)]*\\)", line))[[1]]
    # Convert each to numeric vector, adding 1 to each number
    lapply(switches_matches, function(s) {
      nums <- gsub("[()]", "", s)
      if (nums == "") return(integer(0))
      as.integer(strsplit(nums, ",")[[1]]) + 1L
    })
  }),
  joltage = sub(".*\\{(.*)\\}.*", "\\1", raw)
)]

# Add light_numeric column as a list of integer vectors
dt[, light_numeric := lapply(light, function(l) {
  as.integer(chartr(".#", "01", strsplit(l, "")[[1]]))
})]

# Add joltage_numeric column
dt[, joltage_numeric := lapply(joltage, function(j) {
  as.integer(strsplit(j, ",")[[1]])
})]

# Add joltage_counter column
dt[, joltage_counter := 0L]

# Add zero_target column
dt[, zero_target := lapply(joltage_numeric, function(j) {
  integer(length(j))
})]

# Add num_lights column
dt[, num_lights := nchar(light)]

# Add switch_impact column
dt[, switch_impact := Map(function(sw, nl) {
  # For each switch group, create a binary vector of length nl
  lapply(sw, function(positions) {
    impact <- integer(nl)
    impact[positions] <- 1L
    impact
  })
}, switches, num_lights)]

# Add switch_permutations column
dt[, switch_permutations := lapply(switches, function(sw) {
  n <- length(sw)
  num_perms <- 2^n

  # Generate all binary permutations
  lapply(0:(num_perms - 1), function(i) {
    # Convert i to binary representation of length n
    as.integer(intToBits(i)[1:n])
  })
})]

# Add lights_after_permutation column
dt[, lights_after_permutation := Map(function(perms, impacts) {
  # For each permutation, calculate which switches are active
  lapply(perms, function(perm) {
    # Sum the impacts for active switches (where perm == 1), then mod 2
    result <- integer(length(impacts[[1]]))
    for (i in seq_along(perm)) {
      if (perm[i] == 1) {
        result <- result + impacts[[i]]
      }
    }
    as.integer(result %% 2)
  })
}, switch_permutations, switch_impact)]

# Add joltage_used_from_permutation column
dt[, joltage_used_from_permutation := Map(function(perms, impacts, jolt_num) {
  # For each permutation, calculate joltage used (no mod 2)
  lapply(perms, function(perm) {
    result <- integer(length(jolt_num))
    for (i in seq_along(perm)) {
      if (perm[i] == 1) {
        # Apply the impact pattern to joltage values
        for (pos in which(impacts[[i]] == 1)) {
          result[pos] <- result[pos] + 1
        }
      }
    }
    as.integer(result)
  })
}, switch_permutations, switch_impact, joltage_numeric)]

# Add successful_permutations column
dt[, successful_permutations := Map(function(perms, lights_after, target) {
  # Find which permutations result in the target light configuration
  success_indices <- which(sapply(lights_after, function(x) identical(x, target)))
  perms[success_indices]
}, switch_permutations, lights_after_permutation, light_numeric)]

# Add num_switch_pressed column
dt[, num_switch_pressed := lapply(successful_permutations, function(perms) {
  sapply(perms, sum)
})]

# Add min_num_switch_pressed column
dt[, min_num_switch_pressed := sapply(num_switch_pressed, function(x) {
  if (length(x) == 0) NA_integer_ else min(x)
})]

# Recursive function to find minimum button presses with path tracking
find_min_presses <- function(jolt_current, zero_target, lights_after, switch_perms, jolt_used, current_count = 0, depth = 0, max_depth = 1000000, current_path = c()) {
  indent <- paste(rep("  ", depth), collapse = "")

  cat(indent, "Depth:", depth, "Count:", current_count, "\n")
  cat(indent, "Current joltage:", paste(jolt_current, collapse = ","), "\n")
  cat(indent, "Zero target:    ", paste(zero_target, collapse = ","), "\n")
  cat(indent, "Current path:   ", paste(current_path, collapse = ","), "\n")

  # Check depth limit
  if (depth > max_depth) {
    cat(indent, "MAX DEPTH EXCEEDED - returning 1000000000\n")
    return(list(count = 1000000000, path = c()))
  }

  # Base case: reached target
  # Use all.equal to handle numeric comparison with tolerance
  if (isTRUE(all.equal(jolt_current, zero_target)) && all(jolt_current == zero_target)) {
    cat(indent, "SUCCESS! Reached target with count:", current_count, "\n")
    return(list(count = current_count, path = current_path))
  }

  # Step 1: Calculate parity
  jolt_parity <- as.integer(jolt_current %% 2)
  cat(indent, "Joltage parity:", paste(jolt_parity, collapse = ","), "\n")

  # Step 2: Find matching permutations
  matching_indices <- which(sapply(lights_after, function(x) identical(x, jolt_parity)))

  cat(indent, "Matching indices:", length(matching_indices), "\n")

  # If no matches, return large number
  if (length(matching_indices) == 0) {
    cat(indent, "NO MATCHES - returning 1000000000\n")
    return(list(count = 1000000000, path = c()))
  }

  # Step 3: Try each matching permutation
  min_result <- 1000000000
  best_path <- c()

  for (idx in matching_indices) {
    cat(indent, "  Trying permutation index:", idx, "\n")

    # Step 3A: Count button presses
    button_presses <- sum(switch_perms[[idx]])
    cat(indent, "    Button presses:", button_presses, "\n")
    cat(indent, "    Switch pattern:", paste(switch_perms[[idx]], collapse = ","), "\n")

    # Step 3B: Calculate new joltage
    cat(indent, "    Joltage used:", paste(jolt_used[[idx]], collapse = ","), "\n")

    jolt_diff <- jolt_current - jolt_used[[idx]]
    cat(indent, "    Joltage diff:", paste(jolt_diff, collapse = ","), "\n")

    # Check for negative values
    if (any(jolt_diff < 0)) {
      cat(indent, "    NEGATIVE - skipping\n")
      next  # Skip this path
    }

    new_jolt <- jolt_diff / 2
    cat(indent, "    New joltage:", paste(new_jolt, collapse = ","), "\n")

    # Calculate new count: add (depth + 1) * button_presses
    new_count <- current_count + (2^depth) * button_presses
    cat(indent, "    New count:", new_count, "(was", current_count, "+", depth + 1, "*", button_presses, ")\n")

    # Step 4: Recurse with updated path
    result <- find_min_presses(new_jolt, zero_target, lights_after, switch_perms, jolt_used, new_count, depth + 1, max_depth, c(current_path, idx))

    cat(indent, "  Result from this path:", result$count, "\n")

    if (result$count < min_result) {
      min_result <- result$count
      best_path <- result$path
    }
  }

  cat(indent, "Minimum result at this level:", min_result, "\n")
  return(list(count = min_result, path = best_path))
}

# Apply recursive function to each row
cat("=== PROCESSING ROW 1 ===\n")
dt[, c("min_joltage_presses", "min_path") := {
  results <- Map(function(jolt_num, zero_tgt, lights_after, switch_perms, jolt_used) {
    find_min_presses(jolt_num, zero_tgt, lights_after, switch_perms, jolt_used)
  }, joltage_numeric, zero_target, lights_after_permutation, switch_permutations, joltage_used_from_permutation)

  list(
    min_joltage_presses = lapply(results, function(x) x$count),
    min_path = lapply(results, function(x) x$path)
  )
}]

# Remove the raw column
dt[, raw := NULL]

# Save the sum of min_num_switch_pressed
result_1 <- sum(dt$min_num_switch_pressed, na.rm = TRUE)

# Save the sum of min_joltage_presses
result_2 <- sum(unlist(dt$min_joltage_presses))

# Display result
print(dt)
print(paste("Result 1:", result_1))
print(paste("Result 2:", result_2))

# Display the minimum paths for verification
cat("\n=== MINIMUM PATHS ===\n")
for (i in 1:nrow(dt)) {
  cat("Row", i, "- Min presses:", dt$min_joltage_presses[[i]], "- Path indices:", paste(dt$min_path[[i]], collapse = ", "), "\n")
}
