# Build global connectivity matrices from:
# 1) one fixed adjacency matrix
# 2) three environmental similarity matrices
# 3) three topographic similarity matrices
#
# Formula used for each cell:
# global connectivity = adjacency * environmental similarity * topographic similarity

base_dir <- "data/connectivity"
raw_dir <- file.path(base_dir, "raw")
output_dir <- file.path(base_dir, "output")

options(scipen = 999)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Remove old generated connectivity matrices before writing the current set.
old_global_files <- list.files(output_dir,
                               pattern = "^global_connectivity_[0-9]+\\.txt$",
                               full.names = TRUE)

unlink(old_global_files)

# Read the simple event timing file.
# start_ma is the older boundary, end_ma is the younger boundary.
event_times <- read.csv(file.path(base_dir, "event_times.csv"))

# Read the fixed adjacency matrix.
adjacency <- read.csv(file.path(raw_dir, "adjacency", "adjacency.csv"),
                      row.names = 1,
                      check.names = FALSE)
adjacency <- as.matrix(adjacency)

# Read the environmental matrices.
env_before <- read.csv(file.path(raw_dir, "environmental", "before_dry_diagonal.csv"),
                       row.names = 1,
                       check.names = FALSE)
env_during <- read.csv(file.path(raw_dir, "environmental", "dry_diagonal_formation.csv"),
                       row.names = 1,
                       check.names = FALSE)
env_after <- read.csv(file.path(raw_dir, "environmental", "after_dry_diagonal.csv"),
                      row.names = 1,
                      check.names = FALSE)

env_before <- as.matrix(env_before)
env_during <- as.matrix(env_during)
env_after <- as.matrix(env_after)

# Read the topographic matrices.
topo_before <- read.csv(file.path(raw_dir, "topographic", "before_andes_uplift.csv"),
                        row.names = 1,
                        check.names = FALSE)
topo_during <- read.csv(file.path(raw_dir, "topographic", "during_andes_uplift.csv"),
                        row.names = 1,
                        check.names = FALSE)
topo_after <- read.csv(file.path(raw_dir, "topographic", "after_andes_uplift.csv"),
                       row.names = 1,
                       check.names = FALSE)

topo_before <- as.matrix(topo_before)
topo_during <- as.matrix(topo_during)
topo_after <- as.matrix(topo_after)

# Pull out the event times for each variable.
env_event <- event_times[event_times$variable == "environmental", ]
topo_event <- event_times[event_times$variable == "topographic", ]

env_start <- env_event$start_ma
env_end <- env_event$end_ma

topo_start <- topo_event$start_ma
topo_end <- topo_event$end_ma

# Build the global time intervals automatically from all event boundaries.
breaks <- sort(unique(c(event_times$start_ma, event_times$end_ma)), decreasing = TRUE)
breaks <- c(Inf, breaks, 0)

interval_summary <- data.frame()

for (i in 1:(length(breaks) - 1)) {

  interval_start <- breaks[i]
  interval_end <- breaks[i + 1]

  # Use a representative age inside the interval to decide which state applies.
  if (is.infinite(interval_start)) {
    interval_age <- interval_end + 1
  } else {
    interval_age <- mean(c(interval_start, interval_end))
  }

  # Environmental state.
  if (interval_age > env_start) {
    env_state <- "before dry diagonal"
    env_file <- "before_dry_diagonal.csv"
    env_matrix <- env_before
  }

  if (interval_age <= env_start & interval_age >= env_end) {
    env_state <- "dry diagonal formation"
    env_file <- "dry_diagonal_formation.csv"
    env_matrix <- env_during
  }

  if (interval_age < env_end) {
    env_state <- "after dry diagonal"
    env_file <- "after_dry_diagonal.csv"
    env_matrix <- env_after
  }

  # Topographic state.
  if (interval_age > topo_start) {
    topo_state <- "before Andes uplift"
    topo_file <- "before_andes_uplift.csv"
    topo_matrix <- topo_before
  }

  if (interval_age <= topo_start & interval_age >= topo_end) {
    topo_state <- "during Andes uplift"
    topo_file <- "during_andes_uplift.csv"
    topo_matrix <- topo_during
  }

  if (interval_age < topo_end) {
    topo_state <- "after Andes uplift"
    topo_file <- "after_andes_uplift.csv"
    topo_matrix <- topo_after
  }

  # Multiply the matrices cell-by-cell.
  global_matrix <- adjacency * env_matrix * topo_matrix

  output_file <- paste0("global_connectivity_",
                        i,
                        ".txt")

  # Save the global matrix as plain numbers only:
  # no row names, no column names, and one space between values.
  write.table(global_matrix,
              file.path(output_dir, output_file),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)

  interval_summary <- rbind(interval_summary,
                            data.frame(interval = i,
                                       start_ma = interval_start,
                                       end_ma = interval_end,
                                       environmental_state = env_state,
                                       environmental_file = env_file,
                                       topographic_state = topo_state,
                                       topographic_file = topo_file,
                                       global_connectivity_file = output_file))
}

# Save the interval information table.
write.csv(interval_summary,
          file.path(output_dir, "interval_info.csv"),
          row.names = FALSE,
          quote = FALSE)

# Save only the finite time breakpoints, one per line.
# This is useful as a simple input file for RevBayes.
interval_breakpoints <- breaks[is.finite(breaks) & breaks != 0]

writeLines(as.character(interval_breakpoints),
           file.path(output_dir, "interval_breakpoints.txt"))

print(interval_summary)
