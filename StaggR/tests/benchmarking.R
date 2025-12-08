#!/usr/bin/env Rscript
# ==============================================================================
# StaggR Algorithm: Comprehensive Performance & RAM Profiling
# ==============================================================================

if (!requireNamespace("bench", quietly = TRUE)) install.packages("bench")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(bench)
library(patchwork) 

# === CONFIGURATION ===
n_runs <- 500
# =====================

# --- 1. The Core Algorithm ---
findOptimalInterval <- function(step_durations, time_to_next, n_samples, 
                                granularity, buffer_time, max_iterations = 20000) {
  
  if (n_samples < 2) return(0)
  n_steps <- length(step_durations)
  if (n_steps == 0) return(0)
  
  # Base Profile Construction
  step_starts <- numeric(n_steps)
  step_starts[1] <- 0
  if (n_steps > 1) {
    for (k in 2:n_steps) {
      step_starts[k] <- step_starts[k-1] + step_durations[k-1] + time_to_next[k-1]
    }
  }
  base_starts <- step_starts
  base_ends   <- step_starts + step_durations + buffer_time
  single_sample_duration <- if(length(base_ends) > 0) max(base_ends) else 0
  
  # Optimization Loop
  i <- granularity
  solution_found <- FALSE
  iter <- 0
  
  while(!solution_found && iter < max_iterations) {
    iter <- iter + 1
    has_conflict <- FALSE
    
    max_k <- if(i > 0) ceiling(single_sample_duration / i) else n_samples
    k_limit <- min(n_samples - 1, max_k)
    
    if (k_limit > 0) {
      k_vec <- 1:k_limit
      shifts <- k_vec * i
      
      shifted_starts_mat <- outer(base_starts, shifts, "+")
      shifted_ends_mat   <- outer(base_ends,   shifts, "+")
      
      for (col_idx in seq_along(shifts)) {
        s_starts <- shifted_starts_mat[, col_idx]
        s_ends   <- shifted_ends_mat[, col_idx]
        if (any(outer(base_starts, s_ends, "<") & outer(base_ends, s_starts, ">"))) {
          has_conflict <- TRUE
          break 
        }
      }
    }
    
    if (has_conflict) i <- i + granularity else solution_found <- TRUE
  }
  return(i)
}

# --- 2. Data Generators & Measurement Wrapper ---

make_test_protocol <- function(n_steps, density = 0.5) {
  total_time <- n_steps * 10 
  step_time <- total_time * density / n_steps
  wait_time <- total_time * (1 - density) / max(1, n_steps - 1)
  list(durations = rep(step_time, n_steps), ttn = rep(wait_time, max(1, n_steps - 1)))
}

# wrapper to measure time and RAM
measure_perf_row <- function(fun, n_runs) {
  stopifnot(is.function(fun))
  
  res <- bench::mark(fun(), iterations = n_runs, check = FALSE, filter_gc = FALSE)
  
  t_sec <- as.numeric(res$time[[1]])
  
  mem <- res$mem_alloc
  mem_vec <- if (is.list(mem)) mem[[1]] else mem
  
  list(
    time_ms = mean(t_sec) * 1000, 
    sd_ms   = sd(t_sec) * 1000,
    
    mem_mb  = mean(as.numeric(mem_vec)) / 1024^2,
    res = list(res)
  )
}

# --- 3. The Experiments ---

cat(paste0("Running Full Profiling (Time + RAM) with ", n_runs, " replicates...\n"))

# A. Granularity
cat("... Testing Granularity\n")
df_granularity <- expand.grid(granularity = seq(0.1, 5.0, by = 0.1)) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(5, 0.5)
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    g_val <- granularity 
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, 50, g_val, 0.25)
    }
    measure_perf_row(run_algo, n_runs = n_runs)
  })) %>%
  unnest_wider(stats)

# B. Sample Count
cat("... Testing Sample Count\n")
df_samples <- expand.grid(n_samples = c(2, 5, 10, 20, 30, 40, 50, 75, 100)) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(5, 0.5)
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    n_val <- n_samples 
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, n_val, 0.5, 0.25)
    }
    measure_perf_row(run_algo, n_runs = n_runs)
  })) %>%
  unnest_wider(stats)

# C. Step Count
cat("... Testing Step Count\n")
df_steps <- expand.grid(n_steps = seq(2, 50, by = 2)) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(n_steps, 0.5)
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, 50, 0.5, 0.25)
    }
    measure_perf_row(run_algo, n_runs = n_runs)
  })) %>%
  unnest_wider(stats)

# D. Protocol Density
cat("... Testing Density\n")
df_density <- expand.grid(density = seq(0.1, 0.95, by = 0.025)) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(5, density) 
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, 50, 0.1, 0.25)
    }
    measure_perf_row(run_algo, n_runs = n_runs) 
  })) %>%
  unnest_wider(stats)

# E. Heatmap
cat("... Generating Heatmap Data for Granularity vs N_Steps\n")
df_heatmap_granularity_vs_steps <- expand.grid(
  granularity = c(0.1, 0.25, 0.5, 1, 2),
  n_steps = c(5, 10, 20, 30, 40, 50)
) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(n_steps, 0.5)
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    g_val <- granularity 
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, 50, g_val, 0.25)
    }
    measure_perf_row(run_algo, n_runs = n_runs)
  })) %>%
  unnest_wider(stats)


# F. n_samples vs n_steps Heatmap
cat("... Generating Heatmap Data for N_Samples vs N_Steps \n")
df_heatmap_samples_vs_steps <- expand.grid(
  n_samples = c(2, 5, 10, 25, 50),
  n_steps = c(2, 3, 5, 10, 20, 30, 40, 50)
) %>%
  rowwise() %>%
  mutate(stats = list({
    proto_dat <- make_test_protocol(n_steps, 0.5)
    d_val <- proto_dat$durations
    t_val <- proto_dat$ttn
    n_val <- n_samples 
    
    run_algo <- function() {
      findOptimalInterval(d_val, t_val, n_val, 0.5, 0.25)
    }
    measure_perf_row(run_algo, n_runs = 10)
  })) %>%
  unnest_wider(stats)


# --- 4. Visualizations (Time & RAM) ---

# Helper for consistent Time Plots with LM
plot_time <- function(data, x_var, title) {
  ggplot(data, aes_string(x = x_var, y = "time_ms")) +
    geom_ribbon(aes(ymin = pmax(0,time_ms - sd_ms), ymax = time_ms + sd_ms), fill = "#E31A1C", alpha = 0.2) +
    geom_line(color = "#E31A1C", size = 1) +
    labs(title = title,  y = "Time (ms)") + theme_minimal()
}

# Helper for consistent RAM Plots with LM and SD
plot_ram <- function(data, x_var, title) {
  ggplot(data, aes_string(x = x_var, y = "mem_mb")) +
    # Changed from geom_area to ribbon/line to show SD
    geom_line(color = "#1F78B4", size = 1) +
    labs(title = title, y = "Total RAM Allocated (MB)") + theme_minimal()
}

# --- Plot Set 1: Granularity ---
p1_time <- plot_time(df_granularity, "granularity", "A Precision Cost (Time)") 
p1_ram  <- plot_ram(df_granularity, "granularity", "B Precision Cost (RAM)") 

# --- Plot Set 2: Sample Count ---
p2_time <- plot_time(df_samples, "n_samples", "C Throughput Plateau (Time)")
p2_ram  <- plot_ram(df_samples, "n_samples", "D Throughput Plateau (RAM)")

# --- Plot Set 3: Matrix Complexity ---
p3_time <- plot_time(df_steps, "n_steps", "E Step Complexity (Time)")
p3_ram  <- plot_ram(df_steps, "n_steps", "F Step Complexity (RAM)")

# --- Plot Set 4: Density ---
p4_time <- plot_time(df_density, "density", "G Density Difficulty (Time)") 
p4_ram  <- plot_ram(df_density, "density", "H Density Difficulty (RAM)")

# --- Plot Set 5: Heatmap of Granularity vs Steps ---
p5_time <- ggplot(df_heatmap_granularity_vs_steps, aes(x = factor(granularity), y = factor(n_steps), fill = time_ms)) +
  geom_tile() + geom_text(aes(label = round(time_ms, 2)), color = "blue", size = 2.5) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "I Granularity vs Steps Time (ms)", x = "Granularity (s)", y = "Steps") + theme_minimal()

p5_ram <- ggplot(df_heatmap_granularity_vs_steps, aes(x = factor(granularity), y = factor(n_steps), fill = mem_mb)) +
  geom_tile() + geom_text(aes(label = round(mem_mb, 2)), color = "red", size = 2.5) +
  scale_fill_viridis_c(option = "mako", direction = -1) + 
  labs(title = "J Granularity vs Steps RAM (MB)", x = "Granularity (s)", y = "Steps") + theme_minimal()

# --- Plot Set 5: Heatmap of Samples vs Steps ---
p6_time <- ggplot(df_heatmap_samples_vs_steps, aes(x = factor(n_samples), y = factor(n_steps), fill = time_ms)) +
  geom_tile() + geom_text(aes(label = round(time_ms, 2)), color = "blue", size = 2.5) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "K Samples vs Steps Time (ms)", x = "Samples", y = "Steps") + theme_minimal()

p6_ram <- ggplot(df_heatmap_samples_vs_steps, aes(x = factor(n_samples), y = factor(n_steps), fill = mem_mb)) +
  geom_tile() + geom_text(aes(label = round(mem_mb, 2)), color = "red", size = 2.5) +
  scale_fill_viridis_c(option = "mako", direction = -1) + 
  labs(title = "L Samples vs Steps RAM (MB)", x = "Samples", y = "Steps") + theme_minimal()

# --- Print ---
cat("... Saving Plot to ./imgs/test_data/ \n")
if(!dir.exists("imgs")) dir.create("imgs")

pdf(file = "imgs/staggR_performance_ram_profiling.pdf", width = 11, height = 17)
gridExtra::grid.arrange(
  p1_time, p1_ram,
  p2_time, p2_ram,
  p3_time, p3_ram,
  p4_time, p4_ram,
  p5_time, p5_ram,
  p6_time, p6_ram,
  ncol = 2
)
dev.off()

# --- 4. Data Export ---

cat("... Exporting Data to ./data/test_data/ \n")
if(!dir.exists("data/test_data")) dir.create("data/test_data")

# Helper function to strip list columns and write
write_csv <- function(df, name) {
  # Remove the 'res' list-column which cannot be written to CSV
  clean_df <- df %>% select(-res) 
  write.csv(clean_df, file = file.path("data/test_data", name), row.names = FALSE)
}

write_csv(df_granularity, "1_precision_cost_granularity.csv")
write_csv(df_samples,     "2_throughput_plateau_samples.csv")
write_csv(df_steps,       "3_step_complexity.csv")
write_csv(df_density,     "4_density_difficulty.csv")
write_csv(df_heatmap_granularity_vs_steps, "5_heatmap_granularity_steps.csv")
write_csv(df_heatmap_samples_vs_steps,     "6_heatmap_samples_steps.csv")


cat("... Export Complete. \n")
