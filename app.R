# ==============================================================================
# StaggR: An Interactive Shiny App for Optimizing Staggered Protocols
# Version 1.1.2
#
# Author: Alex Francette
# Date: 2025-12-09
#
# Description:
# This application provides a graphical user interface to design, optimize,
# and execute complex, time-sensitive laboratory protocols that involve
# staggering multiple samples over a single, repeated workflow. It is
# particularly useful for time-course experiments in molecular biology,
# biochemistry, and other experimental sciences.
# ==============================================================================

# --- 1. Load Required Libraries ---
# ------------------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(colorspace)
library(DT)
library(colourpicker)
library(zip)
library(openxlsx)
library(rmarkdown)
library(kableExtra)
library(jsonlite)

# --- 2. Helper Functions ---
# ------------------------------------------------------------------------------

# A fixed, colorblind-friendly palette for initial step colors.
fixed_color_palette <- c(
  "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"
)

# Generates a set of visually distinct colors for samples based on a starting hue.
generate_sample_colors <- function(n, base_hue = 210) {
  if (n == 0) return(character(0))
  base_hues <- seq(base_hue - 45, base_hue + 45, length.out = ceiling(n / 2) + 1)[-1] %% 360
  colors <- character(n)
  safe_hcl <- function(h, c_val, l_val) {
    col <- try(hcl(h = h, c = c_val, l = l_val), silent = TRUE)
    if (is.na(col) || inherits(col, "try-error")) {
      while ((is.na(col) || inherits(col, "try-error")) && c_val > 0) {
        c_val <- c_val - 5
        col <- try(hcl(h = h, c = c_val, l = l_val), silent = TRUE)
      }
    }
    ifelse(is.na(col) || inherits(col, "try-error"), "#FFFFFF", col)
  }
  for (i in seq_len(n)) {
    hue_index <- ((i + 1) %/% 2)
    hue_val <- base_hues[hue_index]
    if (i %% 2 == 1) colors[i] <- safe_hcl(hue_val, 60, 80) else colors[i] <- safe_hcl(hue_val, 30, 90)
  }
  colors
}

# Creates a perceptually uniform color gradient from a base color.
generate_color_gradient <- function(base_color_hex, n) {
  if (is.null(base_color_hex) || is.na(base_color_hex) || n <= 0) return(rep("#FFFFFF", n))
  if (n == 1) return(base_color_hex)
  
  rgb_base <- try(colorspace::hex2RGB(base_color_hex), silent = TRUE)
  if (inherits(rgb_base, "try-error")) return(rep("#FF00FF", n))
  
  hcl_base <- methods::as(rgb_base, "polarLUV")
  h <- coords(hcl_base)[, "H"]; c <- coords(hcl_base)[, "C"]; l0 <- coords(hcl_base)[, "L"]
  l_seq <- seq(max(15, l0 - 20), min(95, l0 + 20), length.out = n)
  
  as.character(unname(sapply(l_seq, function(l_val) hcl(h, c, l_val, fixup = TRUE))))
}

# Formats a numeric time in minutes into a "X min Y sec" string.
format_time_interval <- function(interval_in_minutes) {
  minutes <- floor(interval_in_minutes)
  seconds <- round((interval_in_minutes %% 1) * 60)
  paste(minutes, "min", seconds, "sec")
}

# Formats a numeric time in minutes into a "hh:mm:ss" string.
format_hhmmss <- function(minutes) {
  # This sapply() wrapper allows the function to work on single values or vectors.
  sapply(minutes, function(min) {
    # Check for NA or non-finite values for each element individually.
    if (is.na(min) || !is.finite(min)) {
      return("")
    }
    total_seconds <- round(min * 60)
    h <- floor(total_seconds / 3600)
    m <- floor((total_seconds %% 3600) / 60)
    s <- total_seconds %% 60
    sprintf("%02d:%02d:%02d", h, m, s)
  })
}
# Determines if a given color is "dark" to decide on contrasting font color (black/white).
is_color_dark <- function(color) {
  rgb_vals <- try(col2rgb(color), silent = TRUE)
  if (inherits(rgb_vals, "try-error")) return(FALSE)
  luminance <- 0.299*rgb_vals[1,] + 0.587*rgb_vals[2,] + 0.114*rgb_vals[3,]
  luminance < 128
}

# Converts a numeric time value to minutes from a specified unit.
to_minutes <- function(x, unit) {
  unit <- tolower(unit)
  dplyr::case_when(
    unit %in% c("sec", "second", "seconds") ~ x / 60,
    unit %in% c("hr", "hour", "hours")     ~ x * 60,
    TRUE                                   ~ x
  )
}

# Converts a numeric time value from minutes to a specified unit.
from_minutes <- function(x_min, unit) {
  unit <- tolower(unit)
  dplyr::case_when(
    unit %in% c("sec", "second", "seconds") ~ x_min * 60,
    unit %in% c("hr", "hour", "hours")     ~ x_min / 60,
    TRUE                                   ~ x_min
  )
}

# Defines the standard unit choices available in the UI.
unit_choices <- c("sec","min","hr")

# --- Sample name parsing/formatting helpers ---
# Accept either newline-separated or comma-separated lists.
# Also trims whitespace and strips trailing commas often present in example files.
parse_sample_names <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  txt <- paste(x, collapse = "\n")
  txt <- gsub("\r\n?", "\n", txt)

  # Prefer newline splitting when present (safer for names that might contain commas).
  parts <- if (grepl("\n", txt, fixed = TRUE)) {
    unlist(strsplit(txt, "\n", fixed = TRUE))
  } else {
    unlist(strsplit(txt, ",", fixed = TRUE))
  }

  parts <- trimws(parts)
  # Strip trailing delimiter punctuation (common in bundled example files)
  parts <- gsub("[,;]+$", "", parts)
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  parts
}

format_sample_names <- function(sample_names, prefer_newlines = TRUE) {
  if (is.null(sample_names) || length(sample_names) == 0) return("")
  if (prefer_newlines) paste(sample_names, collapse = "\n") else paste(sample_names, collapse = ", ")
}

# --- 3. Core Calculation Function ---
# ------------------------------------------------------------------------------
# This is the main engine of the app. It takes all protocol parameters and an interval,
# then calculates the complete schedule, including hands-on times and potential conflicts.
findOptimalInterval <- function(step_durations, time_to_next, n_samples, 
                                granularity, buffer_time, max_iterations = 20000) {
  
  # dummy test values
  # step_durations <- c(10, 20, 15); time_to_next <- c(5, 10); n_samples <- 4; granularity <- 0.25; buffer_time <- 0; max_iterations <- 20000
  # 1. Edge Case Handling
  if (n_samples < 2) return(0)
  n_steps <- length(step_durations)
  if (n_steps == 0) return(0)
  
  # 2. Calculate Base Profile (Single Sample Busy Times)
  # This recreates the "physics" of a single run to see where the blocks are
  step_starts <- numeric(n_steps)
  step_starts[1] <- 0
  if (n_steps > 1) {
    for (k in 2:n_steps) {
      step_starts[k] <- step_starts[k-1] + step_durations[k-1] + time_to_next[k-1]
    }
  }
  
  # Define "Busy" windows (Start to End + Buffer)
  base_starts <- step_starts
  base_ends   <- step_starts + step_durations + buffer_time
  
  # Total duration of one sample (used to optimize the loop)
  single_sample_duration <- if(length(base_ends) > 0) max(base_ends) else 0
  
  # 3. The Optimization Loop
  i <- granularity
  solution_found <- FALSE
  current_iteration <- 0
  
  while(!solution_found && current_iteration < max_iterations) {
    current_iteration <- current_iteration + 1
    has_conflict <- FALSE
    
    # LOGIC SHORTCUT:
    # Only check if Sample 1 overlaps with shifted versions of itself (Samples 2, 3...)
    # Stop checking if the shift is larger than the whole protocol duration.
    max_k <- if(i > 0) ceiling(single_sample_duration / i) else n_samples
    k_limit <- min(n_samples - 1, max_k)
    
    if (k_limit > 0) {
      # Vectorized check for all relevant subsequent samples
      k_vec <- 1:k_limit
      shifts <- k_vec * i
      
      # Compare the Base Sample against ALL shifted samples at once using outer()
      # This creates matrices of starts and ends
      # Overlap Logic: (StartA < EndB) AND (EndA > StartB)
      
      # Dimensions: [Rows = Steps, Cols = Shifts]
      shifted_starts_mat <- outer(base_starts, shifts, "+")
      shifted_ends_mat   <- outer(base_ends,   shifts, "+")
      
      # Check if ANY step in the base sample overlaps with ANY step in the shifted samples.
      # Iterate over the shifts (k) 
      
      for (col_idx in seq_along(shifts)) {
        s_starts <- shifted_starts_mat[, col_idx]
        s_ends   <- shifted_ends_mat[, col_idx]
        
        # Check Base Steps vs Shifted Steps (Matrix comparison)
        overlaps <- outer(base_starts, s_ends, "<") & outer(base_ends, s_starts, ">")
        
        if (any(overlaps)) {
          has_conflict <- TRUE
          break # Conflict found for this interval 'i', stop checking k
        }
      }
    }
    
    if (has_conflict) {
      i <- i + granularity
    } else {
      solution_found <- TRUE
    }
  }
  
  if (solution_found && i >= single_sample_duration) {
    return("No Stagger Time < Protocol Duration Identified. Consider Decreasing Granularity, Number of Damples, or Step-durration.")
  } else if (solution_found) return(i) else return(NULL)
}

calculateStaggering <- function(step_names, time_to_next, step_duration, n_samples, custom_sample_names,
                                custom_colors, interval, task_switching_time, overbooked_color, buffer_color) {
  
  # Input validation
  if (length(step_names) != length(step_duration) || length(step_names) != (length(time_to_next) + 1)) {
    stop("Input vector lengths are mismatched.")
  }
  if (length(custom_sample_names) != n_samples) {
    stop(paste("Number of custom sample names (", length(custom_sample_names), ") does not match nSamples (", n_samples, ")."))
  }

  # Factor levels cannot contain duplicates; make sample names unique if needed.
  # We do this inside the core engine so callers can't accidentally crash the app.
  if (anyDuplicated(custom_sample_names)) {
    custom_sample_names <- make.unique(custom_sample_names)
  }
  
  # Calculate the start time of each step for a single sample workflow.
  step_starts <- numeric(length(step_names))
  if (length(step_names) > 0) {
    step_starts[1] <- 0
    if (length(step_names) > 1) {
      for (i in 2:length(step_names)) {
        step_starts[i] <- step_starts[i-1] + step_duration[i-1] + time_to_next[i-1]
      }
    }
  }
  
  n_steps <- length(step_names)
  
  # Add buffer time to each step duration for conflict analysis.
  padded_step_duration <- step_duration + task_switching_time
  
  # Generate a data frame of all step events (start and end times) for all samples.
  # 1. Pre-calculate the "Single Sample" profile once
  # (You already have 'step_starts' and 'step_ends' for one sample)
  single_starts <- step_starts
  single_ends   <- step_starts + step_duration
  
  # 2. Generate the offsets for all samples at once
  offsets <- (0:(n_samples - 1)) * interval
  
  # 3. Use 'outer' to add offsets to base times (Matrix Math)
  # This creates a matrix where rows are steps and columns are samples
  all_starts_matrix <- outer(single_starts, offsets, "+")
  all_ends_matrix   <- outer(single_ends, offsets, "+")
  
  # 4. Flatten matrices into vectors
  flat_starts <- as.vector(all_starts_matrix)
  flat_ends   <- as.vector(all_ends_matrix)
  
  # 5. Create the columns for names
  # 'each' repeats AAABBB, 'times' repeats ABCABC
  flat_samples <- rep(custom_sample_names, each = n_steps)
  flat_steps   <- rep(step_names, times = n_samples)
  
  # 6. Build the dataframe 
  plot_events <- data.frame(
    Sample = flat_samples,
    Step   = flat_steps,
    start  = flat_starts,
    end    = flat_ends,
    stringsAsFactors = FALSE 
  )
  
  # Set up factor levels for correct plotting order.
  y_levels <- rev(c(custom_sample_names, "Hands-On Time"))
  plot_events$Sample <- factor(plot_events$Sample, levels = y_levels)
  
  # Calculate wait times (unoccupied periods) between steps for each sample.
  wait_events <- plot_events %>%
    group_by(Sample) %>%
    arrange(start) %>%
    mutate(full_end = lead(start, default = last(end))) %>%
    ungroup() %>%
    select(Sample, Step, start, end = full_end)
  
  # Create a data frame of all hands-on events including buffer time.
  all_padded_events <- plot_events %>% mutate(end = start + padded_step_duration)
  
  # Analyze time intervals to detect when more than one task is scheduled simultaneously.
  if(nrow(all_padded_events) == 0) {
    busy_df <- data.frame(Sample=factor(), Step=factor(), start=numeric(), end=numeric())
  } else {
    time_points <- sort(unique(c(all_padded_events$start, all_padded_events$end)))
    if (length(time_points) < 2) {
      occupancy_df <- data.frame(start=numeric(), end=numeric(), occupancy=numeric())
    } else {
      occupancy_df <- data.frame(start = time_points[-length(time_points)], end = time_points[-1])
      occupancy <- sapply(1:nrow(occupancy_df), function(j) {
        mid_point <- (occupancy_df$start[j] + occupancy_df$end[j]) / 2
        sum(all_padded_events$start <= mid_point & all_padded_events$end > mid_point)
      })
      occupancy_df$occupancy <- occupancy
    }
    
    occupancy_df <- occupancy_df %>% filter(occupancy > 0) %>%
      mutate(Status = ifelse(occupancy > 1, "Overbooked", "Busy"))
    
    if(nrow(occupancy_df) > 0) {
      buffer_events <- plot_events %>%
        mutate(start = end, end = end + task_switching_time) %>%
        filter(task_switching_time > 0) %>%
        select(start, end)
      
      all_hands_on <- rbind(
        data.frame(start = plot_events$start, end = plot_events$end, type = "Busy"),
        data.frame(start = buffer_events$start, end = buffer_events$end, type = "Buffer")
      ) %>% arrange(start)
      
      time_points_ho <- sort(unique(c(all_hands_on$start, all_hands_on$end)))
      if (length(time_points_ho) > 1) {
        occupancy_df_ho <- data.frame(start = time_points_ho[-length(time_points_ho)], end = time_points_ho[-1])
        occupancy_ho <- sapply(1:nrow(occupancy_df_ho), function(j) {
          mid_point <- (occupancy_df_ho$start[j] + occupancy_df_ho$end[j]) / 2
          sum(all_hands_on$start <= mid_point & all_hands_on$end > mid_point)
        })
        occupancy_df_ho$occupancy <- occupancy_ho
        
        overbooked_df <- occupancy_df_ho %>% filter(occupancy > 1) %>%
          mutate(Step = "Overbooked", Sample = factor("Hands-On Time", levels = y_levels)) %>%
          select(Sample, Step, start, end)
      } else {
        overbooked_df <- data.frame()
      }
      
      busy_df <- rbind(
        plot_events %>% select(start, end) %>% mutate(Step = "Busy", Sample = factor("Hands-On Time", levels = y_levels)),
        buffer_events %>% mutate(Step = "Buffer", Sample = factor("Hands-On Time", levels = y_levels)),
        overbooked_df
      )
      
    } else {
      busy_df <- data.frame()
    }
  }
  
  # Combine per-sample events and hands-on events into a single data frame for plotting.
  final_plot_df <- rbind(select(plot_events, Sample, Step, start, end), busy_df)
  
  # Set up colors and factor levels for the plot.
  has_overbooked <- "Overbooked" %in% final_plot_df$Step
  plot_levels <- c(step_names, "Busy", "Buffer")
  colors_cb <- c(custom_colors, "black", buffer_color)
  if(has_overbooked) { plot_levels <- c(plot_levels, "Overbooked"); colors_cb <- c(colors_cb, overbooked_color) }
  names(colors_cb) <- plot_levels
  final_plot_df$Step <- factor(final_plot_df$Step, levels = plot_levels)
  
  # Create a wide-format data frame for the "Schedule by Sample" table.
  all_steps_df <- data.frame(matrix(ncol = n_steps, nrow = n_samples))
  colnames(all_steps_df) <- step_names
  rownames(all_steps_df) <- custom_sample_names
  for (sample_j in 0:(n_samples - 1)) {
    all_steps_df[sample_j + 1,] <- step_starts + (sample_j * interval)
  }
  
  # Create a long-format data frame for the "Schedule by Time" (chronological) table.
  all_steps_ordered <- plot_events %>%
    mutate(Duration = end - start) %>%
    select(Sample, Step, Time = start, Duration) %>%
    arrange(Time, Sample, Step)
  
  
  # Return a list containing all processed data frames and plot settings.
  list(
    plot_data = final_plot_df, wait_data = wait_events,
    schedule_by_sample = all_steps_df, schedule_by_time = all_steps_ordered,
    optimal_interval = interval, colors = colors_cb, y_levels = y_levels,
    n_samples = n_samples, n_steps = n_steps, is_valid = !has_overbooked
  )
}


# --- 4. Plotting Function ---
# ------------------------------------------------------------------------------
# Generates the main Gantt chart visualization using ggplot2.
generate_gantt_plot <- function(results_list, show_legend, alternate_shading, shading_color, x_axis_interval, chart_name, show_hhmmss, show_wait_times, is_manual_mode) {
  if(is.null(results_list)) return(NULL)
  
  # Extract data and settings from the results list.
  final_plot_df <- results_list$plot_data
  wait_events_df <- results_list$wait_data
  max_time <- max(final_plot_df$end, na.rm = TRUE)
  x_breaks <- seq(0, ceiling(max_time / x_axis_interval) * x_axis_interval, by = x_axis_interval)
  
  # Create the subtitle text based on mode and schedule validity.
  subtitle_text <- paste("Staggering Interval:", format_time_interval(results_list$optimal_interval))
  if (is_manual_mode) {
    status_text <- if(results_list$is_valid) " (Valid Schedule)" else " (CONFLICTS DETECTED)"
    subtitle_text <- paste0(subtitle_text, status_text)
  }
  
  # Initialize the ggplot object.
  p <- ggplot(final_plot_df, aes(fill = Step))
  
  # Add alternating background shading for readability if enabled.
  if (alternate_shading && length(x_breaks) > 1) {
    shade_indices <- seq(1, length(x_breaks) - 1, by = 2)
    shade_df <- data.frame(xmin = x_breaks[shade_indices], xmax = x_breaks[shade_indices + 1])
    p <- p + geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = shading_color, alpha = 0.5, inherit.aes = FALSE)
  }
  
  # Add vertical grid lines.
  p <- p + geom_vline(xintercept = x_breaks, linetype = "dashed", color = "grey70", linewidth = 0.5)
  
  # Add outlines for wait times if enabled.
  if(show_wait_times && nrow(wait_events_df) > 0) {
    p <- p + geom_rect(data = wait_events_df, aes(xmin = start, xmax = end, ymin = as.numeric(Sample) - 0.4, ymax = as.numeric(Sample) + 0.4, color = Step),
                       fill = NA, linetype = "solid", linewidth = 0.6, show.legend = FALSE)
  }
  
  # Add the main rectangles for each step event.
  p <- p +
    geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(Sample) - 0.4, ymax = as.numeric(Sample) + 0.4)) +
    scale_y_continuous(breaks = 1:length(results_list$y_levels), labels = results_list$y_levels, expand = c(0,0)) +
    scale_fill_manual(values = results_list$colors, name = "Step", drop = FALSE) +
    scale_color_manual(values = results_list$colors, name = "Step", drop = FALSE) +
    labs(title = chart_name, subtitle = subtitle_text, x = if(show_hhmmss) "Time (hh:mm:ss)" else "Time (minutes)", y = "") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  # Format the x-axis labels.
  if (show_hhmmss) p <- p + scale_x_continuous(breaks = x_breaks, labels = format_hhmmss, expand = c(0,0))
  else             p <- p + scale_x_continuous(breaks = x_breaks, expand = c(0,0))
  
  # Control legend visibility.
  if (show_legend) p <- p + theme(legend.position = "bottom") else p <- p + theme(legend.position = "none")
  p
}

# --- 5. Shiny UI ---
# ------------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  # Custom CSS for styling UI elements.
  tags$head(tags$style(HTML("
    .livecard p { line-height: 1.35; margin-bottom: 6px; }
    .livecard h4 { margin-top: 0; margin-bottom: 8px; }
    .badge-chip, .badge-sample {
      display: inline-block; padding: 2px 8px; border-radius: 6px;
      line-height: 1.25; white-space: normal; word-break: break-word; vertical-align: middle;
    }
    .badge-chip { box-shadow: inset 0 -1px 0 rgba(0,0,0,0.08); }
    .table-past { color: #888888; background-color: #f2f2f2 !important; }
    .table-active { font-weight: bold; background-color: #fff3cd !important; }
    .table-upcoming { }
  "))),
  titlePanel("StaggR: Protocol Scheduling Optimizer"),
  
  fluidRow(
    # The main layout with a sidebar for inputs and a main panel for outputs.
    sidebarLayout(
      sidebarPanel(
        # Make the sidebar scrollable for long protocols.
        style = "height: calc(100vh - 80px); overflow-y: auto;",
        h4("Experiment Parameters"),
        
        textInput("chartName", "Protocol Name:", value = "My Experiment"),
        
        fluidRow(
          column(9, numericInput("nSamples", "Number of Samples", 4, min = 1, max = 100)),
          column(3, actionLink("help_nSamples", icon("question-circle"), style = "margin-top: 30px;"))
        ),
        textAreaInput("sampleNames", "Custom Sample Names (comma- or newline-separated):", 
                value = paste0("Sample ", 1:4, collapse = "\n"), rows = 3),
        
        hr(),
        fluidRow(
          column(9, numericInput("nSteps", "Number of Steps", 2, min = 2, max = 200)),
          column(3, actionLink("help_nSteps", icon("question-circle"), style = "margin-top: 30px;"))
        ),
        
        # UI for defining each step is dynamically generated on the server.
        uiOutput("dynamicStepInputs"),
        
        hr(),
        
        # UI for the Time Course Helper.
        checkboxInput("useTimeCourseHelper", "Use Time Course Helper", value = FALSE),
        conditionalPanel(
          condition = "input.useTimeCourseHelper == true",
          div(style="background-color: #f7f7f7; border-radius: 5px; padding: 10px;",
              uiOutput("timeCourseUI")
          )
        ),
        
        hr(),
        h4("Scheduling & Plot Settings"),
        
        radioButtons("optMode", "Optimization Mode",
                     choices = c("Automatic (find non-conflicting interval)"="auto",
                                 "Manual interval"="manual"),
                     inline = FALSE, selected = "auto"),
        
        # Show manual interval controls only when "Manual" mode is selected.
        conditionalPanel(
          condition = "input.optMode == 'manual'",
          fluidRow(
            column(7, numericInput("manualIntervalValue", "Manual Interval", value = 5, min = 0, step = 0.25)),
            column(5, selectInput("manualIntervalUnit", "Unit", unit_choices, selected = "min"))
          )
        ),
        
        fluidRow(
          column(7, numericInput("taskSwitchValue", "Buffer Time", value = 15, min = 0, step = 1)),
          column(5, selectInput("taskSwitchUnit", "Unit", unit_choices, selected = "sec"))
        ),
        
        fluidRow(
          column(7, numericInput("xAxisValue", "X-axis Tick Interval", value = 10, min = 1)),
          column(5, selectInput("xAxisUnit", "Unit", unit_choices, selected = "min"))
        ),
        
        fluidRow(
          column(9, selectInput("granularity", "Optimizer Granularity",
                                choices = c("60 min" = 60, "30 min" = 30, "15 min" = 15, "10 min" = 10, "5 min" = 5, 
                                            "60 sec" = 1, "30 sec" = 0.5, "15 sec" = 0.25, "5 sec" = 5/60, "1 sec" = 1/60),
                                selected = 0.25)),
          column(3, actionLink("help_granularity", icon("question-circle"), style = "margin-top: 30px;"))
        ),
        
        # Collapsible section for advanced plot settings.
        actionLink("toggleAdvanced", "Advanced Options..."),
        shinyjs::hidden(
          div(id = "advancedOptions", hr(),
              checkboxInput("alternateShading", "Alternate background shading", value = TRUE),
              checkboxInput("show_hhmmss", "Show time as hh:mm:ss", value = TRUE),
              checkboxInput("showWaitTimes", "Show Wait Times on Plot", value = TRUE),
              checkboxInput("colorCodeTables", "Color-code tables", value = TRUE),
              
              colourInput("overbookedColor", "Overbooked Color", value = "#D7261E"),
              colourInput("bufferColor", "Buffer Time Color", value = "#BDBDBD"),
              colourInput("shadingColor", "Background Shading Color", value = "#F0F0F0"),
              colourInput("sampleColorBase", "Sample Color Base", value = "#A6CEE3")
          )
        ),
        
        hr(),
        
        # UI for saving and loading sessions.
        fluidRow(
          column(9, h5("Save/Load", style = "margin-top: 10px;")),
          column(3, actionLink("help_saveload", icon("question-circle"), style = "margin-top: 12px; font-size: 1.2em;"))
        ),
        fileInput("uploadSession", "Upload File", accept = c(".csv", ".json")),
        downloadButton("downloadSampleSession", "Download Sample Session (.json)", class = "btn-block", style="margin-top: 5px;"),
        fluidRow(
          column(6, downloadButton("saveSession", "Save Session (.json)", class = "btn-block", style="margin-top: 5px;")),
          column(6, downloadButton("saveProtocol", "Save Protocol (.csv)", class = "btn-block", style="margin-top: 5px;"))
        ),
        
        hr(),
        actionButton("run", "Generate Schedule", class = "btn-primary btn-lg", icon = icon("gears")),
        width = 4
      ),
      
      mainPanel(
        style = "height: calc(100vh - 80px); overflow-y: auto;",
        tabsetPanel(id = "main_tabs",
                    tabPanel("Gantt Chart", br(),
                             checkboxInput("showLegendMain", "Show Plot Legend", value = TRUE),
                             actionButton("downloadPlotModal", "Print/Download Plot", icon = icon("camera")),
                             actionButton("downloadReportModal", "Download Full Report (.zip)", icon = icon("file-zipper")),
                             hr(),
                             plotOutput("ganttPlot", height = "600px")
                    ),
                    tabPanel("Schedule by Time", br(),
                             div(
                               id = "sticky-time-header",
                               style = "position: sticky; top: -1px; background-color: white; z-index: 10; padding: 8px 0px; border-bottom: 1px solid #ccc;",
                               fluidRow(
                                 column(8, h4(style="margin-top: 5px; margin-bottom: 0;", textOutput("liveElapsedCaption"))),
                                 column(4, align="right", downloadButton("downloadTimeExcel", "Download as Formatted Excel"))
                               )
                             ),
                             hr(style="margin-top: 0;"),
                             div(style="padding: 0px 15px 10px 15px;",
                                 checkboxGroupInput("showCols", "Show Columns:",
                                                    choices = c("Time", "Duration", "Wait Until Next" = "Wait_Until_Next"),
                                                    selected = c("Time", "Duration", "Wait_Until_Next"),
                                                    inline = TRUE)
                             ),
                             DT::dataTableOutput("scheduleByTime")
                    ),
                    tabPanel("Schedule by Sample", br(),
                             downloadButton("downloadSampleExcel", "Download as Formatted Excel"),
                             hr(),
                             DT::dataTableOutput("scheduleBySample")
                    ),
                    tabPanel("Live Time Course", br(),
                             fluidRow(
                               column(3, numericInput("countdownStart", "Pre-start countdown (sec):", 15, min = 0)),
                               column(3, uiOutput("startResumeButton")),
                               column(3, actionButton("pauseTimer", "Pause", icon = icon("pause"), class = "btn-warning", style="margin-top: 25px; width: 100%;")),
                               column(3, actionButton("stopTimer", "Stop/Reset", icon = icon("stop-circle"), class = "btn-danger", style="margin-top: 25px; width: 100%;"))
                             ),
                             hr(),
                             checkboxInput("liveTableUpdates", "Enable live table updates", value = TRUE),
                             hr(),
                             uiOutput("liveTimerDisplay")
                    ),
                    tabPanel("About", 
                             h3("About StaggR"),
                             p("Welcome to StaggR, an interactive tool designed to solve one of the most common logistical headaches in experimental biology: scheduling complex, multi-sample protocols without running out of hands."),
                             p("Many experiments, from time courses to library preps, require performing the same sequence of timed steps on many samples. Starting all samples simultaneously is often impossible. The alternative is to stagger their start times, but calculating a feasible, conflict-free schedule by hand is tedious and error-prone. StaggR automates this process, helping you design and execute your experiments with confidence and efficiency."),
                             hr(),
                             
                             h4("Key Features"),
                             tags$ul(
                               tags$li(tags$strong("Automatic Optimization:"), "Calculates the shortest possible staggering interval that guarantees you are never required to perform two tasks at once. It accounts for the duration of each step plus a configurable 'buffer time' for switching between tasks."),
                               tags$li(tags$strong("Manual Simulation:"), "Want to use a specific interval (e.g., start a new sample every 5 minutes)? StaggR will simulate the schedule and clearly flag any time points where you would be overbooked."),
                               tags$li(tags$strong("Time Course Helper:"), "Rapidly generate complex experimental arms. Define an anchor step (e.g., 'Add Drug') and a template step (e.g., 'Harvest'), and the helper will automatically replace the template with a series of new, correctly timed steps for each of your collection points."),
                               tags$li(tags$strong("Live Execution Mode:"), "Once your schedule is set, switch to the 'Live Time Course' tab for a real-time guide. It provides a master countdown timer, telling you exactly which step to perform on which sample and when."),
                               tags$li(tags$strong("Comprehensive Outputs:"), "Visualize your entire experiment with an interactive Gantt chart. View and export detailed timetables, either chronologically (by time) or organized by sample."),
                               tags$li(tags$strong("Full Reproducibility:"), "Save your entire experimental design—protocol steps, sample names, colors, and all settings—to a single `.json` file. Load it later to reproduce your setup exactly or share it with collaborators.")
                             ),
                             hr(),
                             
                             h4("Quickstart Guide"),
                             tags$ol(
                               tags$li("Define your protocol in the sidebar by setting the number of steps and filling in the details for each: name, duration, and the wait time until the next step."),
                               tags$li("Set your experiment parameters, including the number of samples, custom sample names, and scheduling options."),
                               tags$li("Click the ", tags$strong("Generate Schedule"), " button to calculate and view the optimized timeline in the Gantt Chart and schedule tabs."),
                               tags$li("Once satisfied, use the 'Live Time Course' tab to execute your experiment, or use the download buttons to export the plots and timetables for your lab notebook.")
                             ),
                             hr(),
                             
                             h4("Example Protocols"),
                             p("Click a button below to load a complete session file (.json) demonstrating a common experimental design. These examples, relevant to transcription biology and other fields, showcase different ways to structure a protocol in StaggR."),
                             # Note: These buttons require the corresponding .json files to be in a 'data' subdirectory.
                             fluidRow(
                               column(4, actionButton("load_rnalabel", "RNA Labeling Time Course", icon = icon("dna"), width = '100%')),
                               column(4, actionButton("load_degron", "Degron/Drug Time Course", icon = icon("pills"), width = '100%')),
                               column(4, actionButton("load_fixation", "Fixation Protocol", icon = icon("vial"), width = '100%'))
                             ),
                             
                             hr(),
                             h4("Citation & Feedback"),
                             p("StaggR was developed as an open source application to streamline benchwork. If you use this tool in your research, please cite it accordingly. For questions, bug reports, or feedback, please open an issue on the GitHub repository or contact the developer directly."),
                             p(tags$em("Contact: amfrancette@gmail.com | Source Code: https://github.com/amfrancette/StaggR"))
                    )
        ),
        width = 8
      )
    )
  )
)


# --- 6. Shiny Server ---
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # This reactiveVal holds the master data frame defining the protocol steps.
  # It's the central, authoritative source of the protocol state.
  steps_state <- reactiveVal(NULL)
  
  # Null-coalescing operator for convenience
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
  
  # Creates a default protocol data frame when the app starts.
  init_steps_state <- function(n) {
    # A simple 2-step protocol serves as a clean starting point for users.
    default_names <- c("Start Treatment", "Harvest")
    default_dur   <- c(0.5, 2)
    default_ttn   <- c(60) # Only one TTN value (for the time between step 1 and 2)
    default_colors <- c("#A6CEE3", "#1F78B4")
    
    nn <- n
    nm <- if (nn <= length(default_names)) default_names[1:nn] else c(default_names, paste0("Step ", (length(default_names)+1):nn))
    du <- if (nn <= length(default_dur)) default_dur[1:nn] else c(default_dur, rep(1, nn-length(default_dur)))
    co <- if (nn <= length(default_colors)) default_colors[1:nn] else vapply(seq_len(nn), function(i) fixed_color_palette[((i-1) %% length(fixed_color_palette)) + 1], character(1))
    
    tt <- if (nn > 1) {
      base <- if (nn-1 <= length(default_ttn)) default_ttn[1:(nn-1)] else c(default_ttn, rep(10, nn-1-length(default_ttn)))
      base
    } else numeric(0)
    
    data.frame(
      step_name = nm,
      step_duration_value = du,
      step_duration_unit  = rep("min", nn),
      time_to_next_value  = c(tt, NA),
      time_to_next_unit   = c(rep("min", max(0, nn-1)), NA),
      step_color = co,
      stringsAsFactors = FALSE
    )
  }
  
  # Initializes the steps_state when the app first loads.
  observeEvent(input$nSteps, {
    st <- steps_state()
    if (is.null(st)) steps_state(init_steps_state(as.integer(input$nSteps)))
  }, once = TRUE, ignoreInit = FALSE)
  
  # A flag to prevent the nSteps observer from misfiring during programmatic updates.
  suppress_nsteps_sync <- reactiveVal(FALSE)

  # Flags to prevent sample name/count observers from triggering each other in a loop.
  suppress_sample_count_sync <- reactiveVal(FALSE)
  suppress_sample_names_sync <- reactiveVal(FALSE)
  
  # Synchronizes the protocol state when the "Number of Steps" input changes.
  observeEvent(input$nSteps, {
    if (isTRUE(suppress_nsteps_sync())) {
      suppress_nsteps_sync(FALSE)
      return()
    }
    req(input$nSteps)
    st <- steps_state(); if (is.null(st)) return()
    current_n <- nrow(st)
    target_n <- as.integer(input$nSteps)
    if (target_n == current_n) return()
    
    # Read current values from the UI to avoid losing user changes.
    current_ui_vals <- isolate({
      temp_df <- st
      if (!is.null(input[[paste0("stepName", current_n)]])) {
        for(i in 1:current_n) {
          temp_df$step_name[i] <- input[[paste0("stepName", i)]]
          temp_df$step_duration_value[i] <- input[[paste0("stepDuration", i)]]
          temp_df$step_duration_unit[i] <- input[[paste0("durUnit", i)]]
          temp_df$step_color[i] <- input[[paste0("stepColor", i)]]
          if (i < current_n) {
            temp_df$time_to_next_value[i] <- input[[paste0("timeToNext", i)]]
            temp_df$time_to_next_unit[i] <- input[[paste0("ttnUnit",i)]]
          }
        }
      }
      temp_df
    })
    
    # Add or remove rows from the protocol data frame to match the target number.
    if (target_n > current_n) {
      add_n <- target_n - current_n
      for (k in seq_len(add_n)) {
        col <- fixed_color_palette[((current_n+k-1) %% length(fixed_color_palette)) + 1]
        new_row <- data.frame(
          step_name=paste0("Step ", current_n+k),
          step_duration_value=1, step_duration_unit="min",
          time_to_next_value=10, time_to_next_unit="min",
          step_color=col, stringsAsFactors = FALSE
        )
        current_ui_vals <- rbind(current_ui_vals, new_row)
      }
    } else {
      current_ui_vals <- current_ui_vals[seq_len(target_n), , drop=FALSE]
    }
    
    # Ensure the last step's "time to next" is always NA.
    if (nrow(current_ui_vals) > 0) {
      last_row <- nrow(current_ui_vals)
      if (last_row > 1 && is.na(current_ui_vals$time_to_next_value[last_row - 1])) {
        current_ui_vals$time_to_next_value[last_row - 1] <- 10
        current_ui_vals$time_to_next_unit[last_row - 1] <- "min"
      }
      current_ui_vals$time_to_next_value[last_row] <- NA
      current_ui_vals$time_to_next_unit[last_row]  <- NA
    }
    steps_state(current_ui_vals)
  }, ignoreInit = TRUE)
  
  # Renders the dynamic UI for defining each protocol step.
  output$dynamicStepInputs <- renderUI({
    st <- steps_state(); req(st)
    n_steps <- nrow(st)
    
    hdr <- tagList(
      fluidRow(column(12, h5("Step Definitions"))),
      fluidRow(
        column(3, tags$strong("Step Name")),
        column(3, tags$strong("Duration")),
        column(3, tags$strong("Unit")),
        column(3, tags$strong("Color"))
      )
    )
    
    rows <- lapply(seq_len(n_steps), function(i){
      fluidRow(
        column(3, textInput(paste0("stepName", i), NULL, value = st$step_name[i])),
        column(3, numericInput(paste0("stepDuration", i), NULL,
                               value = st$step_duration_value[i], min = 0, step = 0.01)),
        column(3, selectInput(paste0("durUnit", i), NULL, choices = unit_choices,
                              selected = st$step_duration_unit[i])),
        column(3, colourInput(paste0("stepColor", i), NULL, st$step_color[i]))
      )
    })
    
    ttn <- lapply(seq_len(max(0, n_steps - 1)), function(i){
      safe_val  <- st$time_to_next_value[i] %||% 0
      safe_unit <- st$time_to_next_unit[i] %||% "min"
      
      fluidRow(
        column(9, numericInput(paste0("timeToNext", i),
                               label = paste0(st$step_name[i], " -> ", st$step_name[i+1]),
                               value = safe_val, min = 0, step = 0.01)),
        column(3, selectInput(paste0("ttnUnit", i), label = HTML("&nbsp;"),
                              choices = unit_choices, selected = safe_unit))
      )
    })
    
    tagList(
      hdr, do.call(tagList, rows), hr(),
      fluidRow(column(12, h5("Time Between Steps"))),
      do.call(tagList, ttn)
    )
  })
  
  # Synchronizes sample names with the "Number of Samples" input.
  observeEvent(input$nSamples, {
    if (isTRUE(suppress_sample_count_sync())) {
      suppress_sample_count_sync(FALSE)
      return()
    }

    req(input$nSamples > 0)
    current_names <- isolate(parse_sample_names(input$sampleNames))

    target_n <- as.integer(input$nSamples)
    current_n <- length(current_names)
    if (target_n == current_n) return()
    if (target_n > current_n) {
      new_names <- c(current_names, paste0("Sample ", (current_n + 1):target_n))
    } else {
      new_names <- current_names[1:target_n]
    }

    suppress_sample_names_sync(TRUE)
    updateTextAreaInput(session, "sampleNames", value = format_sample_names(new_names, prefer_newlines = TRUE))
  }, ignoreInit = TRUE)
  
  # Synchronizes "Number of Samples" with the custom names entered.
  observeEvent(input$sampleNames, {
    if (isTRUE(suppress_sample_names_sync())) {
      suppress_sample_names_sync(FALSE)
      return()
    }

    custom_names <- parse_sample_names(input$sampleNames)
    new_count <- length(custom_names)
    if (new_count > 0 && new_count != isolate(input$nSamples)) {
      suppress_sample_count_sync(TRUE)
      updateNumericInput(session, "nSamples", value = new_count)
    }
  }, ignoreInit = TRUE)

  # --- Help Modals (Question Mark Icons) ---
  observeEvent(input$help_nSamples, {
    showModal(modalDialog(
      title = "Number of Samples",
      tags$p("Set how many samples you want to run through the same protocol."),
      tags$ul(
        tags$li("This value is kept in sync with the Custom Sample Names box."),
        tags$li("If you edit the names list (comma- or newline-separated), the sample count will update automatically."),
        tags$li("Tip: Keep sample names unique to avoid confusion in tables and plots.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$help_nSteps, {
    showModal(modalDialog(
      title = "Number of Steps",
      tags$p("Set how many hands-on steps are in your protocol."),
      tags$ul(
        tags$li("Each step has a name, hands-on duration, unit, and a display color."),
        tags$li("The 'Time Between Steps' section defines the wait/incubation time between consecutive steps."),
        tags$li("Changing the number of steps will add/remove step rows; existing values are preserved when possible.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$help_granularity, {
    showModal(modalDialog(
      title = "Optimizer Granularity",
      tags$p("Controls how finely the automatic optimizer searches for a non-conflicting staggering interval."),
      tags$ul(
        tags$li(tags$strong("Smaller granularity"), " (e.g., 1–15 sec) = more precise interval, but slower."),
        tags$li(tags$strong("Larger granularity"), " (e.g., 1–5 min) = faster, but may slightly overestimate the best interval."),
        tags$li("If optimization is slow, increase granularity; if schedules feel overly conservative, decrease it.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Keeps the labels for "time between steps" updated with the current step names.
  observe({
    st <- steps_state(); req(st); n_steps <- nrow(st)
    if(n_steps < 2) return()
    
    step_names <- sapply(1:n_steps, function(i) input[[paste0("stepName", i)]])
    req(!any(sapply(step_names, is.null)))
    
    for (i in 1:(n_steps - 1)) {
      updateNumericInput(session, paste0("timeToNext", i), label = paste0(step_names[i], " -> ", step_names[i+1]))
    }
  })
  
  # --- Time Course Helper ---
  
  # Renders the UI for the Time Course Helper, reacting to changes in step names.
  output$timeCourseUI <- renderUI({
    n_steps <- input$nSteps; req(n_steps)
    step_names <- sapply(1:n_steps, function(i) input[[paste0("stepName", i)]])
    req(!any(sapply(step_names, is.null)))
    
    choices <- setNames(1:n_steps, step_names)
    
    selected_anchor <- isolate(input$tcAnchorStep) %||% 1
    selected_template <- isolate(input$tcTimeCourseStep) %||% (if (n_steps >= 2) 2 else 1)
    
    tagList(
      selectInput("tcAnchorStep", "Anchor Step (Time Zero)", choices = choices, selected = selected_anchor),
      selectInput("tcTimeCourseStep", "Template Step for Time Points", choices = choices, selected = selected_template),
      textInput("tcTimePoints", "Time Points (comma-separated)", value = "5, 15, 30, 60"),
      selectInput("tcTimePointsUnit", "Unit", unit_choices, selected = "min"),
      actionButton("applyTimeCourse", "Apply Time Course", icon = icon("stream"), class="btn-info", style="width:100%")
    )
  })
  
  # Automatically suggests a template step when the anchor step changes.
  observeEvent(input$tcAnchorStep, {
    req(input$tcAnchorStep, input$nSteps)
    anchor_val <- as.integer(input$tcAnchorStep); n_steps <- as.integer(input$nSteps)
    new_template_idx <- min(n_steps, anchor_val + 1)
    if (new_template_idx == anchor_val && new_template_idx < n_steps) {
      new_template_idx <- new_template_idx + 1
    }
    updateSelectInput(session, "tcTimeCourseStep", selected = new_template_idx)
  }, ignoreInit = TRUE)
  
  # Main logic for the Time Course Helper feature.
  observeEvent(input$applyTimeCourse, {
    req(input$useTimeCourseHelper, input$tcAnchorStep, input$tcTimeCourseStep, input$tcTimePoints)
    
    st_from_ui <- isolate({
      n <- req(input$nSteps); base_df <- req(steps_state())
      if (!is.null(input[[paste0("stepName", n)]])) { 
        for (i in 1:n) {
          base_df$step_name[i] <- input[[paste0("stepName", i)]] %||% paste("Step", i)
          base_df$step_duration_value[i] <- as.numeric(input[[paste0("stepDuration", i)]] %||% 1)
          base_df$step_duration_unit[i] <- input[[paste0("durUnit", i)]] %||% "min"
          base_df$step_color[i] <- input[[paste0("stepColor", i)]] %||% "#FFFFFF"
          if (i < n) {
            base_df$time_to_next_value[i] <- as.numeric(input[[paste0("timeToNext", i)]] %||% 10)
            base_df$time_to_next_unit[i] <- input[[paste0("ttnUnit", i)]] %||% "min"
          }
        }
      }
      base_df
    })
    
    anchor_idx <- as.integer(input$tcAnchorStep)
    template_idx <- as.integer(input$tcTimeCourseStep)
    
    if (anchor_idx >= template_idx) {
      showNotification("The Anchor Step must come before the Time Course Step being replaced.", type = "error", duration = 8)
      return() # Stop execution
    }
    
    time_points_str <- isolate(input$tcTimePoints)
    time_points_num <- try(suppressWarnings(as.numeric(trimws(strsplit(time_points_str, ",")[[1]]))), silent = TRUE)
    
    if (inherits(time_points_num, "try-error") || any(is.na(time_points_num)) || !all(time_points_num >= 0)) {
      showNotification("Invalid time points. Must be non-negative, comma-separated numbers.", type = "error", duration = 8)
      return() # Stop execution
    }
    
    time_points_to_add <- sort(unique(time_points_num[time_points_num > 0]))
    if (length(time_points_to_add) == 0) {
      showNotification("No positive time points were provided to add.", type = "warning"); return()
    }
    
    n_new_steps <- length(time_points_to_add)
    unit <- isolate(input$tcTimePointsUnit)
    template_step <- st_from_ui[template_idx, ]
    new_step_colors <- generate_color_gradient(template_step$step_color, n_new_steps)
    
    time_points_min <- to_minutes(time_points_to_add, unit)
    wait_times_min <- diff(c(0, time_points_min))
    wait_times_in_chosen_unit <- from_minutes(wait_times_min, unit)
    
    ttn_values <- numeric(n_new_steps); ttn_units <- character(n_new_steps)
    if (n_new_steps > 1) {
      ttn_values[1:(n_new_steps - 1)] <- wait_times_in_chosen_unit[2:n_new_steps]
      ttn_units[1:(n_new_steps - 1)] <- unit
    }
    
    post_block <- if (template_idx < nrow(st_from_ui)) st_from_ui[(template_idx + 1):nrow(st_from_ui), ] else NULL
    if (!is.null(post_block)) {
      ttn_values[n_new_steps] <- template_step$time_to_next_value %||% 10 # Use original TTN from template
      ttn_units[n_new_steps] <- template_step$time_to_next_unit %||% "min"
    } else {
      ttn_values[n_new_steps] <- NA_real_
      ttn_units[n_new_steps] <- NA_character_
    }
    
    new_steps_block <- data.frame(
      step_name             = paste0(template_step$step_name, " (", time_points_to_add, " ", unit, ")"),
      step_duration_value   = template_step$step_duration_value,
      step_duration_unit    = template_step$step_duration_unit,
      time_to_next_value    = ttn_values,
      time_to_next_unit     = ttn_units,
      step_color            = new_step_colors,
      stringsAsFactors      = FALSE
    )
    
    protocol_part1 <- st_from_ui[1:anchor_idx, ]
    protocol_part1$time_to_next_value[anchor_idx] <- wait_times_in_chosen_unit[1]
    protocol_part1$time_to_next_unit[anchor_idx] <- unit
    
    new_protocol_df <- do.call(rbind, list(protocol_part1, new_steps_block, post_block))
    
    steps_state(new_protocol_df)
    suppress_nsteps_sync(TRUE) 
    updateNumericInput(session, "nSteps", value = nrow(new_protocol_df))
    
    showNotification(paste("Replaced step '", st_from_ui$step_name[template_idx], "' with ", n_new_steps, " time course steps.", sep=""), type = "message")
  })
  
  # --- Main Calculation and Rendering ---
  
  # This eventReactive block is the main calculation trigger for the app.
  results <- eventReactive(input$run, {
    timer_active(FALSE); timer_paused(FALSE); timer_mode("idle")
    showNotification("Calculating schedule...", type = "message", duration = NULL, id = "calc_msg")
    on.exit({ removeNotification("calc_msg"); removeNotification("optim_msg", session) })
    
    st <- steps_state(); req(st); n_steps <- nrow(st)
    step_names_in <- sapply(1:n_steps, function(i) input[[paste0("stepName", i)]])
    step_dur_val_in <- sapply(1:n_steps, function(i) input[[paste0("stepDuration", i)]])
    step_dur_unit_in <- sapply(1:n_steps, function(i) input[[paste0("durUnit", i)]])
    ttn_val_in <- if(n_steps > 1) sapply(1:(n_steps-1), function(i) input[[paste0("timeToNext", i)]]) else numeric(0)
    ttn_unit_in <- if(n_steps > 1) sapply(1:(n_steps-1), function(i) input[[paste0("ttnUnit", i)]]) else character(0)
    colors_in <- sapply(1:n_steps, function(i) input[[paste0("stepColor", i)]])
    
    step_durationM <- to_minutes(step_dur_val_in, step_dur_unit_in)
    ttnM <- to_minutes(ttn_val_in, ttn_unit_in)
    
    custom_sample_names <- parse_sample_names(input$sampleNames)

    if (length(custom_sample_names) != as.integer(input$nSamples)) {
      showNotification(
        paste0(
          "Number of sample names (", length(custom_sample_names), ") does not match nSamples (", as.integer(input$nSamples), "). ",
          "Please provide exactly one sample name per sample (recommended: one per line)."
        ),
        type = "error",
        duration = 10
      )
      return(NULL)
    }

    if (anyDuplicated(custom_sample_names)) {
      showNotification("Duplicate sample names detected; auto-disambiguating (e.g., adding .1, .2).", type = "warning", duration = 8)
      custom_sample_names <- make.unique(custom_sample_names)
    }
    buffer_min <- to_minutes(input$taskSwitchValue, input$taskSwitchUnit)
    
    # Determine the staggering interval, either automatically or from manual input.
    final_interval <- if (identical(input$optMode, "auto")) {
      showNotification("Finding optimal interval...", type = "message", duration = NULL, id = "optim_msg")
      
      # Call the helper function
      opt_val <- findOptimalInterval(
        step_durations = step_durationM,
        time_to_next = ttnM,
        n_samples = input$nSamples,
        granularity = as.numeric(input$granularity),
        buffer_time = buffer_min
      )
      
      if (is.null(opt_val)) {
        showNotification("Optimizer could not find a valid schedule. Consider reducing sample number, optimizer granularity, buffer time, or operation durrations", type = "error", duration = 8)
        return(NULL)
      } else if(is.character(opt_val)) {
        showNotification(opt_val, type = "error", duration = 8)
      }
      opt_val
    } else { 
      to_minutes(input$manualIntervalValue, input$manualIntervalUnit) 
    }
    
    req(final_interval)
    
    # Call the core calculation function with all parameters.
    res <- tryCatch({
      calculateStaggering(
        step_names = step_names_in, time_to_next = ttnM, step_duration = step_durationM,
        n_samples = input$nSamples, custom_sample_names = custom_sample_names, custom_colors = colors_in,
        interval = final_interval, task_switching_time = buffer_min,
        overbooked_color = input$overbookedColor, buffer_color = input$bufferColor
      )
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error", duration = 10); return(NULL) })
    
    if (!is.null(res)) showNotification("Schedule generated!", type = "message", duration = 3)
    res
  })
  
  # A central reactive that combines static schedule results with live color updates.
  render_data <- reactive({
    res <- results()
    req(res)
    
    n_steps <- res$n_steps
    step_names_in_order <- sapply(1:n_steps, function(i) input[[paste0("stepName", i)]])
    
    step_colors_live <- sapply(1:n_steps, function(i) input[[paste0("stepColor", i)]])
    names(step_colors_live) <- step_names_in_order
    
    new_color_vector <- c(
      step_colors_live, "Busy" = "black", "Buffer" = input$bufferColor
    )
    if ("Overbooked" %in% names(res$colors)) {
      new_color_vector["Overbooked"] <- input$overbookedColor
    }
    
    res$colors <- new_color_vector
    res$sample_colors <- sample_colors_reactive()
    
    return(res)
  })
  
  # Reactive that generates the Gantt chart plot object.
  reactive_plot <- reactive({
    res <- render_data(); req(res)
    x_int_min <- to_minutes(input$xAxisValue, input$xAxisUnit)
    generate_gantt_plot(res, input$showLegendMain %||% TRUE, input$alternateShading %||% TRUE, input$shadingColor %||% "#F0F0F0",
                        x_axis_interval = x_int_min, chart_name = input$chartName %||% "My Experiment",
                        show_hhmmss = input$show_hhmmss %||% TRUE, show_wait_times = input$showWaitTimes %||% TRUE,
                        is_manual_mode = identical(input$optMode, "manual"))
  })
  output$ganttPlot <- renderPlot({ reactive_plot() })
  
  # Reactive that prepares the data frame for the "Schedule by Time" table.
  schedule_time_df <- reactive({
    res <- results(); if (is.null(res)) return(NULL)
    df <- res$schedule_by_time
    df$TimeNumeric <- df$Time; df$DurationNumeric <- df$Duration
    df$EndNumeric <- df$TimeNumeric + df$DurationNumeric
    df <- df %>%
      mutate(WaitUntilNextMin = lead(TimeNumeric) - EndNumeric) %>%
      mutate(WaitUntilNextMin = ifelse(WaitUntilNextMin < 0, 0, WaitUntilNextMin))
    
    df$Wait_Until_Next <- sapply(df$WaitUntilNextMin, format_hhmmss)
    
    if(isTRUE(input$show_hhmmss)) {
      df$Time <- format_hhmmss(df$Time); df$Duration <- format_hhmmss(df$Duration)
    } else {
      df$Time <- sprintf("%02d:%02d", floor(df$Time), floor((df$Time %% 1) * 60))
      df$Duration <- sprintf("%02d:%02d", floor(df$Duration), floor((df$Duration %% 1) * 60))
    }
    df$Status <- "upcoming"
    df %>% select(Status, Sample, Step, Time, Duration, Wait_Until_Next, TimeNumeric, DurationNumeric)
  })
  
 
  schedule_sample_df <- reactive({
    res <- results(); if (is.null(res)) return(NULL)
    df <- res$schedule_by_sample
    if(input$show_hhmmss) df %>% mutate(across(everything(), ~format_hhmmss(.x)))
    else                  df %>% mutate(across(everything(), ~sprintf("%02d:%02d", floor(.x), floor((.x %% 1) * 60))))
  })
  
  base_hue_reactive <- reactive({
    hex <- toupper(trimws(input$sampleColorBase))
    if (!startsWith(hex, "#")) hex <- paste0("#", hex)
    if (!grepl("^#([A-F0-9]{6}|[A-F0-9]{8})$", hex)) return(210)
    if (nchar(hex) == 9) hex <- substr(hex, 1, 7)
    suppressWarnings({
      hue <- try({ as.numeric(colorspace::coords(methods::as(colorspace::hex2RGB(hex), "polarLUV"))[1, "H"])
      }, silent = TRUE) })
    if (inherits(hue, "try-error") || !is.finite(hue)) 210 else hue
  })
  
  # Reactives that generate and store sample names and their corresponding colors.
  sample_names_reactive  <- reactive({ res <- results(); req(res); res$y_levels[res$y_levels != "Hands-On Time"] })
  sample_colors_reactive <- reactive({
    sn <- sample_names_reactive(); bh <- base_hue_reactive()
    cols <- generate_sample_colors(length(sn), bh); names(cols) <- sn; cols
  })
  
  # Renders the "Schedule by Time" data table.
  output$scheduleByTime <- DT::renderDataTable({
    df <- schedule_time_df(); if (is.null(df)) return(datatable(data.frame(Status=character())))
    
    show_status <- isTRUE(timer_active()) & isTRUE(input$liveTableUpdates)
    base_cols <- if(show_status) c("Status", "Sample", "Step") else c("Sample", "Step")
    visible_cols <- c(base_cols, input$showCols)
    df_display <- df %>% select(any_of(visible_cols))
    
    dt_opts <- list(pageLength = 20, dom = 'lfrtip')
    if (show_status) {
      dt_opts$rowCallback <- JS(
        "function(row, data, index) {",
        "  $(row).removeClass('table-past table-active table-upcoming');",
        "  if (data[0] == 'past') { $(row).addClass('table-past'); }",
        "  else if (data[0] == 'active') { $(row).addClass('table-active'); }",
        "  else { $(row).addClass('table-upcoming'); }",
        "}"
      )
    }
    
    potential_renames <- c('Wait Until Next' = 'Wait_Until_Next')
    active_renames <- potential_renames[potential_renames %in% colnames(df_display)]
    
    dt <- DT::datatable(df_display, rownames = FALSE, options = dt_opts, colnames = active_renames)
    
    if (isTRUE(input$colorCodeTables)) {
      res <- render_data(); req(res)
      step_names <- names(res$colors); step_colors <- unname(res$colors)
      font_colors_step <- ifelse(sapply(step_colors, is_color_dark), 'white', 'black')
      if ("Step" %in% visible_cols) {
        dt <- dt %>% formatStyle('Step', backgroundColor = styleEqual(step_names, step_colors), color = styleEqual(step_names, font_colors_step))
      }
      
      sample_names  <- names(res$sample_colors); sample_colors <- unname(res$sample_colors)
      font_colors_sample <- ifelse(sapply(sample_colors, is_color_dark), 'white', 'black')
      if ("Sample" %in% visible_cols) {
        dt <- dt %>% formatStyle('Sample', backgroundColor = styleEqual(sample_names, sample_colors), color = styleEqual(sample_names, font_colors_sample))
      }
    }
    dt
  })
  
  # Renders the "Schedule by Sample" data table.
  output$scheduleBySample <- DT::renderDataTable({
    df <- schedule_sample_df(); if (is.null(df)) return(NULL)
    res <- render_data(); if(is.null(res)) return(NULL)
    
    dt <- DT::datatable(df, rownames = TRUE, options = list(pageLength = 20, dom = 'lfrtip'))
    if (isTRUE(input$colorCodeTables)) {
      step_names <- names(res$colors); step_colors <- unname(res$colors)
      font_colors <- ifelse(sapply(step_colors, is_color_dark), 'white', 'black')
      names(step_colors) <- step_names; names(font_colors) <- step_names
      for (col_name in colnames(df)) {
        if (col_name %in% step_names) {
          dt <- dt %>% formatStyle(col_name, backgroundColor = step_colors[col_name], color = font_colors[col_name])
        }
      }
    }
    dt
  })
  
  # --- Live Time Course Functionality ---
  
  # Reactive values to manage the state of the live timer.
  experiment_start_at     <- reactiveVal(NULL)
  current_segment_index   <- reactiveVal(0L)
  timer_active            <- reactiveVal(FALSE)
  timer_pre_start         <- reactiveVal(FALSE)
  time_remaining          <- reactiveVal(0)
  timer_mode              <- reactiveVal("idle") 
  event_end_at            <- reactiveVal(Sys.time())
  timer_paused            <- reactiveVal(FALSE)
  time_remaining_at_pause <- reactiveVal(0)
  
  # Helper functions to deconstruct the schedule for the live timer display.
  .make_timeline_segments <- function(sched) {
    if (is.null(sched) || nrow(sched) == 0) return(data.frame(kind=character(), idx=integer(), start_sec=integer(), end_sec=integer()))
    starts <- round(sched$TimeNumeric * 60); durs <- pmax(0L, round(sched$DurationNumeric * 60))
    segs <- list(); cursor <- 0L
    for (i in seq_len(nrow(sched))) {
      if (starts[i] > cursor) segs[[length(segs) + 1]] <- data.frame(kind="wait", idx=i, start_sec=cursor, end_sec=starts[i])
      if (durs[i] > 0L) segs[[length(segs) + 1]] <- data.frame(kind="step", idx=i, start_sec=starts[i], end_sec=starts[i] + durs[i])
      cursor <- max(cursor, starts[i] + durs[i])
    }
    do.call(rbind, segs)
  }
  .prev_step_index <- function(sched, ref_idx) { cand <- which(sched$DurationNumeric > 0 & seq_len(nrow(sched)) < ref_idx); if (length(cand) == 0) NA_integer_ else max(cand) }
  .next_step_index <- function(sched, ref_idx) { cand <- which(sched$DurationNumeric > 0 & seq_len(nrow(sched)) > ref_idx); if (length(cand) == 0) NA_integer_ else min(cand) }
  
  segments <- reactive({ req(results()); .make_timeline_segments(schedule_time_df()) })
  
  # UI and state management for timer controls.
  output$startResumeButton <- renderUI({
    label <- if (isTRUE(timer_paused())) "Resume" else "Start Time Course"
    icon_name <- if (isTRUE(timer_paused())) "play-circle" else "play-circle"
    actionButton("startTimer", label, icon = icon(icon_name), class = "btn-success", style="margin-top: 25px; width: 100%;")
  })
  observe({
    active <- timer_active(); paused <- timer_paused()
    shinyjs::toggleState("startTimer", condition = !active || paused)
    shinyjs::toggleState("pauseTimer", condition = active && !paused)
    shinyjs::toggleState("stopTimer", condition = active)
  })
  # Event handlers for timer buttons.
  observeEvent(input$startTimer, {
    # This block handles the "Start" and "Resume" actions for the live timer.
    if (isTRUE(timer_paused())) {
      # Logic for resuming a paused timer.
      timer_paused(FALSE)
      event_end_at(Sys.time() + time_remaining_at_pause())
      showNotification("Time course resumed.", type = "message")
    } else {
      # Logic for starting a new timer run.
      sched <- schedule_time_df()
      
      # Use a standard 'if' condition for validation within an observer.
      if (is.null(sched) || nrow(sched) == 0) {
        showNotification("Please generate a schedule first.", type = "error")
        return() # Stop execution
      }
      
      showNotification("Starting time course...", type = "message")
      timer_pre_start(TRUE); timer_mode("pre"); timer_active(TRUE)
      current_segment_index(0L); experiment_start_at(NULL)
      event_end_at(Sys.time() + as.numeric(input$countdownStart))
      time_remaining(as.numeric(input$countdownStart))
    }
  })
  observeEvent(input$pauseTimer, {
    req(timer_active(), !isTRUE(timer_paused()))
    rem <- as.numeric(difftime(event_end_at(), Sys.time(), units = "secs"))
    time_remaining_at_pause(max(0, rem))
    timer_paused(TRUE)
    showNotification("Time course paused.", type = "warning")
  })
  observeEvent(input$stopTimer, {
    timer_active(FALSE); timer_paused(FALSE); timer_pre_start(FALSE); timer_mode("idle")
    current_segment_index(0L); experiment_start_at(NULL)
    df <- schedule_time_df()
    if(!is.null(df)){
      df$Status <- "upcoming"
      replaceData(dataTableProxy("scheduleByTime"), df %>% select(any_of(c("Status", "Sample", "Step", isolate(input$showCols)))), resetPaging = FALSE, rownames = FALSE)
    }
    showNotification("Time course stopped and reset.", type = "error")
  })
  
  # The main timer loop, which invalidates every 200ms to update the display.
  observe({
    req(timer_active(), !isTRUE(timer_paused())); invalidateLater(200, session)
    now <- Sys.time(); rem <- as.numeric(difftime(event_end_at(), now, units = "secs"))
    time_remaining(max(0, ceiling(rem)))
    
    if (rem > 0) return() # Do nothing until the current segment ends.
    
    segs <- segments(); req(segs)
    if (timer_mode() == "pre") {
      timer_pre_start(FALSE); timer_mode("run"); experiment_start_at(now)
      seg_idx <- 1L
      while (seg_idx <= nrow(segs) && (segs$end_sec[seg_idx] - segs$start_sec[seg_idx]) <= 0) seg_idx <- seg_idx + 1L
      if (seg_idx > nrow(segs)) { timer_active(FALSE); timer_mode("done"); showNotification("Time course complete!", type="message"); return() }
      current_segment_index(seg_idx)
      event_end_at(experiment_start_at() + segs$end_sec[seg_idx])
    } else if (timer_mode() == "run") {
      next_idx <- current_segment_index() + 1L
      while (next_idx <= nrow(segs) && (segs$end_sec[next_idx] - segs$start_sec[next_idx]) <= 0) next_idx <- next_idx + 1L
      if (next_idx > nrow(segs)) { timer_active(FALSE); timer_mode("done"); showNotification("Time course complete!", type="message") }
      else { current_segment_index(next_idx); event_end_at(experiment_start_at() + segs$end_sec[next_idx]) }
    }
  })
  
  # Observer that updates the chronological table rows' status during a live run.
  observe({
    # This observer depends on the timer and the checkbox.
    time_remaining()
    
    # Only execute the update logic if the timer is active AND the box is checked.
    if (isTRUE(timer_active()) && isTRUE(input$liveTableUpdates)) {
      req(!is.null(experiment_start_at()), !isTRUE(timer_paused()))
      
      df <- schedule_time_df()
      req(df)
      
      now_sec <- as.numeric(difftime(Sys.time(), experiment_start_at(), units = "secs"))
      starts_sec <- df$TimeNumeric * 60
      ends_sec <- (df$TimeNumeric + df$DurationNumeric) * 60
      
      df$Status <- ifelse(now_sec >= ends_sec, "past", 
                          ifelse(now_sec >= starts_sec & now_sec < ends_sec, "active", "upcoming"))
      
      # Use a proxy to update the data without redrawing the whole table.
      replaceData(dataTableProxy("scheduleByTime"), 
                  df %>% select(any_of(c("Status", "Sample", "Step", isolate(input$showCols)))), 
                  resetPaging = FALSE, 
                  rownames = FALSE)
    }
  })
  
  # Renders the text for the sticky header above the chronological schedule.
  output$liveElapsedCaption <- renderText({
    time_remaining(); if(!timer_active()) return("Time Course Idle")
    now_sec <- if (is.null(experiment_start_at())) 0 else as.numeric(difftime(Sys.time(), experiment_start_at(), units = "secs"))
    if(isTRUE(timer_paused())){ res <- results(); if(!is.null(res)){ now_sec <- (max(res$plot_data$end) * 60) - time_remaining_at_pause() }}
    elapsed_str <- format_hhmmss(max(0, now_sec) / 60)
    detail_str <- if (isTRUE(timer_paused())) { "PAUSED" } else {
      rem_str <- format_hhmmss(time_remaining()/60)
      header_title <- if (timer_mode() == "pre") "Countdown to Start" else if (timer_mode() == "run") {
        segs <- segments(); sidx <- current_segment_index(); if (is.null(segs) || sidx < 1 || sidx > nrow(segs)) "Calculating..." else { if (segs[sidx, ]$kind == "step") "Time Left in Current Step" else "Time Until Next Step" }
      } else "Complete"
      paste0(header_title, " - ", rem_str)
    }
    paste("Live Elapsed -", elapsed_str, "|", detail_str)
  })
  
  # Renders the detailed display for the "Live Time Course" tab.
  output$liveTimerDisplay <- renderUI({
    # Display an idle message if the timer isn't active.
    if (!timer_active()) {
      return(div(style="text-align: center; padding: 20px;",
                 h4("Ready to start time course."),
                 p("Generate a schedule first, then click 'Start Time Course'.")
      ))
    }
    
    # These helpers create the styled boxes for the UI.
    make_step_box <- function(row, title, highlight = FALSE) {
      if (is.null(row) || nrow(row) == 0) return(NULL)
      res <- render_data()
      bg_step <- res$colors[row$Step] %||% "#DDDDDD"; fg_step <- ifelse(is_color_dark(bg_step), "white", "black")
      sample_name_char <- as.character(row$Sample)
      bg_smpl <- res$sample_colors[sample_name_char] %||% "#DDDDDD";
      fg_smpl <- ifelse(is_color_dark(bg_smpl), "white", "black")
      div(class = "livecard", style = paste0("border:1px solid #ccc; border-radius:6px; padding:12px; margin-bottom:12px; opacity:", if(highlight) 1 else 0.85, ";"),
          h4(title), fluidRow(
            column(6, p(strong("Sample: "), span(class="badge-sample", style=paste0("background-color:", bg_smpl, "; color:", fg_smpl, ";"), row$Sample))),
            column(6, p(strong("Step: "), span(class="badge-chip", style=paste0("background-color:", bg_step, "; color:", fg_step, ";"), row$Step)))
          ), p(strong("Start: "), row$Time, " | ", strong("Duration: "), row$Duration)
      )
    }
    make_info_box <- function(title, body) { div(class="livecard", style="border: 1px dashed #bbb; border-radius: 6px; padding: 12px; margin-bottom: 12px; background-color:#f7f7f7;", h4(title), body) }
    
    # Calculate elapsed time strings.
    now <- Sys.time(); t0 <- experiment_start_at(); elapsed_sec <- if (is.null(t0)) 0 else max(0, as.numeric(difftime(now, t0, units = "secs")))
    if(isTRUE(timer_paused())){ res <- results(); if(!is.null(res)){ elapsed_sec <- (max(res$plot_data$end) * 60) - time_remaining_at_pause() } }
    elapsed_str <- format_hhmmss(elapsed_sec / 60)
    countdown_display <- if (isTRUE(timer_paused())) h1(style="font-size: 3.5em; font-weight: bold; color: #E0A800;", "PAUSED") else h1(style="font-size: 3.5em; font-weight: bold;", format_hhmmss(time_remaining()/60))
    
    sched <- schedule_time_df(); req(sched)
    
    # --- UI Logic for Pre-Start Countdown View ---
    if (timer_pre_start()) {
      first_idx <- if (nrow(sched) >= 1) 1L else NA_integer_
      next_idx  <- if (!is.na(first_idx)) .next_step_index(sched, first_idx) else NA_integer_
      first_row <- if (is.na(first_idx)) NULL else sched[first_idx, , drop=FALSE]
      next_row  <- if (is.na(next_idx))  NULL else sched[next_idx, , drop=FALSE]
      
      return(tagList(
        fluidRow(
          column(8, offset = 2,
                 div(style="text-align: center; background-color: #f2f2f2; border-radius: 10px; padding: 20px;",
                     h3("Countdown to Start"),
                     countdown_display,
                     br(), h5(span(style="opacity:0.8;", "Elapsed since T0: "), strong(elapsed_str))
                 )
          )
        ),
        br(),
        fluidRow(
          column(6, make_step_box(first_row, "First Step (up next)", TRUE)),
          column(6, make_step_box(next_row,  "Next Step"))
        )
      ))
    }
    
    # --- UI Logic for Main Running View ---
    segs <- segments(); req(nrow(segs) > 0)
    sidx <- current_segment_index(); seg <- segs[sidx, ]
    
    if (seg$kind == "step") {
      cur_step_row  <- sched[seg$idx, , drop=FALSE]
      prev_idx      <- .prev_step_index(sched, seg$idx)
      next_idx      <- .next_step_index(sched, seg$idx)
      prev_step_row <- if (is.na(prev_idx)) NULL else sched[prev_idx, , drop=FALSE]
      next_step_row <- if (is.na(next_idx)) NULL else sched[next_idx, , drop=FALSE]
      current_panel <- make_step_box(cur_step_row, "Current Step", TRUE)
      header_title  <- "Time Left in Current Step"
    } else { # "wait"
      prev_idx      <- .prev_step_index(sched, seg$idx)
      next_idx      <- seg$idx
      prev_step_row <- if (is.na(prev_idx)) NULL else sched[prev_idx, , drop=FALSE]
      next_step_row <- sched[next_idx, , drop=FALSE]
      current_panel <- make_info_box("Waiting / Incubation", p("Next: ", next_step_row$Sample, " — ", next_step_row$Step))
      header_title  <- "Time Until Next Step"
    }
    
    # This tagList defines the final layout for the main running view.
    tagList(
      fluidRow(
        column(8, offset = 2,
               div(style="text-align: center; background-color: #f2f2f2; border-radius: 10px; padding: 20px;",
                   h3(header_title),
                   countdown_display,
                   br(), h5(span(style="opacity:0.8;", "Elapsed since T0: "), strong(elapsed_str))
               )
        )
      ),
      br(),
      fluidRow(
        column(4, make_step_box(prev_step_row, "Previous Step")),
        column(4, current_panel),
        column(4, make_step_box(next_step_row, "Next Step"))
      )
    )
  })
  
  # --- Save/Load and Export Functionality ---
  
  # Handler to Save the FULL Session to a .json file
  output$saveSession <- downloadHandler(
    filename = function() {
      sanitized_name <- gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol")
      paste0(sanitized_name, "_StaggR_Session_", Sys.Date(), ".json")
    },
    content = function(file) {
      st <- steps_state(); req(st); n_steps <- nrow(st)
      if (!is.null(input[[paste0("stepName", n_steps)]])) {
        for (i in 1:n_steps) {
          st$step_name[i] <- input[[paste0("stepName", i)]]
          st$step_duration_value[i] <- input[[paste0("stepDuration", i)]]
          st$step_duration_unit[i] <- input[[paste0("durUnit", i)]]
          st$step_color[i] <- input[[paste0("stepColor", i)]]
          if (i < n_steps) {
            st$time_to_next_value[i] <- input[[paste0("timeToNext", i)]]
            st$time_to_next_unit[i] <- input[[paste0("ttnUnit", i)]]
          }
        }
      }
      
      accessory_params <- list(
        chartName = input$chartName, sampleNames = input$sampleNames,
        optMode = input$optMode, manualIntervalValue = input$manualIntervalValue,
        manualIntervalUnit = input$manualIntervalUnit, taskSwitchValue = input$taskSwitchValue,
        taskSwitchUnit = input$taskSwitchUnit, granularity = input$granularity,
        xAxisValue = input$xAxisValue, xAxisUnit = input$xAxisUnit,
        overbookedColor = input$overbookedColor, bufferColor = input$bufferColor,
        shadingColor = input$shadingColor, sampleColorBase = input$sampleColorBase
      )
      
      full_session <- list(staggR_version = "2.0", protocol_steps = st, accessory_params = accessory_params)
      write(jsonlite::toJSON(full_session, pretty = TRUE, auto_unbox = TRUE), file)
    }
  )

  # Download a minimal example session file to demonstrate the expected JSON format.
  output$downloadSampleSession <- downloadHandler(
    filename = function() {
      paste0("StaggR_sample_session_", Sys.Date(), ".json")
    },
    content = function(file) {
      example_path <- file.path("data", "RNA_labeling_params.json")
      if (!file.exists(example_path)) {
        stop("Example session file not found: ", example_path)
      }
      file.copy(example_path, file, overwrite = TRUE)
    },
    contentType = "application/json"
  )
  
  # Handler to Save ONLY the Protocol Steps to a .csv file
  output$saveProtocol <- downloadHandler(
    filename = function() {
      sanitized_name <- gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol")
      paste0(sanitized_name, "_Protocol_Parameters_", Sys.Date(), ".csv")
    },
    content = function(file) {
      st <- steps_state(); req(st)
      # Future-proofing: could also gather from UI here like in saveSession.
      write.csv(st, file, row.names = FALSE, na = "")
    }
  )
  
  # Observer for the main file upload input.
  observeEvent(input$uploadSession, {
    req(input$uploadSession); file_info <- input$uploadSession
    tryCatch({
      if (tools::file_ext(file_info$name) == "json") {
        params <- jsonlite::fromJSON(file_info$datapath, simplifyVector = TRUE)
        load_session_params(params)
        showNotification("Session file (.json) loaded successfully.", type = "message")
      } else { # For backward compatibility with old CSV files
        params <- read.csv(file_info$datapath, stringsAsFactors = FALSE, na.strings = c("NA", ""))
        suppress_nsteps_sync(TRUE); updateNumericInput(session, "nSteps", value = nrow(params)); steps_state(params)
        showNotification("Legacy parameters (.csv) loaded.", type = "warning")
      }
    }, error = function(e) { showNotification(paste("Error reading file:", e$message), type = "error") })
  })
  
  # Helper function to load parameters from a list (read from JSON).
  load_session_params <- function(params) {
    # Perform validation using standard if conditions, which are safer inside observers.
    if (!("protocol_steps" %in% names(params))) {
      showNotification("Invalid JSON: File is missing the required 'protocol_steps' object.", type = "error", duration = 8)
      return(NULL) # Stop execution of this function
    }
    if (!("accessory_params" %in% names(params))) {
      showNotification("Invalid JSON: File is missing the required 'accessory_params' object.", type = "error", duration = 8)
      return(NULL) # Stop execution of this function
    }
    
    protocol_df <- as.data.frame(params$protocol_steps)
    accessory <- params$accessory_params

    # Normalize and sync sample names/count from loaded session.
    loaded_names <- parse_sample_names(accessory$sampleNames %||% "")
    if (length(loaded_names) == 0) {
      loaded_names <- paste0("Sample ", seq_len(max(1L, as.integer(isolate(input$nSamples) %||% 1L))))
    }
    
    # Update all UI inputs from the loaded data.
    updateTextInput(session, "chartName", value = accessory$chartName)

    suppress_sample_names_sync(TRUE)
    updateTextAreaInput(session, "sampleNames", value = format_sample_names(loaded_names, prefer_newlines = TRUE))

    suppress_sample_count_sync(TRUE)
    updateNumericInput(session, "nSamples", value = length(loaded_names))

    updateRadioButtons(session, "optMode", selected = accessory$optMode)
    updateNumericInput(session, "manualIntervalValue", value = accessory$manualIntervalValue)
    updateSelectInput(session, "manualIntervalUnit", selected = accessory$manualIntervalUnit)
    updateNumericInput(session, "taskSwitchValue", value = accessory$taskSwitchValue)
    updateSelectInput(session, "taskSwitchUnit", selected = accessory$taskSwitchUnit)
    updateSelectInput(session, "granularity", selected = accessory$granularity)
    updateNumericInput(session, "xAxisValue", value = accessory$xAxisValue)
    updateSelectInput(session, "xAxisUnit", selected = accessory$xAxisUnit)
    updateColourInput(session, "overbookedColor", value = accessory$overbookedColor)
    updateColourInput(session, "bufferColor", value = accessory$bufferColor)
    updateColourInput(session, "shadingColor", value = accessory$shadingColor)
    updateColourInput(session, "sampleColorBase", value = accessory$sampleColorBase)
    
    suppress_nsteps_sync(TRUE)
    updateNumericInput(session, "nSteps", value = nrow(protocol_df))
    steps_state(protocol_df)
  }
  
  # A helper function to load an example session from a file path.
  load_example <- function(file_path) {
    tryCatch({
      if (!file.exists(file_path)) {
        showNotification(paste("Error: Example file not found at", file_path), type = "error", duration = 8); return()
      }
      params <- jsonlite::fromJSON(file_path, simplifyVector = TRUE)
      load_session_params(params)
      updateTabsetPanel(session, "main_tabs", selected = "Gantt Chart")
      showNotification(paste("Loaded example |", params$accessory_params$chartName, "| Generate a fresh schedule to update"), type = "message", duration = 5)
    }, error = function(e) { showNotification(paste("Error parsing example file:", e$message), type = "error", duration = 8) })
  }
  
  # Observers for the "Load Example" buttons.
  observeEvent(input$load_rnalabel, { load_example("data/RNA_labeling_params.json") })
  observeEvent(input$load_degron,   { load_example("data/degron_time_course_params.json") })
  observeEvent(input$load_fixation, { load_example("data/formaldehyde_fixation_params.json") })
  
  # Handlers for downloading plots and reports.
  output$confirmDownloadPlot <- downloadHandler(
    filename = function() {
      sanitized_name <- gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol")
      paste0(sanitized_name, "_Gantt_Chart_", Sys.Date(), ".", input$plotFormat)
    },
    content = function(file) {
      res <- results(); req(res)
      x_int_min <- to_minutes(input$xAxisValue, input$xAxisUnit)
      plot_to_download <- generate_gantt_plot(res, input$plotShowLegend, input$alternateShading, input$shadingColor,
                                              x_axis_interval = x_int_min, chart_name = input$chartName, 
                                              show_hhmmss = input$show_hhmmss, show_wait_times = input$showWaitTimes,
                                              is_manual_mode = identical(input$optMode, "manual"))
      ggsave(file, plot = plot_to_download, device = input$plotFormat,
             width = input$plotWidth, height = input$plotHeight, units = "in", dpi = 300)
    }
  )
  output$downloadTimeExcel <- downloadHandler(
    filename = function() { paste0(gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol"), "_schedule_by_time.xlsx") },
    content = function(file) {
      res <- render_data(); req(res); df <- schedule_time_df(); req(df)
      show_status <- FALSE # Don't include Status column for live updates if downloading the excel
      df_export <- if (!show_status) df %>% select(Sample, Step, Time, Duration, `Wait Until Next` = Wait_Until_Next) else df %>% select(Status, Sample, Step, Time, Duration, `Wait Until Next` = Wait_Until_Next)
      wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Schedule by Time"); openxlsx::writeDataTable(wb, "Schedule by Time", df_export, tableName = "ScheduleByTime")
      hdr_style <- openxlsx::createStyle(textDecoration = "bold"); openxlsx::addStyle(wb, "Schedule by Time", hdr_style, rows = 1, cols = 1:ncol(df_export), gridExpand = TRUE)
      if(isTRUE(input$colorCodeTables)){
        step_names <- names(res$colors); step_colors <- unname(res$colors)
        for(i in seq_along(step_names)) { style <- openxlsx::createStyle(fgFill = step_colors[i]); rows <- which(df_export$Step == step_names[i]) + 1; if(length(rows) > 0) openxlsx::addStyle(wb, "Schedule by Time", style, rows=rows, cols=which(names(df_export)=="Step"), gridExpand=TRUE, stack=TRUE) }
        samp_names <- names(res$sample_colors); samp_colors <- unname(res$sample_colors)
        for(i in seq_along(samp_names)) { style <- openxlsx::createStyle(fgFill = samp_colors[i]); rows <- which(df_export$Sample == samp_names[i]) + 1; if(length(rows) > 0) openxlsx::addStyle(wb, "Schedule by Time", style, rows=rows, cols=which(names(df_export)=="Sample"), gridExpand=TRUE, stack=TRUE) }
      }
      openxlsx::setColWidths(wb, "Schedule by Time", cols = 1:ncol(df_export), widths = "auto"); openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$downloadSampleExcel <- downloadHandler(
    filename = function() { paste0(gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol"), "_schedule_by_sample.xlsx") },
    content = function(file) {
      res <- render_data(); req(res); df <- schedule_sample_df(); req(df)
      wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Schedule by Sample"); openxlsx::writeData(wb, "Schedule by Sample", df, rowNames = TRUE, tableName = "ScheduleBySample")
      hdr_style <- openxlsx::createStyle(textDecoration = "bold"); openxlsx::addStyle(wb, "Schedule by Sample", hdr_style, rows = 1, cols = 1:(ncol(df)+1), gridExpand = TRUE)
      if(isTRUE(input$colorCodeTables)){
        step_names <- names(res$colors); step_colors <- unname(res$colors)
        for (i in seq_along(step_names)) { hexcol <- step_colors[i]; if(step_names[i] %in% colnames(df)) { style  <- openxlsx::createStyle(fgFill = hexcol); openxlsx::addStyle(wb, "Schedule by Sample", style, rows = 2:(nrow(df)+1), cols = which(colnames(df) == step_names[i]) + 1, gridExpand = TRUE, stack = TRUE) } }
      }
      openxlsx::setColWidths(wb, "Schedule by Sample", cols = 1:(ncol(df)+1), widths = "auto"); openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$confirmDownloadReport <- downloadHandler(
    filename = function() { paste0(gsub("[^A-Za-z0-9_]", "_", input$chartName %||% "Protocol"), "_StaggR_Report_", Sys.Date(), ".zip") },
    content = function(file) {
      # Create a temporary directory to store files before zipping
      temp_dir <- file.path(tempdir(), "StaggR_Report")
      if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Set working directory to temp and ensure it's reset on exit
      owd <- getwd(); setwd(temp_dir); on.exit(setwd(owd))
      
      files_to_zip <- c() # Initialize list of files to be zipped
      
      if("plot" %in% input$reportContents) {
        req(results())
        plot_path <- "Gantt_Chart.pdf"
        x_int_min <- to_minutes(input$xAxisValue, input$xAxisUnit)
        plot_to_download <- generate_gantt_plot(results(), TRUE, input$alternateShading, input$shadingColor,
                                                x_axis_interval = x_int_min, chart_name = input$chartName,
                                                show_hhmmss = input$show_hhmmss, show_wait_times = input$showWaitTimes,
                                                is_manual_mode = identical(input$optMode, "manual"))
        ggsave(plot_path, plot = plot_to_download, device = "pdf", width = 11, height = 8.5, units = "in")
        files_to_zip <- c(files_to_zip, plot_path)
      }
      
      if("params_json" %in% input$reportContents) {
        req(steps_state())
        json_path <- "StaggR_Session.json"
        
        st <- steps_state(); n_steps <- nrow(st)
        if (!is.null(input[[paste0("stepName", n_steps)]])) {
          for (i in 1:n_steps) {
            st$step_name[i] <- input[[paste0("stepName", i)]]
            st$step_duration_value[i] <- input[[paste0("stepDuration", i)]]
            st$step_duration_unit[i] <- input[[paste0("durUnit", i)]]
            st$step_color[i] <- input[[paste0("stepColor", i)]]
            if (i < n_steps) {
              st$time_to_next_value[i] <- input[[paste0("timeToNext", i)]]
              st$time_to_next_unit[i] <- input[[paste0("ttnUnit", i)]]
            }
          }
        }
        accessory_params <- list(
          chartName = input$chartName, sampleNames = input$sampleNames,
          optMode = input$optMode, manualIntervalValue = input$manualIntervalValue,
          manualIntervalUnit = input$manualIntervalUnit, taskSwitchValue = input$taskSwitchValue,
          taskSwitchUnit = input$taskSwitchUnit, granularity = input$granularity,
          xAxisValue = input$xAxisValue, xAxisUnit = input$xAxisUnit,
          overbookedColor = input$overbookedColor, bufferColor = input$bufferColor,
          shadingColor = input$shadingColor, sampleColorBase = input$sampleColorBase
        )
        full_session <- list(staggR_version = "2.0", protocol_steps = st, accessory_params = accessory_params)
        write(jsonlite::toJSON(full_session, pretty = TRUE, auto_unbox = TRUE), json_path)
        files_to_zip <- c(files_to_zip, json_path)
      }
      
      if("params_csv" %in% input$reportContents) {
        req(steps_state())
        params_path <- "protocol_parameters.csv"
        write.csv(steps_state(), params_path, row.names = FALSE, na = "")
        files_to_zip <- c(files_to_zip, params_path)
      }
      
      if("time_sched" %in% input$reportContents) {
        req(schedule_time_df())
        time_path <- "schedule_by_time.csv"
        write.csv(schedule_time_df() %>% select(Status, Sample, Step, Time, Duration, Wait_Until_Next), time_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, time_path)
      }
      if("sample_sched" %in% input$reportContents) {
        req(schedule_sample_df())
        sample_path <- "schedule_by_sample.csv"
        write.csv(schedule_sample_df(), sample_path, row.names = TRUE)
        files_to_zip <- c(files_to_zip, sample_path)
      }
      
      zip::zip(zipfile = file, files = files_to_zip)
    },
    contentType = "application/zip"
  )
  
  # --- Modals for Help and Downloads ---
  observeEvent(input$toggleAdvanced, { shinyjs::toggle(id = "advancedOptions", anim = TRUE) })
  observeEvent(input$downloadPlotModal, { showModal(modalDialog(title = "Download Plot", selectInput("plotFormat", "File Format:", c("pdf", "png", "jpeg", "tiff")), numericInput("plotWidth", "Width (inches):", 10, min = 1, max = 20), numericInput("plotHeight", "Height (inches):", 8, min = 1, max = 20), checkboxInput("plotShowLegend", "Show Legend", input$showLegendMain), footer = tagList(modalButton("Cancel"), downloadButton("confirmDownloadPlot", "Download")))) })
  observeEvent(input$downloadReportModal, { 
    showModal(modalDialog(title = "Download Full Report", p("Select the components to include in the .zip archive."), 
                          checkboxGroupInput("reportContents", "Include:", 
                                             choices = c(
                                               "Gantt Chart (PDF)" = "plot", 
                                               "Full Session (.json)" = "params_json",
                                               "Protocol Parameters (.csv)" = "params_csv", 
                                               "Chronological Schedule (CSV)" = "time_sched", 
                                               "Per-Sample Schedule (CSV)" = "sample_sched"
                                             ), 
                                             selected = c("plot", "params_json", "time_sched", "sample_sched")
                          ), 
                          footer = tagList(modalButton("Cancel"), downloadButton("confirmDownloadReport", "Download .zip")))
    ) 
  })
  observeEvent(input$help_saveload, { showModal(modalDialog(title = "Save & Load Functionality", tags$p(tags$strong("Save Session (.json):"), "Saves your complete experiment, including the protocol, sample names, buffer times, colors, and all other settings. This is the recommended way to save your work for full reproducibility."), tags$p(tags$strong("Save Protocol (.csv):"), "Saves only the table of protocol steps (name, duration, wait time). This is useful for sharing just the core protocol with a colleague or for use in other programs."), tags$p(tags$strong("Upload File:"), "You can upload either a full .json session file to restore all settings, or a legacy .csv protocol file to overwrite only the current step definitions."), easyClose = TRUE, footer = modalButton("Close"))) })
  
}

# --- 7. Run the App ---
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
