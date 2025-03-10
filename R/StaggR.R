#' Optimize Staggering of Sample Preparations
#'
#' This function computes the optimal time intervals for processing samples in a staggered
#' fashion, ensuring that overlapping steps are minimized.
#'
#' @param sampleNames (Optional) Character vector of sample names; defaults to "auto" which creates
#'   names like "Sample 1", "Sample 2", etc.
#' @param stepNames  (Optional) Character vector of step names; defaults to "auto" which creates
#'   names like "Step 1", "Step 2", etc.
#' @param timeToNextStep Numeric vector of the time between each step and the next. Must be 1 less than the length of stepNames.
#' @param stepDuration Numeric vector of durations (in minutes) required for each step.
#' @param nSamples Number of samples to process. 
#' @param granularity  (Optional) The time interval (in minutes) to test for optimal staggering;
#'   defaults to "auto" "auto" to let the function decide.
#' @param forceInterval (Optional) If provided, forces the function to use a specific interval (in minutes).
#' @param verbose (Optional) Logical; if TRUE, prints progress details during computation.
#'
#' @return A list containing the optimal interval, several data frames with timecourse details,
#'   and a ggplot object of the scheduling.
#'
#' @details This function simulates a time course schedule for processing multiple samples
#'   based on user-defined steps and durations. It checks for overlapping operations and
#'   incrementally adjusts the staggering interval until a valid schedule is found.
#'
#' @examples
#' \dontrun{
#' # Simple example with 3 steps and 4 samples:
#' res <- optimizeStaggering(
#'   stepNames = c("Start_treatment", "Load_samples_fuge", "Start_fuge", "Remove_supernatant"),
#'   timeToNextStep = c(59, 1, 15),
#'   stepDuration = c(15/60, 20/60, 30/60, 5),
#'   nSamples = 8,
#'   granularity = "auto",
#'   forceInterval = FALSE,
#'   verbose = TRUE
#' )
#' }
#' @import ggplot2
#' @import tidyverse
#' @import reshape2
#' @import lubridate
#' @import DT
#' @import roxygen2 
#' @import dplyr 
#' 
#' @export
optimizeStaggering <- function(sampleNames = "auto", stepNames = "auto", 
                               timeToNextStep = c(), stepDuration = c(), 
                               nSamples = "auto", granularity = "auto", 
                               forceInterval = F,  verbose = F) {
  
  stepTransitions <- c(0, cumsum(timeToNextStep))
  nSteps <- length(stepTransitions)
  # sample_j <- 0
  #  k <- 1
  try(if (stepNames == "auto") {
    stepNames <- paste0("Step ", 1:nSteps)
  }, silent = T)
  
  try(if (sampleNames == "auto") {
    sampleNames <- paste0("Sample ", 1:nSamples)
  }, silent = T)
  
  if (granularity == "auto") {
    print("Temporal granularity will be automatically determined")
    # establish function to determine whether granularity is appropriate for a given timecourse setup
    detectNonIntegers <- function(x) {
      isEntirelyIntegers <- sum(!unlist(lapply( (x), function(x) (x == round(x)) ) )) == 0
      return(isEntirelyIntegers == T)
    }
    
    # determine the granularity of the time-course
    incrementsOfInterest <- c(stepTransitions, stepDuration)
    residuals <- unique(incrementsOfInterest %% 1) * 60
    if (sum(residuals) == 0) {
      granularity <- 1
    } else if (detectNonIntegers(residuals / 30)) {
      granularity <- 30/60
    }  else if (detectNonIntegers(residuals / 20)) {
      granularity <- 20/60
    }  else if (detectNonIntegers(residuals / 15)) {
      granularity <- 15/60
    }  else if (detectNonIntegers(residuals / 10)) {
      granularity <- 10/60
    }  else if (detectNonIntegers(residuals / 5)) {
      granularity <- 5/60
    }  else if (detectNonIntegers(residuals / 4)) {
      granularity <- 4/60
    }  else if (detectNonIntegers(residuals / 3)) {
      granularity <- 3/60
    }  else if (detectNonIntegers(residuals / 2)) {
      granularity <- 2/60
    } else {
      granularity <- 1/60
    }
  } else {
    print("User supplied granularity: Warning, uneven time intervals may result in clipped edges of Step durations")
  }
  
  print(paste0("Granularity: ", granularity * 60, " seconds"))
  
  i <- granularity
  solution <- FALSE
  
  if (forceInterval != F) {
    i <- forceInterval
  }
  
  while(solution == F) {
    if (verbose) {
      print(paste0("testing interval: ", floor(i) , " minutes ", round((i %% 1) * 60), " seconds"))
    }
    # maximum time point is the last step transition + the last step duration + the longest delay for a sample to begin processing
    maxTimePoint <- (max(stepTransitions) + tail(stepDuration, n = 1) + ((nSamples - 1) * i))
    # build a df to show timecourse details
    df <- as.data.frame(matrix(0, nrow = nSamples, ncol = ceiling(maxTimePoint/granularity) + 1 ))
    timeIncrements <- round(seq(0, maxTimePoint, granularity), digits = 2)
    colnames(df) <- timeIncrements
    
    allSteps_df <- data.frame(matrix(ncol = length(stepNames), nrow = 0))
    colnames(allSteps_df) <- stepNames
    allSteps_df_reformatted <- allSteps_df
    
    for (sample_j in (1:nSamples) - 1) {
      df_row <- df[sample_j + 1,]
      
      stepTransitions_sample_j <- stepTransitions + (sample_j * i)
      stepComplete_sample_j <- stepTransitions_sample_j + stepDuration
      
      for (k in 1:length(stepTransitions_sample_j)) {
        stepName <- stepNames[k]
        df_row[,timeIncrements >= stepTransitions_sample_j[k] & timeIncrements <= stepComplete_sample_j[k]] <- stepName
      }
      df[sample_j + 1,] <- df_row
      allSteps_df[sample_j + 1,] <- stepTransitions_sample_j
      allSteps_df_reformatted[sample_j + 1,] <- as.character(seconds_to_period(round(stepTransitions_sample_j * 60)))
    }
    
    handsOnTime <- sapply(df, function(x) as.numeric(sum(x != 0)))
    # check if any intervals have more than one step at a time
    if (sum(handsOnTime > 1) > 0 && forceInterval == F) {
      i <- i + granularity
    } else {
      handsOnTime[handsOnTime > 1] <- "Overbooked"
      handsOnTime[handsOnTime == 1] <- "Busy"
      handsOnTime[handsOnTime == 0] <- NA
      
      df <- rbind(df, handsOnTime = handsOnTime)
      solution <- TRUE
      df_plot <- t(df)
      colnames(df_plot) <- c(sampleNames, "hands On Time")
      df_plot <- melt(df_plot)
      colnames(df_plot) <- c("Time", "Sample", "Step")
      df_plot$Step[df_plot$Step == 0] <- NA
      
      df_plot$Sample <- factor(df_plot$Sample, levels = rev(c(sampleNames, "hands On Time")))
      df_plot$Step <- factor(df_plot$Step, levels = c(stepNames, "Busy", "Overbooked"))
      
      p <- ggplot(df_plot, aes(Time, Sample, fill = Step, alpha = !is.na(Step))) + 
        theme_minimal()  +
        geom_tile(colour = "gray50", linewidth = 0) +
        scale_alpha_identity(guide = "none") +
        ggtitle(paste0("Sample treatment with ", seconds_to_period(round(i * 60)), " interval | Forced interval = ", is.numeric(forceInterval))
        ) 
      plot(p)
      print(allSteps_df)
      print(allSteps_df_reformatted)
      datatable(allSteps_df_reformatted)
      
      # sort by time to start each step 
      allSteps_df$Sample <- sampleNames
      allSteps_df_ordered <- melt(allSteps_df) %>% arrange(value)
      colnames(allSteps_df_ordered) <- c("Sample", "Step", "Time")
      allSteps_df_ordered_reformatted <- allSteps_df_ordered
      
      allSteps_df_ordered_reformatted$Time <- seconds_to_period(round(allSteps_df_ordered_reformatted$Time * 60))
      print(allSteps_df_ordered_reformatted)
      solution_dfs <- list(plot = p, df = df, allSteps_df_ordered = allSteps_df_ordered, allSteps_df_ordered_reformatted = allSteps_df_ordered_reformatted, allSteps_df = allSteps_df, allSteps_df_reformatted = allSteps_df_reformatted, optimalInterval = i)
      print(paste0("!!! Valid Interval: ", floor(i) , " minutes ", round((i %% 1) * 60), " seconds !!!"))
      return(solution_dfs)
      
    }
  }
}
