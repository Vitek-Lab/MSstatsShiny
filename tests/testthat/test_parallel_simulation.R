library(data.table)
library(survival)
library(parallel)

# -------------------------------------------------------------------------
# 1. Mock Internal MSstats Functions
# -------------------------------------------------------------------------

.fitSurvival <- function(data, iterations) {
  tryCatch({
    # Create a real survreg object to simulate memory usage
    # Use a subset of data to ensure stability/speed, as we only care about object size
    fit <- survreg(Surv(newABUNDANCE, !cen, type="left") ~ 1, 
                   data = head(data, 500), dist = "gaussian")
    # Artificially bloat the object to simulate a complex model (approx 100MB)
    # Real MSstats models can be very large due to model frames and environments
    fit$bloat <- numeric(12.5 * 1024 * 1024) 
    return(fit)
  }, error = function(e) return(NULL))
}

.isSummarizable <- function(data, remove50missing) return(data)

.runTukey <- function(data, is_labeled, censored_symbol, remove50missing) {
  return(data.table(Protein = "TestProtein", LogIntensities = mean(data$newABUNDANCE, na.rm=TRUE)))
}

# -------------------------------------------------------------------------
# 2. Define Functions with "Work Simulation" (Sleep)
# -------------------------------------------------------------------------

# LEAKY VERSION
MSstatsSummarizeSingleTMP_Leaky_Sim <- function (single_protein, impute, censored_symbol, remove50missing, aft_iterations = 90) {
    # ... Setup ...
    newABUNDANCE = n_obs = n_obs_run = RUN = FEATURE = LABEL = NULL
    predicted = censored = NULL
    cols = intersect(colnames(single_protein), c("newABUNDANCE", "cen", "RUN", "FEATURE", "ref"))
    single_protein = single_protein[(n_obs > 1 & !is.na(n_obs)) & (n_obs_run > 0 & !is.na(n_obs_run))]
    if (nrow(single_protein) == 0) return(list(NULL, NULL))
    single_protein[, `:=`(RUN, factor(RUN))]
    single_protein[, `:=`(FEATURE, factor(FEATURE))]
    
    if (impute & any(single_protein[["censored"]])) {
        converged = TRUE
        survival_fit = withCallingHandlers({
            .fitSurvival(single_protein[LABEL == "L", cols, with = FALSE], aft_iterations)
        }, warning = function(w) { if (grepl("converge", conditionMessage(w), ignore.case = TRUE)) converged <<- FALSE })
        
        if (converged && !is.null(survival_fit)) {
            single_protein[, `:=`(predicted, predict(survival_fit, newdata = .SD))]
        } else {
            single_protein[, `:=`(predicted, NA_real_)]
        }
        
        # --- LEAK SIMULATION ---
        # The object 'survival_fit' is still in memory here.
        # We simulate "doing other work" (predictions, formatting) by sleeping.
        Sys.sleep(1) 
        
        # Report Memory Usage of this Worker
        mem_used <- sum(gc()[,2])
        msg <- sprintf("[Worker %d] LEAKY State - Holding Memory: %.2f MB\n", Sys.getpid(), mem_used)
        cat(msg)
        cat(msg, file = "parallel_log.txt", append = TRUE)
        
        single_protein[, `:=`(predicted, ifelse(censored & (LABEL == "L"), predicted, NA))]
        single_protein[, `:=`(newABUNDANCE, ifelse(censored & LABEL == "L", predicted, newABUNDANCE))]
        survival = single_protein[, c(cols, "predicted"), with = FALSE]
    } else {
        survival = single_protein[, cols, with = FALSE]
        survival[, `:=`(predicted, NA)]
    }
    # ... Finalize ...
    single_protein = .isSummarizable(single_protein, remove50missing)
    if (is.null(single_protein)) return(list(NULL, NULL))
    result = .runTukey(single_protein, TRUE, censored_symbol, remove50missing)
    list(result, survival)
}

# FIXED VERSION
MSstatsSummarizeSingleTMP_Fixed_Sim <- function (single_protein, impute, censored_symbol, remove50missing, aft_iterations = 90) {
    # ... Setup ...
    newABUNDANCE = n_obs = n_obs_run = RUN = FEATURE = LABEL = NULL
    predicted = censored = NULL
    cols = intersect(colnames(single_protein), c("newABUNDANCE", "cen", "RUN", "FEATURE", "ref"))
    single_protein = single_protein[(n_obs > 1 & !is.na(n_obs)) & (n_obs_run > 0 & !is.na(n_obs_run))]
    if (nrow(single_protein) == 0) return(list(NULL, NULL))
    single_protein[, `:=`(RUN, factor(RUN))]
    single_protein[, `:=`(FEATURE, factor(FEATURE))]
    
    if (impute & any(single_protein[["censored"]])) {
        converged = TRUE
        survival_fit = withCallingHandlers({
            .fitSurvival(single_protein[LABEL == "L", cols, with = FALSE], aft_iterations)
        }, warning = function(w) { if (grepl("converge", conditionMessage(w), ignore.case = TRUE)) converged <<- FALSE })
        
        if (converged && !is.null(survival_fit)) {
            single_protein[, `:=`(predicted, predict(survival_fit, newdata = .SD))]
        } else {
            single_protein[, `:=`(predicted, NA_real_)]
        }
        
        # --- FIX APPLIED ---
        rm(survival_fit) 
        
        # --- FIXED SIMULATION ---
        # We simulate "doing other work" by sleeping.
        Sys.sleep(1)
        
        # Report Memory Usage of this Worker
        mem_used <- sum(gc()[,2])
        msg <- sprintf("[Worker %d] FIXED State - Holding Memory: %.2f MB\n", Sys.getpid(), mem_used)
        cat(msg)
        cat(msg, file = "parallel_log.txt", append = TRUE)
        
        single_protein[, `:=`(predicted, ifelse(censored & (LABEL == "L"), predicted, NA))]
        single_protein[, `:=`(newABUNDANCE, ifelse(censored & LABEL == "L", predicted, newABUNDANCE))]
        survival = single_protein[, c(cols, "predicted"), with = FALSE]
    } else {
        survival = single_protein[, cols, with = FALSE]
        survival[, `:=`(predicted, NA)]
    }
    # ... Finalize ...
    single_protein = .isSummarizable(single_protein, remove50missing)
    if (is.null(single_protein)) return(list(NULL, NULL))
    result = .runTukey(single_protein, TRUE, censored_symbol, remove50missing)
    list(result, survival)
}

# -------------------------------------------------------------------------
# 3. Run Simulation
# -------------------------------------------------------------------------

set.seed(123)
n_rows <- 20000 
dt <- data.table(
    newABUNDANCE = rnorm(n_rows, 20, 5),
    censored = sample(c(TRUE, FALSE), n_rows, replace=TRUE, prob=c(0.3, 0.7)),
    LABEL = "L", RUN = sample(1:20, n_rows, replace=TRUE), FEATURE = sample(1:500, n_rows, replace=TRUE),
    n_obs = 5, n_obs_run = 5, cen = FALSE, ref = "ref"
)
dt$cen <- dt$censored
dt$newABUNDANCE[dt$censored] <- dt$newABUNDANCE[dt$censored] - 5

# Clear log file
file.create("parallel_log.txt")

cat("\n--- Simulating LEAKY Parallel Execution ---\n")
# We run 2 cores. Both will hit the 'sleep' at the same time.
# Both will report HIGH memory because they haven't cleaned up yet.
invisible(mclapply(1:2, function(i) MSstatsSummarizeSingleTMP_Leaky_Sim(copy(dt), TRUE, "NA", FALSE), mc.cores = 2))

cat("\n--- Simulating FIXED Parallel Execution ---\n")
# We run 2 cores. Both will hit the 'sleep' at the same time.
# Both will report LOW memory because they cleaned up BEFORE sleeping.
invisible(mclapply(1:2, function(i) MSstatsSummarizeSingleTMP_Fixed_Sim(copy(dt), TRUE, "NA", FALSE), mc.cores = 2))

cat("\n--- Log File Content ---\n")
cat(readLines("parallel_log.txt"), sep = "\n")