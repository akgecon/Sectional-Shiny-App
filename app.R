# ============================================================================
# PRE-IMPLEMENTATION FIXES FOR PORTFOLIO ANALYTICS APP
# Address critical issues before deploying the main fixes
# ============================================================================

# ============================================================================
# 1. ROBUST SPY DATA FETCHING WITH FALLBACKS
# ============================================================================

robust_spy_fetch <- function(start_date, end_date, max_retries = 3) {
  
  cat("Attempting to fetch SPY data from", as.character(start_date), "to", as.character(end_date), "\n")
  
  # Try multiple data sources in order of preference
  data_sources <- list(
    list(symbol = "SPY", source = "yahoo"),
    list(symbol = "^GSPC", source = "yahoo"),  # S&P 500 index as backup
    list(symbol = "SPY", source = "tiingo"),   # Alternative source if available
    list(symbol = "IVV", source = "yahoo")     # iShares Core S&P 500 as last resort
  )
  
  for (source_config in data_sources) {
    for (attempt in 1:max_retries) {
      tryCatch({
        cat("Trying", source_config$symbol, "from", source_config$source, "(attempt", attempt, ")\n")
        
        # Add delay between retries
        if (attempt > 1) {
          Sys.sleep(2)
        }
        
        # Fetch data with error handling
        spy_raw <- tq_get(
          source_config$symbol, 
          get = "stock.prices", 
          from = start_date, 
          to = end_date,
          complete_cases = FALSE
        )
        
        if (nrow(spy_raw) > 10) {  # Minimum viable dataset
          # Calculate returns and clean data
          spy_returns <- spy_raw %>%
            tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                         period = "daily", col_rename = "Rb") %>%
            filter(row_number() > 1, !is.na(Rb)) %>%  # Remove first row and NAs
            mutate(date = as.Date(date))
          
          if (nrow(spy_returns) > 10) {
            cat("Successfully fetched", nrow(spy_returns), "SPY returns\n")
            return(spy_returns)
          }
        }
        
      }, error = function(e) {
        cat("Error with", source_config$symbol, "attempt", attempt, ":", e$message, "\n")
      })
    }
  }
  
  # If all sources fail, return NULL with warning
  cat("WARNING: Could not fetch SPY data from any source\n")
  return(NULL)
}

# ============================================================================
# 2. ENHANCED DATA VALIDATION
# ============================================================================

validate_portfolio_data <- function(data) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  # Check basic structure
  if (!is.data.frame(data)) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, "Data must be a data frame")
    return(validation_results)
  }
  
  # Check required columns
  required_cols <- c("ticker", "date", "daily_return")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, 
                                   paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check for weights column
  weight_candidates <- setdiff(names(data), required_cols)
  weight_candidates <- weight_candidates[!grepl("^__index|^X\\.|^\\.\\.\\.|^Unnamed", weight_candidates)]
  
  if (length(weight_candidates) == 0) {
    validation_results$valid <- FALSE
    validation_results$errors <- c(validation_results$errors, "No weights column found")
  } else if (length(weight_candidates) > 1) {
    validation_results$warnings <- c(validation_results$warnings, 
                                     paste("Multiple weight candidates found:", paste(weight_candidates, collapse = ", "), 
                                           "- using first one"))
  }
  
  if (!validation_results$valid) {
    return(validation_results)
  }
  
  # Check date column
  tryCatch({
    test_dates <- as.Date(data$date[1:min(10, nrow(data))])
    if (any(is.na(test_dates))) {
      validation_results$errors <- c(validation_results$errors, "Date column contains invalid dates")
      validation_results$valid <- FALSE
    }
  }, error = function(e) {
    validation_results$errors <- c(validation_results$errors, "Date column cannot be converted to Date format")
    validation_results$valid <- FALSE
  })
  
  # Check for numeric columns
  if (!"numeric" %in% class(data$daily_return) && !"integer" %in% class(data$daily_return)) {
    validation_results$errors <- c(validation_results$errors, "daily_return must be numeric")
    validation_results$valid <- FALSE
  }
  
  # Check for reasonable data ranges
  if (validation_results$valid) {
    returns_range <- range(data$daily_return, na.rm = TRUE)
    if (returns_range[1] < -0.5 || returns_range[2] > 0.5) {
      validation_results$warnings <- c(validation_results$warnings, 
                                       "Daily returns outside typical range (-50% to +50%) - check data format")
    }
    
    # Check for excessive missing data
    na_pct <- sum(is.na(data$daily_return)) / nrow(data)
    if (na_pct > 0.1) {
      validation_results$warnings <- c(validation_results$warnings, 
                                       paste("High percentage of missing returns:", round(na_pct * 100, 1), "%"))
    }
  }
  
  return(validation_results)
}

# ============================================================================
# 3. IMPROVED DATA ALIGNMENT FUNCTION
# ============================================================================

align_portfolio_benchmark <- function(portfolio_data, benchmark_data, max_missing_pct = 0.05) {
  
  cat("Starting data alignment...\n")
  cat("Portfolio data:", nrow(portfolio_data), "rows\n")
  cat("Benchmark data:", nrow(benchmark_data), "rows\n")
  
  # Ensure date columns are Date type
  portfolio_data$date <- as.Date(portfolio_data$date)
  benchmark_data$date <- as.Date(benchmark_data$date)
  
  # Check date ranges
  portfolio_range <- range(portfolio_data$date, na.rm = TRUE)
  benchmark_range <- range(benchmark_data$date, na.rm = TRUE)
  
  cat("Portfolio date range:", as.character(portfolio_range[1]), "to", as.character(portfolio_range[2]), "\n")
  cat("Benchmark date range:", as.character(benchmark_range[1]), "to", as.character(benchmark_range[2]), "\n")
  
  # Find overlap period
  overlap_start <- max(portfolio_range[1], benchmark_range[1])
  overlap_end <- min(portfolio_range[2], benchmark_range[2])
  
  if (overlap_start > overlap_end) {
    stop("No overlapping dates between portfolio and benchmark data")
  }
  
  cat("Overlap period:", as.character(overlap_start), "to", as.character(overlap_end), "\n")
  
  # Filter to overlap period
  portfolio_overlap <- portfolio_data %>%
    filter(date >= overlap_start, date <= overlap_end)
  
  benchmark_overlap <- benchmark_data %>%
    filter(date >= overlap_start, date <= overlap_end)
  
  # Inner join to get only matching dates
  aligned_data <- portfolio_overlap %>%
    inner_join(benchmark_overlap, by = "date", suffix = c("", "_bench")) %>%
    arrange(date)
  
  cat("After alignment:", nrow(aligned_data), "rows\n")
  
  # Check data loss
  original_portfolio_days <- length(unique(portfolio_data$date))
  aligned_days <- nrow(aligned_data)
  data_loss_pct <- 1 - (aligned_days / original_portfolio_days)
  
  cat("Data loss:", round(data_loss_pct * 100, 1), "%\n")
  
  if (data_loss_pct > max_missing_pct) {
    warning(paste("High data loss during alignment:", round(data_loss_pct * 100, 1), "%"))
  }
  
  if (nrow(aligned_data) < 100) {
    warning("Very few observations after alignment (", nrow(aligned_data), ") - results may be unreliable")
  }
  
  return(aligned_data)
}

# ============================================================================
# 4. ENHANCED ERROR HANDLING FOR PERFORMANCE ANALYTICS
# ============================================================================

safe_performance_metrics <- function(returns_xts, benchmark_xts = NULL) {
  
  metrics <- list()
  
  # Basic metrics with error handling
  metrics$total_return <- tryCatch({
    Return.cumulative(returns_xts)
  }, error = function(e) {
    prod(1 + as.numeric(returns_xts), na.rm = TRUE) - 1
  })
  
  metrics$annualized_return <- tryCatch({
    Return.annualized(returns_xts)
  }, error = function(e) {
    n_years <- length(returns_xts) / 252
    (1 + metrics$total_return)^(1/n_years) - 1
  })
  
  metrics$volatility <- tryCatch({
    StdDev.annualized(returns_xts)
  }, error = function(e) {
    sd(as.numeric(returns_xts), na.rm = TRUE) * sqrt(252)
  })
  
  metrics$sharpe <- tryCatch({
    SharpeRatio.annualized(returns_xts, Rf = 0)
  }, error = function(e) {
    metrics$annualized_return / metrics$volatility
  })
  
  # Manual Sortino calculation (PA version often fails)
  metrics$sortino <- tryCatch({
    returns_vec <- as.numeric(returns_xts)
    negative_returns <- returns_vec[returns_vec < 0 & !is.na(returns_vec)]
    if (length(negative_returns) > 0) {
      downside_vol <- sqrt(mean(negative_returns^2, na.rm = TRUE)) * sqrt(252)
      metrics$annualized_return / downside_vol
    } else {
      Inf
    }
  }, error = function(e) {
    NA
  })
  
  metrics$max_drawdown <- tryCatch({
    maxDrawdown(returns_xts)
  }, error = function(e) {
    cumulative <- cumprod(1 + as.numeric(returns_xts))
    drawdowns <- (cumulative - cummax(cumulative)) / cummax(cumulative)
    abs(min(drawdowns, na.rm = TRUE))
  })
  
  # VaR with multiple methods as fallback
  metrics$var_95 <- tryCatch({
    VaR(returns_xts, p = 0.95, method = "historical")
  }, error = function(e) {
    quantile(as.numeric(returns_xts), 0.05, na.rm = TRUE)
  })
  
  # Benchmark metrics if available
  if (!is.null(benchmark_xts)) {
    # Beta calculation
    metrics$beta <- tryCatch({
      CAPM.beta(returns_xts, benchmark_xts)
    }, error = function(e) {
      portfolio_vec <- as.numeric(returns_xts)
      benchmark_vec <- as.numeric(benchmark_xts)
      paired_data <- na.omit(data.frame(p = portfolio_vec, b = benchmark_vec))
      if (nrow(paired_data) > 10) {
        cov(paired_data$p, paired_data$b) / var(paired_data$b)
      } else {
        NA
      }
    })
    
    # Manual alpha calculation (CAPM.alpha is buggy)
    metrics$alpha <- tryCatch({
      portfolio_vec <- as.numeric(returns_xts)
      benchmark_vec <- as.numeric(benchmark_xts)
      paired_data <- na.omit(data.frame(p = portfolio_vec, b = benchmark_vec))
      
      if (nrow(paired_data) > 10) {
        beta_val <- cov(paired_data$p, paired_data$b) / var(paired_data$b)
        portfolio_annual <- mean(paired_data$p, na.rm = TRUE) * 252
        benchmark_annual <- mean(paired_data$b, na.rm = TRUE) * 252
        portfolio_annual - beta_val * benchmark_annual
      } else {
        NA
      }
    }, error = function(e) {
      NA
    })
    
    # Information ratio with fallback
    metrics$info_ratio <- tryCatch({
      InformationRatio(returns_xts, benchmark_xts)
    }, error = function(e) {
      active_returns <- as.numeric(returns_xts) - as.numeric(benchmark_xts)
      mean_active <- mean(active_returns, na.rm = TRUE) * 252
      tracking_error <- sd(active_returns, na.rm = TRUE) * sqrt(252)
      if (tracking_error > 0) mean_active / tracking_error else NA
    })
  }
  
  return(metrics)
}

# ============================================================================
# 5. MEMORY-EFFICIENT ROLLING CALCULATIONS
# ============================================================================

efficient_rolling_metrics <- function(returns_xts, window = 252) {
  
  n_obs <- nrow(returns_xts)
  
  # Skip rolling calculations if data is too small
  if (n_obs < window * 1.5) {
    warning("Insufficient data for reliable rolling calculations")
    return(data.frame(
      date = index(returns_xts),
      rolling_vol = NA,
      rolling_sharpe = NA
    ))
  }
  
  # Use zoo's rollapply with efficient settings
  rolling_vol <- tryCatch({
    zoo::rollapply(returns_xts, width = window, FUN = function(x) {
      sd(as.numeric(x), na.rm = TRUE) * sqrt(252)
    }, fill = NA, align = "right", by.column = FALSE)
  }, error = function(e) {
    rep(NA, n_obs)
  })
  
  rolling_sharpe <- tryCatch({
    zoo::rollapply(returns_xts, width = window, FUN = function(x) {
      x_numeric <- as.numeric(x)
      mean_ret <- mean(x_numeric, na.rm = TRUE)
      sd_ret <- sd(x_numeric, na.rm = TRUE)
      if (sd_ret > 0) (mean_ret / sd_ret) * sqrt(252) else NA
    }, fill = NA, align = "right", by.column = FALSE)
  }, error = function(e) {
    rep(NA, n_obs)
  })
  
  return(data.frame(
    date = index(returns_xts),
    rolling_vol = as.numeric(rolling_vol),
    rolling_sharpe = as.numeric(rolling_sharpe)
  ))
}

# ============================================================================
# 6. COMPREHENSIVE DATA PROCESSING WITH ALL FIXES
# ============================================================================

process_portfolio_data_robust <- function(data, rescale_vol = FALSE, spy_fallback = TRUE) {
  
  cat("=== ROBUST PORTFOLIO DATA PROCESSING ===\n")
  
  # Step 1: Validate input data
  validation <- validate_portfolio_data(data)
  
  if (!validation$valid) {
    stop("Data validation failed:\n", paste(validation$errors, collapse = "\n"))
  }
  
  if (length(validation$warnings) > 0) {
    cat("Warnings:\n", paste(validation$warnings, collapse = "\n"), "\n")
  }
  
  # Step 2: Clean and process portfolio data
  cat("Processing portfolio data...\n")
  
  # Remove index columns
  index_cols <- grep("^__index|^X\\.|^\\.\\.\\.|^Unnamed", names(data), value = TRUE)
  if (length(index_cols) > 0) {
    data <- data %>% select(-all_of(index_cols))
    cat("Removed index columns:", paste(index_cols, collapse = ", "), "\n")
  }
  
  # Handle weights column
  standard_cols <- c("ticker", "daily_return", "date")
  weight_col_candidates <- setdiff(names(data), standard_cols)
  weight_col_candidates <- weight_col_candidates[!grepl("^__index|^X\\.|^\\.\\.\\.|^Unnamed", weight_col_candidates)]
  
  weight_col_name <- weight_col_candidates[1]
  data$weights <- data[[weight_col_name]]
  cat("Using weights column:", weight_col_name, "\n")
  
  # Convert dates and calculate portfolio returns
  data$date <- as.Date(data$date)
  
  processed_data <- data %>% 
    group_by(date) %>% 
    summarise(Ra = sum(weights * daily_return, na.rm = TRUE), .groups = 'drop') %>% 
    arrange(date) %>% 
    mutate(date = as.Date(date))
  
  cat("Portfolio calculated:", nrow(processed_data), "daily observations\n")
  cat("Date range:", as.character(min(processed_data$date)), "to", as.character(max(processed_data$date)), "\n")
  
  # Step 3: Fetch benchmark data with robust error handling
  if (spy_fallback) {
    cat("Fetching SPY benchmark data...\n")
    
    min_date <- min(processed_data$date) - 10  # Extra buffer
    max_date <- max(processed_data$date) + 5
    
    spy_data <- robust_spy_fetch(min_date, max_date)
    
    if (!is.null(spy_data)) {
      # Step 4: Align data safely
      cat("Aligning portfolio and benchmark data...\n")
      
      aligned_data <- tryCatch({
        align_portfolio_benchmark(processed_data, spy_data)
      }, error = function(e) {
        cat("Alignment failed:", e$message, "\n")
        cat("Proceeding without benchmark data...\n")
        processed_data$Rb <- NA
        processed_data
      })
      
      processed_data <- aligned_data
      cat("Final aligned dataset:", nrow(processed_data), "observations\n")
      
    } else {
      cat("SPY data unavailable - proceeding without benchmark\n")
      processed_data$Rb <- NA
    }
  } else {
    processed_data$Rb <- NA
  }
  
  # Step 5: Volatility rescaling if requested
  if (rescale_vol) {
    cat("Rescaling to 10% volatility...\n")
    current_vol <- sd(processed_data$Ra, na.rm = TRUE) * sqrt(252)
    target_vol <- 0.10
    
    if (current_vol > 0) {
      scaling_factor <- target_vol / current_vol
      processed_data$Ra <- processed_data$Ra * scaling_factor
      cat("Volatility rescaled from", round(current_vol * 100, 1), "% to 10.0%\n")
    }
  }
  
  # Step 6: Final validation
  cat("Final validation...\n")
  cat("Final dataset:", nrow(processed_data), "rows,", ncol(processed_data), "columns\n")
  cat("Columns:", paste(names(processed_data), collapse = ", "), "\n")
  cat("Portfolio return NAs:", sum(is.na(processed_data$Ra)), "\n")
  if ("Rb" %in% names(processed_data)) {
    cat("Benchmark return NAs:", sum(is.na(processed_data$Rb)), "\n")
    cat("Usable benchmark data:", sum(!is.na(processed_data$Rb)), "observations\n")
  }
  
  cat("=== ROBUST PROCESSING COMPLETE ===\n")
  return(processed_data)
}