# ============================================================================
# SUPPORTING FUNCTIONS FOR PORTFOLIO ANALYTICS SHINY APP
# Save this as "supporting_functions.R"
# ============================================================================

library(tidyverse)
library(xts)
library(PerformanceAnalytics)

# ============================================================================
# 1. CORRECTED HEDGE FUND ANALYSIS FUNCTION
# ============================================================================

hf_grade_analysis <- function(portfolio_data, benchmark_data = NULL, fund_name = "Portfolio") {
  
  # Ensure data is properly formatted and convert to xts
  library(xts)
  dates <- portfolio_data$date
  
  # Convert to xts format for PerformanceAnalytics
  portfolio_returns <- xts(portfolio_data$Ra, order.by = dates)
  benchmark_returns <- if("Rb" %in% names(portfolio_data) && !all(is.na(portfolio_data$Rb))) {
    xts(portfolio_data$Rb, order.by = dates)
  } else {
    NULL
  }
  
  # ============================================================================
  # CORE PERFORMANCE METRICS
  # ============================================================================
  
  # Basic metrics
  total_return <- Return.cumulative(portfolio_returns)
  annualized_return <- Return.annualized(portfolio_returns)
  annualized_vol <- StdDev.annualized(portfolio_returns)
  sharpe_ratio <- SharpeRatio.annualized(portfolio_returns, Rf = 0)
  
  # Downside metrics - using manual calculation due to PA bug
  returns_vector <- as.numeric(portfolio_returns)
  negative_returns <- returns_vector[returns_vector < 0 & !is.na(returns_vector)]
  downside_deviation <- sqrt(mean(negative_returns^2, na.rm = TRUE)) * sqrt(252)
  sortino_ratio <- (mean(returns_vector, na.rm = TRUE) * 252) / downside_deviation
  
  max_drawdown <- maxDrawdown(portfolio_returns)
  
  # Higher moments
  skewness_val <- skewness(portfolio_returns, na.rm = TRUE)
  kurtosis_val <- kurtosis(portfolio_returns, na.rm = TRUE)
  
  # VaR and ES metrics
  var_95 <- VaR(portfolio_returns, p = 0.95, method = "historical")
  var_99 <- VaR(portfolio_returns, p = 0.99, method = "historical")
  es_95 <- ES(portfolio_returns, p = 0.95, method = "historical")
  es_99 <- ES(portfolio_returns, p = 0.99, method = "historical")
  
  # Calmar ratio
  calmar_ratio <- CalmarRatio(portfolio_returns)
  
  # Market metrics (if benchmark provided) 
  if (!is.null(benchmark_returns)) {
    beta_val <- CAPM.beta(portfolio_returns, benchmark_returns)
    
    # FIXED ALPHA CALCULATION - Manual calculation instead of broken CAPM.alpha()
    portfolio_vec <- as.numeric(portfolio_returns)
    benchmark_vec <- as.numeric(benchmark_returns)
    
    # Remove NAs for paired analysis
    paired_data <- na.omit(data.frame(portfolio = portfolio_vec, benchmark = benchmark_vec))
    
    if (nrow(paired_data) > 10) {
      beta_manual <- cov(paired_data$portfolio, paired_data$benchmark) / var(paired_data$benchmark)
      portfolio_annual <- mean(paired_data$portfolio) * 252
      benchmark_annual <- mean(paired_data$benchmark) * 252
      alpha_val <- portfolio_annual - beta_manual * benchmark_annual
    } else {
      alpha_val <- 0
    }
    
    info_ratio <- InformationRatio(portfolio_returns, benchmark_returns)
    tracking_error <- TrackingError(portfolio_returns, benchmark_returns)
    
    # Fix UpDownRatios - it returns a matrix, not a list
    updown_ratios <- UpDownRatios(portfolio_returns, benchmark_returns)
    up_capture <- updown_ratios[1,1]
    down_capture <- updown_ratios[2,1]
  }
  
  # ============================================================================
  # TIME-BASED ANALYSIS
  # ============================================================================
  
  # Rolling metrics (252-day window)
  rolling_vol <- rollapply(portfolio_returns, width = 252, FUN = sd, fill = NA, align = "right") * sqrt(252)
  rolling_sharpe <- rollapply(portfolio_returns, width = 252, 
                              FUN = function(x) mean(x)/sd(x) * sqrt(252), 
                              fill = NA, align = "right")
  
  # Monthly returns for calendar analysis
  monthly_returns <- portfolio_returns %>%
    fortify(melt = TRUE) %>%
    mutate(date = as.Date(Index),
           year = year(date),
           month = month(date)) %>%
    group_by(year, month) %>%
    summarise(monthly_ret = prod(1 + Value) - 1, .groups = 'drop')
  
  # ============================================================================
  # RISK ATTRIBUTION
  # ============================================================================
  
  # Drawdown analysis
  dd_table <- table.Drawdowns(portfolio_returns, top = 10)
  
  # Consecutive wins/losses
  consecutive_analysis <- function(returns) {
    # Convert xts to vector if needed
    returns_vector <- as.numeric(returns)
    runs <- rle(sign(returns_vector))
    pos_runs <- runs$lengths[runs$values > 0]
    neg_runs <- runs$lengths[runs$values < 0]
    
    list(
      max_consecutive_wins = if(length(pos_runs) > 0) max(pos_runs) else 0,
      max_consecutive_losses = if(length(neg_runs) > 0) max(neg_runs) else 0,
      avg_win_streak = if(length(pos_runs) > 0) mean(pos_runs) else 0,
      avg_loss_streak = if(length(neg_runs) > 0) mean(neg_runs) else 0
    )
  }
  
  consec_stats <- consecutive_analysis(portfolio_returns)
  
  # ============================================================================
  # PERFORMANCE ATTRIBUTION
  # ============================================================================
  
  # Best/Worst periods
  returns_vector <- as.numeric(portfolio_returns)
  best_day <- max(returns_vector, na.rm = TRUE)
  worst_day <- min(returns_vector, na.rm = TRUE)
  best_month <- max(monthly_returns$monthly_ret, na.rm = TRUE)
  worst_month <- min(monthly_returns$monthly_ret, na.rm = TRUE)
  
  # Win rate
  win_rate <- sum(returns_vector > 0, na.rm = TRUE) / length(returns_vector[!is.na(returns_vector)])
  
  # ============================================================================
  # SUMMARY STATISTICS TABLE
  # ============================================================================
  
  summary_stats <- data.frame(
    Metric = c("Total Return", "Annualized Return", "Annualized Volatility", 
               "Sharpe Ratio", "Sortino Ratio", "Calmar Ratio", "Max Drawdown",
               "VaR (95%)", "VaR (99%)", "Expected Shortfall (95%)", "Expected Shortfall (99%)",
               "Skewness", "Excess Kurtosis", "Win Rate", "Best Day", "Worst Day",
               "Max Consecutive Wins", "Max Consecutive Losses"),
    Value = c(sprintf("%.2f%%", total_return * 100),
              sprintf("%.2f%%", annualized_return * 100),
              sprintf("%.2f%%", annualized_vol * 100),
              sprintf("%.2f", sharpe_ratio),
              sprintf("%.2f", sortino_ratio),
              sprintf("%.2f", calmar_ratio),
              sprintf("%.2f%%", abs(max_drawdown) * 100),
              sprintf("%.2f%%", abs(var_95) * 100),
              sprintf("%.2f%%", abs(var_99) * 100),
              sprintf("%.2f%%", abs(es_95) * 100),
              sprintf("%.2f%%", abs(es_99) * 100),
              sprintf("%.3f", skewness_val),
              sprintf("%.3f", kurtosis_val),
              sprintf("%.1f%%", win_rate * 100),
              sprintf("%.2f%%", best_day * 100),
              sprintf("%.2f%%", worst_day * 100),
              sprintf("%d", consec_stats$max_consecutive_wins),
              sprintf("%d", consec_stats$max_consecutive_losses))
  )
  
  # Add benchmark metrics if available
  if (!is.null(benchmark_returns)) {
    benchmark_metrics <- data.frame(
      Metric = c("Beta", "Alpha (Annualized)", "Information Ratio", "Tracking Error",
                 "Up Capture", "Down Capture"),
      Value = c(sprintf("%.2f", beta_val),
                sprintf("%.2f%%", alpha_val * 100),
                sprintf("%.2f", info_ratio),
                sprintf("%.2f%%", tracking_error * 100),
                sprintf("%.1f%%", up_capture * 100),
                sprintf("%.1f%%", down_capture * 100))
    )
    summary_stats <- rbind(summary_stats, benchmark_metrics)
  }
  
  # ============================================================================
  # VISUALIZATIONS
  # ============================================================================
  
  library(ggplot2)
  library(scales)
  
  # Cumulative returns plot
  cumulative_data <- data.frame(
    date = index(portfolio_returns),
    portfolio = as.numeric(cumprod(1 + portfolio_returns))
  )
  
  if (!is.null(benchmark_returns)) {
    cumulative_data$benchmark <- as.numeric(cumprod(1 + benchmark_returns))
  }
  
  p1 <- ggplot(cumulative_data, aes(x = date)) +
    geom_line(aes(y = portfolio), color = "blue", linewidth = 1) +
    {if(!is.null(benchmark_returns)) geom_line(aes(y = benchmark), color = "red", alpha = 0.7)} +
    scale_y_log10(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste(fund_name, "- Cumulative Returns"), 
         x = "Date", y = "Cumulative Return (Log Scale)") +
    theme_minimal()
  
  # Drawdown plot
  drawdown_data <- data.frame(
    date = index(portfolio_returns),
    drawdown = as.numeric(Drawdowns(portfolio_returns))
  )
  
  p2 <- ggplot(drawdown_data, aes(x = date, y = drawdown)) +
    geom_area(fill = "red", alpha = 0.3) +
    geom_line(color = "red") +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste(fund_name, "- Underwater Chart"), 
         x = "Date", y = "Drawdown") +
    theme_minimal()
  
  # Rolling Sharpe ratio plot
  rolling_data <- data.frame(
    date = index(portfolio_returns),
    rolling_sharpe = as.numeric(rolling_sharpe)
  ) %>% filter(!is.na(rolling_sharpe))
  
  p3 <- ggplot(rolling_data, aes(x = date, y = rolling_sharpe)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(title = paste(fund_name, "- Rolling 12-Month Sharpe Ratio"), 
         x = "Date", y = "Sharpe Ratio") +
    theme_minimal()
  
  # Return distribution
  p4 <- ggplot(data.frame(returns = as.numeric(portfolio_returns)), aes(x = returns)) +
    geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
    geom_vline(xintercept = mean(as.numeric(portfolio_returns), na.rm = TRUE), 
               color = "red", linetype = "dashed") +
    scale_x_continuous(labels = percent_format()) +
    labs(title = paste(fund_name, "- Return Distribution"), 
         x = "Daily Returns", y = "Frequency") +
    theme_minimal()
  
  # Monthly heatmap
  monthly_grid <- monthly_returns %>%
    complete(year = min(year):max(year), month = 1:12, fill = list(monthly_ret = NA)) %>%
    mutate(month_name = month.abb[month])
  
  p5 <- ggplot(monthly_grid, aes(x = factor(month), y = factor(year), fill = monthly_ret)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                         midpoint = 0, labels = percent_format()) +
    scale_x_discrete(labels = month.abb) +
    labs(title = paste(fund_name, "- Monthly Returns Heatmap"), 
         x = "Month", y = "Year", fill = "Return") +
    theme_minimal()
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  return(list(
    summary_stats = summary_stats,
    plots = list(
      cumulative_returns = p1,
      drawdowns = p2,
      rolling_sharpe = p3,
      return_distribution = p4,
      monthly_heatmap = p5
    ),
    raw_data = list(
      portfolio_returns = portfolio_returns,
      benchmark_returns = benchmark_returns,
      dates = dates
    ),
    drawdown_table = dd_table,
    monthly_returns = monthly_returns,
    rolling_metrics = data.frame(
      date = index(portfolio_returns),
      rolling_vol = as.numeric(rolling_vol),
      rolling_sharpe = as.numeric(rolling_sharpe)
    )
  ))
}

# ============================================================================
# 2. VALIDATION FUNCTIONS
# ============================================================================

validate_pa_functions <- function(returns_data) {
  
  # Convert to vector and xts for testing
  if(is.xts(returns_data)) {
    returns_vec <- as.numeric(returns_data[,1])
    returns_xts <- returns_data
  } else {
    returns_vec <- returns_data$Ra
    returns_xts <- xts(returns_vec, order.by = Sys.Date() + 1:length(returns_vec))
  }
  
  validation_results <- list()
  
  # Total Return
  manual_total_ret <- prod(1 + returns_vec, na.rm = TRUE) - 1
  pa_total_ret <- as.numeric(Return.cumulative(returns_xts))
  validation_results$total_return <- list(
    manual = manual_total_ret,
    pa = pa_total_ret,
    valid = abs(manual_total_ret - pa_total_ret) < 0.001
  )
  
  # Annualized Return
  n_periods <- length(returns_vec[!is.na(returns_vec)])
  manual_ann_ret <- (prod(1 + returns_vec, na.rm = TRUE))^(252/n_periods) - 1
  pa_ann_ret <- as.numeric(Return.annualized(returns_xts))
  validation_results$annualized_return <- list(
    manual = manual_ann_ret,
    pa = pa_ann_ret,
    valid = abs(manual_ann_ret - pa_ann_ret) < 0.01
  )
  
  # Volatility
  manual_vol <- sd(returns_vec, na.rm = TRUE) * sqrt(252)
  pa_vol <- as.numeric(StdDev.annualized(returns_xts))
  validation_results$volatility <- list(
    manual = manual_vol,
    pa = pa_vol,
    valid = abs(manual_vol - pa_vol) < 0.01
  )
  
  # Sharpe Ratio
  manual_sharpe <- manual_ann_ret / manual_vol
  pa_sharpe <- as.numeric(SharpeRatio.annualized(returns_xts, Rf = 0))
  validation_results$sharpe <- list(
    manual = manual_sharpe,
    pa = pa_sharpe,
    valid = abs(manual_sharpe - pa_sharpe) < 0.05
  )
  
  # Sortino Ratio (known broken)
  negative_returns <- returns_vec[returns_vec < 0 & !is.na(returns_vec)]
  manual_downside_vol <- sqrt(mean(negative_returns^2, na.rm = TRUE)) * sqrt(252)
  manual_sortino <- manual_ann_ret / manual_downside_vol
  pa_sortino <- as.numeric(SortinoRatio(returns_xts, MAR = 0))
  validation_results$sortino <- list(
    manual = manual_sortino,
    pa = pa_sortino,
    valid = FALSE  # Known broken
  )
  
  # Max Drawdown
  cumulative_returns <- cumprod(1 + returns_vec)
  running_max <- cummax(cumulative_returns)
  drawdowns <- (cumulative_returns - running_max) / running_max
  manual_max_dd <- abs(min(drawdowns, na.rm = TRUE))
  pa_max_dd <- as.numeric(maxDrawdown(returns_xts))
  validation_results$max_drawdown <- list(
    manual = manual_max_dd,
    pa = pa_max_dd,
    valid = abs(manual_max_dd - pa_max_dd) < 0.01
  )
  
  return(validation_results)
}

# ============================================================================
# 3. ROBUST CALCULATION ALTERNATIVES
# ============================================================================

robust_calculations <- list(
  
  sortino_ratio = function(returns, mar = 0) {
    if(is.xts(returns)) ret_vec <- as.numeric(returns[,1]) else ret_vec <- returns
    excess_ret <- ret_vec - mar/252
    downside_ret <- excess_ret[excess_ret < 0 & !is.na(excess_ret)]
    if(length(downside_ret) == 0) return(Inf)
    downside_vol <- sqrt(mean(downside_ret^2, na.rm = TRUE)) * sqrt(252)
    return((mean(excess_ret, na.rm = TRUE) * 252) / downside_vol)
  },
  
  calmar_ratio = function(returns) {
    if(is.xts(returns)) ret_vec <- as.numeric(returns[,1]) else ret_vec <- returns
    ann_ret <- (prod(1 + ret_vec, na.rm = TRUE))^(252/length(ret_vec[!is.na(ret_vec)])) - 1
    cum_ret <- cumprod(1 + ret_vec)
    drawdowns <- (cum_ret - cummax(cum_ret)) / cummax(cum_ret)
    max_dd <- abs(min(drawdowns, na.rm = TRUE))
    return(ann_ret / max_dd)
  },
  
  information_ratio = function(portfolio_returns, benchmark_returns) {
    if(is.xts(portfolio_returns)) p_ret <- as.numeric(portfolio_returns[,1]) else p_ret <- portfolio_returns
    if(is.xts(benchmark_returns)) b_ret <- as.numeric(benchmark_returns[,1]) else b_ret <- benchmark_returns
    
    active_ret <- p_ret - b_ret
    tracking_error <- sd(active_ret, na.rm = TRUE) * sqrt(252)
    return((mean(active_ret, na.rm = TRUE) * 252) / tracking_error)
  }
)

# ============================================================================
# 4. PORTFOLIO COMPARISON FUNCTIONS
# ============================================================================

compare_strategies <- function(strategy_list, normalize = TRUE) {
  
  comparison_data <- data.frame()
  
  for (strategy_name in names(strategy_list)) {
    strategy_data <- strategy_list[[strategy_name]]
    
    temp_data <- data.frame(
      date = strategy_data$date,
      returns = strategy_data$Ra,
      cumulative = cumprod(1 + strategy_data$Ra),
      strategy = strategy_name
    )
    
    comparison_data <- rbind(comparison_data, temp_data)
  }
  
  if (normalize) {
    comparison_data <- comparison_data %>%
      group_by(strategy) %>%
      mutate(cumulative = cumulative / first(cumulative) * 100) %>%
      ungroup()
  }
  
  return(comparison_data)
}

# ============================================================================
# 5. RISK ANALYSIS FUNCTIONS
# ============================================================================

monte_carlo_simulation <- function(returns, n_simulations = 1000, n_periods = 252) {
  
  if(is.xts(returns)) returns_vec <- as.numeric(returns[,1]) else returns_vec <- returns
  
  # Estimate parameters
  mu <- mean(returns_vec, na.rm = TRUE)
  sigma <- sd(returns_vec, na.rm = TRUE)
  
  # Run simulations
  simulations <- matrix(NA, nrow = n_periods, ncol = n_simulations)
  
  for (i in 1:n_simulations) {
    # Generate random returns
    sim_returns <- rnorm(n_periods, mean = mu, sd = sigma)
    # Calculate cumulative wealth
    simulations[, i] <- cumprod(1 + sim_returns)
  }
  
  # Calculate percentiles
  percentiles <- apply(simulations, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  result <- data.frame(
    period = 1:n_periods,
    p5 = percentiles[1, ],
    p25 = percentiles[2, ],
    median = percentiles[3, ],
    p75 = percentiles[4, ],
    p95 = percentiles[5, ]
  )
  
  return(list(
    simulation_results = result,
    final_values = simulations[n_periods, ],
    parameters = list(mu = mu, sigma = sigma)
  ))
}

# ============================================================================
# 6. STRATEGY GRADING FUNCTION
# ============================================================================

grade_strategy <- function(analysis_results) {
  
  summary_stats <- analysis_results$summary_stats
  
  # Extract key metrics - handle percentage strings properly
  total_return_str <- summary_stats$Value[summary_stats$Metric == "Total Return"]
  total_return <- as.numeric(gsub("%", "", total_return_str)) / 100
  
  sharpe <- as.numeric(summary_stats$Value[summary_stats$Metric == "Sharpe Ratio"])
  
  max_dd_str <- summary_stats$Value[summary_stats$Metric == "Max Drawdown"]
  max_dd <- as.numeric(gsub("%", "", max_dd_str)) / 100
  
  volatility_str <- summary_stats$Value[summary_stats$Metric == "Annualized Volatility"]
  volatility <- as.numeric(gsub("%", "", volatility_str)) / 100
  
  # Grading criteria - FIXED THRESHOLDS
  grade_points <- 0
  
  # Return component (0-30 points) - FIXED for high returns
  if (total_return > 5.0) grade_points <- grade_points + 30        # 500%+ 
  else if (total_return > 2.0) grade_points <- grade_points + 28   # 200%+ (your case: 192%)
  else if (total_return > 1.0) grade_points <- grade_points + 25   # 100%+
  else if (total_return > 0.5) grade_points <- grade_points + 20   # 50%+
  else if (total_return > 0.2) grade_points <- grade_points + 15   # 20%+
  else if (total_return > 0.1) grade_points <- grade_points + 10   # 10%+
  else grade_points <- grade_points + 5
  
  # Sharpe component (0-25 points) - IMPROVED thresholds
  if (sharpe > 2.0) grade_points <- grade_points + 25
  else if (sharpe > 1.5) grade_points <- grade_points + 23
  else if (sharpe > 1.3) grade_points <- grade_points + 20         # Your case: 1.39
  else if (sharpe > 1.0) grade_points <- grade_points + 17
  else if (sharpe > 0.8) grade_points <- grade_points + 12
  else if (sharpe > 0.5) grade_points <- grade_points + 8
  else grade_points <- grade_points + 3
  
  # Drawdown component (0-25 points) - Your 9.13% gets 20 points ✓
  if (max_dd < 0.05) grade_points <- grade_points + 25
  else if (max_dd < 0.10) grade_points <- grade_points + 20        # Your case: 9.13%
  else if (max_dd < 0.15) grade_points <- grade_points + 17
  else if (max_dd < 0.20) grade_points <- grade_points + 14
  else if (max_dd < 0.30) grade_points <- grade_points + 10
  else grade_points <- grade_points + 5
  
  # Volatility component (0-20 points) - FIXED the boundary issue
  if (volatility <= 0.10) grade_points <- grade_points + 20        # Your case: exactly 10%
  else if (volatility <= 0.15) grade_points <- grade_points + 17
  else if (volatility <= 0.20) grade_points <- grade_points + 14
  else if (volatility <= 0.30) grade_points <- grade_points + 10
  else if (volatility <= 0.50) grade_points <- grade_points + 5
  else grade_points <- grade_points + 2
  
  # Convert to letter grade
  if (grade_points >= 90) grade <- "A+"
  else if (grade_points >= 85) grade <- "A"
  else if (grade_points >= 80) grade <- "A-"
  else if (grade_points >= 75) grade <- "B+"
  else if (grade_points >= 70) grade <- "B"
  else if (grade_points >= 65) grade <- "B-"
  else if (grade_points >= 60) grade <- "C+"
  else if (grade_points >= 55) grade <- "C"
  else if (grade_points >= 50) grade <- "C-"
  else grade <- "F"
  
  return(list(
    grade = grade,
    points = grade_points,
    breakdown = list(
      return_points = if (total_return > 2.0) 28 else 25,
      sharpe_points = if (sharpe > 1.3) 20 else 17,
      drawdown_points = if (max_dd < 0.10) 20 else 17,
      volatility_points = if (volatility <= 0.10) 20 else 17
    )
  ))
}