# ============================================================================
# PORTFOLIO ANALYTICS SHINY APP WITH S3 INTEGRATION
# ============================================================================

# Install required packages if needed
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly", "tidyverse", 
  "tidyquant", "PerformanceAnalytics", "arrow", "xts", "scales",
  "aws.s3", "aws.signature", "paws", "shinyWidgets"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Load libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(tidyverse)
  library(tidyquant)
  library(PerformanceAnalytics)
  library(arrow)
  library(xts)
  library(scales)
  library(aws.s3)
  library(aws.signature)
  library(httr)
  library(shinyWidgets)
})

# CRITICAL: Fix SSL certificate issue for corporate networks
httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

# Source supporting functions
if (file.exists("supporting_functions.R")) {
  source("supporting_functions.R")
}

# ============================================================================
# S3 HELPER FUNCTIONS
# ============================================================================

# Auto-load AWS credentials from multiple sources
auto_load_aws_credentials <- function() {
  # Method 1: Check environment variables
  if (Sys.getenv("AWS_ACCESS_KEY_ID") != "" && Sys.getenv("AWS_SECRET_ACCESS_KEY") != "") {
    return(list(
      access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"),
      bucket = Sys.getenv("AWS_S3_BUCKET", ""),
      source = "environment"
    ))
  }
  
  # Method 2: Check .Renviron file
  renviron_file <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (file.exists(renviron_file)) {
    readRenviron(renviron_file)
    if (Sys.getenv("AWS_ACCESS_KEY_ID") != "" && Sys.getenv("AWS_SECRET_ACCESS_KEY") != "") {
      return(list(
        access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"),
        bucket = Sys.getenv("AWS_S3_BUCKET", ""),
        source = ".Renviron"
      ))
    }
  }
  
  # Method 3: Check AWS credentials file
  aws_creds_file <- file.path(Sys.getenv("HOME"), ".aws", "credentials")
  if (file.exists(aws_creds_file)) {
    tryCatch({
      creds_lines <- readLines(aws_creds_file)
      default_section <- FALSE
      access_key <- ""
      secret_key <- ""
      
      for (line in creds_lines) {
        line <- trimws(line)
        if (line == "[default]") {
          default_section <- TRUE
        } else if (startsWith(line, "[") && line != "[default]") {
          default_section <- FALSE
        } else if (default_section) {
          if (startsWith(line, "aws_access_key_id")) {
            access_key <- trimws(sub("aws_access_key_id\\s*=\\s*", "", line))
          } else if (startsWith(line, "aws_secret_access_key")) {
            secret_key <- trimws(sub("aws_secret_access_key\\s*=\\s*", "", line))
          }
        }
      }
      
      if (access_key != "" && secret_key != "") {
        # Check for region in config file
        config_file <- file.path(Sys.getenv("HOME"), ".aws", "config")
        region <- "us-east-1"
        if (file.exists(config_file)) {
          config_lines <- readLines(config_file)
          for (line in config_lines) {
            if (startsWith(trimws(line), "region")) {
              region <- trimws(sub("region\\s*=\\s*", "", line))
              break
            }
          }
        }
        
        return(list(
          access_key = access_key,
          secret_key = secret_key,
          region = region,
          bucket = Sys.getenv("AWS_S3_BUCKET", ""),
          source = "~/.aws/credentials"
        ))
      }
    }, error = function(e) {
      # Continue to next method
    })
  }
  
  return(NULL)
}

# Configure AWS credentials
configure_aws <- function(access_key, secret_key, region = "us-east-1") {
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = access_key,
    "AWS_SECRET_ACCESS_KEY" = secret_key,
    "AWS_DEFAULT_REGION" = region
  )
}

# List S3 objects with folder structure using aws.s3
list_s3_objects <- function(bucket, prefix = "", max_keys = 1000) {
  tryCatch({
    # Get all objects with the prefix
    all_objects <- aws.s3::get_bucket(bucket, prefix = prefix, max = max_keys)
    
    if (length(all_objects) == 0) {
      return(data.frame(
        name = character(0),
        display_name = character(0),
        type = character(0),
        size = numeric(0),
        last_modified = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract object information safely
    object_info <- data.frame(
      name = character(length(all_objects)),
      size = numeric(length(all_objects)),
      last_modified = character(length(all_objects)),
      stringsAsFactors = FALSE
    )
    
    # Safely extract data from each object
    for (i in 1:length(all_objects)) {
      obj <- all_objects[[i]]
      object_info$name[i] <- if ("Key" %in% names(obj)) as.character(obj$Key) else ""
      object_info$size[i] <- if ("Size" %in% names(obj)) as.numeric(obj$Size) else 0
      object_info$last_modified[i] <- if ("LastModified" %in% names(obj)) as.character(obj$LastModified) else ""
    }
    
    # Remove empty entries
    object_info <- object_info[object_info$name != "", ]
    
    if (nrow(object_info) == 0) {
      return(data.frame(
        name = character(0),
        display_name = character(0),
        type = character(0),
        size = numeric(0),
        last_modified = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Process keys to separate folders and files
    if (prefix != "") {
      # Remove the prefix from keys for display
      relative_keys <- sub(paste0("^", prefix), "", object_info$name)
    } else {
      relative_keys <- object_info$name
    }
    
    # Identify folders (keys that have content after removing prefix and contain "/")
    # and files (keys that don't contain "/" after prefix removal)
    is_folder_content <- grepl("/", relative_keys)
    
    result_df <- data.frame(
      name = character(0),
      display_name = character(0),
      type = character(0),
      size = numeric(0),
      last_modified = character(0),
      stringsAsFactors = FALSE
    )
    
    # Get unique folder names at current level
    if (any(is_folder_content)) {
      folder_paths <- relative_keys[is_folder_content]
      folder_names <- unique(sapply(strsplit(folder_paths, "/"), function(x) x[1]))
      folder_names <- folder_names[folder_names != ""]
      
      if (length(folder_names) > 0) {
        folders <- data.frame(
          name = paste0(prefix, folder_names, "/"),
          display_name = folder_names,
          type = "folder",
          size = NA,
          last_modified = NA,
          stringsAsFactors = FALSE
        )
        result_df <- rbind(result_df, folders)
      }
    }
    
    # Get files at current level (no "/" in relative path)
    file_indices <- which(!is_folder_content)
    if (length(file_indices) > 0) {
      files <- data.frame(
        name = object_info$name[file_indices],
        display_name = relative_keys[file_indices],
        type = "file",
        size = object_info$size[file_indices],
        last_modified = object_info$last_modified[file_indices],
        stringsAsFactors = FALSE
      )
      
      # Filter for relevant file types
      if (nrow(files) > 0) {
        file_extensions <- tools::file_ext(files$display_name)
        relevant_extensions <- c("parquet", "csv", "xlsx", "xls", "tar", "gz", "zip")
        relevant_files <- file_extensions %in% relevant_extensions
        files <- files[relevant_files, ]
        
        if (nrow(files) > 0) {
          result_df <- rbind(result_df, files)
        }
      }
    }
    
    return(result_df)
    
  }, error = function(e) {
    # Return empty data frame with error info
    cat("Error in list_s3_objects:", e$message, "\n")
    return(data.frame(
      name = paste("Error:", e$message),
      display_name = "Error loading files",
      type = "error",
      size = NA,
      last_modified = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# Download file from S3 to temp location
download_s3_file <- function(bucket, key) {
  temp_file <- tempfile(fileext = paste0(".", tools::file_ext(key)))
  
  tryCatch({
    aws.s3::save_object(
      object = key,
      bucket = bucket,
      file = temp_file
    )
    return(temp_file)
  }, error = function(e) {
    stop(paste("Failed to download S3 file:", e$message))
  })
}

# Format file size for display
format_file_size <- function(size_bytes) {
  # Handle vectors properly
  sapply(size_bytes, function(size) {
    if (is.na(size) || is.null(size)) return("")
    
    size <- as.numeric(size)
    if (is.na(size)) return("")
    
    if (size < 1024) {
      return(paste(size, "B"))
    } else if (size < 1024^2) {
      return(paste(round(size / 1024, 1), "KB"))
    } else if (size < 1024^3) {
      return(paste(round(size / 1024^2, 1), "MB"))
    } else {
      return(paste(round(size / 1024^3, 1), "GB"))
    }
  })
}

# ============================================================================
# ENHANCED UI WITH S3 BROWSER
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; font-weight: 600;",
      tags$i(class = "fa fa-chart-line", style = "margin-right: 8px; color: #fff;"),
      "Portfolio Analytics Pro"
    ), 
    titleWidth = 300,
    # Add user info and timestamp in header
    tags$li(class = "dropdown",
            style = "margin: 15px 10px; color: #fff; font-size: 0.9em;",
            div(
              div(style = "font-weight: 500;", "S3 Analytics"),
              div(style = "opacity: 0.8; font-size: 0.8em;", 
                  format(Sys.time(), "%Y-%m-%d %H:%M"))
            )
    )
  ),
  
  dashboardSidebar(
    width = 280,
    div(style = "padding: 15px 15px 5px 15px; border-bottom: 1px solid #333;",
        div(style = "color: #b8c7ce; font-size: 0.9em; font-weight: 500; margin-bottom: 8px;",
            "NAVIGATION"),
        div(style = "color: #7a8a99; font-size: 0.8em; line-height: 1.3;",
            "Connect → Browse → Analyze")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("🔐 S3 Connection", tabName = "s3_setup", icon = icon("cloud-upload-alt")),
      menuItem("📁 File Browser", tabName = "s3_browse", icon = icon("folder-open")),
      menuItem("📊 Performance Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("📈 Visualizations", tabName = "charts", icon = icon("chart-line")),
      menuItem("🔍 System Validation", tabName = "validation", icon = icon("check-circle"))
    ),
    
    # Add dynamic status indicator at bottom
    div(style = "position: absolute; bottom: 15px; left: 15px; right: 15px;",
        uiOutput("connection_status_indicator")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Professional color scheme and typography */
        .main-header .navbar { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important; }
        .main-header .logo { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important; }
        .skin-blue .main-sidebar { background: #2c3e50 !important; }
        
        /* Enhanced content styling */
        .content-wrapper { background: #f8f9fa !important; }
        .box { box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important; border: none !important; }
        .box-header { border-bottom: 2px solid #e9ecef !important; background: #fff !important; }
        .box-title { font-weight: 600 !important; color: #2c3e50 !important; }
        
        /* Professional table styling */
        .dataTables_wrapper { background: white; border-radius: 8px; }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_processing,
        .dataTables_wrapper .dataTables_paginate {
          color: #6c757d !important;
          font-size: 0.9em !important;
        }
        
        /* File selection styling */
        .s3-file-row:hover { 
          background: linear-gradient(135deg, #e3f2fd 0%, #f3e5f5 100%) !important; 
          cursor: pointer; 
          transition: all 0.2s ease;
        }
        .dataTables_wrapper tbody tr:hover {
          background: linear-gradient(135deg, #e3f2fd 0%, #f3e5f5 100%) !important;
        }
        
        /* Professional buttons */
        .btn { 
          font-weight: 500; 
          border-radius: 6px; 
          transition: all 0.3s ease;
          text-transform: none;
        }
        .btn-primary { 
          background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); 
          border: none; 
          box-shadow: 0 2px 8px rgba(0,123,255,0.3);
        }
        .btn-primary:hover { 
          transform: translateY(-1px); 
          box-shadow: 0 4px 12px rgba(0,123,255,0.4);
        }
        .btn-success { 
          background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); 
          border: none; 
          box-shadow: 0 2px 8px rgba(40,167,69,0.3);
        }
        .btn-success:hover { 
          transform: translateY(-1px); 
          box-shadow: 0 4px 12px rgba(40,167,69,0.4);
        }
        
        /* Value boxes enhancement */
        .small-box { 
          border-radius: 12px !important; 
          box-shadow: 0 4px 16px rgba(0,0,0,0.1) !important;
          transition: all 0.3s ease !important;
        }
        .small-box:hover { 
          transform: translateY(-2px) !important; 
          box-shadow: 0 8px 24px rgba(0,0,0,0.15) !important;
        }
        
        /* Professional notifications */
        .shiny-notification { 
          border-radius: 8px; 
          box-shadow: 0 4px 16px rgba(0,0,0,0.15); 
          border: none;
          font-weight: 500;
        }
        
        /* Loading states */
        .progress { 
          border-radius: 8px; 
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.1);
        }
        .progress-bar { 
          background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
        }
        
        /* Professional breadcrumbs */
        .breadcrumb { 
          background: linear-gradient(135deg, #fff 0%, #f8f9fa 100%); 
          border: 1px solid #e9ecef; 
          font-weight: 500;
        }
        
        /* Professional spacing */
        .tab-content { padding-top: 20px; }
        h4 { 
          color: #2c3e50; 
          font-weight: 600; 
          margin-bottom: 15px;
          border-bottom: 2px solid #e9ecef;
          padding-bottom: 8px;
        }
      "))
    ),
    
    tabItems(
      # ========================================================================
      # S3 SETUP TAB - Enhanced Professional Design
      # ========================================================================
      tabItem(tabName = "s3_setup",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, 
                    title = div(icon("cloud-upload-alt"), " AWS S3 Configuration"),
                    fluidRow(
                      column(6,
                             div(
                               style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); 
                                        padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                               h4("🔑 Credentials", style = "margin-top: 0; color: #495057;"),
                               textInput("aws_access_key", 
                                         label = div(style = "font-weight: 500;", "AWS Access Key ID:"),
                                         placeholder = "AKIAIOSFODNN7EXAMPLE"),
                               passwordInput("aws_secret_key", 
                                             label = div(style = "font-weight: 500;", "AWS Secret Access Key:"),
                                             placeholder = "Enter your secret key..."),
                               selectInput("aws_region", 
                                           label = div(style = "font-weight: 500;", "AWS Region:"),
                                           choices = c(
                                             "US East 1 (N. Virginia)" = "us-east-1",
                                             "US West 2 (Oregon)" = "us-west-2", 
                                             "EU West 1 (Ireland)" = "eu-west-1",
                                             "Asia Pacific (Singapore)" = "ap-southeast-1",
                                             "Custom Region" = "custom"
                                           ),
                                           selected = "us-east-1"),
                               conditionalPanel(
                                 condition = "input.aws_region == 'custom'",
                                 textInput("custom_region", 
                                           label = div(style = "font-weight: 500;", "Custom Region:"),
                                           placeholder = "us-gov-west-1")
                               ),
                               textInput("s3_bucket", 
                                         label = div(style = "font-weight: 500;", "S3 Bucket Name:"),
                                         placeholder = "my-portfolio-data-bucket"),
                               div(style = "margin-top: 20px;",
                                   actionButton("test_connection", 
                                                HTML("<i class='fa fa-plug'></i> Test Connection"), 
                                                class = "btn-primary btn-lg",
                                                style = "width: 100%;")),
                               div(style = "margin-top: 15px;",
                                   verbatimTextOutput("connection_status"))
                             )
                      ),
                      column(6,
                             div(
                               style = "background: linear-gradient(135deg, #e8f4f8 0%, #d1ecf1 100%); 
                                        padding: 20px; border-radius: 8px; border-left: 4px solid #17a2b8;",
                               h4("📋 Setup Guide", style = "margin-top: 0; color: #0c5460;"),
                               div(style = "font-size: 0.95em; line-height: 1.5;",
                                   h5("Step 1: AWS Credentials", style = "color: #0c5460; margin-top: 15px;"),
                                   tags$ul(
                                     tags$li("Access AWS IAM Console"),
                                     tags$li("Create or select user with S3 permissions"),
                                     tags$li("Generate new Access Key pair")
                                   ),
                                   
                                   h5("Step 2: Required Permissions", style = "color: #0c5460; margin-top: 15px;"),
                                   div(style = "background: white; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.85em;",
                                       "s3:ListBucket<br>s3:GetObject<br>s3:GetObjectVersion"),
                                   
                                   h5("Step 3: Test & Connect", style = "color: #0c5460; margin-top: 15px;"),
                                   p("Enter credentials and test connection to verify access to your S3 bucket.")
                               )
                             )
                      )
                    )
                )
              )
      ),
      
      # ========================================================================
      # S3 BROWSE TAB
      # ========================================================================
      tabItem(tabName = "s3_browse",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "📁 Browse S3 Bucket",
                    
                    # Breadcrumb navigation
                    div(class = "breadcrumb",
                        htmlOutput("breadcrumb_nav")
                    ),
                    
                    fluidRow(
                      column(9,
                             h4("📂 Files & Folders"),
                             div(style = "margin-bottom: 10px;",
                                 actionButton("refresh_s3", "🔄 Refresh", class = "btn-info btn-sm"),
                                 actionButton("jump_to_output", "📁 Jump to Output Folder", 
                                              class = "btn-success btn-sm", 
                                              style = "margin-left: 10px;")
                             ),
                             DT::dataTableOutput("s3_file_list")
                      ),
                      column(3,
                             # Compact file selection area
                             conditionalPanel(
                               condition = "false", # Initially hidden
                               id = "compact_file_info",
                               div(id = "selected_file_info",
                                   style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px; font-size: 0.9em;"
                               )
                             ),
                             # Download section placeholder
                             uiOutput("download_section_display")
                      )
                    ),
                    
                    br(),
                    verbatimTextOutput("s3_processing_log")
                )
              )
      ),
      
      # ========================================================================
      # DASHBOARD TAB (same as before)
      # ========================================================================
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_return_box", width = 3),
                valueBoxOutput("sharpe_box", width = 3),
                valueBoxOutput("sortino_box", width = 3),
                valueBoxOutput("max_dd_box", width = 3)
              ),
              
              fluidRow(
                box(width = 8, status = "primary", solidHeader = TRUE, 
                    title = "📊 Performance Metrics",
                    DT::dataTableOutput("performance_table")
                ),
                box(width = 4, status = "success", solidHeader = TRUE, 
                    title = "🎯 Strategy Grade",
                    htmlOutput("strategy_grade")
                )
              )
      ),
      
      # ========================================================================
      # CHARTS TAB (same as before)
      # ========================================================================
      tabItem(tabName = "charts",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE, 
                    title = "📈 Performance Visualizations",
                    tabsetPanel(
                      tabPanel("📊 Cumulative Returns", 
                               plotlyOutput("cumulative_plot", height = "500px")),
                      tabPanel("🌊 Drawdowns", 
                               plotlyOutput("drawdown_plot", height = "500px")),
                      tabPanel("📅 Monthly Returns", 
                               div(style = "padding: 15px;",
                                   h4("Monthly & Annual Returns by Year"),
                                   DT::dataTableOutput("monthly_returns_table", height = "600px")
                               )),
                      tabPanel("📈 Return Distribution", 
                               plotlyOutput("return_dist_plot", height = "500px"))
                    )
                )
              )
      ),
      
      # ========================================================================
      # VALIDATION TAB (same as before)
      # ========================================================================
      tabItem(tabName = "validation",
              fluidRow(
                box(width = 12, status = "warning", solidHeader = TRUE, 
                    title = "🔍 Function Validation",
                    actionButton("run_validation", "🔍 Validate Functions", class = "btn-warning"),
                    br(), br(),
                    verbatimTextOutput("validation_output")
                )
              )
      )
    )
  )
)

# ============================================================================
# ENHANCED SERVER WITH S3 INTEGRATION
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    aws_configured = FALSE,
    current_s3_path = "mace/output-file/",  # Start in your output folder
    s3_objects = NULL,
    selected_file = NULL,
    processed_data = NULL,
    analysis_results = NULL,
    processing_log = "Configure AWS credentials to begin..."
  )
  
  # Initialize connection status indicator
  output$connection_status_indicator <- renderUI({
    if (values$aws_configured) {
      div(style = "background: #27ae60; padding: 10px; border-radius: 6px; border-left: 4px solid #2ecc71;",
          div(style = "color: white; font-size: 0.85em; font-weight: 500; margin-bottom: 3px;",
              "Connection Status"),
          div(style = "color: #ecf0f1; font-size: 0.75em;",
              "✅ Connected")
      )
    } else {
      div(style = "background: #34495e; padding: 10px; border-radius: 6px; border-left: 4px solid #7f8c8d;",
          div(style = "color: #ecf0f1; font-size: 0.85em; font-weight: 500; margin-bottom: 3px;",
              "Connection Status"),
          div(style = "color: #bdc3c7; font-size: 0.75em;",
              "⚪ Ready to Connect")
      )
    }
  })
  
  # ========================================================================
  # S3 SETUP FUNCTIONS
  # ========================================================================
  
  # Auto-load credentials on app start
  observe({
    auto_creds <- auto_load_aws_credentials()
    
    if (!is.null(auto_creds)) {
      # Pre-populate the form
      updateTextInput(session, "aws_access_key", value = auto_creds$access_key)
      updateTextInput(session, "aws_secret_key", value = auto_creds$secret_key)
      updateSelectInput(session, "aws_region", selected = auto_creds$region)
      updateTextInput(session, "s3_bucket", value = auto_creds$bucket)
      
      # Auto-test connection if bucket is specified
      if (auto_creds$bucket != "") {
        # Small delay to ensure UI is updated
        shinyjs::delay(500, {
          shinyjs::click("test_connection")
        })
        
        values$processing_log <- paste0(
          "✅ Auto-loaded credentials from ", auto_creds$source, "\n",
          "🔄 Testing connection automatically...\n",
          "📁 Will start in: mace/output-file/"
        )
      } else {
        values$processing_log <- paste0(
          "✅ Auto-loaded credentials from ", auto_creds$source, "\n",
          "⚠️ Please specify S3 bucket name and test connection.\n",
          "📁 Will start in: mace/output-file/"
        )
      }
      
      output$connection_status <- renderText({
        paste0("🤖 Auto-loaded from ", auto_creds$source, "\n",
               "Ready to test connection...")
      })
    }
  })
  
  # Test AWS connection
  observeEvent(input$test_connection, {
    req(input$aws_access_key, input$aws_secret_key, input$s3_bucket)
    
    region <- if (input$aws_region == "custom") input$custom_region else input$aws_region
    
    withProgress(message = "Testing AWS connection...", value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "Configuring credentials...")
        
        # Configure AWS
        configure_aws(input$aws_access_key, input$aws_secret_key, region)
        
        incProgress(0.6, detail = "Testing bucket access...")
        
        # Test bucket access
        test_objects <- list_s3_objects(input$s3_bucket, "", max_keys = 1)
        
        incProgress(1.0, detail = "Success!")
        
        values$aws_configured <- TRUE
        values$current_s3_path <- "mace/output-file/"  # Start in output folder
        
        # Update sidebar status indicator
        output$connection_status_indicator <- renderUI({
          div(style = "background: #27ae60; padding: 10px; border-radius: 6px; border-left: 4px solid #2ecc71;",
              div(style = "color: white; font-size: 0.85em; font-weight: 500; margin-bottom: 3px;",
                  "Connection Status"),
              div(style = "color: #ecf0f1; font-size: 0.75em;",
                  paste("✅ Connected to", input$s3_bucket))
          )
        })
        
        output$connection_status <- renderText({
          paste("✅ Connection successful!\n",
                "Bucket:", input$s3_bucket, "\n",
                "Region:", region, "\n",
                "Starting path: mace/output-file/\n",
                "Ready to browse files.")
        })
        
        # Auto-switch to browse tab
        updateTabItems(session, "tabs", "s3_browse")
        
      }, error = function(e) {
        values$aws_configured <- FALSE
        
        # Update sidebar status indicator
        output$connection_status_indicator <- renderUI({
          div(style = "background: #e74c3c; padding: 10px; border-radius: 6px; border-left: 4px solid #c0392b;",
              div(style = "color: white; font-size: 0.85em; font-weight: 500; margin-bottom: 3px;",
                  "Connection Status"),
              div(style = "color: #ecf0f1; font-size: 0.75em;",
                  "❌ Connection Failed")
          )
        })
        
        output$connection_status <- renderText({
          paste("❌ Connection failed:\n", e$message, "\n\n",
                "Please check your credentials and bucket name.")
        })
      })
    })
  })
  
  # ========================================================================
  # S3 BROWSE FUNCTIONS  
  # ========================================================================
  
  # Load S3 objects for current path
  load_s3_objects <- reactive({
    req(values$aws_configured)
    req(input$s3_bucket)
    
    tryCatch({
      cat("Loading S3 objects from path:", values$current_s3_path, "\n")
      objects <- list_s3_objects(input$s3_bucket, values$current_s3_path)
      cat("Found", nrow(objects), "objects\n")
      values$s3_objects <- objects
      return(objects)
    }, error = function(e) {
      cat("Error loading S3 objects:", e$message, "\n")
      values$processing_log <- paste("Error loading S3 objects:", e$message)
      return(data.frame(
        name = paste("Error:", e$message),
        display_name = "Error loading files",
        type = "error", 
        size = NA,
        last_modified = NA,
        stringsAsFactors = FALSE
      ))
    })
  })
  
  # Refresh S3 listing
  observeEvent(input$refresh_s3, {
    if (values$aws_configured) {
      load_s3_objects()
      values$processing_log <- "S3 listing refreshed."
    }
  })
  
  # Auto-load when tab is accessed
  observeEvent(input$tabs, {
    if (input$tabs == "s3_browse" && values$aws_configured) {
      load_s3_objects()
    }
  })
  
  # Breadcrumb navigation
  output$breadcrumb_nav <- renderUI({
    if (!values$aws_configured) {
      return(p("Configure AWS credentials first"))
    }
    
    # Build breadcrumb
    bucket_name <- input$s3_bucket
    current_path <- values$current_s3_path
    
    # Split path into parts
    if (current_path == "" || current_path == "/") {
      path_parts <- character(0)
    } else {
      path_parts <- strsplit(gsub("/$", "", current_path), "/")[[1]]
    }
    
    # Build breadcrumb HTML
    crumbs <- list()
    
    # Root bucket
    crumbs <- append(crumbs, 
                     list(tags$a(href = "#", onclick = "Shiny.setInputValue('navigate_to', '', {priority: 'event'});",
                                 paste("🪣", bucket_name))))
    
    # Path parts
    if (length(path_parts) > 0) {
      for (i in 1:length(path_parts)) {
        path_to_here <- paste(path_parts[1:i], collapse = "/") 
        if (path_to_here != "") path_to_here <- paste0(path_to_here, "/")
        
        if (i == length(path_parts)) {
          # Current folder (no link)
          crumbs <- append(crumbs, list(span(paste("📁", path_parts[i]))))
        } else {
          # Clickable path
          crumbs <- append(crumbs, 
                           list(tags$a(href = "#", 
                                       onclick = paste0("Shiny.setInputValue('navigate_to', '", path_to_here, "', {priority: 'event'});"),
                                       paste("📁", path_parts[i]))))
        }
      }
    }
    
    # Join with separators
    breadcrumb_html <- list()
    for (i in 1:length(crumbs)) {
      breadcrumb_html <- append(breadcrumb_html, crumbs[i])
      if (i < length(crumbs)) {
        breadcrumb_html <- append(breadcrumb_html, list(span(" > ")))
      }
    }
    
    return(div(breadcrumb_html))
  })
  
  # Handle breadcrumb navigation
  observeEvent(input$navigate_to, {
    values$current_s3_path <- input$navigate_to
    load_s3_objects()
    values$selected_file <- NULL
  })
  
  # Quick navigation shortcut - Add a button to jump to output folder
  observeEvent(input$jump_to_output, {
    values$current_s3_path <- "mace/output-file/"
    load_s3_objects()
    values$selected_file <- NULL
  }, ignoreInit = TRUE)
  
  # S3 file list table
  output$s3_file_list <- DT::renderDataTable({
    objects <- load_s3_objects()
    
    if (is.null(objects) || nrow(objects) == 0) {
      empty_df <- data.frame(
        Name = "No files found in this folder",
        Size = "",
        Modified = "",
        stringsAsFactors = FALSE
      )
      return(empty_df)
    }
    
    # Prepare display data with simpler logic
    display_data <- tryCatch({
      # Create clean display data
      clean_data <- data.frame(
        Name = character(nrow(objects)),
        Size = character(nrow(objects)),
        Modified = character(nrow(objects)),
        row_type = objects$type,
        stringsAsFactors = FALSE
      )
      
      # Process each row individually
      for (i in 1:nrow(objects)) {
        obj <- objects[i, ]
        
        # Icon and name
        icon <- if (obj$type == "folder") "📁" else "📄"
        clean_data$Name[i] <- paste(icon, obj$display_name)
        
        # Size
        if (obj$type == "folder") {
          clean_data$Size[i] <- ""
        } else if (is.na(obj$size) || is.null(obj$size)) {
          clean_data$Size[i] <- ""
        } else {
          size_num <- as.numeric(obj$size)
          if (is.na(size_num)) {
            clean_data$Size[i] <- ""
          } else if (size_num < 1024) {
            clean_data$Size[i] <- paste(size_num, "B")
          } else if (size_num < 1024^2) {
            clean_data$Size[i] <- paste(round(size_num / 1024, 1), "KB")
          } else if (size_num < 1024^3) {
            clean_data$Size[i] <- paste(round(size_num / 1024^2, 1), "MB")
          } else {
            clean_data$Size[i] <- paste(round(size_num / 1024^3, 1), "GB")
          }
        }
        
        # Modified date
        if (obj$type == "folder" || is.na(obj$last_modified) || obj$last_modified == "") {
          clean_data$Modified[i] <- ""
        } else {
          tryCatch({
            parsed_date <- as.POSIXct(obj$last_modified)
            if (is.na(parsed_date)) {
              clean_data$Modified[i] <- obj$last_modified
            } else {
              clean_data$Modified[i] <- format(parsed_date, "%Y-%m-%d %H:%M")
            }
          }, error = function(e) {
            clean_data$Modified[i] <- obj$last_modified
          })
        }
      }
      
      # Return just the display columns
      clean_data[, c("Name", "Size", "Modified")]
      
    }, error = function(e) {
      error_df <- data.frame(
        Name = paste("❌ Error:", e$message),
        Size = "",
        Modified = "",
        stringsAsFactors = FALSE
      )
      return(error_df)
    })
    
    return(display_data)
    
  }, options = list(
    pageLength = 20,
    dom = 't',
    scrollX = TRUE,
    columnDefs = list(
      list(className = 'dt-center', targets = c(1, 2))
    )
  ), selection = 'single', escape = FALSE)
  
  # Handle file/folder selection
  observeEvent(input$s3_file_list_rows_selected, {
    req(input$s3_file_list_rows_selected)
    
    selected_row <- input$s3_file_list_rows_selected
    objects <- values$s3_objects
    
    if (!is.null(objects) && selected_row <= nrow(objects)) {
      selected_item <- objects[selected_row, ]
      
      cat("Selected item:", selected_item$display_name, "Type:", selected_item$type, "\n")
      
      if (selected_item$type == "folder") {
        # Navigate into folder
        new_path <- selected_item$name
        values$current_s3_path <- new_path
        load_s3_objects()
        values$selected_file <- NULL
        
        values$processing_log <- paste("📁 Navigated to:", new_path)
        
        # Hide download section
        output$download_section_display <- renderUI({
          # Return empty div when folder is selected
          div()
        })
        
      } else {
        # Select file
        values$selected_file <- selected_item
        
        cat("File selected for download:", selected_item$name, "\n")
        
        # Update file info display - Much more compact
        output$selected_file_info <- renderUI({
          file_size_display <- if (is.na(selected_item$size)) {
            "Unknown"
          } else {
            size_num <- as.numeric(selected_item$size)
            if (size_num < 1024^2) {
              paste(round(size_num / 1024, 0), "KB")
            } else if (size_num < 1024^3) {
              paste(round(size_num / 1024^2, 1), "MB")
            } else {
              paste(round(size_num / 1024^3, 1), "GB")
            }
          }
          
          div(
            style = "background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); 
                     color: white; padding: 12px; border-radius: 6px; margin-bottom: 15px;
                     box-shadow: 0 2px 8px rgba(0,123,255,0.3);",
            div(style = "font-weight: bold; font-size: 0.95em; margin-bottom: 6px;",
                "📄 ", substr(selected_item$display_name, 1, 30),
                if(nchar(selected_item$display_name) > 30) "..." else ""),
            div(style = "font-size: 0.8em; opacity: 0.9;",
                "💾 ", file_size_display, " • ✅ Ready")
          )
        })
        
        # Show download section - More compact with responsive button
        output$download_section_display <- renderUI({
          div(
            actionButton("download_and_process", 
                         HTML("<i class='fa fa-download'></i><br><span style='font-size: 0.85em;'>Download &<br>Analyze</span>"), 
                         class = "btn-success", 
                         style = "width: 100%; height: 60px; margin-bottom: 12px; font-weight: bold; 
                                  padding: 8px 4px; line-height: 1.2; white-space: normal;"),
            div(
              style = "background: #f8f9fa; padding: 8px; border-radius: 4px; border: 1px solid #dee2e6;",
              checkboxInput("rescale_volatility_s3", 
                            HTML("<span style='font-size: 0.85em; font-weight: 500;'>
                                 <i class='fa fa-target'></i> 10% Vol</span>"), 
                            value = FALSE,
                            width = "100%")
            )
          )
        })
        
        values$processing_log <- paste("📄 Selected file:", selected_item$display_name)
      }
    }
  })
  
  # ========================================================================
  # FILE PROCESSING
  # ========================================================================
  
  # Download and process selected S3 file
  observeEvent(input$download_and_process, {
    req(values$selected_file)
    
    selected_file <- values$selected_file
    
    withProgress(message = "Processing S3 file...", value = 0, {
      
      tryCatch({
        incProgress(0.1, detail = "Downloading from S3...")
        values$processing_log <- paste("🔄 Downloading:", selected_file$display_name)
        
        # Download file
        temp_file_path <- download_s3_file(input$s3_bucket, selected_file$name)
        values$processing_log <- paste(values$processing_log, "\n✅ Downloaded successfully")
        
        incProgress(0.3, detail = "Reading file...")
        
        # Read the file based on extension
        file_ext <- tools::file_ext(selected_file$display_name)
        
        if (file_ext == "parquet") {
          data <- arrow::read_parquet(temp_file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(temp_file_path, show_col_types = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(temp_file_path)
        } else {
          stop("Unsupported file format: ", file_ext)
        }
        
        values$processing_log <- paste(values$processing_log, "\n📊 File loaded:", nrow(data), "rows,", ncol(data), "columns")
        
        incProgress(0.5, detail = "Processing data structure...")
        
        # Process the data using the helper function
        processed_data <- process_portfolio_data(data, input$rescale_volatility_s3)
        values$processed_data <- processed_data
        
        values$processing_log <- paste(values$processing_log, "\n🔧 Data processed successfully")
        
        incProgress(0.8, detail = "Running portfolio analysis...")
        
        # Run analysis
        strategy_name <- paste0(tools::file_path_sans_ext(selected_file$display_name),
                                if (input$rescale_volatility_s3) " (10% Vol)" else "")
        
        analysis <- hf_grade_analysis(processed_data, fund_name = strategy_name)
        values$analysis_results <- analysis
        
        incProgress(1.0, detail = "Complete!")
        
        values$processing_log <- paste(values$processing_log, "\n✅ Analysis complete! Check Dashboard and Charts tabs.")
        
        # Clean up temp file
        unlink(temp_file_path)
        
        # Auto-switch to dashboard
        updateTabItems(session, "tabs", "dashboard")
        
        # Show success message
        showNotification(
          paste("Successfully analyzed:", selected_file$display_name),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        values$processing_log <- paste(values$processing_log, "\n❌ Error processing file:", e$message)
        showNotification(
          paste("Error processing file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
  
  # Helper function to process portfolio data (extracted from original logic)
  process_portfolio_data <- function(data, rescale_vol = FALSE) {
    
    # [Reuse the data processing logic from the original app]
    # This includes detecting column formats, calculating returns, adding SPY benchmark, etc.
    
    # Data structure detection logic
    if ("ticker" %in% names(data) && "date" %in% names(data) && ncol(data) >= 3) {
      
      # Remove any index columns
      index_cols <- grep("^__index|^X\\.|^\\.\\.\\.", names(data), value = TRUE)
      if (length(index_cols) > 0) {
        data <- data %>% select(-all_of(index_cols))
      }
      
      # Standard format handling
      if (all(c("ticker", "daily_return", "date") %in% names(data))) {
        standard_cols <- c("ticker", "daily_return", "date")
        weight_col_candidates <- setdiff(names(data), standard_cols)
        
        if (length(weight_col_candidates) >= 1) {
          weight_col_name <- weight_col_candidates[1]
          data$weights <- data[[weight_col_name]]
          data <- data %>% select(-all_of(weight_col_name))
        }
        
      } else {
        # Alternative format mapping
        # [Include the column mapping logic from original app]
        stop("Alternative format handling not implemented in this example")
      }
      
      # Convert date
      data$date <- as.Date(data$date)
      
      # Get SPY benchmark
      min_date <- min(data$date, na.rm = TRUE) - 1
      max_date <- max(data$date, na.rm = TRUE)
      
      spy <- tq_get("SPY", from = min_date, to = max_date) %>% 
        tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                     period = "daily", col_rename = "Rb") %>% 
        filter(row_number() > 1)
      
      # Calculate portfolio returns
      processed_data <- data %>% 
        group_by(date) %>% 
        summarise(Ra = sum(weights * daily_return, na.rm = TRUE), .groups = 'drop') %>% 
        arrange(desc(date)) %>% 
        mutate(date = as.Date(date))
      
      # Volatility rescaling
      if (rescale_vol) {
        current_vol <- sd(processed_data$Ra, na.rm = TRUE) * sqrt(252)
        target_vol <- 0.10
        
        if (current_vol > 0) {
          scaling_factor <- target_vol / current_vol
          processed_data$Ra <- processed_data$Ra * scaling_factor
        }
      }
      
      # Add SPY benchmark
      processed_data <- processed_data %>% 
        left_join(spy, by = "date") %>%
        arrange(date)
      
      return(processed_data)
      
    } else {
      stop("Data format not recognized. Expected columns: ticker, daily_return, date, [weights]")
    }
  }
  
  # ========================================================================
  # OUTPUT FUNCTIONS (reuse from original app)
  # ========================================================================
  
  output$s3_processing_log <- renderText({
    values$processing_log
  })
  
  # [Include all the output functions from the original app: value boxes, tables, charts, etc.]
  # Value boxes
  output$total_return_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      total_ret <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Total Return"]
      valueBox(value = total_ret, subtitle = "Total Return", icon = icon("arrow-up"), color = "green")
    } else {
      valueBox(value = "Load S3 Data", subtitle = "Total Return", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  output$sharpe_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      sharpe <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Sharpe Ratio"]
      color <- if (as.numeric(sharpe) > 1) "blue" else "yellow"
      valueBox(value = sharpe, subtitle = "Sharpe Ratio", icon = icon("chart-line"), color = color)
    } else {
      valueBox(value = "Load S3 Data", subtitle = "Sharpe Ratio", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  output$sortino_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      sortino <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Sortino Ratio"]
      valueBox(value = sortino, subtitle = "Sortino Ratio (Fixed)", icon = icon("shield-alt"), color = "purple")
    } else {
      valueBox(value = "Load S3 Data", subtitle = "Sortino Ratio", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  output$max_dd_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      max_dd <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Max Drawdown"]
      dd_numeric <- as.numeric(gsub("%", "", max_dd))
      color <- if (dd_numeric < 10) "green" else if (dd_numeric < 20) "yellow" else "red"
      valueBox(value = max_dd, subtitle = "Max Drawdown", icon = icon("arrow-down"), color = color)
    } else {
      valueBox(value = "Load S3 Data", subtitle = "Max Drawdown", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  # Performance table
  output$performance_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      values$analysis_results$summary_stats
    }
  }, options = list(pageLength = 25, dom = 'ft'))
  
  # Strategy grade
  output$strategy_grade <- renderUI({
    if (!is.null(values$analysis_results)) {
      grade_result <- grade_strategy(values$analysis_results)
      
      grade_color <- case_when(
        grade_result$grade %in% c("A+", "A") ~ "success",
        grade_result$grade %in% c("A-", "B+", "B") ~ "primary", 
        grade_result$grade %in% c("B-", "C+", "C") ~ "warning",
        TRUE ~ "danger"
      )
      
      div(
        div(class = paste("alert alert-", grade_color),
            h3(paste("Grade:", grade_result$grade)),
            p(paste("Score:", grade_result$points, "/100"))
        )
      )
    } else {
      div(class = "alert alert-info", 
          h4("Load S3 data to see strategy grade"))
    }
  })
  
  # Charts
  output$cumulative_plot <- renderPlotly({
    if (!is.null(values$analysis_results)) {
      ggplotly(values$analysis_results$plots$cumulative_returns)
    }
  })
  
  output$drawdown_plot <- renderPlotly({
    if (!is.null(values$analysis_results)) {
      ggplotly(values$analysis_results$plots$drawdowns)
    }
  })
  
  # Monthly returns table
  output$monthly_returns_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      
      # Get the raw portfolio returns (xts object)
      portfolio_returns <- values$analysis_results$raw_data$portfolio_returns
      
      if (!is.null(portfolio_returns)) {
        # Convert xts to data frame with proper date handling
        returns_df <- data.frame(
          date = index(portfolio_returns),
          returns = as.numeric(portfolio_returns[,1])
        ) %>%
          filter(!is.na(returns), !is.na(date)) %>%
          mutate(
            year = year(date),
            month = month(date),
            year_month = paste(year, sprintf("%02d", month), sep = "-")
          )
        
        cat("Debug - Returns data sample:\n")
        cat("Rows:", nrow(returns_df), "\n")
        cat("Date range:", as.character(range(returns_df$date)), "\n")
        cat("Sample returns:", head(returns_df$returns, 5), "\n")
        
        # Calculate monthly returns by compounding daily returns within each month
        monthly_returns_data <- returns_df %>%
          group_by(year, month) %>%
          summarise(
            monthly_ret = prod(1 + returns, na.rm = TRUE) - 1,
            days_in_month = n(),
            start_date = min(date),
            end_date = max(date),
            .groups = 'drop'
          ) %>%
          filter(!is.na(monthly_ret), !is.infinite(monthly_ret))
        
        cat("Debug - Monthly returns sample:\n")
        print(head(monthly_returns_data))
        
        if (nrow(monthly_returns_data) == 0) {
          return(DT::datatable(
            data.frame(Message = "No valid monthly returns calculated from daily data"),
            options = list(dom = 't'), rownames = FALSE
          ))
        }
        
        # Create the monthly grid with proper pivot logic
        monthly_grid <- monthly_returns_data %>%
          # First, ensure we have all year-month combinations
          complete(year, month = 1:12, fill = list(monthly_ret = NA)) %>%
          # Create month names
          mutate(month_name = month.abb[month]) %>%
          # Select only what we need for the pivot
          select(year, month_name, monthly_ret) %>%
          # Pivot to wide format
          pivot_wider(
            names_from = month_name,
            values_from = monthly_ret,
            values_fill = NA
          ) %>%
          # Ensure proper column order
          select(year, all_of(month.abb[month.abb %in% names(.)])) %>%
          arrange(year)
        
        # Add any missing month columns as NA
        for (month_col in month.abb) {
          if (!month_col %in% names(monthly_grid)) {
            monthly_grid[[month_col]] <- NA
          }
        }
        
        # Reorder columns to ensure Jan-Dec order
        monthly_grid <- monthly_grid %>%
          select(year, all_of(month.abb))
        
        # Calculate annual returns for each year (only from months with data)
        annual_returns <- monthly_returns_data %>%
          group_by(year) %>%
          summarise(
            annual_return = prod(1 + monthly_ret, na.rm = TRUE) - 1,
            months_with_data = n(),
            .groups = 'drop'
          )
        
        cat("Debug - Monthly grid after pivot:\n")
        print(head(monthly_grid, 3))
        cat("Debug - Annual returns:\n")
        print(annual_returns)
        
        # Add annual column to the monthly grid
        monthly_table <- monthly_grid %>%
          left_join(annual_returns %>% select(year, annual_return), by = "year") %>%
          rename(Year = year, Annual = annual_return)
        
        cat("Debug - Final table before formatting:\n")
        print(head(monthly_table, 3))
        cat("Debug - Table dimensions:", nrow(monthly_table), "x", ncol(monthly_table), "\n")
        
        # Format the table for professional display
        formatted_table <- monthly_table %>%
          mutate(
            across(all_of(month.abb), ~ case_when(
              is.na(.x) ~ "",
              abs(.x) > 10 ~ sprintf("%.0f%%", .x * 100),
              TRUE ~ sprintf("%.1f%%", .x * 100)
            )),
            Annual = case_when(
              is.na(Annual) ~ "",
              abs(Annual) > 10 ~ sprintf("%.0f%%", Annual * 100),
              TRUE ~ sprintf("%.1f%%", Annual * 100)
            )
          )
        
        cat("Debug - Sample formatted data:\n")
        print(head(formatted_table, 3))
        
        DT::datatable(
          formatted_table,
          options = list(
            pageLength = 15,
            dom = 't',
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = 0, width = "60px"),
              list(className = 'dt-center', targets = 1:12, width = "70px"),
              list(className = 'dt-center', targets = 13, width = "80px")
            )
          ),
          rownames = FALSE,
          escape = FALSE
        ) %>%
          DT::formatStyle(
            columns = 2:13,
            backgroundColor = DT::styleEqual(c(""), c("#f8f9fa")),
            color = DT::styleInterval(
              cuts = c(-5, 0, 5),
              values = c("#dc3545", "#6c757d", "#28a745", "#155724")
            )
          ) %>%
          DT::formatStyle(
            columns = 14,
            backgroundColor = "#e3f2fd",
            fontWeight = "bold"
          )
        
      } else {
        DT::datatable(
          data.frame(Message = "No portfolio returns data available"),
          options = list(dom = 't'), rownames = FALSE
        )
      }
    } else {
      DT::datatable(
        data.frame(Message = "No analysis results available. Please process a file first."),
        options = list(dom = 't'), rownames = FALSE
      )
    }
  })
  
  output$return_dist_plot <- renderPlotly({
    if (!is.null(values$analysis_results)) {
      ggplotly(values$analysis_results$plots$return_distribution)
    }
  })
  
  # Validation
  observeEvent(input$run_validation, {
    if (!is.null(values$processed_data)) {
      validation_results <- validate_pa_functions(values$processed_data)
      
      valid_count <- sum(sapply(validation_results, function(x) x$valid))
      total_count <- length(validation_results)
      
      broken_functions <- names(validation_results)[!sapply(validation_results, function(x) x$valid)]
      working_functions <- names(validation_results)[sapply(validation_results, function(x) x$valid)]
      
      output$validation_output <- renderText({
        result_text <- paste0(
          "VALIDATION RESULTS:\n",
          "Functions Tested: ", total_count, "\n",
          "Valid Functions: ", valid_count, "\n",
          "Success Rate: ", round(valid_count/total_count * 100, 1), "%\n\n"
        )
        
        if (length(broken_functions) > 0) {
          result_text <- paste0(result_text, 
                                "BROKEN FUNCTIONS:\n",
                                paste("  ❌", broken_functions, collapse = "\n"), "\n\n")
        }
        
        if (length(working_functions) > 0) {
          result_text <- paste0(result_text,
                                "WORKING FUNCTIONS:\n",
                                paste("  ✅", working_functions, collapse = "\n"), "\n\n")
        }
        
        if ("sortino" %in% broken_functions) {
          result_text <- paste0(result_text,
                                "NOTE: SortinoRatio() is broken in PerformanceAnalytics\n",
                                "This app uses a corrected manual calculation instead.\n",
                                "Your Sortino ratio is properly calculated and reliable.")
        }
        
        result_text
      })
    } else {
      output$validation_output <- renderText("Load and process S3 data first!")
    }
  })
}

# ============================================================================
# ENHANCED PROCESS PORTFOLIO DATA FUNCTION
# ============================================================================

# Complete implementation of the portfolio data processing function
process_portfolio_data <- function(data, rescale_vol = FALSE) {
  
  # Check if this is raw portfolio data
  if ("ticker" %in% names(data) && "date" %in% names(data) && ncol(data) >= 3) {
    
    # Remove any index columns that might have been added during export
    index_cols <- grep("^__index|^X\\.|^\\.\\.\\.|^Unnamed", names(data), value = TRUE)
    if (length(index_cols) > 0) {
      data <- data %>% select(-all_of(index_cols))
      cat("🧹 Removed index columns:", paste(index_cols, collapse = ", "), "\n")
    }
    
    # CASE 1: Standard format (ticker, daily_return, date, [weights_column])
    if (all(c("ticker", "daily_return", "date") %in% names(data))) {
      
      cat("✅ Standard format detected\n")
      
      # Find the weights column (any column that's not the standard 3)
      standard_cols <- c("ticker", "daily_return", "date")
      weight_col_candidates <- setdiff(names(data), standard_cols)
      
      if (length(weight_col_candidates) >= 1) {
        weight_col_name <- weight_col_candidates[1]
        data$weights <- data[[weight_col_name]]
        data <- data %>% select(-all_of(weight_col_name))  # Remove original
        cat("🤖 Renamed", sprintf("'%s' → weights", weight_col_name), "\n")
      } else {
        stop("No weights column found")
      }
      
    } else {
      # CASE 2: Alternative format - need to map columns
      cat("🔄 Alternative format - mapping columns...\n")
      
      # Look for return column
      return_col <- NULL
      return_candidates <- c("daily_return", "return", "returns", "ret", "daily_ret")
      for (col in return_candidates) {
        if (col %in% names(data)) {
          return_col <- col
          break
        }
      }
      
      # Look for weight column
      weight_col <- NULL  
      weight_candidates <- c("weight", "weights", "allocation", "position", "portfolio_weight")
      for (col in weight_candidates) {
        if (col %in% names(data)) {
          weight_col <- col
          break
        }
      }
      
      # If still missing, try to infer from numeric columns
      if (is.null(return_col) || is.null(weight_col)) {
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        available_numeric <- setdiff(numeric_cols, c("date", return_col, weight_col))
        
        cat("🔢 Numeric columns detected:", paste(numeric_cols, collapse = ", "), "\n")
        
        # Force convert weight column to numeric if it exists but isn't numeric
        if ("weight" %in% names(data) && !is.numeric(data$weight)) {
          data$weight <- as.numeric(data$weight)
          if (is.null(weight_col)) weight_col <- "weight"
        }
        
        if (is.null(return_col) && length(available_numeric) > 0) {
          # Look for small values (likely returns)
          for (col in available_numeric) {
            sample_vals <- data[[col]][!is.na(data[[col]])]
            if (length(sample_vals) > 10 && max(abs(sample_vals), na.rm = TRUE) <= 1) {
              return_col <- col
              available_numeric <- setdiff(available_numeric, col)
              break
            }
          }
          
          if (is.null(return_col)) {
            stop("Could not find returns column. File appears to contain aggregated data.")
          }
        }
        
        if (is.null(weight_col) && length(available_numeric) > 0) {
          weight_col <- available_numeric[1]  # Take remaining column
        }
      }
      
      # Apply mappings
      if (!is.null(return_col) && !is.null(weight_col)) {
        if (return_col != "daily_return") {
          data$daily_return <- data[[return_col]]
          data <- data %>% select(-all_of(return_col))
          cat("🤖 Mapped", sprintf("'%s' → daily_return", return_col), "\n")
        }
        
        if (weight_col != "weights") {
          data$weights <- data[[weight_col]]
          data <- data %>% select(-all_of(weight_col))
          cat("🤖 Mapped", sprintf("'%s' → weights", weight_col), "\n")
        }
      } else {
        stop(paste("Missing required columns. Need returns and weights columns.",
                   "Found:", paste(names(data), collapse = ", "),
                   "Detected return col:", ifelse(is.null(return_col), "NONE", return_col),
                   "Detected weight col:", ifelse(is.null(weight_col), "NONE", weight_col)))
      }
    }
    
    # Convert date to proper format
    cat("📅 Converting dates...\n")
    data$date <- as.Date(data$date)
    
    # Get date range from data and fetch SPY
    min_date <- min(data$date, na.rm = TRUE) - 1  # One day before for return calculation
    max_date <- max(data$date, na.rm = TRUE)
    
    cat("📈 Fetching SPY benchmark from", as.character(min_date), "to", as.character(max_date), "\n")
    
    spy <- tq_get("SPY", from = min_date, to = max_date) %>% 
      tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                   period = "daily", col_rename = "Rb") %>% 
      filter(row_number() > 1)
    
    cat("📊 Calculating portfolio returns...\n")
    
    # Process like MACE workflow
    processed_data <- data %>% 
      group_by(date) %>% 
      summarise(Ra = sum(weights * daily_return, na.rm = TRUE), .groups = 'drop') %>% 
      arrange(desc(date)) %>% 
      mutate(date = as.Date(date))
    
    cat("💼 Portfolio has", nrow(processed_data), "daily observations\n")
    
    # Volatility rescaling if requested
    if (rescale_vol) {
      cat("🎯 Rescaling to 10% volatility...\n")
      
      # Calculate current annualized volatility
      current_vol <- sd(processed_data$Ra, na.rm = TRUE) * sqrt(252)
      target_vol <- 0.10  # 10% target
      
      if (current_vol > 0) {
        scaling_factor <- target_vol / current_vol
        processed_data$Ra <- processed_data$Ra * scaling_factor
        
        cat(sprintf("📊 Volatility rescaled: %.1f%% → 10.0%% (factor: %.3f)", 
                    current_vol * 100, scaling_factor), "\n")
      }
    }
    
    # Add SPY benchmark
    cat("🔗 Adding SPY benchmark...\n")
    processed_data <- processed_data %>% 
      left_join(spy, by = "date") %>%
      arrange(date)  # Ensure chronological order
    
    cat("✅ Data processing complete!\n")
    return(processed_data)
    
  } else {
    stop("Data format not recognized. Expected columns: ticker, daily_return, date, [weights]")
  }
}

# ============================================================================
# LAUNCH APP
# ============================================================================

cat("🚀 Starting Portfolio Analytics Pro...\n")
cat("✨ Professional Edition Features:\n")
cat("  • Enterprise S3 Integration\n")
cat("  • Advanced Portfolio Analytics\n") 
cat("  • Professional Reporting\n")
cat("  • Real-time Performance Monitoring\n")
shinyApp(ui = ui, server = server)