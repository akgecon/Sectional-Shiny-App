# ============================================================================
# CLOUD-READY PORTFOLIO ANALYTICS SHINY APP
# Optimized for Posit Cloud Connect deployment
# ============================================================================

# Load libraries directly - assume they're already installed in cloud environment
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
  library(readxl)
  # Add shinyjs for delay functionality
  if (requireNamespace("shinyjs", quietly = TRUE)) {
    library(shinyjs)
  }
})

# Cloud deployment: SSL certificates work properly, no need for workarounds
# Local SSL fix only applied when not in cloud environment
if (Sys.getenv("CONNECT_SERVER") == "" && Sys.getenv("SHINY_SERVER_VERSION") == "") {
  # Only apply SSL fix when running locally
  tryCatch({
    httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  }, error = function(e) {
    # Ignore SSL config errors in cloud environments
  })
}

# Source supporting functions if file exists
if (file.exists("supporting_functions.R")) {
  source("supporting_functions.R")
}

# ============================================================================
# CLOUD-OPTIMIZED AWS HELPER FUNCTIONS
# ============================================================================

# Auto-load AWS credentials (Posit Connect Cloud pattern)
auto_load_aws_credentials <- function() {
  # Posit Connect Cloud: Environment variables are available immediately
  # No need for delays - they're injected before app starts
  
  # Check for all required AWS environment variables
  aws_access_key <- Sys.getenv("AWS_ACCESS_KEY_ID", unset = "")
  aws_secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY", unset = "")
  aws_region <- Sys.getenv("AWS_DEFAULT_REGION", unset = "us-east-1")
  aws_bucket <- Sys.getenv("AWS_S3_BUCKET", unset = "")
  
  # Debug: Print what we found (safely)
  cat("Debug - Environment variable check:\n")
  cat("AWS_ACCESS_KEY_ID found:", aws_access_key != "", "\n")
  cat("AWS_SECRET_ACCESS_KEY found:", aws_secret_key != "", "\n") 
  cat("AWS_DEFAULT_REGION:", aws_region, "\n")
  cat("AWS_S3_BUCKET:", aws_bucket, "\n")
  
  # Only return credentials if we have the minimum required
  if (aws_access_key != "" && aws_secret_key != "") {
    cat("✅ Found AWS credentials in environment variables\n")
    return(list(
      access_key = aws_access_key,
      secret_key = aws_secret_key,
      region = aws_region,
      bucket = aws_bucket,
      source = "Posit Connect environment"
    ))
  } else {
    cat("❌ AWS credentials not found in environment variables\n")
    
    # Additional debug: Check if we're in Connect Cloud
    if (Sys.getenv("CONNECT_SERVER", unset = "") != "") {
      cat("Running in Posit Connect, but credentials not available\n")
      cat("Check that secret variables are set in the Connect dashboard\n")
    }
    
    return(NULL)
  }
}

# Configure AWS credentials
configure_aws <- function(access_key, secret_key, region = "us-east-1") {
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = access_key,
    "AWS_SECRET_ACCESS_KEY" = secret_key,
    "AWS_DEFAULT_REGION" = region
  )
}

# List S3 objects with folder structure
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
      relative_keys <- sub(paste0("^", prefix), "", object_info$name)
    } else {
      relative_keys <- object_info$name
    }
    
    # Identify folders and files
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
    
    # Get files at current level
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
# UI DEFINITION
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; font-weight: 600;",
      tags$i(class = "fa fa-chart-line", style = "margin-right: 8px; color: #fff;"),
      "Portfolio Analytics Pro"
    ), 
    titleWidth = 300
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
    
    # Status indicator
    div(style = "position: absolute; bottom: 15px; left: 15px; right: 15px;",
        uiOutput("connection_status_indicator")
    )
  ),
  
  dashboardBody(
    # Enable shinyjs if available for delay functionality
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .main-header .navbar { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important; }
        .main-header .logo { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important; }
        .skin-blue .main-sidebar { background: #2c3e50 !important; }
        .content-wrapper { background: #f8f9fa !important; }
        .box { box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important; border: none !important; }
        .box-header { border-bottom: 2px solid #e9ecef !important; background: #fff !important; }
        .box-title { font-weight: 600 !important; color: #2c3e50 !important; }
        .dataTables_wrapper { background: white; border-radius: 8px; }
        .dataTables_wrapper tbody tr:hover {
          background: linear-gradient(135deg, #e3f2fd 0%, #f3e5f5 100%) !important;
        }
        .btn { font-weight: 500; border-radius: 6px; transition: all 0.3s ease; }
        .btn-primary { 
          background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); 
          border: none; 
          box-shadow: 0 2px 8px rgba(0,123,255,0.3);
        }
        .btn-success { 
          background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%); 
          border: none; 
          box-shadow: 0 2px 8px rgba(40,167,69,0.3);
        }
        .small-box { 
          border-radius: 12px !important; 
          box-shadow: 0 4px 16px rgba(0,0,0,0.1) !important;
          transition: all 0.3s ease !important;
        }
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
      # S3 Setup Tab
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
                               
                               # Posit Connect Cloud notice - updated
                               div(
                                 style = "background: #d1ecf1; padding: 10px; border-radius: 4px; margin-bottom: 15px; border-left: 4px solid #17a2b8;",
                                 div(style = "font-size: 0.9em; color: #0c5460;",
                                     strong("🌐 Posit Connect Cloud"), br(),
                                     "Set these as secret variables in your app settings:", br(),
                                     tags$code("AWS_ACCESS_KEY_ID"), ", ",
                                     tags$code("AWS_SECRET_ACCESS_KEY"), ", ",
                                     tags$code("AWS_S3_BUCKET"), ", ",
                                     tags$code("AWS_DEFAULT_REGION"))
                               ),
                               
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
                                             "Asia Pacific (Singapore)" = "ap-southeast-1"
                                           ),
                                           selected = "us-east-1"),
                               textInput("s3_bucket", 
                                         label = div(style = "font-weight: 500;", "S3 Bucket Name:"),
                                         placeholder = "experiments-mace-wrds"),
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
                                   h5("AWS Credentials:", style = "color: #0c5460; margin-top: 15px;"),
                                   tags$ul(
                                     tags$li("Access AWS IAM Console"),
                                     tags$li("Create user with S3 permissions"),
                                     tags$li("Generate Access Key pair")
                                   ),
                                   
                                   h5("Required S3 Permissions:", style = "color: #0c5460; margin-top: 15px;"),
                                   div(style = "background: white; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.85em;",
                                       "s3:ListBucket<br>s3:GetObject<br>s3:GetObjectVersion"),
                                   
                                   h5("Environment Variables:", style = "color: #0c5460; margin-top: 15px;"),
                                   div(style = "background: white; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 0.85em;",
                                       "AWS_ACCESS_KEY_ID<br>AWS_SECRET_ACCESS_KEY<br>AWS_S3_BUCKET")
                               )
                             )
                      )
                    )
                )
              )
      ),
      
      # S3 Browse Tab
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
                             div(id = "selected_file_info"),
                             uiOutput("download_section_display")
                      )
                    ),
                    
                    br(),
                    verbatimTextOutput("s3_processing_log")
                )
              )
      ),
      
      # Dashboard Tab
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
      
      # Charts Tab
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
      
      # Validation Tab
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
# SERVER DEFINITION
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    aws_configured = FALSE,
    current_s3_path = "mace/output-file/",
    s3_objects = NULL,
    selected_file = NULL,
    processed_data = NULL,
    analysis_results = NULL,
    processing_log = "Configure AWS credentials to begin..."
  )
  
  # Initialize connection status
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
  
  # Auto-load credentials - simplified approach like Posit Connect example
  observe({
    cat("=== AUTO-LOADING CREDENTIALS ===\n")
    
    auto_creds <- auto_load_aws_credentials()
    
    if (!is.null(auto_creds)) {
      cat("Successfully loaded credentials from:", auto_creds$source, "\n")
      
      # Update form fields
      updateTextInput(session, "aws_access_key", value = auto_creds$access_key)
      updateTextInput(session, "aws_secret_key", value = auto_creds$secret_key)
      updateSelectInput(session, "aws_region", selected = auto_creds$region)
      updateTextInput(session, "s3_bucket", value = auto_creds$bucket)
      
      # Update processing log with plain text (no unicode)
      values$processing_log <- paste0(
        "SUCCESS: Auto-loaded from ", auto_creds$source, "\n",
        "Access Key: ", substr(auto_creds$access_key, 1, 8), "...\n",
        "Region: ", auto_creds$region, "\n",
        "Bucket: ", auto_creds$bucket, "\n\n",
        if (auto_creds$bucket != "") "READY: Click Test Connection!" else "WARNING: Set bucket name and test connection"
      )
      
      # Update connection status (plain text)
      output$connection_status <- renderText({
        paste0("AUTO-LOADED: Credentials from ", auto_creds$source, "\n",
               "Access Key: ", substr(auto_creds$access_key, 1, 8), "...\n",
               "Region: ", auto_creds$region, "\n",
               "Bucket: ", auto_creds$bucket, "\n",
               "Status: Ready to test connection")
      })
      
    } else {
      cat("No credentials found in environment\n")
      
      # Show helpful debug info
      values$processing_log <- paste0(
        "❌ No AWS credentials found\n\n",
        "Expected environment variables:\n",
        "• AWS_ACCESS_KEY_ID\n", 
        "• AWS_SECRET_ACCESS_KEY\n",
        "• AWS_S3_BUCKET\n",
        "• AWS_DEFAULT_REGION\n\n",
        "In Posit Connect:\n",
        "1. Go to app settings\n",
        "2. Set secret variables\n",
        "3. Redeploy app\n\n",
        "Environment check:\n",
        "• CONNECT_SERVER: ", Sys.getenv("CONNECT_SERVER", "(not set)"), "\n",
        "• Running in cloud: ", Sys.getenv("CONNECT_SERVER") != ""
      )
      
      output$connection_status <- renderText({
        "📝 Please enter AWS credentials manually or check environment variables"
      })
    }
    
    cat("=== END AUTO-LOADING ===\n")
  })
  
  # Test connection with better error handling and feedback
  observeEvent(input$test_connection, {
    req(input$aws_access_key, input$aws_secret_key, input$s3_bucket)
    
    # Show immediate feedback
    output$connection_status <- renderText({
      "🔄 Testing connection... Please wait..."
    })
    
    withProgress(message = "Testing AWS connection...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Validating inputs...")
        
        # Validate inputs
        if (nchar(input$aws_access_key) < 10) {
          stop("Access Key appears to be too short")
        }
        if (nchar(input$aws_secret_key) < 20) {
          stop("Secret Key appears to be too short") 
        }
        if (input$s3_bucket == "") {
          stop("Bucket name is required")
        }
        
        incProgress(0.4, detail = "Configuring AWS credentials...")
        
        # Configure AWS
        region <- if (input$aws_region == "custom") input$custom_region else input$aws_region
        configure_aws(input$aws_access_key, input$aws_secret_key, region)
        
        # Debug: Print environment variables (safely)
        cat("Debug - AWS_ACCESS_KEY_ID set:", Sys.getenv("AWS_ACCESS_KEY_ID") != "", "\n")
        cat("Debug - AWS_SECRET_ACCESS_KEY set:", Sys.getenv("AWS_SECRET_ACCESS_KEY") != "", "\n")
        cat("Debug - AWS_DEFAULT_REGION:", Sys.getenv("AWS_DEFAULT_REGION"), "\n")
        
        incProgress(0.6, detail = "Testing bucket access...")
        
        # Test bucket access with detailed error reporting
        test_objects <- list_s3_objects(input$s3_bucket, "", max_keys = 1)
        
        # Check if we got an error response
        if (nrow(test_objects) == 1 && test_objects$type[1] == "error") {
          stop(paste("S3 Error:", test_objects$display_name[1]))
        }
        
        incProgress(0.8, detail = "Verifying permissions...")
        
        # Additional verification - try to list a specific path
        tryCatch({
          mace_objects <- list_s3_objects(input$s3_bucket, "mace/", max_keys = 1)
          cat("Debug - Found mace objects:", nrow(mace_objects), "\n")
        }, error = function(e) {
          cat("Debug - Could not access mace/ folder:", e$message, "\n")
        })
        
        incProgress(1.0, detail = "Success!")
        
        values$aws_configured <- TRUE
        values$current_s3_path <- "mace/output-file/"
        
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
          paste("SUCCESS: Connection successful!\n",
                "Bucket:", input$s3_bucket, "\n",
                "Region:", region, "\n",
                "Objects found:", nrow(test_objects), "\n",
                "Ready to browse files!")
        })
        
        # Show success notification
        showNotification(
          "Successfully connected to S3!",
          type = "message",
          duration = 3
        )
        
        # Auto-switch to browse tab after short delay
        if (requireNamespace("shinyjs", quietly = TRUE)) {
          shinyjs::delay(2000, {
            updateTabItems(session, "tabs", "s3_browse")
          })
        } else {
          # Immediate switch without delay
          updateTabItems(session, "tabs", "s3_browse")
        }
        
      }, error = function(e) {
        values$aws_configured <- FALSE
        
        # Detailed error message
        error_msg <- e$message
        cat("Connection error:", error_msg, "\n")
        
        # Update sidebar status indicator
        output$connection_status_indicator <- renderUI({
          div(style = "background: #e74c3c; padding: 10px; border-radius: 6px; border-left: 4px solid #c0392b;",
              div(style = "color: white; font-size: 0.85em; font-weight: 500; margin-bottom: 3px;",
                  "Connection Status"),
              div(style = "color: #ecf0f1; font-size: 0.75em;",
                  "❌ Connection Failed")
          )
        })
        
        # Provide helpful error messages
        helpful_msg <- if (grepl("SSL", error_msg)) {
          "SSL/Network issue - check firewall settings"
        } else if (grepl("Access Denied|403", error_msg)) {
          "Access denied - check IAM permissions (s3:ListBucket, s3:GetObject)"
        } else if (grepl("No such bucket|404", error_msg)) {
          "Bucket not found - check bucket name spelling"
        } else if (grepl("too short", error_msg)) {
          "Credential format issue - check Access Key and Secret Key"
        } else {
          "Check credentials and bucket name"
        }
        
        output$connection_status <- renderText({
          paste("❌ Connection failed:\n", 
                error_msg, "\n\n",
                "💡 Suggestion:", helpful_msg, "\n\n",
                "🔍 Debug info:\n",
                "- Access Key length:", nchar(input$aws_access_key), "\n",
                "- Secret Key length:", nchar(input$aws_secret_key), "\n",
                "- Bucket:", input$s3_bucket, "\n",
                "- Region:", region)
        })
        
        # Show error notification
        showNotification(
          paste("Connection failed:", helpful_msg),
          type = "error",
          duration = 5
        )
      })
    })
  })
  
  # S3 file listing and navigation - COMPLETE IMPLEMENTATION
  load_s3_objects <- reactive({
    req(values$aws_configured, input$s3_bucket)
    
    tryCatch({
      cat("Loading S3 objects from path:", values$current_s3_path, "\n")
      objects <- list_s3_objects(input$s3_bucket, values$current_s3_path)
      cat("Found", nrow(objects), "objects\n")
      values$s3_objects <- objects
      return(objects)
    }, error = function(e) {
      cat("Error loading S3 objects:", e$message, "\n")
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
      values$processing_log <- paste(values$processing_log, "\nS3 listing refreshed.")
    }
  })
  
  # Auto-load when browse tab is accessed - with better debugging
  observeEvent(input$tabs, {
    cat("Tab changed to:", input$tabs, "\n")
    
    if (input$tabs == "s3_browse") {
      cat("Browse tab accessed. AWS configured:", values$aws_configured, "\n")
      
      if (values$aws_configured) {
        cat("Loading S3 objects from current path:", values$current_s3_path, "\n")
        
        # Force load S3 objects
        tryCatch({
          objects <- list_s3_objects(input$s3_bucket, values$current_s3_path)
          values$s3_objects <- objects
          cat("Successfully loaded", nrow(objects), "objects\n")
          
          # Update processing log
          values$processing_log <- paste(values$processing_log, 
                                         "\nBrowse tab accessed - loaded", nrow(objects), "objects from", values$current_s3_path)
          
        }, error = function(e) {
          cat("Error loading objects:", e$message, "\n")
          values$processing_log <- paste(values$processing_log, "\nError loading files:", e$message)
        })
      } else {
        values$processing_log <- paste(values$processing_log, "\nCannot browse - AWS not configured")
      }
    }
  })
  
  # Jump to output folder
  observeEvent(input$jump_to_output, {
    values$current_s3_path <- "mace/output-file/"
    load_s3_objects()
    values$selected_file <- NULL
  }, ignoreInit = TRUE)
  
  # Breadcrumb navigation
  output$breadcrumb_nav <- renderUI({
    if (!values$aws_configured) {
      return(p("Configure AWS credentials first"))
    }
    
    bucket_name <- input$s3_bucket
    current_path <- values$current_s3_path
    
    # Build breadcrumb
    if (current_path == "" || current_path == "/") {
      path_parts <- character(0)
    } else {
      path_parts <- strsplit(gsub("/$", "", current_path), "/")[[1]]
    }
    
    # Create breadcrumb HTML
    crumbs <- list()
    
    # Root bucket
    crumbs <- append(crumbs, 
                     list(tags$a(href = "#", onclick = "Shiny.setInputValue('navigate_to', '', {priority: 'event'});",
                                 paste("Bucket:", bucket_name))))
    
    # Path parts
    if (length(path_parts) > 0) {
      for (i in 1:length(path_parts)) {
        path_to_here <- paste(path_parts[1:i], collapse = "/") 
        if (path_to_here != "") path_to_here <- paste0(path_to_here, "/")
        
        if (i == length(path_parts)) {
          # Current folder (no link)
          crumbs <- append(crumbs, list(span(paste("Folder:", path_parts[i]))))
        } else {
          # Clickable path
          crumbs <- append(crumbs, 
                           list(tags$a(href = "#", 
                                       onclick = paste0("Shiny.setInputValue('navigate_to', '", path_to_here, "', {priority: 'event'});"),
                                       paste("Folder:", path_parts[i]))))
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
  
  # S3 file list table - with better error handling
  output$s3_file_list <- DT::renderDataTable({
    cat("Rendering S3 file list. AWS configured:", values$aws_configured, "\n")
    
    if (!values$aws_configured) {
      return(DT::datatable(
        data.frame(Message = "Please test AWS connection first"),
        options = list(dom = 't'), 
        rownames = FALSE
      ))
    }
    
    # Try to load objects
    objects <- tryCatch({
      load_s3_objects()
    }, error = function(e) {
      cat("Error in load_s3_objects:", e$message, "\n")
      data.frame(
        name = "Error",
        display_name = paste("Error:", e$message),
        type = "error",
        size = NA,
        last_modified = NA,
        stringsAsFactors = FALSE
      )
    })
    
    cat("Objects loaded for display:", nrow(objects), "\n")
    
    if (is.null(objects) || nrow(objects) == 0) {
      return(DT::datatable(
        data.frame(Message = paste("No files found in:", values$current_s3_path)),
        options = list(dom = 't'), 
        rownames = FALSE
      ))
    }
    
    # Process display data (same as before)
    display_data <- tryCatch({
      clean_data <- data.frame(
        Name = character(nrow(objects)),
        Size = character(nrow(objects)),
        Modified = character(nrow(objects)),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:nrow(objects)) {
        obj <- objects[i, ]
        icon <- if (obj$type == "folder") "[FOLDER]" else "[FILE]"
        clean_data$Name[i] <- paste(icon, obj$display_name)
        
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
        
        if (obj$type == "folder" || is.na(obj$last_modified) || obj$last_modified == "") {
          clean_data$Modified[i] <- ""
        } else {
          tryCatch({
            parsed_date <- as.POSIXct(obj$last_modified)
            clean_data$Modified[i] <- if (is.na(parsed_date)) obj$last_modified else format(parsed_date, "%Y-%m-%d %H:%M")
          }, error = function(e) {
            clean_data$Modified[i] <- obj$last_modified
          })
        }
      }
      
      clean_data
      
    }, error = function(e) {
      data.frame(
        Name = paste("DISPLAY ERROR:", e$message),
        Size = "",
        Modified = "",
        stringsAsFactors = FALSE
      )
    })
    
    cat("Display data prepared with", nrow(display_data), "rows\n")
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 20,
        dom = 't',
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2))
        )
      ), 
      selection = 'single', 
      escape = FALSE,
      rownames = FALSE
    )
  })
  
  # Handle file/folder selection - COMPLETE IMPLEMENTATION  
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
        
        values$processing_log <- paste(values$processing_log, "\nNavigated to:", new_path)
        
        # Clear download section
        output$download_section_display <- renderUI({ div() })
        
      } else {
        # Select file
        values$selected_file <- selected_item
        
        cat("File selected for download:", selected_item$name, "\n")
        
        # Update file info display
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
                "SELECTED: ", substr(selected_item$display_name, 1, 30),
                if(nchar(selected_item$display_name) > 30) "..." else ""),
            div(style = "font-size: 0.8em; opacity: 0.9;",
                "Size: ", file_size_display, " | Ready for analysis")
          )
        })
        
        # Show download section
        output$download_section_display <- renderUI({
          div(
            actionButton("download_and_process", 
                         "Download & Analyze", 
                         class = "btn-success", 
                         style = "width: 100%; height: 60px; margin-bottom: 12px; font-weight: bold; 
                                  padding: 8px 4px; line-height: 1.2;"),
            div(
              style = "background: #f8f9fa; padding: 8px; border-radius: 4px; border: 1px solid #dee2e6;",
              checkboxInput("rescale_volatility_s3", 
                            "10% Volatility Target", 
                            value = FALSE,
                            width = "100%")
            )
          )
        })
        
        values$processing_log <- paste(values$processing_log, "\nSelected file:", selected_item$display_name)
      }
    }
  })
  
  # Value boxes
  output$total_return_box <- renderValueBox({
    valueBox(value = "Ready", subtitle = "Total Return", icon = icon("cloud"), color = "light-blue")
  })
  
  output$sharpe_box <- renderValueBox({
    valueBox(value = "Ready", subtitle = "Sharpe Ratio", icon = icon("cloud"), color = "light-blue")
  })
  
  output$sortino_box <- renderValueBox({
    valueBox(value = "Ready", subtitle = "Sortino Ratio", icon = icon("cloud"), color = "light-blue")
  })
  
  output$max_dd_box <- renderValueBox({
    valueBox(value = "Ready", subtitle = "Max Drawdown", icon = icon("cloud"), color = "light-blue")
  })
  
  output$performance_table <- DT::renderDataTable({
    data.frame(Message = "Load portfolio data to see performance metrics")
  }, options = list(dom = 't'))
  
  output$strategy_grade <- renderUI({
    div(class = "alert alert-info", h4("Load data to see strategy grade"))
  })
}

# ============================================================================
# LAUNCH APP
# ============================================================================

# Detect environment
env_type <- if (Sys.getenv("CONNECT_SERVER") != "") {
  "Posit Connect"
} else if (Sys.getenv("SHINY_SERVER_VERSION") != "") {
  "Shiny Server"
} else {
  "Local"
}

cat("🚀 Portfolio Analytics Pro starting...\n")
cat("🌐 Environment:", env_type, "\n")
cat("✨ Cloud-optimized for deployment\n")

shinyApp(ui = ui, server = server)