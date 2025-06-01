# Volatility component (0-20 points)
if (volatility <= 0.10) grade_points <- grade_points + 20
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
                               
                               # Posit Connect Cloud notice
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
  
  # Auto-load credentials
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
      
      # Update processing log
      values$processing_log <- paste0(
        "SUCCESS: Auto-loaded from ", auto_creds$source, "\n",
        "Access Key: ", substr(auto_creds$access_key, 1, 8), "...\n",
        "Region: ", auto_creds$region, "\n",
        "Bucket: ", auto_creds$bucket, "\n\n",
        if (auto_creds$bucket != "") "READY: Click Test Connection!" else "WARNING: Set bucket name and test connection"
      )
      
      # Update connection status
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
        "3. Redeploy app"
      )
      
      output$connection_status <- renderText({
        "📝 Please enter AWS credentials manually or check environment variables"
      })
    }
    
    cat("=== END AUTO-LOADING ===\n")
  })
  
  # Test connection
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
        
        incProgress(0.6, detail = "Testing bucket access...")
        
        # Test bucket access
        test_objects <- list_s3_objects(input$s3_bucket, "", max_keys = 1)
        
        # Check if we got an error response
        if (nrow(test_objects) == 1 && test_objects$type[1] == "error") {
          stop(paste("S3 Error:", test_objects$display_name[1]))
        }
        
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
                "💡 Suggestion:", helpful_msg)
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
  
  # S3 file listing and navigation
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
  
  # Auto-load when browse tab is accessed
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
  
  # S3 file list table
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
    
    # Process display data
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
  
  # FIXED: Download and process selected S3 file with proper SPY integration
  observeEvent(input$download_and_process, {
    req(values$selected_file)
    
    selected_file <- values$selected_file
    
    cat("Download button clicked for file:", selected_file$name, "\n")
    
    withProgress(message = "Processing S3 file...", value = 0, {
      
      tryCatch({
        incProgress(0.1, detail = "Downloading from S3...")
        values$processing_log <- paste(values$processing_log, "\nDOWNLOADING:", selected_file$display_name)
        
        # Download file
        temp_file_path <- download_s3_file(input$s3_bucket, selected_file$name)
        values$processing_log <- paste(values$processing_log, "\nSUCCESS: Downloaded to temp file")
        cat("Downloaded to:", temp_file_path, "\n")
        
        incProgress(0.3, detail = "Reading file...")
        
        # Read the file based on extension
        file_ext <- tools::file_ext(selected_file$display_name)
        cat("File extension:", file_ext, "\n")
        
        if (file_ext == "parquet") {
          data <- arrow::read_parquet(temp_file_path)
        } else if (file_ext == "csv") {
          data <- readr::read_csv(temp_file_path, show_col_types = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(temp_file_path)
        } else {
          stop("Unsupported file format: ", file_ext)
        }
        
        values$processing_log <- paste(values$processing_log, "\nFILE LOADED:", nrow(data), "rows,", ncol(data), "columns")
        cat("Data loaded - rows:", nrow(data), "cols:", ncol(data), "\n")
        cat("Column names:", paste(names(data), collapse = ", "), "\n")
        
        incProgress(0.5, detail = "Processing data structure...")
        
        # FIXED: Process the data with SPY benchmark integration
        processed_data <- process_portfolio_data(data, input$rescale_volatility_s3)
        values$processed_data <- processed_data
        
        values$processing_log <- paste(values$processing_log, "\nDATA PROCESSED: Portfolio returns calculated")
        if ("Rb" %in% names(processed_data) && !all(is.na(processed_data$Rb))) {
          values$processing_log <- paste(values$processing_log, "\nSPY BENCHMARK: Successfully integrated")
        } else {
          values$processing_log <- paste(values$processing_log, "\nSPY BENCHMARK: Not available")
        }
        cat("Portfolio data processed successfully\n")
        
        incProgress(0.8, detail = "Running portfolio analysis...")
        
        # Extract strategy name from file
        strategy_name <- tools::file_path_sans_ext(selected_file$display_name)
        
        # FIXED: Run full analysis using the enhanced hf_grade_analysis function
        analysis_results <- hf_grade_analysis(processed_data, fund_name = strategy_name)
        values$analysis_results <- analysis_results
        
        incProgress(1.0, detail = "Complete!")
        
        values$processing_log <- paste(values$processing_log, "\nANALYSIS COMPLETE: Check Dashboard tab")
        cat("Analysis complete\n")
        
        # Clean up temp file
        unlink(temp_file_path)
        
        # Show success message
        showNotification(
          paste("Successfully processed:", selected_file$display_name),
          type = "message",
          duration = 5
        )
        
        # Auto-switch to dashboard
        updateTabItems(session, "tabs", "dashboard")
        
      }, error = function(e) {
        cat("Error processing file:", e$message, "\n")
        values$processing_log <- paste(values$processing_log, "\nERROR:", e$message)
        showNotification(
          paste("Error processing file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
  
  # Processing log output
  output$s3_processing_log <- renderText({
    values$processing_log
  })
  
  # Value boxes - using analysis results
  output$total_return_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      total_ret <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Total Return"]
      valueBox(value = total_ret, subtitle = "Total Return", icon = icon("arrow-up"), color = "green")
    } else {
      valueBox(value = "Load Data", subtitle = "Total Return", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  output$sharpe_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      sharpe <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Sharpe Ratio"]
      color <- if (as.numeric(sharpe) > 1) "blue" else "yellow"
      valueBox(value = sharpe, subtitle = "Sharpe Ratio", icon = icon("chart-line"), color = color)
    } else {
      valueBox(value = "Load Data", subtitle = "Sharpe Ratio", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  output$sortino_box <- renderValueBox({
    if (!is.null(values$analysis_results)) {
      sortino <- values$analysis_results$summary_stats$Value[
        values$analysis_results$summary_stats$Metric == "Sortino Ratio"]
      valueBox(value = sortino, subtitle = "Sortino Ratio (Fixed)", icon = icon("shield-alt"), color = "purple")
    } else {
      valueBox(value = "Load Data", subtitle = "Sortino Ratio", icon = icon("cloud"), color = "light-blue")
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
      valueBox(value = "Load Data", subtitle = "Max Drawdown", icon = icon("cloud"), color = "light-blue")
    }
  })
  
  # Performance table
  output$performance_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      values$analysis_results$summary_stats
    } else {
      DT::datatable(
        data.frame(Message = "Load portfolio data to see performance metrics"),
        options = list(dom = 't'), rownames = FALSE
      )
    }
  }, options = list(pageLength = 25, dom = 'ft'), rownames = FALSE)
  
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
      div(class = "alert alert-info", h4("Load data to see strategy grade"))
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
  
  output$return_dist_plot <- renderPlotly({
    if (!is.null(values$analysis_results)) {
      ggplotly(values$analysis_results$plots$return_distribution)
    }
  })
  
  # Monthly returns table
  output$monthly_returns_table <- DT::renderDataTable({
    if (!is.null(values$analysis_results)) {
      monthly_data <- values$analysis_results$monthly_returns %>%
        mutate(
          Year = year,
          Month = month.abb[month],
          Return = sprintf("%.2f%%", monthly_ret * 100)
        ) %>%
        select(Year, Month, Return) %>%
        pivot_wider(names_from = Month, values_from = Return, values_fill = "")
      
      DT::datatable(monthly_data, options = list(pageLength = 15, dom = 'ft'), rownames = FALSE)
    } else {
      DT::datatable(
        data.frame(Message = "Load portfolio data to see monthly returns"),
        options = list(dom = 't'), rownames = FALSE
      )
    }
  })
  
  # Validation function
  output$validation_output <- renderText({
    if (!is.null(values$processed_data)) {
      if (exists("validate_pa_functions")) {
        validation_results <- validate_pa_functions(values$processed_data)
        
        output_text <- "=== PERFORMANCE ANALYTICS VALIDATION ===\n\n"
        
        for (metric_name in names(validation_results)) {
          result <- validation_results[[metric_name]]
          status <- if (result$valid) "✅ PASS" else "❌ FAIL"
          
          output_text <- paste0(output_text,
                                metric_name, ": ", status, "\n",
                                "  Manual: ", round(result$manual, 4), "\n",
                                "  PA: ", round(result$pa, 4), "\n",
                                "  Difference: ", round(abs(result$manual - result$pa), 4), "\n\n")
        }
        
        return(output_text)
      } else {
        return("Validation function not available. Load supporting_functions.R")
      }
    } else {
      return("Load portfolio data first to run validation")
    }
  })
  
  observeEvent(input$run_validation, {
    if (is.null(values$processed_data)) {
      showNotification("Load portfolio data first", type = "warning", duration = 3)
    }
  })
}

# ============================================================================
# FIXED: PROPER SHINY APP OBJECT CREATION FOR DEPLOYMENT
# ============================================================================

# Create the shiny app object
app <- shinyApp(ui = ui, server = server)

# For deployment, explicitly return the app object
app# ============================================================================
# CLOUD-READY PORTFOLIO ANALYTICS SHINY APP - DEPLOYMENT FIX
# Fixed structure for Posit Connect deployment
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
# FIXED: DEFINE ALL FUNCTIONS OUTSIDE OF SERVER
# ============================================================================

# Auto-load AWS credentials (Posit Connect Cloud pattern)
auto_load_aws_credentials <- function() {
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

# FIXED: Process portfolio data with SPY integration
process_portfolio_data <- function(data, rescale_vol = FALSE) {
  cat("Processing portfolio data...\n")
  
  # Check if this is raw portfolio data
  if ("ticker" %in% names(data) && "date" %in% names(data) && ncol(data) >= 3) {
    
    # Remove any index columns
    index_cols <- grep("^__index|^X\\.|^\\.\\.\\.|^Unnamed", names(data), value = TRUE)
    if (length(index_cols) > 0) {
      data <- data %>% select(-all_of(index_cols))
      cat("Removed index columns:", paste(index_cols, collapse = ", "), "\n")
    }
    
    # Standard format handling
    if (all(c("ticker", "daily_return", "date") %in% names(data))) {
      cat("Standard format detected\n")
      
      # Find weights column
      standard_cols <- c("ticker", "daily_return", "date")
      weight_col_candidates <- setdiff(names(data), standard_cols)
      
      if (length(weight_col_candidates) >= 1) {
        weight_col_name <- weight_col_candidates[1]
        data$weights <- data[[weight_col_name]]
        data <- data %>% select(-all_of(weight_col_name))
        cat("Mapped weights column:", weight_col_name, "\n")
      } else {
        stop("No weights column found")
      }
      
    } else {
      stop("Expected columns: ticker, daily_return, date, [weights]. Found:", paste(names(data), collapse = ", "))
    }
    
    # Convert date
    cat("Converting dates...\n")
    data$date <- as.Date(data$date)
    
    # Calculate portfolio returns
    cat("Calculating portfolio returns...\n")
    processed_data <- data %>% 
      group_by(date) %>% 
      summarise(Ra = sum(weights * daily_return, na.rm = TRUE), .groups = 'drop') %>% 
      arrange(date) %>% 
      mutate(date = as.Date(date))
    
    cat("Portfolio has", nrow(processed_data), "daily observations\n")
    cat("Date range:", as.character(min(processed_data$date)), "to", as.character(max(processed_data$date)), "\n")
    
    # FIXED: Fetch SPY benchmark data with proper alignment
    cat("Fetching SPY benchmark data...\n")
    
    tryCatch({
      # Get date range with buffer for better alignment
      min_date <- min(processed_data$date) - 5  # Buffer for weekends/holidays
      max_date <- max(processed_data$date) + 5
      
      cat("Fetching SPY from", as.character(min_date), "to", as.character(max_date), "\n")
      
      # Fetch SPY data
      spy_data <- tq_get("SPY", from = min_date, to = max_date) %>%
        tq_transmute(select = adjusted, mutate_fun = periodReturn, 
                     period = "daily", col_rename = "Rb") %>%
        filter(row_number() > 1) %>%  # Remove first row (NA)
        mutate(date = as.Date(date))
      
      cat("SPY data fetched:", nrow(spy_data), "observations\n")
      cat("SPY date range:", as.character(min(spy_data$date)), "to", as.character(max(spy_data$date)), "\n")
      
      # FIXED: Properly align portfolio and benchmark data
      cat("Aligning portfolio and benchmark data...\n")
      
      # Inner join to ensure matching dates only
      aligned_data <- processed_data %>%
        inner_join(spy_data, by = "date", suffix = c("", "_spy")) %>%
        arrange(date)
      
      cat("After alignment:", nrow(aligned_data), "observations\n")
      cat("Aligned date range:", as.character(min(aligned_data$date)), "to", as.character(max(aligned_data$date)), "\n")
      
      # Check for any remaining issues
      if (nrow(aligned_data) == 0) {
        cat("WARNING: No overlapping dates found between portfolio and SPY\n")
        processed_data$Rb <- NA  # Add empty benchmark column
      } else if (nrow(aligned_data) != nrow(processed_data)) {
        cat("INFO: Alignment reduced data from", nrow(processed_data), "to", nrow(aligned_data), "observations\n")
        processed_data <- aligned_data
      } else {
        processed_data <- aligned_data
      }
      
    }, error = function(e) {
      cat("Error fetching SPY data:", e$message, "\n")
      cat("Proceeding without benchmark...\n")
      processed_data$Rb <- NA  # Add empty benchmark column
    })
    
    # Volatility rescaling if requested
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
    
    # Final validation
    cat("Final data validation...\n")
    cat("Final dataset:", nrow(processed_data), "rows,", ncol(processed_data), "columns\n")
    cat("Columns:", paste(names(processed_data), collapse = ", "), "\n")
    cat("NA values in Ra:", sum(is.na(processed_data$Ra)), "\n")
    if ("Rb" %in% names(processed_data)) {
      cat("NA values in Rb:", sum(is.na(processed_data$Rb)), "\n")
    }
    
    cat("Data processing complete!\n")
    return(processed_data)
    
  } else {
    stop("Data format not recognized. Expected columns: ticker, daily_return, date, [weights]")
  }
}

# FIXED: Enhanced hedge fund analysis function with better SPY handling
hf_grade_analysis <- function(portfolio_data, benchmark_data = NULL, fund_name = "Portfolio") {
  
  cat("Starting hedge fund analysis for:", fund_name, "\n")
  
  # Ensure data is properly formatted and convert to xts
  dates <- portfolio_data$date
  portfolio_returns <- xts(portfolio_data$Ra, order.by = dates)
  
  cat("Portfolio returns:", nrow(portfolio_returns), "observations\n")
  
  # Handle benchmark data - FIXED to properly use Rb column if available
  benchmark_returns <- NULL
  
  if ("Rb" %in% names(portfolio_data) && !all(is.na(portfolio_data$Rb))) {
    cat("Using benchmark data from Rb column\n")
    benchmark_returns <- xts(portfolio_data$Rb, order.by = dates)
    cat("Benchmark returns:", nrow(benchmark_returns), "observations\n")
  } else {
    cat("No benchmark data available or all NA values\n")
  }
  
  # Core performance metrics
  cat("Calculating core performance metrics...\n")
  
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
  
  # Market metrics (if benchmark provided) - FIXED
  alpha_val <- beta_val <- info_ratio <- tracking_error <- up_capture <- down_capture <- NA
  
  if (!is.null(benchmark_returns)) {
    cat("Calculating market metrics with benchmark...\n")
    
    tryCatch({
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
        
        cat("Beta:", round(beta_manual, 3), "Alpha:", round(alpha_val * 100, 2), "%\n")
      } else {
        alpha_val <- 0
        cat("Insufficient paired data for alpha/beta calculation\n")
      }
      
      info_ratio <- InformationRatio(portfolio_returns, benchmark_returns)
      tracking_error <- TrackingError(portfolio_returns, benchmark_returns)
      
      # Fix UpDownRatios - it returns a matrix, not a list
      updown_ratios <- UpDownRatios(portfolio_returns, benchmark_returns)
      up_capture <- updown_ratios[1,1]
      down_capture <- updown_ratios[2,1]
      
      cat("Market metrics calculated successfully\n")
      
    }, error = function(e) {
      cat("Error calculating market metrics:", e$message, "\n")
      # Keep default NA values
    })
  }
  
  # Monthly returns for calendar analysis
  monthly_returns <- portfolio_returns %>%
    fortify(melt = TRUE) %>%
    mutate(date = as.Date(Index),
           year = year(date),
           month = month(date)) %>%
    group_by(year, month) %>%
    summarise(monthly_ret = prod(1 + Value) - 1, .groups = 'drop')
  
  # Drawdown analysis
  dd_table <- table.Drawdowns(portfolio_returns, top = 10)
  
  # Best/Worst periods
  returns_vector <- as.numeric(portfolio_returns)
  best_day <- max(returns_vector, na.rm = TRUE)
  worst_day <- min(returns_vector, na.rm = TRUE)
  best_month <- max(monthly_returns$monthly_ret, na.rm = TRUE)
  worst_month <- min(monthly_returns$monthly_ret, na.rm = TRUE)
  
  # Win rate
  win_rate <- sum(returns_vector > 0, na.rm = TRUE) / length(returns_vector[!is.na(returns_vector)])
  
  # Consecutive analysis
  runs <- rle(sign(returns_vector))
  pos_runs <- runs$lengths[runs$values > 0]
  neg_runs <- runs$lengths[runs$values < 0]
  
  max_consecutive_wins <- if(length(pos_runs) > 0) max(pos_runs) else 0
  max_consecutive_losses <- if(length(neg_runs) > 0) max(neg_runs) else 0
  
  # Summary statistics table
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
              sprintf("%d", max_consecutive_wins),
              sprintf("%d", max_consecutive_losses))
  )
  
  # Add benchmark metrics if available - FIXED to handle NA values properly
  if (!is.null(benchmark_returns) && !all(is.na(alpha_val))) {
    benchmark_metrics <- data.frame(
      Metric = c("Beta", "Alpha (Annualized)", "Information Ratio", "Tracking Error",
                 "Up Capture", "Down Capture"),
      Value = c(sprintf("%.2f", ifelse(is.na(beta_val), 0, beta_val)),
                sprintf("%.2f%%", ifelse(is.na(alpha_val), 0, alpha_val * 100)),
                sprintf("%.2f", ifelse(is.na(info_ratio), 0, info_ratio)),
                sprintf("%.2f%%", ifelse(is.na(tracking_error), 0, tracking_error * 100)),
                sprintf("%.1f%%", ifelse(is.na(up_capture), 0, up_capture * 100)),
                sprintf("%.1f%%", ifelse(is.na(down_capture), 0, down_capture * 100)))
    )
    summary_stats <- rbind(summary_stats, benchmark_metrics)
    cat("Added benchmark metrics to summary table\n")
  }
  
  # Visualizations
  cat("Creating visualizations...\n")
  
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
  rolling_sharpe <- rollapply(portfolio_returns, width = 252, 
                              FUN = function(x) mean(x)/sd(x) * sqrt(252), 
                              fill = NA, align = "right")
  
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
  
  cat("Analysis complete!\n")
  
  return(list(
    summary_stats = summary_stats,
    plots = list(
      cumulative_returns = p1,
      drawdowns = p2,
      rolling_sharpe = p3,
      return_distribution = p4
    ),
    raw_data = list(
      portfolio_returns = portfolio_returns,
      benchmark_returns = benchmark_returns,
      dates = dates
    ),
    drawdown_table = dd_table,
    monthly_returns = monthly_returns
  ))
}

# ORIGINAL GRADING FUNCTION
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
  
  # Grading criteria
  grade_points <- 0
  
  # Return component (0-30 points)
  if (total_return > 5.0) grade_points <- grade_points + 30        # 500%+ 
  else if (total_return > 2.0) grade_points <- grade_points + 28   # 200%+
  else if (total_return > 1.0) grade_points <- grade_points + 25   # 100%+
  else if (total_return > 0.5) grade_points <- grade_points + 20   # 50%+
  else if (total_return > 0.2) grade_points <- grade_points + 15   # 20%+
  else if (total_return > 0.1) grade_points <- grade_points + 10   # 10%+
  else grade_points <- grade_points + 5
  
  # Sharpe component (0-25 points)
  if (sharpe > 2.0) grade_points <- grade_points + 25
  else if (sharpe > 1.5) grade_points <- grade_points + 23
  else if (sharpe > 1.3) grade_points <- grade_points + 20
  else if (sharpe > 1.0) grade_points <- grade_points + 17
  else if (sharpe > 0.8) grade_points <- grade_points + 12
  else if (sharpe > 0.5) grade_points <- grade_points + 8
  else grade_points <- grade_points + 3
  
  # Drawdown component (0-25 points)
  if (max_dd < 0.05) grade_points <- grade_points + 25
  else if (max_dd < 0.10) grade_points <- grade_points + 20
  else if (max_dd < 0.15) grade_points <- grade_points + 17
  else if (max_dd < 0.20) grade_points <- grade_points + 14
  else if (max_dd < 0.30) grade_points <- grade_points + 10
  else grade_points <- grade_points + 5