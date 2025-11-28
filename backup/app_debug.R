# app_modified.R
library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(digest)
library(DT)
library(shinyWidgets)
library(mailR)

# UI definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
  // Debug and force visibility
  Shiny.addCustomMessageHandler('showMainApp', function(message) {
    console.log('Showing main_app');
    var mainApp = document.getElementById('main_app');
    var loginScreen = document.getElementById('login_screen');
    
    loginScreen.style.display = 'none';
    mainApp.style.display = 'block';
    mainApp.style.visibility = 'visible';
    mainApp.style.opacity = '1';
    
    console.log('main_app display:', mainApp.style.display);
    console.log('main_app visibility:', mainApp.style.visibility);
  });
")),
 tags$script(HTML("
  // Check every second if main_app is visible
  setInterval(function() {
    console.log('main_app visible:', $('#main_app').is(':visible'));
    console.log('main_app children:', $('#main_app').children().length);
  }, 1000);
 ")),
#    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style("
  .login-info {
    font-size: 0.9em;
    line-height: 1.5;
  }
  .login-info ul {
    margin: 10px 0;
    padding-left: 20px;
  }
  .login-info li {
    margin-bottom: 5px;
  }
  .login-info a {
    color: #3498db;
    text-decoration: none;
  }
  .login-info a:hover {
    text-decoration: underline;
  }
  .modal-body .form-group {
        margin-bottom: 15px;
      }
      .shiny-input-container {
        width: 100% !important;
      }
      .action-buttons {
        margin: 20px 0;
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }
      .projects-table {
        margin-top: 20px;
      }
      .admin-management-table {
        margin: 15px 0;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
      .status-application-received { background-color: #fff3cd !important; }
      .status-under-review { background-color: #cce7ff !important; }
      .status-approved { background-color: #d4edda !important; }
      .status-rejected { background-color: #f8d7da !important; }
      .status-sequencing-in-progress { background-color: #e2e3e5 !important; }
      .status-data-delivered { background-color: #d1ecf1 !important; }
      .status-project-completed { background-color: #d4edda !important; }
      .home-page {
        text-align: center;
        padding: 40px 20px;
      }
      .home-title {
        color: #2c3e50;
        margin-bottom: 30px;
        font-size: 2.5em;
      }
      .home-subtitle {
        color: #34495e;
        font-size: 1.3em;
        margin-bottom: 40px;
        line-height: 1.6;
      }
      .home-features {
        display: flex;
        justify-content: center;
        gap: 30px;
        margin-top: 40px;
        flex-wrap: wrap;
      }
      .feature-card {
        background: white;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        width: 300px;
        text-align: center;
      }
      .feature-icon {
        font-size: 3em;
        color: #3498db;
        margin-bottom: 20px;
      }
      .feature-title {
        font-size: 1.5em;
        color: #2c3e50;
        margin-bottom: 15px;
      }
      .feature-description {
        color: #7f8c8d;
        line-height: 1.5;
      }
      .header-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
        padding: 0 20px;
      }
      .header-left {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      .header-right {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      .header-logo {
        height: 50px;
        width: auto;
      }
      .institute-name {
        color: white;
        font-size: 1.2em;
        font-weight: bold;
        margin: 0;
      }
      .app-title {
        color: white;
        font-size: 1.5em;
        font-weight: bold;
        margin: 0;
        text-align: center;
        flex-grow: 1;
      }
      .user-info-panel {
        display: flex;
        align-items: center;
        gap: 15px;
        color: white;
      }
      .instructions-panel {
        background-color: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 20px;
        margin: 20px 0;
        border-radius: 4px;
      }
      .instructions-title {
        color: #2c3e50;
        font-size: 1.3em;
        margin-bottom: 15px;
        font-weight: bold;
      }
      .instructions-list {
        color: #34495e;
        line-height: 1.6;
        margin: 0;
        padding-left: 20px;
      }
      .instructions-list li {
        margin-bottom: 8px;
      }
      .cost-calculation {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-top: 20px;
      }
      .cost-total {
        font-size: 1.2em;
        font-weight: bold;
        color: #2c3e50;
      }
      .cost-warning {
        color: #e74c3c;
        font-style: italic;
      }
    "),
    tags$script("
      $(document).ready(function() {
        $(document).on('keyup', '#login_username, #login_password', function(e) {
          if (e.keyCode === 13) {
            $('#login_btn').click();
          }
        });
      });
    "),
    tags$script(HTML("
  // Debug JavaScript
  console.log('Shiny app loading...');
  $(document).on('shiny:connected', function() {
    console.log('Shiny connected successfully');
  });
  $(document).on('shiny:error', function(e) {
    console.log('Shiny error:', e.error);
  });
  // Check if main_app is visible
  $(document).on('shiny:value', function(e) {
    if(e.name === 'main_content') {
      console.log('main_content rendered');
      console.log('main_app visibility:', $('#main_app').is(':visible'));
      console.log('main_app height:', $('#main_app').height());
    }
    });
  "))
  ),
  
  # Login screen
  div(
    id = "login_screen",
    class = "login-container",
    div(
      class = "login-box",
      h2("Sequencing Project Application", class = "login-title"),
      div(
        class = "login-info",
        style = "text-align: left; margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #3498db;",
        h4("NGS Sequencing Service", style = "color: #2c3e50; margin-top: 0;"),
        p("The NGS lab of the Core Facility is providing sequencing as a service. We highly recommend to contact us before the start of your experiment. In collaboration with the Bioinformatics Core Facility, we provide:"),
        tags$ul(
          tags$li("Assistance with experimental design of the studies (required number of samples and replicates)"),
          tags$li("Assistance with the use of NGS open source analysis tools"),
          tags$li("Data analysis on collaborative basis")
        ),
        p("If you have any further questions, please do not hesitate to ", 
          tags$a(href = "mailto:omicsdesk.biochem.mpg.de", "contact us @ omicsdesk"), "!"),
        
        h4("Registration", style = "color: #2c3e50; margin-top: 20px;"),
        p("If you want to use the NGS service for the first time, you have to ",
          tags$a(href = "https://ngs-vm.biochem.mpg.de/register.cgi", "register for an account", target = "_blank"),
          ". Please use your MPIB credentials for account registration."),
        
        h4("Login", style = "color: #2c3e50; margin-top: 20px;"),
        p("After registration is complete, you can directly login with your MPIB account to submit your new project(s).")
      ),
      textInput("login_username", "Username", placeholder = "Enter your username"),
      passwordInput("login_password", "Password", placeholder = "Enter your password"),
      actionButton("login_btn", "Login", class = "btn-login"),
      div(
        class = "login-links",
        actionLink("show_register_btn", "New user? Register here"),
        style = "text-align: center; margin-top: 1rem;"
      ),
      div(id = "login_error", class = "error-message")
    )
  ),
  
  # Main application (hidden until login)
  hidden(
    div(
      id = "main_app",
      
      # Header with logos and logout on right
      div(
        class = "app-header",
        div(
          class = "header-container",
          div(
            class = "header-left",
            img(src = "logo.png", class = "header-logo", alt = "Logo"),
            h3("MPIB Sequencing facility", class = "institute-name")
          ),
          div(
            class = "app-title",
            "Sequencing Project Application System"
          ),
          div(
            class = "header-right",
            img(src = "minerva.png", class = "header-logo", alt = "Minerva"),
            div(
              class = "user-info-panel",
              textOutput("welcome_user"),
              actionButton("logout_btn", "Logout", class = "btn-logout")
            )
          )
        )
      ),
      
      # ADD THIS TEMPORARY DEBUG OUTPUT
      uiOutput("debug_visible"),

      # Main content with tabs for admins
      uiOutput("main_content")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Debug: Print to console when server starts
  cat("Server function started\n")
  
  ##################################################################
  # SOLUTION 1: DATABASE VALIDATION AND REPAIR FUNCTIONS
  ##################################################################
  
  # Database validation and repair function
  validate_and_repair_database <- function() {
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
      
      # Check if all essential tables exist
      required_tables <- c("users", "projects", "budget_holders", "service_types", 
                           "sequencing_depths", "sequencing_cycles", "types",
                           "sequencing_platforms", "reference_genomes")
      
      existing_tables <- dbListTables(con)
      missing_tables <- setdiff(required_tables, existing_tables)
      
      if (length(missing_tables) > 0) {
        showNotification(paste("Missing tables detected:", paste(missing_tables, collapse = ", ")), 
                         type = "warning", duration = 10)
        # Don't auto-recreate, just warn the admin
        dbDisconnect(con)
        return(FALSE)
      }
      
      # Test basic query
      test_query <- dbGetQuery(con, "SELECT 1 as test")
      
      dbDisconnect(con)
      return(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Database error:", e$message), type = "error", duration = 10)
      return(FALSE)
    })
  }
  
  # Check database on app start
  observe({
    if (!validate_and_repair_database()) {
      showModal(modalDialog(
        title = "Database Issue Detected",
        tagList(
          p("There seems to be an issue with the database."),
          p("Please contact the administrator."),
          p("Do NOT recreate the database as this will lose all existing data."),
          br(),
          actionButton("debug_db_btn", "Show Database Debug Info", class = "btn-info")
        ),
        footer = modalButton("Close"),
        size = "m"
      ))
    }
  })
  
  # Test observer
  observeEvent(input$test_btn, {
    showNotification("Test button clicked! App is working.", type = "message")
  })
  
  # Check if main_app element exists and is visible
  observe({
    if(user$logged_in) {
      cat("DEBUG: Checking main_app element...\n")
      runjs("
      console.log('main_app element:', $('#main_app'));
      console.log('main_app visible:', $('#main_app').is(':visible'));
      console.log('main_app children:', $('#main_app').children().length);
    ")
    }
  })
  
  # Debug database info
  observeEvent(input$debug_db_btn, {
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
      tables <- dbListTables(con)
      table_counts <- sapply(tables, function(table) {
        count <- dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))$count
        paste0(table, ": ", count, " rows")
      })
      dbDisconnect(con)
      
      showModal(modalDialog(
        title = "Database Debug Information",
        tagList(
          h4("Tables and Row Counts:"),
          pre(paste(table_counts, collapse = "\n")),
          h4("Database File Info:"),
          pre(paste(capture.output(file.info("sequencing_projects.db")), collapse = "\n"))
        ),
        footer = modalButton("Close"),
        size = "l"
      ))
    }, error = function(e) {
      showNotification(paste("Debug failed:", e$message), type = "error")
    })
  })
  
  ##################################################################
  # END SOLUTION 1
  ##################################################################
  
  # Reactive values
  user <- reactiveValues(logged_in = FALSE, username = NULL, user_id = NULL, is_admin = FALSE)
  projects_data <- reactiveVal()
  
  # Reactive values for admin management
  admin_data <- reactiveValues(
    users = data.frame(),
    reference_genomes = c(),
    budget_holders = data.frame(),
    types = data.frame(),
    service_types = data.frame(),
    sequencing_depths = data.frame(),
    sequencing_cycles = data.frame(),
    sequencing_platforms = data.frame()
  )
  
  # Define status options
  status_options <- c(
    "Created",
    "Samples received", 
    "Library preparation",
    "QC done",
    "On the sequencer", 
    "Data analysis", 
    "Data released"
  )
  
  # Database connection
  get_db_connection <- function() {
    dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
  }
  
  # Get all usernames for responsible user dropdown
  get_all_usernames <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    users <- dbGetQuery(con, "SELECT username FROM users ORDER BY username")
    return(users$username)
  }
  
  # Load budget holders from database
  load_budget_holders <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    holders <- dbGetQuery(con, "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY name, surname")
    return(holders)
  }
  
  # Load service types from database
  load_service_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    service_types <- dbGetQuery(con, "SELECT id, service_type, kit, costs_per_sample FROM service_types ORDER BY service_type")
    return(service_types)
  }
  
  # Load sequencing depths from database
  load_sequencing_depths <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    depths <- dbGetQuery(con, "SELECT id, depth_description, cost_upto_150_cycles, cost_upto_300_cycles FROM sequencing_depths ORDER BY depth_description")
    return(depths)
  }
  
  # Load sequencing cycles from database
  load_sequencing_cycles <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    cycles <- dbGetQuery(con, "SELECT id, cycles_description FROM sequencing_cycles ORDER BY cycles_description")
    return(cycles)
  }
  
  # Load reference genomes from database
  load_reference_genomes <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    genomes <- dbGetQuery(con, "SELECT name FROM reference_genomes ORDER BY name")
    return(genomes$name)
  }
  
  # Load types from database
  load_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    types <- dbGetQuery(con, "SELECT id, name FROM types ORDER BY name")
    return(types)
  }
  
  # Load sequencing platforms from database
  load_sequencing_platforms <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    platforms <- dbGetQuery(con, "SELECT id, name FROM sequencing_platforms ORDER BY name")
    return(platforms)
  }
  
  # Load admin data
  load_admin_data <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Load users
    admin_data$users <- dbGetQuery(con, "SELECT id, username, email, is_admin FROM users")
    
    # Load all reference data from database
    admin_data$budget_holders <- load_budget_holders()
    admin_data$reference_genomes <- load_reference_genomes()
    admin_data$types <- load_types()
    admin_data$service_types <- load_service_types()
    admin_data$sequencing_depths <- load_sequencing_depths()
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
  }
  
  # Calculate total cost
  calculate_total_cost <- function(num_samples, service_type_id, sequencing_depth_id, sequencing_cycles_id) {
    if(is.null(num_samples) || is.null(service_type_id) || is.null(sequencing_depth_id) || is.null(sequencing_cycles_id)) {
      return(0)
    }
    
    service_type <- admin_data$service_types[admin_data$service_types$id == as.numeric(service_type_id), ]
    sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == as.numeric(sequencing_depth_id), ]
    
    if(nrow(service_type) == 0 || nrow(sequencing_depth) == 0) {
      return(0)
    }
    
    prep_cost <- service_type$costs_per_sample * as.numeric(num_samples)
    
    # Check if "other" is selected for sequencing depth
    if(sequencing_depth$depth_description == "other") {
      return(prep_cost)
    }
    
    # Determine which cost column to use based on sequencing cycles
    if(sequencing_cycles_id == "1") {
      seq_cost <- sequencing_depth$cost_upto_150_cycles
    } else {
      seq_cost <- sequencing_depth$cost_upto_300_cycles
    }
    
    total_cost <- prep_cost + ifelse(is.na(seq_cost), 0, seq_cost)
    return(total_cost)
  }
  
  # Send email notification
  send_project_creation_email <- function(project_data, budget_holder, user_email) {
    tryCatch({
      cat("DEBUG: Starting email function\n")
      cat("DEBUG: From: yeroslaviz@biochem.mpg.de\n")
      cat("DEBUG: To:", paste(c(budget_holder$email, user_email), collapse = ", "), "\n")
      
      # Get email template
      con <- get_db_connection()
      template <- dbGetQuery(con, "SELECT subject, body_template FROM email_templates WHERE template_name = 'project_creation' AND is_active = 1")
      dbDisconnect(con)
      
      if(nrow(template) == 0) {
        cat("DEBUG: Template not found\n")
        return(list(success = FALSE, error = "Email template not found"))
      }
      
      cat("DEBUG: Template found, preparing content\n")
      
      # Get email template from database
      con <- get_db_connection()
      template <- dbGetQuery(con, "SELECT subject, body_template FROM email_templates WHERE template_name = 'project_creation' AND is_active = 1")
      dbDisconnect(con)
      
      if(nrow(template) == 0) {
        showNotification("Email template not found", type = "warning")
        return(list(success = FALSE, error = "Email template not found"))
      }
      
      # Prepare cost warning
      sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == project_data$sequencing_depth_id, ]
      cost_warning <- ""
      if(sequencing_depth$depth_description == "other") {
        cost_warning <- "NOTE: The costs shown are preliminary. Please contact us to discuss your specific sequencing needs as the 'other' option was selected for sequencing depth."
      }
      
      # Replace placeholders in template
      email_body <- template$body_template
      email_body <- gsub("\\{name\\}", budget_holder$name, email_body)
      email_body <- gsub("\\{surname\\}", budget_holder$surname, email_body)
      email_body <- gsub("\\{cost_center\\}", budget_holder$cost_center, email_body)
      email_body <- gsub("\\{project_name\\}", project_data$project_name, email_body)
      email_body <- gsub("\\{responsible_user\\}", project_data$responsible_user, email_body)
      email_body <- gsub("\\{num_samples\\}", project_data$num_samples, email_body)
      email_body <- gsub("\\{service_type\\}", admin_data$service_types$service_type[admin_data$service_types$id == project_data$service_type_id], email_body)
      email_body <- gsub("\\{total_cost\\}", sprintf("%.2f", project_data$total_cost), email_body)
      email_body <- gsub("\\{cost_warning\\}", cost_warning, email_body)
      
      # Test with simple content first
      test_subject <- "Test from Shiny App"
      test_body <- "This is a test email from the Shiny application."
      
      cat("DEBUG: Attempting to send email...\n")
      
      # Try the exact same call that works in command line
      send.mail(
        from = "yeroslaviz@biochem.mpg.de",
        to = c(budget_holder$email, user_email),
        #        to = "yeroslaviz@biochem.mpg.de",  # Send to myself for testing
        #        html = TRUE,  # Send as HTML
        encoding = "utf-8",
        subject = template$subject,
        body = email_body,
        #        subject = test_subject,
        #        body = test_body,
        smtp = list(
          host.name = "msx.biochem.mpg.de",
          port = 25,
          ssl = FALSE,
          tls = FALSE,
          authenticate = FALSE  # Try without auth first
        ),
        send = TRUE
      )
      
      cat("DEBUG: Email sent successfully\n")
      return(list(success = TRUE, message = "Email sent successfully"))
      
    }, error = function(e) {
      cat("DEBUG: Error occurred:", e$message, "\n")
      return(list(success = FALSE, error = e$message))
    })
  }
  
  # User registration modal
  observeEvent(input$show_register_btn, {
    showModal(modalDialog(
      title = "User Registration",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("register_btn", "Register", class = "btn-primary")
      ),
      
      textInput("reg_username", "Username *", placeholder = "Choose a username"),
      textInput("reg_email", "Email *", placeholder = "your.email@institute.org"),
      passwordInput("reg_password", "Password *", placeholder = "Choose a password"),
      passwordInput("reg_confirm_password", "Confirm Password *", placeholder = "Confirm your password"),
      tags$small("* Required fields")
    ))
  })
  
  # Login functionality
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    user_data <- dbGetQuery(con, 
                            "SELECT * FROM users WHERE username = ?", 
                            params = list(input$login_username)
    )
    
    if(nrow(user_data) == 1 && user_data$password == digest(input$login_password)) {
      user$logged_in <- TRUE
      user$username <- user_data$username
      user$user_id <- user_data$id
      user$is_admin <- as.logical(user_data$is_admin)
      
      session$sendCustomMessage("showMainApp", "show")

      # Direct JavaScript to force show/hide
      runjs("
      document.getElementById('login_screen').style.display = 'none';
      document.getElementById('main_app').style.display = 'block';
    ")
      
      # Load admin data if user is admin
      if(user$is_admin) {
        load_admin_data()
      }
    } else {
      show("login_error")
      output$login_error <- renderText("Invalid username or password")
    }
  })
  
  # Logout functionality
  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
    user$username <- NULL
    user$user_id <- NULL
    user$is_admin <- FALSE
    
    show("login_screen")
    hide("main_app")
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    hide("login_error")
  })
  
  # Welcome message
  output$welcome_user <- renderText({
    if(user$logged_in) {
      paste("Welcome,", user$username, if(user$is_admin) "(Admin)" else "")
    }
  })
  
  # Main content with tabs for admins
  output$main_content <- renderUI({
    if(!user$logged_in) return()
    
    # SIMPLE DEBUG - add this line
    return(div(style = "background: red; color: white; padding: 20px;", h1("TEST - Can you see this?")))
    
    # Comment out the rest for now
    # if(user$is_admin) {
    #   ... rest of your code ...
    # } else {
    #   ... rest of your code ...
    # }
  })
  
  # TEMPORARY DEBUG: Add visible debug output
  output$debug_visible <- renderUI({
    if(!user$logged_in) return(NULL)
    
    tagList(
      div(
        style = "position: fixed; top: 10px; right: 10px; background: red; color: white; padding: 10px; z-index: 9999;",
        h4("DEBUG INFO"),
        p(paste("Logged in:", user$logged_in)),
        p(paste("Username:", user$username)),
        p(paste("Is admin:", user$is_admin)),
        p(paste("Main app visible:", !is.null(input$main_app)))
      ),
      div(
        style = "background: yellow; padding: 20px; margin: 20px;",
        h3("MAIN CONTENT TEST - CAN YOU SEE THIS?"),
        p("If you can see this yellow box, the main content is rendering but might be hidden by CSS"),
        actionButton("test_btn", "Test Button - Click Me!")
      )
    )
  })
  
  # Load projects data
  load_projects <- function() {
    cat("DEBUG: Loading projects\n")
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    if(user$is_admin) {
      cat("DEBUG: Loading all projects (admin view)\n")
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by, t.name as type_name, 
               bh.name as budget_holder_name, bh.surname as budget_holder_surname, bh.cost_center,
               st.service_type, sd.depth_description, sc.cycles_description
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN service_types st ON p.service_type_id = st.id
        LEFT JOIN sequencing_depths sd ON p.sequencing_depth_id = sd.id
        LEFT JOIN sequencing_cycles sc ON p.sequencing_cycles_id = sc.id
        ORDER BY p.project_id DESC
      ")
    } else {
      cat("DEBUG: Loading user-specific projects\n")
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by, t.name as type_name,
               bh.name as budget_holder_name, bh.surname as budget_holder_surname, bh.cost_center,
               st.service_type, sd.depth_description, sc.cycles_description
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN service_types st ON p.service_type_id = st.id
        LEFT JOIN sequencing_depths sd ON p.sequencing_depth_id = sd.id
        LEFT JOIN sequencing_cycles sc ON p.sequencing_cycles_id = sc.id
        WHERE p.responsible_user = ? OR p.user_id = ?
        ORDER BY p.project_id DESC
      ", params = list(user$username, user$user_id))
    }
    
    cat("DEBUG: Projects loaded:", nrow(projects), "rows\n")
    projects_data(projects)
  }
  
  # Regular user interface (project application)
  output$user_interface <- renderUI({
    if(!user$logged_in) return()
    
    tagList(
      div(
        class = "action-buttons",
        actionButton("new_project_btn", "Create New Project", 
                     class = "btn-primary", icon = icon("plus")),
        actionButton("edit_project_btn", "Edit Project", 
                     class = "btn-secondary", icon = icon("edit")),
        actionButton("delete_project_btn", "Delete Project", 
                     class = "btn-danger", icon = icon("trash"))
      ),
      
      if(user$is_admin) {
        div(
          class = "action-buttons",
          actionButton("update_status_btn", "Update Project Status", 
                       class = "btn-info", icon = icon("sync"))
        )
      },
      
      div(
        class = "projects-table",
        DTOutput("projects_table")
      )
    )
  })
  
  # Admin interface (project administration)
  output$admin_interface <- renderUI({
    if(!user$logged_in || !user$is_admin) return()
    
    tagList(
      h3("Administrator Panel"),
      
      fluidRow(
        column(4,
               wellPanel(
                 h4("User Management"),
                 actionButton("manage_users_btn", "Manage Users", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Budget Holders"),
                 actionButton("manage_budget_holders_btn", "Manage Budget Holders", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Reference Genomes"),
                 actionButton("manage_genomes_btn", "Manage Genomes", class = "btn-primary")
               )
        )
      ),
      
      ##################################################################
      # SOLUTION 2: DATABASE BACKUP/RESTORE FEATURES
      ##################################################################
      fluidRow(
        column(12,
               wellPanel(
                 h4("Database Management"),
                 p("Download a complete backup of the database for safekeeping."),
                 downloadButton("download_db_btn", "Download Database Backup", 
                                class = "btn-warning", style = "color: white;"),
                 fileInput("restore_db_btn", "Restore Backup", accept = ".sqlite"),
                 actionButton("validate_db_btn", "Validate Database", class = "btn-info")
               )
        )
      ),
      ##################################################################
      # END SOLUTION 2
      ##################################################################
      
      div(
        class = "projects-table",
        DTOutput("admin_projects_table")
      )
    )
  })
  
  # Load projects when user logs in
  observeEvent(user$logged_in, {
    if(user$logged_in) {
      cat("DEBUG: User logged in, loading projects\n")
      load_projects()
    }
  })
  
  # === ADMIN MANAGEMENT MODALS ===
  
  # Budget Holders Management Modal
  observeEvent(input$manage_budget_holders_btn, {
    showModal(modalDialog(
      title = "Budget Holders Management",
      size = "l",
      footer = modalButton("Close"),
      
      h4("Add New Budget Holder"),
      fluidRow(
        column(3,
               textInput("new_bh_name", "Name", placeholder = "Enter name")
        ),
        column(3,
               textInput("new_bh_surname", "Surname", placeholder = "Enter surname")
        ),
        column(3,
               textInput("new_bh_cost_center", "Cost Center", placeholder = "Enter cost center")
        ),
        column(3,
               textInput("new_bh_email", "Email", placeholder = "Enter email")
        )
      ),
      fluidRow(
        column(12,
               actionButton("add_budget_holder_btn", "Add Budget Holder", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Existing Budget Holders"),
      DTOutput("budget_holders_table_admin"),
      actionButton("delete_budget_holder_btn", "Delete Selected Budget Holder", class = "btn-danger")
    ))
  })
  
  # Service Types Management Modal
  observeEvent(input$manage_service_types_btn, {
    showModal(modalDialog(
      title = "Service Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Service Type"),
      fluidRow(
        column(4,
               textInput("new_service_type", "Service Type", placeholder = "Enter service type")
        ),
        column(4,
               textInput("new_service_kit", "Kit", placeholder = "Enter kit name")
        ),
        column(4,
               numericInput("new_service_cost", "Cost per Sample", value = 0, min = 0)
        )
      ),
      fluidRow(
        column(12,
               actionButton("add_service_type_btn", "Add Service Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Service Types"),
      DTOutput("service_types_table_admin"),
      actionButton("delete_service_type_btn", "Delete Selected Service Type", class = "btn-danger")
    ))
  })
  
  # Sequencing Depths Management Modal
  observeEvent(input$manage_sequencing_depths_btn, {
    showModal(modalDialog(
      title = "Sequencing Depths Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Depth"),
      fluidRow(
        column(3,
               textInput("new_depth_description", "Depth Description", placeholder = "e.g., upto 200M")
        ),
        column(3,
               numericInput("new_depth_cost_150", "Cost up to 150 cycles", value = 0, min = 0)
        ),
        column(3,
               numericInput("new_depth_cost_300", "Cost up to 300 cycles", value = 0, min = 0)
        ),
        column(3,
               actionButton("add_sequencing_depth_btn", "Add Depth", class = "btn-primary", style = "margin-top: 25px;")
        )
      ),
      
      hr(),
      h4("Current Sequencing Depths"),
      DTOutput("sequencing_depths_table_admin"),
      actionButton("delete_sequencing_depth_btn", "Delete Selected Depth", class = "btn-danger")
    ))
  })
  
  # Sequencing Cycles Management Modal
  observeEvent(input$manage_sequencing_cycles_btn, {
    showModal(modalDialog(
      title = "Sequencing Cycles Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Cycle"),
      fluidRow(
        column(8,
               textInput("new_cycles_description", "Cycles Description", placeholder = "e.g., upto 100/150 cycles (2x60 or 2x 75)")
        ),
        column(4,
               actionButton("add_sequencing_cycles_btn", "Add Cycles", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Cycles"),
      DTOutput("sequencing_cycles_table_admin"),
      actionButton("delete_sequencing_cycles_btn", "Delete Selected Cycles", class = "btn-danger")
    ))
  })
  
  # Existing management modals (updated for new structure)
  observeEvent(input$manage_users_btn, {
    showModal(modalDialog(
      title = "User Management",
      size = "l",
      footer = modalButton("Close"),
      
      h4("Add New User"),
      fluidRow(
        column(6,
               textInput("new_user_username", "Username", placeholder = "Enter username")
        ),
        column(6,
               textInput("new_user_email", "Email", placeholder = "Enter email")
        )
      ),
      fluidRow(
        column(6,
               passwordInput("new_user_password", "Password", placeholder = "Enter password")
        ),
        column(6,
               passwordInput("new_user_confirm_password", "Confirm Password", placeholder = "Confirm password")
        )
      ),
      fluidRow(
        column(12,
               checkboxInput("new_user_admin", "Administrator", value = FALSE),
               actionButton("add_user_admin_btn", "Add User", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Existing Users"),
      DTOutput("users_table_admin"),
      actionButton("delete_user_admin_btn", "Delete Selected User", class = "btn-danger")
    ))
  })
  
  observeEvent(input$manage_genomes_btn, {
    showModal(modalDialog(
      title = "Reference Genome Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Reference Genome"),
      fluidRow(
        column(8,
               textInput("new_genome_name", "Genome Name", placeholder = "Enter genome name")
        ),
        column(4,
               actionButton("add_genome_btn", "Add Genome", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Reference Genomes"),
      DTOutput("genomes_table_admin"),
      actionButton("delete_genome_btn", "Delete Selected Genome", class = "btn-danger")
    ))
  })
  
  observeEvent(input$manage_types_btn, {
    showModal(modalDialog(
      title = "Project Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Project Type"),
      fluidRow(
        column(8,
               textInput("new_type_name", "Type Name", placeholder = "Enter project type name")
        ),
        column(4,
               actionButton("add_type_btn", "Add Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Project Types"),
      DTOutput("types_table_admin"),
      actionButton("delete_type_btn", "Delete Selected Type", class = "btn-danger")
    ))
  })
  
  observeEvent(input$manage_sequencing_platforms_btn, {
    showModal(modalDialog(
      title = "Sequencing Platforms Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Platform"),
      fluidRow(
        column(8,
               textInput("new_sequencing_platform_name", "Platform Name", placeholder = "Enter sequencing platform name")
        ),
        column(4,
               actionButton("add_sequencing_platform_btn", "Add Platform", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Platforms"),
      DTOutput("sequencing_platforms_table_admin"),
      actionButton("delete_sequencing_platform_btn", "Delete Selected Platform", class = "btn-danger")
    ))
  })
  
  # === ADMIN TABLES RENDERING ===
  
  output$budget_holders_table_admin <- renderDT({
    datatable(
      admin_data$budget_holders[, c("name", "surname", "cost_center", "email")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Name", "Surname", "Cost Center", "Email")
    )
  })
  
  output$service_types_table_admin <- renderDT({
    datatable(
      admin_data$service_types[, c("service_type", "kit", "costs_per_sample")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Service Type", "Kit", "Cost per Sample")
    ) %>% formatCurrency('costs_per_sample', currency = "€", digits = 2)
  })
  
  output$sequencing_depths_table_admin <- renderDT({
    datatable(
      admin_data$sequencing_depths[, c("depth_description", "cost_upto_150_cycles", "cost_upto_300_cycles")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Depth Description", "Cost up to 150 cycles", "Cost up to 300 cycles")
    ) %>% formatCurrency(c('cost_upto_150_cycles', 'cost_upto_300_cycles'), currency = "€", digits = 2)
  })
  
  output$sequencing_cycles_table_admin <- renderDT({
    datatable(
      admin_data$sequencing_cycles[, c("cycles_description")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Cycles Description")
    )
  })
  
  output$users_table_admin <- renderDT({
    datatable(
      admin_data$users[, c("username", "email", "is_admin")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Username", "Email", "Is Admin")
    )
  })
  
  output$genomes_table_admin <- renderDT({
    genomes_df <- data.frame(name = admin_data$reference_genomes)
    datatable(
      genomes_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Reference Genome Name")
    )
  })
  
  output$types_table_admin <- renderDT({
    types_df <- admin_data$types
    if(nrow(types_df) > 0) {
      types_df <- types_df[, c("id", "name"), drop = FALSE]
    }
    datatable(
      types_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Type Name")
    )
  })
  
  output$sequencing_platforms_table_admin <- renderDT({
    platforms_df <- admin_data$sequencing_platforms
    if(nrow(platforms_df) > 0) {
      platforms_df <- platforms_df[, c("id", "name"), drop = FALSE]
    }
    datatable(
      platforms_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Platform Name")
    )
  })
  
  # === ADD NEW ITEMS HANDLERS ===
  
  # Add new budget holder
  observeEvent(input$add_budget_holder_btn, {
    req(input$new_bh_name, input$new_bh_surname, input$new_bh_cost_center, input$new_bh_email)
    
    if(input$new_bh_name == "" || input$new_bh_surname == "" || input$new_bh_cost_center == "" || input$new_bh_email == "") {
      showNotification("Please fill in all budget holder fields", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Check if cost center already exists
    existing_bh <- dbGetQuery(con, 
                              "SELECT id FROM budget_holders WHERE cost_center = ?", 
                              params = list(input$new_bh_cost_center))
    
    if(nrow(existing_bh) > 0) {
      showNotification("Cost center already exists", type = "error")
      return()
    }
    
    # Insert new budget holder
    dbExecute(con, "
      INSERT INTO budget_holders (name, surname, cost_center, email)
      VALUES (?, ?, ?, ?)
    ", params = list(
      input$new_bh_name,
      input$new_bh_surname,
      input$new_bh_cost_center,
      input$new_bh_email
    ))
    
    # Clear form
    updateTextInput(session, "new_bh_name", value = "")
    updateTextInput(session, "new_bh_surname", value = "")
    updateTextInput(session, "new_bh_cost_center", value = "")
    updateTextInput(session, "new_bh_email", value = "")
    
    # Reload admin data
    load_admin_data()
    
    showNotification("Budget holder added successfully!", type = "message")
  })
  
  # Add new service type
  observeEvent(input$add_service_type_btn, {
    req(input$new_service_type, input$new_service_kit, input$new_service_cost)
    
    if(input$new_service_type == "") {
      showNotification("Please enter a service type", type = "error")
      return()
    }
    
    if(input$new_service_type %in% admin_data$service_types$service_type) {
      showNotification("This service type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO service_types (service_type, kit, costs_per_sample)
      VALUES (?, ?, ?)
    ", params = list(
      input$new_service_type,
      input$new_service_kit,
      input$new_service_cost
    ))
    
    admin_data$service_types <- load_service_types()
    updateTextInput(session, "new_service_type", value = "")
    updateTextInput(session, "new_service_kit", value = "")
    updateNumericInput(session, "new_service_cost", value = 0)
    showNotification("Service type added successfully!", type = "message")
  })
  
  # Add new sequencing depth
  observeEvent(input$add_sequencing_depth_btn, {
    req(input$new_depth_description)
    
    if(input$new_depth_description == "") {
      showNotification("Please enter a depth description", type = "error")
      return()
    }
    
    if(input$new_depth_description %in% admin_data$sequencing_depths$depth_description) {
      showNotification("This sequencing depth already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO sequencing_depths (depth_description, cost_upto_150_cycles, cost_upto_300_cycles)
      VALUES (?, ?, ?)
    ", params = list(
      input$new_depth_description,
      ifelse(is.na(input$new_depth_cost_150), NULL, input$new_depth_cost_150),
      ifelse(is.na(input$new_depth_cost_300), NULL, input$new_depth_cost_300)
    ))
    
    admin_data$sequencing_depths <- load_sequencing_depths()
    updateTextInput(session, "new_depth_description", value = "")
    updateNumericInput(session, "new_depth_cost_150", value = 0)
    updateNumericInput(session, "new_depth_cost_300", value = 0)
    showNotification("Sequencing depth added successfully!", type = "message")
  })
  
  # Add new sequencing cycles
  observeEvent(input$add_sequencing_cycles_btn, {
    req(input$new_cycles_description)
    
    if(input$new_cycles_description == "") {
      showNotification("Please enter a cycles description", type = "error")
      return()
    }
    
    if(input$new_cycles_description %in% admin_data$sequencing_cycles$cycles_description) {
      showNotification("This sequencing cycle already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO sequencing_cycles (cycles_description)
      VALUES (?)
    ", params = list(input$new_cycles_description))
    
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    updateTextInput(session, "new_cycles_description", value = "")
    showNotification("Sequencing cycles added successfully!", type = "message")
  })
  
  # Add new user (admin)
  observeEvent(input$add_user_admin_btn, {
    req(input$new_user_username, input$new_user_email, input$new_user_password)
    
    if(input$new_user_password != input$new_user_confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    if(nchar(input$new_user_password) < 6) {
      showNotification("Password must be at least 6 characters", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    existing_user <- dbGetQuery(con, 
                                "SELECT id FROM users WHERE username = ?", 
                                params = list(input$new_user_username))
    
    if(nrow(existing_user) > 0) {
      showNotification("Username already exists", type = "error")
      return()
    }
    
    dbExecute(con, "
      INSERT INTO users (username, password, email, is_admin)
      VALUES (?, ?, ?, ?)
    ", params = list(
      input$new_user_username,
      digest(input$new_user_password),
      input$new_user_email,
      as.integer(input$new_user_admin)
    ))
    
    updateTextInput(session, "new_user_username", value = "")
    updateTextInput(session, "new_user_email", value = "")
    updateTextInput(session, "new_user_password", value = "")
    updateTextInput(session, "new_user_confirm_password", value = "")
    updateCheckboxInput(session, "new_user_admin", value = FALSE)
    
    load_admin_data()
    showNotification("User added successfully!", type = "message")
  })
  
  # Add new reference genome
  observeEvent(input$add_genome_btn, {
    req(input$new_genome_name)
    
    if(input$new_genome_name == "") {
      showNotification("Please enter a genome name", type = "error")
      return()
    }
    
    if(input$new_genome_name %in% admin_data$reference_genomes) {
      showNotification("This genome already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO reference_genomes (name)
      VALUES (?)
    ", params = list(input$new_genome_name))
    
    admin_data$reference_genomes <- load_reference_genomes()
    updateTextInput(session, "new_genome_name", value = "")
    showNotification("Reference genome added successfully!", type = "message")
  })
  
  # Add new type
  observeEvent(input$add_type_btn, {
    req(input$new_type_name)
    
    if(input$new_type_name == "") {
      showNotification("Please enter a type name", type = "error")
      return()
    }
    
    if(input$new_type_name %in% admin_data$types$name) {
      showNotification("This type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO types (name) VALUES (?)", params = list(input$new_type_name))
    admin_data$types <- load_types()
    updateTextInput(session, "new_type_name", value = "")
    showNotification("Type added successfully!", type = "message")
  })
  
  # Add new sequencing platform
  observeEvent(input$add_sequencing_platform_btn, {
    req(input$new_sequencing_platform_name)
    
    if(input$new_sequencing_platform_name == "") {
      showNotification("Please enter a sequencing platform name", type = "error")
      return()
    }
    
    if(input$new_sequencing_platform_name %in% admin_data$sequencing_platforms$name) {
      showNotification("This platform already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO sequencing_platforms (name) VALUES (?)", params = list(input$new_sequencing_platform_name))
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    updateTextInput(session, "new_sequencing_platform_name", value = "")
    showNotification("Sequencing platform added successfully!", type = "message")
  })
  
  # === DELETE HANDLERS ===
  
  # Delete budget holder
  observeEvent(input$delete_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a budget holder to delete", type = "warning")
      return()
    }
    
    bh_to_delete <- admin_data$budget_holders[selected_row, ]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete budget holder:", bh_to_delete$name, bh_to_delete$surname, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_budget_holder_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    bh_to_delete <- admin_data$budget_holders[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM budget_holders WHERE id = ?", params = list(bh_to_delete$id))
    
    removeModal()
    load_admin_data()
    showNotification("Budget holder deleted successfully!", type = "message")
  })
  
  # Delete service type
  observeEvent(input$delete_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a service type to delete", type = "warning")
      return()
    }
    
    service_to_delete <- admin_data$service_types[selected_row, "service_type"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete service type:", service_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_service_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    service_to_delete <- admin_data$service_types[selected_row, "service_type"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM service_types WHERE service_type = ?", params = list(service_to_delete))
    
    removeModal()
    admin_data$service_types <- load_service_types()
    showNotification("Service type deleted successfully!", type = "message")
  })
  
  # Delete sequencing depth
  observeEvent(input$delete_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing depth to delete", type = "warning")
      return()
    }
    
    depth_to_delete <- admin_data$sequencing_depths[selected_row, "depth_description"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing depth:", depth_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_depth_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    depth_to_delete <- admin_data$sequencing_depths[selected_row, "depth_description"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_depths WHERE depth_description = ?", params = list(depth_to_delete))
    
    removeModal()
    admin_data$sequencing_depths <- load_sequencing_depths()
    showNotification("Sequencing depth deleted successfully!", type = "message")
  })
  
  # Delete sequencing cycles
  observeEvent(input$delete_sequencing_cycles_btn, {
    selected_row <- input$sequencing_cycles_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing cycle to delete", type = "warning")
      return()
    }
    
    cycles_to_delete <- admin_data$sequencing_cycles[selected_row, "cycles_description"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing cycle:", cycles_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_cycles_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_cycles_btn, {
    selected_row <- input$sequencing_cycles_table_admin_rows_selected
    cycles_to_delete <- admin_data$sequencing_cycles[selected_row, "cycles_description"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_cycles WHERE cycles_description = ?", params = list(cycles_to_delete))
    
    removeModal()
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    showNotification("Sequencing cycles deleted successfully!", type = "message")
  })
  
  # Delete user
  observeEvent(input$delete_user_admin_btn, {
    selected_row <- input$users_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a user to delete", type = "warning")
      return()
    }
    
    user_to_delete <- admin_data$users[selected_row, ]
    
    if(user_to_delete$username == user$username) {
      showNotification("You cannot delete your own account", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete user:", user_to_delete$username, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_user_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_user_btn, {
    selected_row <- input$users_table_admin_rows_selected
    user_to_delete <- admin_data$users[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM users WHERE id = ?", params = list(user_to_delete$id))
    
    removeModal()
    load_admin_data()
    showNotification("User deleted successfully!", type = "message")
  })
  
  # Delete reference genome
  observeEvent(input$delete_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a reference genome to delete", type = "warning")
      return()
    }
    
    genome_to_delete <- admin_data$reference_genomes[selected_row]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete reference genome:", genome_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_genome_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    genome_to_delete <- admin_data$reference_genomes[selected_row]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM reference_genomes WHERE name = ?", params = list(genome_to_delete))
    
    removeModal()
    admin_data$reference_genomes <- load_reference_genomes()
    showNotification("Reference genome deleted successfully!", type = "message")
  })
  
  # Delete type
  observeEvent(input$delete_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a type to delete", type = "warning")
      return()
    }
    
    type_to_delete <- admin_data$types[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete type:", type_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    type_to_delete <- admin_data$types[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM types WHERE name = ?", params = list(type_to_delete))
    
    removeModal()
    admin_data$types <- load_types()
    showNotification("Type deleted successfully!", type = "message")
  })
  
  # Delete sequencing platform
  observeEvent(input$delete_sequencing_platform_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing platform to delete", type = "warning")
      return()
    }
    
    platform_to_delete <- admin_data$sequencing_platforms[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing platform:", platform_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_platform_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_platform_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    platform_to_delete <- admin_data$sequencing_platforms[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_platforms WHERE name = ?", params = list(platform_to_delete))
    
    removeModal()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    showNotification("Sequencing platform deleted successfully!", type = "message")
  })
  
  # === EDIT PROJECT FUNCTIONALITY ===
  
  # Edit project modal
  observeEvent(input$edit_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to edit", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    can_edit <- user$is_admin || 
      project$user_id == user$user_id || 
      project$responsible_user == user$username
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Edit Project",
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_project_btn", "Update Project", class = "btn-primary")
      ),
      
      fluidRow(
        column(6,
               textInput("edit_project_name", "Project Name *", value = project$project_name),
               numericInput("edit_num_samples", "Number of Samples *", value = project$num_samples, min = 1),
               selectInput("edit_sequencing_platform", "Sequencing Platform *",
                           choices = admin_data$sequencing_platforms$name,
                           selected = project$sequencing_platform),
               selectInput("edit_service_type_id", "Service Type *",
                           choices = setNames(admin_data$service_types$id, 
                                              paste(admin_data$service_types$service_type, 
                                                    "- €", admin_data$service_types$costs_per_sample, "/sample")),
                           selected = project$service_type_id),
               selectInput("edit_type_id", "Project Type *",
                           choices = setNames(admin_data$types$id, admin_data$types$name),
                           selected = project$type_id)
        ),
        column(6,
               selectInput("edit_reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes,
                           selected = project$reference_genome),
               selectInput("edit_sequencing_depth_id", "Sequencing Depth *",
                           choices = setNames(admin_data$sequencing_depths$id, 
                                              admin_data$sequencing_depths$depth_description),
                           selected = project$sequencing_depth_id),
               selectInput("edit_sequencing_cycles_id", "Sequencing Cycles *",
                           choices = setNames(admin_data$sequencing_cycles$id, 
                                              admin_data$sequencing_cycles$cycles_description),
                           selected = project$sequencing_cycles_id),
               selectInput("edit_budget_id", "Budget Holder *",
                           choices = setNames(admin_data$budget_holders$id,
                                              paste(admin_data$budget_holders$name, 
                                                    admin_data$budget_holders$surname, 
                                                    "-", admin_data$budget_holders$cost_center)),
                           selected = project$budget_id),
               selectInput("edit_responsible_user", "Responsible User *",
                           choices = get_all_usernames(),
                           selected = project$responsible_user),
               radioButtons("edit_kickoff_meeting", "Kick-off Meeting Required? *",
                            choices = c("Yes" = 1, "No" = 0), 
                            selected = as.character(project$kickoff_meeting), inline = TRUE)
        )
      ),
      fluidRow(
        column(12,
               textAreaInput("edit_project_description", "Project Description", 
                             value = project$description, rows = 4)
        )
      ),
      # Cost calculation for edit
      fluidRow(
        column(12,
               div(class = "cost-calculation",
                   h4("Cost Calculation"),
                   uiOutput("edit_cost_calculation_display")
               )
        )
      ),
      if(user$is_admin) {
        fluidRow(
          column(12,
                 selectInput("edit_project_status", "Project Status *",
                             choices = status_options,
                             selected = project$status)
          )
        )
      }
    ))
  })
  
  # Edit cost calculation
  output$edit_cost_calculation_display <- renderUI({
    total_cost <- calculate_total_cost(input$edit_num_samples, input$edit_service_type_id, 
                                       input$edit_sequencing_depth_id, input$edit_sequencing_cycles_id)
    
    service_type <- admin_data$service_types[admin_data$service_types$id == as.numeric(input$edit_service_type_id), ]
    sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == as.numeric(input$edit_sequencing_depth_id), ]
    
    prep_cost <- if(nrow(service_type) > 0) service_type$costs_per_sample * as.numeric(input$edit_num_samples) else 0
    
    tagList(
      p(paste("Preparation Cost: €", prep_cost)),
      p(paste("Sequencing Cost: €", total_cost - prep_cost)),
      p(class = "cost-total", paste("Total Estimated Cost: €", total_cost)),
      if(!is.null(sequencing_depth) && nrow(sequencing_depth) > 0 && sequencing_depth$depth_description == "other") {
        p(class = "cost-warning", 
          "Note: 'Other' sequencing depth selected. These costs are preliminary. Please contact us to discuss your specific needs.")
      }
    )
  })
  
  # Update project
  observeEvent(input$update_project_btn, {
    selected_row <- input$projects_table_rows_selected
    project <- projects_data()[selected_row, ]
    project_id <- project$id
    
    can_edit <- user$is_admin || 
      project$user_id == user$user_id || 
      project$responsible_user == user$username
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }
    
    # Calculate total cost
    total_cost <- calculate_total_cost(input$edit_num_samples, input$edit_service_type_id, 
                                       input$edit_sequencing_depth_id, input$edit_sequencing_cycles_id)
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    if(user$is_admin) {
      dbExecute(con, "
        UPDATE projects 
        SET project_name = ?, reference_genome = ?, service_type_id = ?, 
            budget_id = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
            num_samples = ?, sequencing_platform = ?, sequencing_depth_id = ?,
            sequencing_cycles_id = ?, kickoff_meeting = ?, status = ?,
            type_id = ?, total_cost = ?
        WHERE id = ?
      ", params = list(
        input$edit_project_name,
        input$edit_reference_genome,
        as.numeric(input$edit_service_type_id),
        as.numeric(input$edit_budget_id),
        input$edit_responsible_user,
        input$edit_project_description,
        input$edit_num_samples,
        input$edit_sequencing_platform,
        as.numeric(input$edit_sequencing_depth_id),
        as.numeric(input$edit_sequencing_cycles_id),
        as.numeric(input$edit_kickoff_meeting),
        input$edit_project_status,
        as.numeric(input$edit_type_id),
        total_cost,
        project_id
      ))
    } else {
      dbExecute(con, "
        UPDATE projects 
        SET project_name = ?, reference_genome = ?, service_type_id = ?, 
            budget_id = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
            num_samples = ?, sequencing_platform = ?, sequencing_depth_id = ?,
            sequencing_cycles_id = ?, kickoff_meeting = ?,
            type_id = ?, total_cost = ?
        WHERE id = ?
      ", params = list(
        input$edit_project_name,
        input$edit_reference_genome,
        as.numeric(input$edit_service_type_id),
        as.numeric(input$edit_budget_id),
        input$edit_responsible_user,
        input$edit_project_description,
        input$edit_num_samples,
        input$edit_sequencing_platform,
        as.numeric(input$edit_sequencing_depth_id),
        as.numeric(input$edit_sequencing_cycles_id),
        as.numeric(input$edit_kickoff_meeting),
        as.numeric(input$edit_type_id),
        total_cost,
        project_id
      ))
    }
    
    removeModal()
    load_projects()
    showNotification("Project updated successfully!", type = "message")
  })
  
  # === DELETE PROJECT ===
  
  observeEvent(input$delete_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to delete", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    can_delete <- user$is_admin || project$user_id == user$user_id
    
    if(!can_delete) {
      showNotification("You don't have permission to delete this project", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete project:", project$project_name, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_btn, {
    selected_row <- input$projects_table_rows_selected
    project_id <- projects_data()[selected_row, "id"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM projects WHERE id = ?", params = list(project_id))
    
    removeModal()
    load_projects()
    showNotification("Project deleted successfully!", type = "message")
  })
  
  # === UPDATE STATUS ===
  
  observeEvent(input$update_status_btn, {
    if(!user$is_admin) {
      showNotification("Only administrators can update project status", type = "error")
      return()
    }
    
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to update status", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    showModal(modalDialog(
      title = "Update Project Status",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_status_update_btn", "Update Status", class = "btn-primary")
      ),
      
      h4(paste("Project:", project$project_name)),
      p(paste("Current Status:", project$status)),
      
      selectInput("new_project_status", "New Status:",
                  choices = status_options,
                  selected = project$status),
      
      textAreaInput("status_notes", "Status Notes (Optional):", 
                    placeholder = "Add any notes about this status change...",
                    rows = 3)
    ))
  })
  
  observeEvent(input$confirm_status_update_btn, {
    selected_row <- input$projects_table_rows_selected
    project <- projects_data()[selected_row, ]
    project_id <- project$id
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      UPDATE projects 
      SET status = ?, updated_at = CURRENT_TIMESTAMP
      WHERE id = ?
    ", params = list(
      input$new_project_status,
      project_id
    ))
    
    removeModal()
    load_projects()
    showNotification("Project status updated successfully!", type = "message")
  })
  
  ##################################################################
  # DATABASE BACKUP/RESTORE HANDLERS
  ##################################################################
  
  # Database validation button
  observeEvent(input$validate_db_btn, {
    if (validate_and_repair_database()) {
      showNotification("Database validation successful!", type = "message")
    }
  })
  
  # Enhanced backup handler
  output$download_db_btn <- downloadHandler(
    filename = function() {
      paste0("sequencing_db_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite")
    },
    content = function(file) {
      showNotification("Creating database backup...", type = "message", duration = NULL)
      
      tryCatch({
        if (!file.exists("sequencing_projects.db")) {
          stop("Database file not found")
        }
        
        # Create backup
        file.copy("sequencing_projects.db", file)
        
        # Log the backup
        con <- get_db_connection()
        on.exit(dbDisconnect(con))
        
        file_info <- file.info("sequencing_projects.db")
        dbExecute(con, "
          INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
          VALUES (?, ?, ?)
        ", params = list(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          file_info$size,
          user$username
        ))
        
        showNotification("Backup created successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Backup failed:", e$message), type = "error")
      })
    },
    contentType = "application/x-sqlite3"
  )
  
  # Restore handler
  observeEvent(input$restore_db_btn, {
    req(input$restore_db_btn)
    
    showModal(modalDialog(
      title = "Confirm Database Restore",
      tagList(
        p("WARNING: This will replace the current database with the backup file."),
        p("All current data will be lost!"),
        p("Backup file:", input$restore_db_btn$name),
        br(),
        p("Are you sure you want to continue?")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_restore_btn", "Restore Database", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_restore_btn, {
    tryCatch({
      # Backup current database first
      if(file.exists("sequencing_projects.db")) {
        backup_name <- paste0("sequencing_projects_pre_restore_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".db")
        file.copy("sequencing_projects.db", backup_name)
      }
      
      # Restore from uploaded file
      file.copy(input$restore_db_btn$datapath, "sequencing_projects.db", overwrite = TRUE)
      
      removeModal()
      showNotification("Database restored successfully! Previous database backed up.", type = "message")
      
      # Reload app
      session$reload()
      
    }, error = function(e) {
      showNotification(paste("Restore failed:", e$message), type = "error")
    })
  })
  
  ##################################################################
  # END DATABASE BACKUP/RESTORE HANDLERS
  ##################################################################
  
  ##################################################################
  # AUTOMATIC BI-WEEKLY BACKUPS (COMMENTED OUT)
  ##################################################################
  #
  # To activate automatic bi-weekly backups, remove the # comments below
  # and adjust the backup directory path if needed.
  #
  # observe({
  #   # Check if backup needed (every 14 days)
  #   invalidateLater(14 * 24 * 60 * 60 * 1000) # 14 days in milliseconds
  #   
  #   backup_dir <- "auto_backups"
  #   if (!dir.exists(backup_dir)) dir.create(backup_dir)
  #   
  #   # Create backup filename with bi-weekly indicator
  #   current_date <- Sys.Date()
  #   bi_week <- as.numeric(format(current_date, "%U")) %/% 2  # Bi-weekly indicator
  #   backup_file <- file.path(backup_dir, paste0("backup_", format(current_date, "%Y"), "_biweek_", bi_week, ".sqlite"))
  #   
  #   if (!file.exists(backup_file) && file.exists("sequencing_projects.db")) {
  #     tryCatch({
  #       file.copy("sequencing_projects.db", backup_file)
  #       cat("Automatic bi-weekly backup created:", backup_file, "\n")
  #       
  #       # Log the automatic backup
  #       con <- get_db_connection()
  #       file_info <- file.info("sequencing_projects.db")
  #       dbExecute(con, "
  #         INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
  #         VALUES (?, ?, ?)
  #       ", params = list(
  #         format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  #         file_info$size,
  #         'auto_backup_system'
  #       ))
  #       dbDisconnect(con)
  #       
  #     }, error = function(e) {
  #       cat("Automatic backup failed:", e$message, "\n")
  #     })
  #   }
  # })
  #
  ##################################################################
  # END AUTOMATIC BI-WEEKLY BACKUPS
  ##################################################################
  
}

# Run the application
shinyApp(ui = ui, server = server)