# app.R
library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(digest)
library(DT)
library(shinyWidgets)

# UI definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style("
      /* Global background settings */
      body {
        background-color: #f8f9fa !important;
        margin: 0;
        padding: 0;
        min-height: 100vh;
      }
      
      /* Modal dialog styling */
      .modal-body .form-group {
        margin-bottom: 15px;
      }
      
      /* Form input container sizing */
      .shiny-input-container {
        width: 100% !important;
      }
      
      /* Action buttons container */
      .action-buttons {
        margin: 20px 0;
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }
      
      /* Projects table spacing */
      .projects-table {
        margin-top: 20px;
      }
      
      /* Admin management table spacing */
      .admin-management-table {
        margin: 15px 0;
      }
      
      /* DataTables responsive wrapper */
      .dataTables_wrapper {
        overflow-x: auto;
      }
      
      /* Home page main container */
      .home-page {
        text-align: center;
        padding: 40px 20px;
      }
      
      /* Home page title styling */
      .home-title {
        color: #2c3e50;
        margin-bottom: 30px;
        font-size: 2.5em;
      }
      
      /* Home page subtitle styling */
      .home-subtitle {
        color: #34495e;
        font-size: 1.3em;
        margin-bottom: 40px;
        line-height: 1.6;
      }
      
      /* Home page features grid */
      .home-features {
        display: flex;
        justify-content: center;
        gap: 30px;
        margin-top: 40px;
        flex-wrap: wrap;
      }
      
      /* Feature card styling */
      .feature-card {
        background: white;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        width: 300px;
        text-align: center;
      }
      
      /* Feature icon styling */
      .feature-icon {
        font-size: 3em;
        color: #3498db;
        margin-bottom: 20px;
      }
      
      /* Feature title styling */
      .feature-title {
        font-size: 1.5em;
        color: #2c3e50;
        margin-bottom: 15px;
      }
      
      /* Feature description styling */
      .feature-description {
        color: #7f8c8d;
        line-height: 1.5;
      }
      
      /* Page title styling (below header) */
      .page-title {
        color: #2c3e50;
        text-align: center;
        margin: 20px 0;
        font-size: 2em;
        font-weight: bold;
      }
      
      /* Header styles - Light Blue background */
      .app-header, .login-header {
        background-color: #e3f2fd !important;
        border-bottom: 2px solid #bbdefb;
        padding: 15px 0;
      }
      
      /* Header container layout */
      .header-container, .login-header-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
        padding: 0 20px;
        max-width: 1200px;
        margin: 0 auto;
      }
      
      /* Left side of header */
      .header-left, .login-header-left {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      /* Right side of header */
      .header-right, .login-header-right {
        display: flex;
        align-items: center;
        gap: 15px;
      }
      
      /* Logo sizing in headers */
      .header-logo, .login-header-logo {
        height: 50px;
        width: auto;
      }
      
      /* Institute name text styling */
      .institute-name, .login-institute-name {
        color: #1565c0;
        font-size: 1.2em;
        font-weight: bold;
        margin: 0;
      }
      
      /* User info panel in header */
      .user-info-panel {
        display: flex;
        align-items: center;
        gap: 15px;
        color: #1565c0;
      }
      
      /* Instructions panel styling */
      .instructions-panel {
        background-color: #ffffff;
        border-left: 4px solid #3498db;
        padding: 20px;
        margin: 20px 0;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Instructions title styling */
      .instructions-title {
        color: #2c3e50;
        font-size: 1.3em;
        margin-bottom: 15px;
        font-weight: bold;
      }
      
      /* Instructions list styling */
      .instructions-list {
        color: #34495e;
        line-height: 1.6;
        margin: 0;
        padding-left: 20px;
      }
      
      /* Instructions list items */
      .instructions-list li {
        margin-bottom: 8px;
      }
      
      /* Footer styles - Light Blue background */
      .app-footer {
        background-color: #e3f2fd !important;
        color: #1565c0;
        padding: 10px 0; # Reduced padding to accommodate larger logo
        border-top: 2px solid #bbdefb;
        width: 100%;
        margin-top: auto;
      }
      
      /* Footer container layout */
      .footer-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 20px;
      }
      
      /* Left side of footer */
      .footer-left {
        flex: 1;
        text-align: left;
      }
      
      /* Center of footer */
      .footer-center {
        flex: 2;
        text-align: center;
      }
      
      /* Right side of footer */
      .footer-right {
        flex: 1;
        text-align: right;
      }
      
      /* Footer logo sizing */
      .footer-logo {
        height: 80px;
        width: auto;
      }
      
      /* Footer text styling */
      .footer-text {
        margin: 5px 0;
        font-size: 0.9em;
        color: #1565c0;
      }
      
      /* Footer link styling */
      .footer-link {
        color: #1976d2;
        text-decoration: none;
        font-weight: 500;
      }
      
      /* Footer link hover effect */
      .footer-link:hover {
        text-decoration: underline;
        color: #0d47a1;
      }
      
      /* Login container styling */
      .login-container {
        background-color: #f8f9fa;
        min-height: 60vh;
        display: flex;
        justify-content: center;
        align-items: center;
        padding: 40px 20px;
      }
      
      /* Login box styling */
      .login-box {
        background: white;
        padding: 40px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        width: 100%;
        max-width: 400px;
      }
      
      /* Login title styling */
      .login-title {
        color: #2c3e50;
        text-align: center;
        margin-bottom: 30px;
      }
      
      /* Login button styling */
      .btn-login {
        width: 100%;
        background-color: #3498db;
        border-color: #3498db;
        margin-top: 20px;
      }
      
      /* Logout button styling */
      .btn-logout {
        background-color: #e74c3c;
        border-color: #e74c3c;
      }
      
      /* Main content wrapper */
      .main-content-wrapper {
        min-height: calc(100vh - 140px);
        background-color: #f8f9fa;
        padding: 20px 0;
      }
    "),
    
    # JavaScript for Enter key login functionality
    tags$script("
      $(document).ready(function() {
        // Bind Enter key to login button when in username or password fields
        $(document).on('keyup', '#login_username, #login_password', function(e) {
          if (e.keyCode === 13) { // 13 is the Enter key
            $('#login_btn').click();
          }
        });
      });
    ")
  ),
  
  # ============================================================================
  # LOGIN SCREEN
  # ============================================================================
  div(
    id = "login_screen",
    style = "min-height: 100vh; display: flex; flex-direction: column;",
    
    # Login page header - MPIB logo on left, Minerva on right
    div(
      class = "login-header",
      div(
        class = "login-header-container",
        # Left side: MPIB logo and facility name
        div(
          class = "login-header-left",
          img(src = "logo.png", class = "login-header-logo", alt = "MPIB Logo"),
          h3("MPIB Sequencing Core Facility", class = "login-institute-name")
        ),
        # Right side: Minerva logo only
        div(
          class = "login-header-right",
          img(src = "minerva.png", class = "login-header-logo", alt = "Minerva")
        )
      )
    ),
    
    # Page title for login screen
    div(
      class = "page-title",
      "Sequencing Project Application System"
    ),
    
    # Login form container
    div(
      class = "login-container",
      div(
        class = "login-box",
        h2("Login", class = "login-title"),
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
    
    # Footer for login page - Only bioinformatics logo on right
    div(
      class = "app-footer",
      div(
        class = "footer-container",
        # Left footer: Empty
        div(class = "footer-left"),
        # Center footer: Contact information and credits
        div(
          class = "footer-center",
          tags$p("Application developed and maintained by", class = "footer-text"),
          tags$p("MPIB Bioinformatics Core Facility", class = "footer-text"),
          tags$p(
            "Contact: ",
            tags$a(href = "mailto:omicsdesk@biochem.mpg.de", 
                   "omicsdesk@biochem.mpg.de", 
                   class = "footer-link"),
            class = "footer-text"
          )
        ),
        # Right footer: Bioinformatics logo only
        div(
          class = "footer-right",
          img(src = "bioInf_logo.png", class = "footer-logo", alt = "Bioinformatics Core Facility")
        )
      )
    )
  ),
  
  # ============================================================================
  # MAIN APPLICATION (hidden until login)
  # ============================================================================
  hidden(
    div(
      id = "main_app",
      style = "min-height: 100vh; display: flex; flex-direction: column;",
      
      # Main application header - MPIB logo on left, Minerva + user panel on right
      div(
        class = "app-header",
        div(
          class = "header-container",
          # Left side: MPIB logo and facility name
          div(
            class = "header-left",
            img(src = "logo.png", class = "header-logo", alt = "MPIB Logo"),
            h3("MPIB Sequencing Core Facility", class = "institute-name")
          ),
          # Right side: Minerva logo and user info
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
      
      # Page title for main application
      div(
        class = "page-title",
        "Sequencing Project Application System"
      ),
      
      # Main content area
      div(
        class = "main-content-wrapper",
        uiOutput("main_content")
      ),
      
      # Footer for main application - Only bioinformatics logo on right
      div(
        class = "app-footer",
        div(
          class = "footer-container",
          # Left footer: Empty
          div(class = "footer-left"),
          # Center footer: Contact information and credits
          div(
            class = "footer-center",
            tags$p("Application developed and maintained by", class = "footer-text"),
            tags$p("MPIB Bioinformatics Core Facility", class = "footer-text"),
            tags$p(
              "Contact: ",
              tags$a(href = "mailto:omicsdesk@biochem.mpg.de", 
                     "omicsdesk@biochem.mpg.de", 
                     class = "footer-link"),
              class = "footer-text"
            )
          ),
          # Right footer: Bioinformatics logo only
          div(
            class = "footer-right",
            img(src = "bioInf_logo.png", class = "footer-logo", alt = "Bioinformatics Core Facility")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive values
  user <- reactiveValues(logged_in = FALSE, username = NULL, user_id = NULL, is_admin = FALSE)
  projects_data <- reactiveVal()
  
  # Reactive values for admin management
  admin_data <- reactiveValues(
    users = data.frame(),
    reference_genomes = c(),
    budget_groups = c(),
    types = data.frame(),
    sequencing_types = data.frame(),
    sample_types = data.frame()
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
  
  # Load budget groups from database
  load_budget_groups <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    groups <- dbGetQuery(con, "SELECT name FROM budget_groups ORDER BY name")
    return(groups$name)
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
  
  # Load sequencing types from database
  load_sequencing_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    seq_types <- dbGetQuery(con, "SELECT id, name FROM sequencing_types ORDER BY name")
    return(seq_types)
  }
  
  # Load sample types from database
  load_sample_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    sample_types <- dbGetQuery(con, "SELECT id, name FROM sample_types ORDER BY name")
    return(sample_types)
  }
  
  # Load sequencing platforms from database
  load_sequencing_platforms <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    platforms <- dbGetQuery(con, "SELECT id, name FROM sequencing_platforms ORDER BY name")
    return(platforms)
  }
  
  # Load sequencing kits from database
  load_sequencing_kits <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    kits <- dbGetQuery(con, "SELECT id, name FROM sequencing_kits ORDER BY name")
    return(kits)
  }
  
  # Load read lengths from database  
  load_read_lengths <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    lengths <- dbGetQuery(con, "SELECT id, name FROM read_lengths ORDER BY name")
    return(lengths)
  }
  
  # Load admin data
  load_admin_data <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Load users
    admin_data$users <- dbGetQuery(con, "SELECT id, username, email, is_admin FROM users")
    
    # Load all reference data from database
    admin_data$budget_groups <- load_budget_groups()
    admin_data$reference_genomes <- load_reference_genomes()
    admin_data$types <- load_types()
    admin_data$sequencing_types <- load_sequencing_types()
    admin_data$sample_types <- load_sample_types()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    admin_data$sequencing_kits <- load_sequencing_kits()
    admin_data$read_lengths <- load_read_lengths()
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
  
  # Registration logic
  observeEvent(input$register_btn, {
    req(input$reg_username, input$reg_email, input$reg_password, input$reg_confirm_password)
    
    if(input$reg_password != input$reg_confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    if(nchar(input$reg_password) < 6) {
      showNotification("Password must be at least 6 characters", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Check if username exists
    existing_user <- dbGetQuery(con, 
                                "SELECT id FROM users WHERE username = ?", 
                                params = list(input$reg_username)
    )
    
    if(nrow(existing_user) > 0) {
      showNotification("Username already exists", type = "error")
      return()
    }
    
    # Insert new user
    dbExecute(con, "
    INSERT INTO users (username, password, email, is_admin)
    VALUES (?, ?, ?, 0)
  ", params = list(
    input$reg_username,
    digest(input$reg_password),
    input$reg_email
  ))
    
    removeModal()
    showNotification("Registration successful! You can now login.", type = "message")
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
      
      hide("login_screen")
      show("main_app")
      
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
    
    if(user$is_admin) {
      # Admin interface with tabs
      tabsetPanel(
        id = "admin_tabs",
        tabPanel(
          "Home",
          div(
            class = "home-page",
            h1("Welcome to the Sequencing Project Application System", class = "home-title"),
            div(
              class = "home-subtitle",
              p("This platform facilitates the submission and management of sequencing project applications."),
              p("As an administrator, you have access to both project application and management tools."),
              p("Use the tabs above to navigate between different functionalities.")
            ),
            div(
              class = "home-features",
              div(
                class = "feature-card",
                div(icon("plus-circle", class = "feature-icon")),
                h3("Apply for Projects", class = "feature-title"),
                p("Submit new sequencing project applications for your research needs.")
              ),
              div(
                class = "feature-card",
                div(icon("cogs", class = "feature-icon")),
                h3("Administer Projects", class = "feature-title"),
                p("Manage existing projects, update statuses, and oversee system settings.")
              )
            )
          )
        ),
        tabPanel(
          "Apply for Projects",
          # Instructions panel for project application
          div(
            class = "instructions-panel",
            h3("New Projects", class = "instructions-title"),
            tags$ul(
              class = "instructions-list",
              tags$li("Click on 'Create New Project' to create a project."),
              tags$li("Click on a project name to display its full details, upload additional files, define the samples and submit it to the sequencing facility."),
              tags$li("Select a project row to enable the Edit and Delete options."),
              tags$li("Deleting a project will remove all associated data, and cannot be undone."),
              tags$li("Note that editing and deleting is possible only before project submission.")
            )
          ),
          # Regular user interface for project application
          uiOutput("user_interface")
        ),
        tabPanel(
          "Administer Projects",
          # Admin interface for project management
          uiOutput("admin_interface")
        )
      )
    } else {
      # Regular user interface - direct to project management with instructions
      tagList(
        # Instructions panel for regular users
        div(
          class = "instructions-panel",
          h3("New Projects", class = "instructions-title"),
          tags$ul(
            class = "instructions-list",
            tags$li("Click on 'Create New Project' to create a project."),
            tags$li("A pop-up window will appear which allows you to provide information about your project(s). If anything is not clear, just contact us!"),
            tags$li("Project Name: The name of the project (YYYYMMDD_AB_CD format preferred, AB-Groupleader's initial, CD-Researcher's initial)"),
            tags$li("Ref. genome: Which organism & genome version are used in the project."),
            tags$li("Number of samples: Consider each specific barcoded sample, 1 final library of 10x scRNAseq contains 4 barcoded samples"),
            tags$li("Type: What kind of sequencing you want to perform (10X scRNAseq, RNAseq and ChIPseq ect.)"),
            tags$li("Sample type: What kind of sample did you submitted to us? (total RNA, genomic DNA, ChIP/fragmented DNA, cDNA, single-stranded DNA, DNA - Final library)"),
            tags$li("Sequencing Platform: Which machine should the samples be run on?"),
            tags$li("Index Type: TDB"),
            tags$li("Sequencing Kit: Mid kit (Total around 130M Reads) or High kit (Total around 400M Reads), S4 up to 1B reads, ONT, Aviti???"),
            tags$li("Reads length: Cycles = Total base pairs for Read 1 (& 2), i.g. 150 cycles = 75 bp Paired-end sequencing"),
            tags$li("Budget group: Choose your group cost center for a billing"),
            tags$li("Sequencing type: Single-end (One directional reading) or Paired-end (Both directional reading)"),
            tags$li("Note that editing and deleting is possible only before project submission.")
          )
        ),
        # Regular user interface for project management
        uiOutput("user_interface")
      )
    }
  })
  
  # Load projects data
  load_projects <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    if(user$is_admin) {
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by, t.name as type_name, 
               st.name as sequencing_type_name, sst.name as sample_type_name
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN sequencing_types st ON p.sequencing_type_id = st.id
        LEFT JOIN sample_types sst ON p.sample_type_id = sst.id
        ORDER BY p.project_id DESC
      ")
    } else {
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by, t.name as type_name, 
               st.name as sequencing_type_name, sst.name as sample_type_name
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN sequencing_types st ON p.sequencing_type_id = st.id
        LEFT JOIN sample_types sst ON p.sample_type_id = sst.id
        WHERE p.responsible_user = ? OR p.user_id = ?
        ORDER BY p.project_id DESC
      ", params = list(user$username, user$user_id))
    }
    
    projects_data(projects)
  }
  
  # Regular user interface (project application)
  output$user_interface <- renderUI({
    if(!user$logged_in) return()
    
    tagList(
      # Action buttons
      div(
        class = "action-buttons",
        actionButton("new_project_btn", "Create New Project", 
                     class = "btn-primary", icon = icon("plus")),
        actionButton("edit_project_btn", "Edit Project", 
                     class = "btn-secondary", icon = icon("edit")),
        actionButton("delete_project_btn", "Delete Project", 
                     class = "btn-danger", icon = icon("trash"))
      ),
      
      # Admin-only status update button (show for admins in any tab)
      if(user$is_admin) {
        div(
          class = "action-buttons",
          actionButton("update_status_btn", "Update Project Status", 
                       class = "btn-info", icon = icon("sync"))
        )
      },
      
      # Projects table
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
                 h4("Reference Genomes"),
                 actionButton("manage_genomes_btn", "Manage Genomes", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Budget Settings"),
                 actionButton("manage_budgets_btn", "Manage Budgets", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Project Types"),
                 actionButton("manage_types_btn", "Manage Types", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Types"),
                 actionButton("manage_sequencing_types_btn", "Manage Seq Types", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sample Types"),
                 actionButton("manage_sample_types_btn", "Manage Sample Types", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Platforms"),
                 actionButton("manage_sequencing_platforms_btn", "Manage Platforms", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Kits"),
                 actionButton("manage_kits_btn", "Manage Kits", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Read Lengths"),
                 actionButton("manage_lengths_btn", "Manage Lengths", class = "btn-primary")
               )
        )
      ),
      
      # Database Backup Section
      fluidRow(
        column(12,
               wellPanel(
                 h4("Database Management"),
                 p("Download a complete backup of the database for safekeeping."),
                 downloadButton("download_db_btn", "Download Database Backup", 
                                class = "btn-warning", style = "color: white;")
               )
        )
      ),
      
      # Projects table will be shown here as well for admin management
      div(
        class = "projects-table",
        DTOutput("admin_projects_table")
      )
    )
  })
  
  # User Management Modal
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
  
  # Reference Genome Management Modal
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
  
  # Budget Group Management Modal
  observeEvent(input$manage_budgets_btn, {
    showModal(modalDialog(
      title = "Budget Group Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Budget Group"),
      fluidRow(
        column(8,
               textInput("new_budget_group_name", "Budget Group Name", placeholder = "Enter budget group name")
        ),
        column(4,
               actionButton("add_budget_group_btn", "Add Group", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Budget Groups"),
      DTOutput("budget_groups_table_admin"),
      actionButton("delete_budget_group_btn", "Delete Selected Group", class = "btn-danger")
    ))
  })
  
  # Update Status Modal (Admin only)
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
  
  # Confirm status update
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
  
  # Users table for admin
  output$users_table_admin <- renderDT({
    datatable(
      admin_data$users[, c("username", "email", "is_admin")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Username", "Email", "Is Admin")
    )
  })
  
  # Budget groups table for admin
  output$budget_groups_table_admin <- renderDT({
    groups_df <- data.frame(name = admin_data$budget_groups)
    datatable(
      groups_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Budget Group Name")
    )
  })
  
  # Genomes table for admin
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
  
  # Types table for admin
  output$types_table_admin <- renderDT({
    # Ensure we have a proper data frame structure
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
  
  # Sequencing types table for admin
  output$sequencing_types_table_admin <- renderDT({
    # Ensure we have a proper data frame structure
    seq_types_df <- admin_data$sequencing_types
    if(nrow(seq_types_df) > 0) {
      seq_types_df <- seq_types_df[, c("id", "name"), drop = FALSE]
    }
    
    datatable(
      seq_types_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Sequencing Type Name")
    )
  })
  
  # Sample types table for admin
  output$sample_types_table_admin <- renderDT({
    # Ensure we have a proper data frame structure
    sample_types_df <- admin_data$sample_types
    if(nrow(sample_types_df) > 0) {
      sample_types_df <- sample_types_df[, c("id", "name"), drop = FALSE]
    }
    
    datatable(
      sample_types_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Sample Type Name")
    )
  })
  
  # Sequencing platforms table for admin
  output$sequencing_platforms_table_admin <- renderDT({
    # Ensure we have a proper data frame structure
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
  
  # Sequencing kits table for admin
  output$kits_table_admin <- renderDT({
    kits_df <- admin_data$sequencing_kits
    if(nrow(kits_df) > 0) {
      kits_df <- kits_df[, c("id", "name"), drop = FALSE]
    }
    
    datatable(
      kits_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Kit Name")
    )
  })
  
  # Read lengths table for admin
  output$lengths_table_admin <- renderDT({
    lengths_df <- admin_data$read_lengths
    if(nrow(lengths_df) > 0) {
      lengths_df <- lengths_df[, c("id", "name"), drop = FALSE]
    }
    
    datatable(
      lengths_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE, 
      colnames = c("ID", "Read Length")
    )
  })
  # Enhanced database backup download handler
  output$download_db_btn <- downloadHandler(
    filename = function() {
      paste0("sequencing_db_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite")
    },
    content = function(file) {
      # Show loading notification
      showNotification("Creating database backup...", type = "message", duration = NULL, id = "backup")
      
      tryCatch({
        db_file <- "sequencing_projects.db"
        
        if (!file.exists(db_file)) {
          stop("Database file not found")
        }
        
        # Get file info for logging
        file_info <- file.info(db_file)
        
        # Copy the database file
        file.copy(db_file, file)
        
        # Log the backup operation
        con <- get_db_connection()
        on.exit(dbDisconnect(con))
        
#        Create backup log entry 
        dbExecute(con, "
         INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
          VALUES (?, ?, ?)
                  ", params = list(
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    file_info$size,
                    user$username
                  ))
        
        # Close notification
        removeNotification("backup")
        showNotification(
          paste("Database backup completed successfully!", 
                "File size:", round(file_info$size/1024, 1), "KB"),
          type = "message"
        )
      }, error = function(e) {
        removeNotification("backup")
        showNotification(paste("Backup failed:", e$message), type = "error")
      })
    },
    contentType = "application/x-sqlite3"
  )

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
    
    # Check if username exists
    existing_user <- dbGetQuery(con, 
                                "SELECT id FROM users WHERE username = ?", 
                                params = list(input$new_user_username)
    )
    
    if(nrow(existing_user) > 0) {
      showNotification("Username already exists", type = "error")
      return()
    }
    
    # Insert new user
    dbExecute(con, "
      INSERT INTO users (username, password, email, is_admin)
      VALUES (?, ?, ?, ?)
    ", params = list(
      input$new_user_username,
      digest(input$new_user_password),
      input$new_user_email,
      as.integer(input$new_user_admin)
    ))
    
    # Clear form
    updateTextInput(session, "new_user_username", value = "")
    updateTextInput(session, "new_user_email", value = "")
    updateTextInput(session, "new_user_password", value = "")
    updateTextInput(session, "new_user_confirm_password", value = "")
    updateCheckboxInput(session, "new_user_admin", value = FALSE)
    
    # Reload admin data
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
    
    # Insert into database
    dbExecute(con, "
      INSERT INTO reference_genomes (name)
      VALUES (?)
    ", params = list(input$new_genome_name))
    
    # Reload from database
    admin_data$reference_genomes <- load_reference_genomes()
    
    # Clear input
    updateTextInput(session, "new_genome_name", value = "")
    
    showNotification("Reference genome added successfully!", type = "message")
  })
  
  # Add new budget group
  observeEvent(input$add_budget_group_btn, {
    req(input$new_budget_group_name)
    
    if(input$new_budget_group_name == "") {
      showNotification("Please enter a budget group name", type = "error")
      return()
    }
    
    if(input$new_budget_group_name %in% admin_data$budget_groups) {
      showNotification("This budget group already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Insert into database
    dbExecute(con, "
      INSERT INTO budget_groups (name)
      VALUES (?)
    ", params = list(input$new_budget_group_name))
    
    # Reload from database
    admin_data$budget_groups <- load_budget_groups()
    
    # Clear input
    updateTextInput(session, "new_budget_group_name", value = "")
    
    showNotification("Budget group added successfully!", type = "message")
  })
  
  # Delete user (admin)
  observeEvent(input$delete_user_admin_btn, {
    selected_row <- input$users_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a user to delete", type = "warning")
      return()
    }
    
    user_to_delete <- admin_data$users[selected_row, ]
    
    # Prevent admin from deleting themselves
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
  
  # Delete budget group
  observeEvent(input$delete_budget_group_btn, {
    selected_row <- input$budget_groups_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a budget group to delete", type = "warning")
      return()
    }
    
    group_to_delete <- admin_data$budget_groups[selected_row]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete budget group:", group_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_budget_group_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_budget_group_btn, {
    selected_row <- input$budget_groups_table_admin_rows_selected
    group_to_delete <- admin_data$budget_groups[selected_row]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM budget_groups WHERE name = ?", params = list(group_to_delete))
    
    removeModal()
    admin_data$budget_groups <- load_budget_groups()
    showNotification("Budget group deleted successfully!", type = "message")
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
  
  # Projects table with status coloring - VISIBLE TO EVERYONE
  output$projects_table <- renderDT({
    req(projects_data())
    
    # Prepare data for display - ensure we only select columns that exist
    display_data <- projects_data()
    
    # Convert kickoff_meeting to descriptive text
    if("kickoff_meeting" %in% names(display_data)) {
      display_data$kickoff_meeting <- ifelse(
        display_data$kickoff_meeting == 1, 
        "Yes, I would like to get a support.", 
        "No, I am a self-sufficient user."
      )
    }
    
    # Define the columns we want to show in the correct order
    display_columns <- c("project_id", "project_name", "num_samples", "sequencing_platform", 
                         "reference_genome", "sample_type_name", "type_name", "sequencing_type_name", 
                         "responsible_user", "budget_group", "kickoff_meeting", "status")
    
    # Add created_by for admins, created_at for all
    if(user$is_admin) {
      display_columns <- c(display_columns, "created_by", "created_at")
    } else {
      display_columns <- c(display_columns, "created_at")
    }
    
    # Select only columns that exist in the data
    available_columns <- display_columns[display_columns %in% names(display_data)]
    display_data <- display_data[, available_columns, drop = FALSE]
    
    # Define column names that match the data structure
    column_names <- c(
      "Project ID", 
      "Project Name", 
      "Samples", 
      "Platform", 
      "Reference", 
      "Sample Type", 
      "Project Type",
      "Sequencing Type",
      "Responsible User", 
      "Budget Group", 
      "Kick-off Meeting",
      "Status"
    )
    
    # Add additional columns for admins
    if(user$is_admin) {
      column_names <- c(column_names, "Created By", "Created")
    } else {
      column_names <- c(column_names, "Created")
    }
    
    datatable(
      display_data,
      selection = 'single',
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '80px', targets = 0),   # Project ID
          list(width = '200px', targets = 1),  # Project Name
          list(width = '80px', targets = 2),   # Samples
          list(width = '150px', targets = 3),  # Platform
          list(width = '120px', targets = 4),  # Reference
          list(width = '120px', targets = 5),  # Sample Type
          list(width = '150px', targets = 6),  # Project Type
          list(width = '120px', targets = 7),  # Sequencing Type
          list(width = '150px', targets = 8),  # Responsible User
          list(width = '120px', targets = 9),  # Budget Group
          list(width = '200px', targets = 10), # Kick-off Meeting (wider for text)
          list(width = '150px', targets = 11)  # Status
        )
      ),
      rownames = FALSE,
      colnames = column_names
    ) %>%
      formatStyle(
        'status',
        backgroundColor = styleEqual(
          status_options,
          c('#fff3cd',           # Created - light yellow
            '#cce7ff',           # Samples received - light blue
            '#e2e3e5',           # Library preparation - light gray
            '#d1ecf1',           # QC done - light cyan
            '#d4edda',           # On the sequencer - light green
            '#f8d7da',           # Data analysis - light red/pink
            '#d4edda'            # Data released - light green (same as completed)
          )
        )
      )
  }) 
  
  # Load projects when user logs in
  observeEvent(user$logged_in, {
    if(user$logged_in) {
      load_projects()
    }
  })
  
  # New project modal
  observeEvent(input$new_project_btn, {
    # Load dynamic data from database
    con <- get_db_connection()
    types <- dbGetQuery(con, "SELECT id, name FROM types ORDER BY name")
    sequencing_types <- dbGetQuery(con, "SELECT id, name FROM sequencing_types ORDER BY name")
    sample_types <- dbGetQuery(con, "SELECT id, name FROM sample_types ORDER BY name")
    dbDisconnect(con)
    
    showModal(modalDialog(
      title = "Create New Sequencing Project",
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_project_btn", "Create Project", class = "btn-primary")
      ),
      
      fluidRow(
        column(6,
               textInput("project_name", "Project Name *", placeholder = "Enter project name"),
               numericInput("num_samples", "Number of Samples *", value = 1, min = 1, max = 1000),
               selectInput("sequencing_platform", "Sequencing Platform *",
                           choices = admin_data$sequencing_platforms$name),
               selectInput("sequencing_kit", "Sequencing Kit *",
                           choices = c("SP", "S1", "S2", "S3", "S4", "ONT", "Mid", "High")),
               selectInput("type_id", "Project Type *",
                           choices = setNames(types$id, types$name)),
               selectInput("sequencing_type_id", "Sequencing Type *",
                           choices = setNames(sequencing_types$id, sequencing_types$name))
        ),
        column(6,
               selectInput("reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes),
               selectInput("sample_type_id", "Sample Type *",
                           choices = setNames(sample_types$id, sample_types$name)),
               selectInput("index_type", "Index Type *",
                           choices = c("Single Index", "Dual Index", "No Index")),
               selectInput("seq_length", "Read Length (cycles) *",
                           choices = c("35", "75", "150", "200", "500")),
               selectInput("budget_group", "Budget Group *",
                           choices = admin_data$budget_groups),
               selectInput("responsible_user", "Responsible User *",
                           choices = get_all_usernames(),
                           selected = user$username),
               radioButtons("kickoff_meeting", "Kick-off Meeting Required? *",
                            choices = c("Yes" = 1, "No" = 0), 
                            selected = 0, inline = TRUE)
        )
      ),
      fluidRow(
        column(12,
               textAreaInput("project_description", "Project Description", 
                             placeholder = "Describe your project objectives, samples, and any special requirements...", 
                             rows = 4)
        )
      ),
      tags$small("* Required fields")
    ))
  })
  
  # Create project
  observeEvent(input$create_project_btn, {
    # Validate required fields
    required_fields <- c("project_name", "num_samples", "sequencing_platform", 
                         "sequencing_kit", "reference_genome", "sample_type_id", 
                         "index_type", "seq_length", "budget_group", "responsible_user",
                         "type_id", "sequencing_type_id")
    
    missing_fields <- sapply(required_fields, function(field) {
      value <- input[[field]]
      is.null(value) || value == "" || (is.numeric(value) && is.na(value))
    })
    
    if(any(missing_fields)) {
      showNotification("Please fill in all required fields", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
    INSERT INTO projects 
    (project_name, user_id, responsible_user, reference_genome, sample_type_id, budget_group, description,
     num_samples, sequencing_platform, sequencing_kit, index_type, seq_length, kickoff_meeting, 
     type_id, sequencing_type_id, status)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Created')
  ", params = list(
    input$project_name,
    user$user_id,
    input$responsible_user,
    input$reference_genome,
    as.numeric(input$sample_type_id),
    input$budget_group,
    input$project_description,
    input$num_samples,
    input$sequencing_platform,
    input$sequencing_kit,
    input$index_type,
    as.numeric(input$seq_length),
    as.numeric(input$kickoff_meeting),
    as.numeric(input$type_id),
    as.numeric(input$sequencing_type_id)
  ))
    
    removeModal()
    load_projects()
    showNotification("Project created successfully!", type = "message")
  })
  
  # Edit project modal - STATUS FIELD REMOVED FOR REGULAR USERS
  observeEvent(input$edit_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to edit", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    # Check if user has permission to edit this project
    # Users can edit if they are the creator OR the responsible user OR admin
    can_edit <- user$is_admin || 
      project$user_id == user$user_id || 
      project$responsible_user == user$username
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }
    
    # Load dynamic data from database
    con <- get_db_connection()
    types <- dbGetQuery(con, "SELECT id, name FROM types ORDER BY name")
    sequencing_types <- dbGetQuery(con, "SELECT id, name FROM sequencing_types ORDER BY name")
    sample_types <- dbGetQuery(con, "SELECT id, name FROM sample_types ORDER BY name")
    dbDisconnect(con)
    
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
               selectInput("edit_sequencing_kit", "Sequencing Kit *",
                           choices = c("SP", "S1", "S2", "S3", "S4", "ONT", "Mid", "High"),
                           selected = project$sequencing_kit),
               selectInput("edit_type_id", "Project Type *",
                           choices = setNames(types$id, types$name),
                           selected = project$type_id),
               selectInput("edit_sequencing_type_id", "Sequencing Type *",
                           choices = setNames(sequencing_types$id, sequencing_types$name),
                           selected = project$sequencing_type_id)
        ),
        column(6,
               selectInput("edit_reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes,
                           selected = project$reference_genome),
               selectInput("edit_sample_type_id", "Sample Type *",
                           choices = setNames(sample_types$id, sample_types$name),
                           selected = project$sample_type_id),
               selectInput("edit_index_type", "Index Type *",
                           choices = c("Single Index", "Dual Index", "No Index"),
                           selected = project$index_type),
               selectInput("edit_seq_length", "Read Length (cycles) *",
                           choices = c("35", "75", "150", "200", "500"),
                           selected = as.character(project$seq_length)),
               selectInput("edit_budget_group", "Budget Group *",
                           choices = admin_data$budget_groups,
                           selected = project$budget_group),
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
      # Only show status field to admins
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
  
  # Update project - STATUS UPDATE ONLY FOR ADMINS
  observeEvent(input$update_project_btn, {
    selected_row <- input$projects_table_rows_selected
    project <- projects_data()[selected_row, ]
    project_id <- project$id
    
    # Check if user has permission to edit this project
    can_edit <- user$is_admin || 
      project$user_id == user$user_id || 
      project$responsible_user == user$username
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Different SQL for admins (include status) vs regular users
    if(user$is_admin) {
      dbExecute(con, "
      UPDATE projects 
      SET project_name = ?, reference_genome = ?, sample_type_id = ?, 
          budget_group = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
          num_samples = ?, sequencing_platform = ?, sequencing_kit = ?,
          index_type = ?, seq_length = ?, kickoff_meeting = ?, status = ?,
          type_id = ?, sequencing_type_id = ?
      WHERE id = ?
    ", params = list(
      input$edit_project_name,
      input$edit_reference_genome,
      as.numeric(input$edit_sample_type_id),
      input$edit_budget_group,
      input$edit_responsible_user,
      input$edit_project_description,
      input$edit_num_samples,
      input$edit_sequencing_platform,
      input$edit_sequencing_kit,
      input$edit_index_type,
      as.numeric(input$edit_seq_length),
      as.numeric(input$edit_kickoff_meeting),
      input$edit_project_status,
      as.numeric(input$edit_type_id),
      as.numeric(input$edit_sequencing_type_id),
      project_id
    ))
    } else {
      dbExecute(con, "
      UPDATE projects 
      SET project_name = ?, reference_genome = ?, sample_type_id = ?, 
          budget_group = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
          num_samples = ?, sequencing_platform = ?, sequencing_kit = ?,
          index_type = ?, seq_length = ?, kickoff_meeting = ?,
          type_id = ?, sequencing_type_id = ?
      WHERE id = ?
    ", params = list(
      input$edit_project_name,
      input$edit_reference_genome,
      as.numeric(input$edit_sample_type_id),
      input$edit_budget_group,
      input$edit_responsible_user,
      input$edit_project_description,
      input$edit_num_samples,
      input$edit_sequencing_platform,
      input$edit_sequencing_kit,
      input$edit_index_type,
      as.numeric(input$edit_seq_length),
      as.numeric(input$edit_kickoff_meeting),
      as.numeric(input$edit_type_id),
      as.numeric(input$edit_sequencing_type_id),
      project_id
    ))
    }
    
    removeModal()
    load_projects()
    showNotification("Project updated successfully!", type = "message")
  })
  
  # Delete project
  observeEvent(input$delete_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to delete", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    # Check if user has permission to delete this project
    # Users can delete if they are the creator OR admin
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
  
  # Types Management Modal
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
  
  # Sequencing Types Management Modal
  observeEvent(input$manage_sequencing_types_btn, {
    showModal(modalDialog(
      title = "Sequencing Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Type"),
      fluidRow(
        column(8,
               textInput("new_sequencing_type_name", "Sequencing Type Name", placeholder = "Enter sequencing type name")
        ),
        column(4,
               actionButton("add_sequencing_type_btn", "Add Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Types"),
      DTOutput("sequencing_types_table_admin"),
      actionButton("delete_sequencing_type_btn", "Delete Selected Type", class = "btn-danger")
    ))
  })
  
  # Sample Types Management Modal
  observeEvent(input$manage_sample_types_btn, {
    showModal(modalDialog(
      title = "Sample Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sample Type"),
      fluidRow(
        column(8,
               textInput("new_sample_type_name", "Sample Type Name", placeholder = "Enter sample type name")
        ),
        column(4,
               actionButton("add_sample_type_btn", "Add Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sample Types"),
      DTOutput("sample_types_table_admin"),
      actionButton("delete_sample_type_btn", "Delete Selected Type", class = "btn-danger")
    ))
  })
  
  # Sequencing Kits Management Modal
  observeEvent(input$manage_kits_btn, {
    showModal(modalDialog(
      title = "Sequencing Kits Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Kit"),
      fluidRow(
        column(8,
               textInput("new_kit_name", "Kit Name", placeholder = "Enter sequencing kit name")
        ),
        column(4,
               actionButton("add_kit_btn", "Add Kit", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Kits"),
      DTOutput("kits_table_admin"),
      actionButton("delete_kit_btn", "Delete Selected Kit", class = "btn-danger")
    ))
  })
  
  # Read Lengths Management Modal
  observeEvent(input$manage_lengths_btn, {
    showModal(modalDialog(
      title = "Read Lengths Management", 
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Read Length"),
      fluidRow(
        column(8,
               textInput("new_length_name", "Read Length", placeholder = "Enter read length")
        ),
        column(4,
               actionButton("add_length_btn", "Add Length", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Read Lengths"),
      DTOutput("lengths_table_admin"),
      actionButton("delete_length_btn", "Delete Selected Length", class = "btn-danger")
    ))
  })
  
  # Add new project type
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
  
  # Add new sequencing type
  observeEvent(input$add_sequencing_type_btn, {
    req(input$new_sequencing_type_name)
    
    if(input$new_sequencing_type_name == "") {
      showNotification("Please enter a sequencing type name", type = "error")
      return()
    }
    
    if(input$new_sequencing_type_name %in% admin_data$sequencing_types$name) {
      showNotification("This sequencing type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO sequencing_types (name) VALUES (?)", params = list(input$new_sequencing_type_name))
    admin_data$sequencing_types <- load_sequencing_types()
    updateTextInput(session, "new_sequencing_type_name", value = "")
    showNotification("Sequencing Type added successfully!", type = "message")
  })
  
  # Add new sample type
  observeEvent(input$add_sample_type_btn, {
    req(input$new_sample_type_name)
    
    if(input$new_sample_type_name == "") {
      showNotification("Please enter a sample type name", type = "error")
      return()
    }
    
    if(input$new_sample_type_name %in% admin_data$sample_types$name) {
      showNotification("This sample type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO sample_types (name) VALUES (?)", params = list(input$new_sample_type_name))
    admin_data$sample_types <- load_sample_types()
    updateTextInput(session, "new_sample_type_name", value = "")
    showNotification("Sample Type added successfully!", type = "message")
  })
  
  # Delete sequencing type
  observeEvent(input$delete_sequencing_type_btn, {
    selected_row <- input$sequencing_types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing type to delete", type = "warning")
      return()
    }
    
    type_to_delete <- admin_data$sequencing_types[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing type:", type_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_type_btn, {
    selected_row <- input$sequencing_types_table_admin_rows_selected
    type_to_delete <- admin_data$sequencing_types[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_types WHERE name = ?", params = list(type_to_delete))
    
    removeModal()
    admin_data$sequencing_types <- load_sequencing_types()
    showNotification("Sequencing type deleted successfully!", type = "message")
  })
  
  # Delete sample type
  observeEvent(input$delete_sample_type_btn, {
    selected_row <- input$sample_types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sample type to delete", type = "warning")
      return()
    }
    
    type_to_delete <- admin_data$sample_types[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sample type:", type_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sample_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sample_type_btn, {
    selected_row <- input$sample_types_table_admin_rows_selected
    type_to_delete <- admin_data$sample_types[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sample_types WHERE name = ?", params = list(type_to_delete))
    
    removeModal()
    admin_data$sample_types <- load_sample_types()
    showNotification("Sample type deleted successfully!", type = "message")
  })

  # Delete Type
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
  
  # Sequencing Platforms Management Modal
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
  
  # Add new sequencing kit
  observeEvent(input$add_kit_btn, {
    req(input$new_kit_name)
    
    if(input$new_kit_name == "") {
      showNotification("Please enter a kit name", type = "error")
      return()
    }
    
    if(input$new_kit_name %in% admin_data$sequencing_kits$name) {
      showNotification("This kit already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO sequencing_kits (name) VALUES (?)", params = list(input$new_kit_name))
    admin_data$sequencing_kits <- load_sequencing_kits()
    updateTextInput(session, "new_kit_name", value = "")
    showNotification("Sequencing kit added successfully!", type = "message")
  })
  
  # Delete sequencing kit
  observeEvent(input$delete_kit_btn, {  # has to match the button ID from modal
    selected_row <- input$kits_table_admin_rows_selected  # has to match table output ID
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing kit to delete", type = "warning")
      return()
    }
    
    kit_to_delete <- admin_data$sequencing_kits[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing kit:", kit_to_delete, "?"), 
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_kit_btn", "Delete", class = "btn-danger")  # Consistent naming
      )
    ))
  })
  
  observeEvent(input$confirm_delete_kit_btn, {  # has to match the confirm button ID
    selected_row <- input$kits_table_admin_rows_selected  # Consistent table ID
    kit_to_delete <- admin_data$sequencing_kits[selected_row, "name"]  # Fixed variable name
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_kits WHERE name = ?", params = list(kit_to_delete))
    
    removeModal()
    admin_data$sequencing_kits <- load_sequencing_kits()
    showNotification("Sequencing kit deleted successfully!", type = "message")
  })

    # Add new read length
  observeEvent(input$add_length_btn, {
    req(input$new_length_name)
    
    if(input$new_length_name == "") {
      showNotification("Please enter a read length", type = "error")
      return()
    }
    
    if(input$new_length_name %in% admin_data$read_lengths$name) {
      showNotification("This read length already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO read_lengths (name) VALUES (?)", params = list(input$new_length_name))
    admin_data$read_lengths <- load_read_lengths()
    updateTextInput(session, "new_length_name", value = "")
    showNotification("Read length added successfully!", type = "message")
  })
  
  # Delete read length 
  observeEvent(input$delete_length_btn, {  # has to match the button ID from modal
    selected_row <- input$lengths_table_admin_rows_selected  # has to match table output ID
    if(length(selected_row) == 0) {
      showNotification("Please select a read length to delete", type = "warning")  # Fixed text
      return()
    }
    
    length_to_delete <- admin_data$read_lengths[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete this read length:", length_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_length_btn", "Delete", class = "btn-danger")  # Consistent naming
      )
    ))
  })
  
  observeEvent(input$confirm_delete_length_btn, {  # has to match the confirm button ID
    selected_row <- input$lengths_table_admin_rows_selected  # Consistent table ID
    length_to_delete <- admin_data$read_lengths[selected_row, "name"]  # Fixed variable name
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM read_lengths WHERE name = ?", params = list(length_to_delete))
    
    removeModal()
    admin_data$read_lengths <- load_read_lengths()
    showNotification("Read length deleted successfully!", type = "message")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)