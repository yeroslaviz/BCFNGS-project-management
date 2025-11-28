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
    "),
    # JavaScript for Enter key login
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
  
  # Login screen
  div(
    id = "login_screen",
    class = "login-container",
    div(
      class = "login-box",
      h2("Sequencing Project Application", class = "login-title"),
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
      
      # Main content with tabs for admins
      uiOutput("main_content")
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
    budget_groups = c()
  )
  
  # Define status options
  status_options <- c(
    "Application received",
    "Under review", 
    "Approved",
    "Rejected",
    "Sequencing in progress",
    "Data delivered",
    "Project completed"
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
  
  # Load admin data
  load_admin_data <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Load users
    admin_data$users <- dbGetQuery(con, "SELECT id, username, email, is_admin FROM users")
    
    # Load budget groups and reference genomes from database
    admin_data$budget_groups <- load_budget_groups()
    admin_data$reference_genomes <- load_reference_genomes()
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
            tags$li("Sample type: What kind of sample did you submitted to us? (total RNA, DNA, and Final Library)"),
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
        SELECT p.*, u.username as created_by
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        ORDER BY p.project_id DESC
      ")
    } else {
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by
        FROM projects p 
        JOIN users u ON p.user_id = u.id
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
      
      # Admin-only status update button (only show in admin tab)
      if(user$is_admin && !is.null(input$admin_tabs) && input$admin_tabs == "Administer Projects") {
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
        )
      ),
      
      # Projects table will be shown here as well for admin management
      div(
        class = "projects-table",
        DTOutput("admin_projects_table")
      )
    )
  })

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
  
  # Load projects data
  load_projects <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    if(user$is_admin) {
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        ORDER BY p.project_id DESC
      ")
    } else {
      projects <- dbGetQuery(con, "
        SELECT p.*, u.username as created_by
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        WHERE p.responsible_user = ? OR p.user_id = ?
        ORDER BY p.project_id DESC
      ", params = list(user$username, user$user_id))
    }
    
    projects_data(projects)
  }
  
  # Regular user interface
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
      
      # Admin-only status update button
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
  
  # Admin interface
  output$admin_interface <- renderUI({
    if(!user$logged_in || !user$is_admin) return()
    
    tagList(
      hr(),
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
        )
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
    
    # Define the columns we want to show in the correct order
    display_columns <- c("project_id", "project_name", "num_samples", "sequencing_platform", 
                         "reference_genome", "sample_type", "responsible_user", "budget_group", "status")
    
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
      "Responsible User", 
      "Budget Group", 
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
          list(width = '80px', targets = 0),  # Project ID
          list(width = '200px', targets = 1), # Project Name
          list(width = '80px', targets = 2),  # Samples
          list(width = '150px', targets = 3), # Platform
          list(width = '120px', targets = 4), # Reference
          list(width = '120px', targets = 5), # Sample Type
          list(width = '150px', targets = 6), # Responsible User
          list(width = '120px', targets = 7), # Budget Group
          list(width = '150px', targets = 8)  # Status
        )
      ),
      rownames = FALSE,
      colnames = column_names
    ) %>%
      formatStyle(
        'status',
        backgroundColor = styleEqual(
          status_options,
          c('#fff3cd', '#cce7ff', '#d4edda', '#f8d7da', '#e2e3e5', '#d1ecf1', '#d4edda')
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
                           choices = c("Illumina NovaSeq", "Illumina MiSeq", "Illumina HiSeq", 
                                       "PacBio", "Oxford Nanopore", "Ion Torrent")),
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
               selectInput("sample_type", "Sample Type *",
                           choices = c("total RNA", "genomic DNA", "ChIP/fragmented DNA", 
                                       "cDNA", "single-stranded DNA", "DNA - Final library")),
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
                         "sequencing_kit", "reference_genome", "sample_type", 
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
    (project_name, user_id, responsible_user, reference_genome, sample_type, budget_group, description,
     num_samples, sequencing_platform, sequencing_kit, index_type, seq_length, kickoff_meeting, 
     type_id, sequencing_type_id, status)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Application received')
  ", params = list(
    input$project_name,
    user$user_id,
    input$responsible_user,
    input$reference_genome,
    input$sample_type,
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
                           choices = c("Illumina NovaSeq", "Illumina MiSeq", "Illumina HiSeq", 
                                       "PacBio", "Oxford Nanopore", "Ion Torrent"),
                           selected = project$sequencing_platform),
               selectInput("edit_sequencing_kit", "Sequencing Kit *",
                           choices = c("SP", "S1", "S2", "S3", "S4", "ONT", "Mid", "High"),
                           selected = project$sequencing_kit)
        ),
        column(6,
               selectInput("edit_reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes,
                           selected = project$reference_genome),
               selectInput("edit_sample_type", "Sample Type *",
                           choices = c("DNA", "RNA", "ChIP-seq", "ATAC-seq", "WGS", "WES"),
                           selected = project$sample_type),
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
      # In the edit project modal, add these fields after the existing ones:
      selectInput("edit_type_id", "Project Type *",
                  choices = setNames(types$id, types$name),
                  selected = project$type_id),
      selectInput("edit_sequencing_type_id", "Sequencing Type *",
                  choices = setNames(sequencing_types$id, sequencing_types$name),
                  selected = project$sequencing_type_id)
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
      SET project_name = ?, reference_genome = ?, sample_type = ?, 
          budget_group = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
          num_samples = ?, sequencing_platform = ?, sequencing_kit = ?,
          index_type = ?, seq_length = ?, kickoff_meeting = ?, status = ?
      WHERE id = ?
    ", params = list(
        input$edit_project_name,
        input$edit_reference_genome,
        input$edit_sample_type,
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
        project_id
      ))
    } else {
      dbExecute(con, "
      UPDATE projects 
      SET project_name = ?, reference_genome = ?, sample_type = ?, 
          budget_group = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
          num_samples = ?, sequencing_platform = ?, sequencing_kit = ?,
          index_type = ?, seq_length = ?, kickoff_meeting = ?
      WHERE id = ?
    ", params = list(
        input$edit_project_name,
        input$edit_reference_genome,
        input$edit_sample_type,
        input$edit_budget_group,
        input$edit_responsible_user,
        input$edit_project_description,
        input$edit_num_samples,
        input$edit_sequencing_platform,
        input$edit_sequencing_kit,
        input$edit_index_type,
        as.numeric(input$edit_seq_length),
        as.numeric(input$edit_kickoff_meeting),
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
}

# Run the application
shinyApp(ui = ui, server = server)