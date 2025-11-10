# app.R
library(DBI)
library(RSQLite)
library(digest)
library(DT)
library(shinyWidgets)
library(shiny)
library(shinyjs)

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
      
      # Header
      div(
        class = "app-header",
        h1("Sequencing Project Application System"),
        div(
          class = "user-info",
          textOutput("welcome_user"),
          actionButton("logout_btn", "Logout", class = "btn-logout")
        )
      ),
      
      # Main content
      fluidRow(
        column(12,
               # Regular user interface
               uiOutput("user_interface"),
               
               # Admin interface (only for admins)
               uiOutput("admin_interface")
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
  
  # Reactive values for admin management - now loaded from DB
  admin_data <- reactiveValues(
    users = data.frame(),
    reference_genomes = c(),
    budget_groups = c()
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
  
  # Projects table with FIXED COLUMN ALIGNMENT
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
          list(width = '100px', targets = 8)  # Status
        )
      ),
      rownames = FALSE,
      colnames = column_names
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
                                       "Oxford Nanopore", "Aviti")),
               selectInput("sequencing_kit", "Sequencing Kit *",
                           choices = c("SP", "S1", "S2", "S3", "S4", "ONT", "Mid", "High"))
        ),
        column(6,
               selectInput("reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes),
               selectInput("sample_type", "Sample Type *",
                           choices = c("DNA", "RNA", "ChIP-seq", "ATAC-seq", "WGS", "WES")),
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
                         "index_type", "seq_length", "budget_group", "responsible_user")
    
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
     num_samples, sequencing_platform, sequencing_kit, index_type, seq_length, kickoff_meeting)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
    as.numeric(input$kickoff_meeting)
  ))
    
    removeModal()
    load_projects()
    showNotification("Project created successfully!", type = "message")
  })
  
  # Edit project modal
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
      )
    ))
  })
  
  # Update project
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