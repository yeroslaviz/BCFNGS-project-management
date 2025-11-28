# setup_complete_database.R
library(DBI)
library(RSQLite)
library(digest)

setup_complete_database <- function() {
  
  # Delete old database if it exists
  if(file.exists("sequencing_projects.db")) {
    file.remove("sequencing_projects.db")
    message("Old database deleted")
  }
  
  # Connect to new SQLite database
  con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
  
  # Users table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password TEXT NOT NULL,
      email TEXT NOT NULL,
      is_admin INTEGER DEFAULT 0,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Projects table with all modifications
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER UNIQUE,
      project_name TEXT NOT NULL,
      user_id INTEGER NOT NULL,
      responsible_user TEXT NOT NULL,
      reference_genome TEXT NOT NULL,
      sample_type TEXT NOT NULL,
      budget_group TEXT NOT NULL,
      description TEXT,
      num_samples INTEGER,
      sequencing_platform TEXT,
      sequencing_kit TEXT,
      index_type TEXT,
      seq_length INTEGER,
      kickoff_meeting INTEGER,
      status TEXT DEFAULT 'pending',
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users (id)
    )
  ")
  
  # Settings tables for budget groups and reference genomes
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS budget_groups (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reference_genomes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create trigger to auto-generate project_id
  dbExecute(con, "
    CREATE TRIGGER IF NOT EXISTS auto_project_id 
    AFTER INSERT ON projects
    FOR EACH ROW
    WHEN NEW.project_id IS NULL
    BEGIN
      UPDATE projects 
      SET project_id = (SELECT COALESCE(MAX(project_id), 0) + 1 FROM projects) 
      WHERE id = NEW.id;
    END;
  ")
  
  # Insert default admin user
  default_password <- digest::digest("admin123") # Change this in production!
  dbExecute(con, "
    INSERT OR IGNORE INTO users (username, password, email, is_admin) 
    VALUES ('admin', ?, 'admin@institute.org', 1)
  ", params = list(default_password))
  
  # Insert some sample users
  sample_users <- data.frame(
    username = c('user1', 'user2'),
    password = c(digest('user1123'), digest('user2123')),
    email = c('user1@institute.org', 'user2@institute.org'),
    is_admin = c(0, 0)
  )
  
  for(i in 1:nrow(sample_users)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO users (username, password, email, is_admin) 
      VALUES (?, ?, ?, ?)
    ", params = list(
      sample_users$username[i],
      sample_users$password[i],
      sample_users$email[i],
      sample_users$is_admin[i]
    ))
  }
  
  # Insert default budget groups
  default_budget_groups <- c("Group A", "Group B", "Group C", "Group D", "External", "Internal")
  for(group in default_budget_groups) {
    dbExecute(con, "
      INSERT OR IGNORE INTO budget_groups (name) 
      VALUES (?)
    ", params = list(group))
  }
  
  # Insert default reference genomes
  default_genomes <- c("GRCh38", "GRCh37", "mm10", "rn6", "Custom")
  for(genome in default_genomes) {
    dbExecute(con, "
      INSERT OR IGNORE INTO reference_genomes (name) 
      VALUES (?)
    ", params = list(genome))
  }
  
  # Insert some sample projects to test the project_id generation
  sample_projects <- data.frame(
    project_name = c('Test Project 1', 'Test Project 2'),
    user_id = c(2, 3), # Assuming user1 has id=2, user2 has id=3
    responsible_user = c('user1', 'user2'),
    reference_genome = c('GRCh38', 'mm10'),
    sample_type = c('WGS', 'RNA'),
    budget_group = c('Group A', 'Group B'),
    description = c('Test description 1', 'Test description 2'),
    num_samples = c(10, 5),
    sequencing_platform = c('Illumina NovaSeq', 'Illumina MiSeq'),
    sequencing_kit = c('S2', 'SP'),
    index_type = c('Dual Index', 'Single Index'),
    seq_length = c(150, 75),
    kickoff_meeting = c(1, 0)
  )
  
  for(i in 1:nrow(sample_projects)) {
    dbExecute(con, "
      INSERT INTO projects 
      (project_name, user_id, responsible_user, reference_genome, sample_type, 
       budget_group, description, num_samples, sequencing_platform, sequencing_kit, 
       index_type, seq_length, kickoff_meeting)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      sample_projects$project_name[i],
      sample_projects$user_id[i],
      sample_projects$responsible_user[i],
      sample_projects$reference_genome[i],
      sample_projects$sample_type[i],
      sample_projects$budget_group[i],
      sample_projects$description[i],
      sample_projects$num_samples[i],
      sample_projects$sequencing_platform[i],
      sample_projects$sequencing_kit[i],
      sample_projects$index_type[i],
      sample_projects$seq_length[i],
      sample_projects$kickoff_meeting[i]
    ))
  }
  
  dbDisconnect(con)
  message("New database created successfully with all modifications!")
  message("Default login: admin/admin123")
  message("Sample users: user1/user1123, user2/user2123")
}

# Run the setup
setup_complete_database()