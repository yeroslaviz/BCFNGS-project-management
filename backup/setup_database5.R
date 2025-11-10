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
  
  # Seqeuncing platform table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sequencing_platforms (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Sequencing kits table
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS sequencing_kits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )
")
  
  # Read lengths table  
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS read_lengths (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )
")
  
  # Types table (from first image)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Sequencing types table (from second image)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sequencing_types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Sample types table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sample_types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
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
  
  # Projects table with all modifications - NOW USING sample_type_id
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER UNIQUE,
      project_name TEXT NOT NULL,
      user_id INTEGER NOT NULL,
      responsible_user TEXT NOT NULL,
      reference_genome TEXT NOT NULL,
      sample_type_id INTEGER NOT NULL,
      budget_group TEXT NOT NULL,
      description TEXT,
      num_samples INTEGER,
      sequencing_platform TEXT,
      sequencing_kit TEXT,
      index_type TEXT,
      seq_length INTEGER,
      kickoff_meeting INTEGER,
      type_id INTEGER,
      sequencing_type_id INTEGER,
      status TEXT DEFAULT 'Created' CHECK(status IN (
        'Created',
        'Samples received', 
        'Library preparation',
        'QC done',
        'On the sequencer', 
        'Data analysis', 
        'Data released'
      )),
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users (id),
      FOREIGN KEY (type_id) REFERENCES types (id),
      FOREIGN KEY (sequencing_type_id) REFERENCES sequencing_types (id),
      FOREIGN KEY (sample_type_id) REFERENCES sample_types (id)
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
  
  # Create backup_logs table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS backup_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      backup_timestamp TEXT NOT NULL,
      backup_size INTEGER NOT NULL,
      backed_up_by TEXT NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
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
  
  # Insert default types (from first image) - ALPHABETICAL ORDER
  default_types <- c("test run", "10x single cell ATACseq", "10x single cell RNAseq", "10x single nucleus RNAseq", "ATACseq", "ChlPseq", "CRISPRscreen", "Cut&Run/Tag", "DNAseq - single stranded", "HiCseq - Bulk", "HiCseq - single nucleus", "MicroCseq", "Riboseq", "RNAseq - mRNAs (poly-A enrichment) RNAseq - single cell/embryo", "RNAseq - smallRNAs", "RNAseq - total + rRNAdepletion SeENseq", "TIPseq", "tRNAseq", "WGS")
  default_types <- sort(default_types)
  for(type in default_types) {
    dbExecute(con, "
      INSERT OR IGNORE INTO types (name) 
      VALUES (?)
    ", params = list(type))
  }
  
  # Insert default sequencing platforms
  default_platforms <- c("Illumina NovaSeq", "Illumina NextSeq", 
                         "Aviti", "Oxford Nanopore")
  for(platform in default_platforms) {
    dbExecute(con, "INSERT OR IGNORE INTO sequencing_platforms (name) VALUES (?)", params = list(platform))
  }
  
  # Insert default sequencing kits
  default_sequencing_kits <- c("SP", "S1", "S2", "S3", "S4", "ONT", "Mid", "High")
  for(kit in default_sequencing_kits) {
    dbExecute(con, "INSERT OR IGNORE INTO sequencing_kits (name) VALUES (?)", params = list(kit))
  }
  
  # Insert default read lengths
  default_read_lengths <- c("35", "75", "150", "200", "500")
  for(length in default_read_lengths) {
    dbExecute(con, "INSERT OR IGNORE INTO read_lengths (name) VALUES (?)", params = list(length))
  }
  
  # Insert default sequencing types (from second image) - PAIRED-END FIRST
  default_sequencing_types <- c("paired-end", "single-end")
  for(seq_type in default_sequencing_types) {
    dbExecute(con, "
      INSERT OR IGNORE INTO sequencing_types (name) 
      VALUES (?)
    ", params = list(seq_type))
  }
  
  # Insert new budget groups in ALPHABETICAL ORDER
  default_budget_groups <- c(
    "Baier - P350",
    "Baldwin - P550", 
    "Baumeister - K190", 
    "Borst - P300", 
    "Conti - K230", 
    "Cox - K435", 
    "Faessler - K170", 
    "Fenk - P865", 
    "Gahr- K0064", 
    "Harbauer - P835", 
    "Harbauer - PSY350", 
    "Hartl - K120", 
    "Hartl - PSBIOC_533A", 
    "Klein - P400", 
    "Kuepper - M.TN.APSY00002", 
    "Mann - K250", 
    "Mayer - M.TN-PSY00001", 
    "Mayer- P870", 
    "Mayer - P.S.PSY331", 
    "Mu,eller - K411", 
    "Murray - K442", 
    "Murray - PSBIOC5034", 
    "Nedialkova - K446", 
    "Nedialkova - M.TN.A. BIOC0006", 
    "Pfander - PSBIOC459", 
    "Schulman - K085", 
    "Sourjik - 10400", 
    "Spatz - 130", 
    "Tachibana - K050", 
    "Tachibana - P.S.BIOC5027", 
    "Unknown/Not in the list", 
    "Wolf - M.FW.A.PSY00002"
  )
  default_budget_groups <- sort(default_budget_groups)
  for(group in default_budget_groups) {
    dbExecute(con, "
      INSERT OR IGNORE INTO budget_groups (name) 
      VALUES (?)
    ", params = list(group))
  }
  
  # Insert default reference genomes - HSP FIRST
  default_genomes <- c(
    "Hsp.GRCh38",
    "Mmu.GrCm38", 
    "Cel.WBcel235", 
    "Sce.R64-1-1", 
    "Dme.BDGP6.28", 
    "Dps.3.0.49", 
    "Dre.GRCz11", 
    "Eco.HUSEC2011CHR1", 
    "Others or Mixed", 
    "Custom"
  )
  for(genome in default_genomes) {
    dbExecute(con, "
      INSERT OR IGNORE INTO reference_genomes (name) 
      VALUES (?)
    ", params = list(genome))
  }
  
  # Insert default sample types - ALPHABETICAL ORDER
  default_sample_types <- c("total RNA", "genomic DNA", "ChIP/fragmented DNA", 
                            "cDNA", "single-stranded DNA", "DNA - Final library")
  default_sample_types <- sort(default_sample_types)
  for(sample_type in default_sample_types) {
    dbExecute(con, "
      INSERT OR IGNORE INTO sample_types (name) 
      VALUES (?)
    ", params = list(sample_type))
  }
  
  # Insert some sample projects to test the project_id generation - NOW USING sample_type_id
  sample_projects <- data.frame(
    project_name = c('Test Project 1', 'Test Project 2'),
    user_id = c(2, 3), # Assuming user1 has id=2, user2 has id=3
    responsible_user = c('user1', 'user2'),
    reference_genome = c("Hsp.GRCh38", "Mmu.GrCm38"),
    sample_type_id = c(1, 2), # Now using IDs instead of text
    budget_group = c('Baier - P350', 'Baldwin - P550'),
    description = c('Test description 1', 'Test description 2'),
    num_samples = c(10, 5),
    sequencing_platform = c('Illumina NovaSeq', 'Illumina MiSeq'),
    sequencing_kit = c('S2', 'SP'),
    index_type = c('Dual Index', 'Single Index'),
    seq_length = c(150, 75),
    kickoff_meeting = c(1, 0),
    type_id = c(1, 2),
    sequencing_type_id = c(1, 1), # Both use paired-end as default
    status = c('Created', 'Samples received') # UPDATED STATUS VALUES
  )
  
  for(i in 1:nrow(sample_projects)) {
    dbExecute(con, "
      INSERT INTO projects 
      (project_name, user_id, responsible_user, reference_genome, sample_type_id, 
       budget_group, description, num_samples, sequencing_platform, sequencing_kit, 
       index_type, seq_length, kickoff_meeting, type_id, sequencing_type_id, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      sample_projects$project_name[i],
      sample_projects$user_id[i],
      sample_projects$responsible_user[i],
      sample_projects$reference_genome[i],
      sample_projects$sample_type_id[i],
      sample_projects$budget_group[i],
      sample_projects$description[i],
      sample_projects$num_samples[i],
      sample_projects$sequencing_platform[i],
      sample_projects$sequencing_kit[i],
      sample_projects$index_type[i],
      sample_projects$seq_length[i],
      sample_projects$kickoff_meeting[i],
      sample_projects$type_id[i],
      sample_projects$sequencing_type_id[i],
      sample_projects$status[i]
    ))
  }
  
  dbDisconnect(con)
  message("New database created successfully with all modifications!")
  message("Default login: admin/admin123")
  message("Sample users: user1/user1123, user2/user2123")
  message("All lists are now in alphabetical order")
  message("Sample types now use proper foreign key relationships")
  message("Hsp.GRCh38 is the default reference genome")
  message("paired-end is the default sequencing type")
  message("Updated status workflow: Created → Samples received → Library preparation → QC done → On the sequencer → Data analysis → Data released")
}

# Run the setup
setup_complete_database()