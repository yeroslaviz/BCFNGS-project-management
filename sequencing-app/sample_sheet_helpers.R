NGS_SAMPLE_SHEET_NAME <- "project_metadata"
NGS_SAMPLE_SHEET_HEADERS <- c(
  "SampleName",
  "SampleType",
  "Volume",
  "Description_Condition (optional)",
  "Concentration (optional)",
  "ProjectNumber",
  "SampleIndex",
  "DataName"
)
NGS_SAMPLE_SHEET_FACILITY_COLUMNS <- c(
  "ProjectNumber",
  "SampleIndex",
  "DataName"
)
NGS_SAMPLE_SHEET_REQUIRED_COLUMNS <- c("SampleName", "SampleType", "Volume")
NGS_SAMPLE_SHEET_USER_COLUMNS <- NGS_SAMPLE_SHEET_HEADERS[1:5]

ngs_env_flag <- function(name, default = FALSE) {
  value <- trimws(tolower(Sys.getenv(name, if (isTRUE(default)) "1" else "0")))
  value %in% c("1", "true", "yes", "on")
}

ngs_env_flag_value <- function(value) {
  trimws(tolower(ngs_scalar_text(value))) %in% c("1", "true", "yes", "on")
}

ngs_scalar_text <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) return("")
  trimws(as.character(value[[1]]))
}

ngs_cell_text <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  trimws(value)
}

ngs_safe_path_part <- function(value, fallback = "unknown") {
  value <- ngs_scalar_text(value)
  value <- gsub("[^A-Za-z0-9._-]+", "_", value)
  value <- gsub("^\\.+|\\.+$", "", value)
  value <- gsub("_+", "_", value)
  if (!nzchar(value)) value <- fallback
  substr(value, 1, 120)
}

ngs_sample_sheet_template_path <- function() {
  Sys.getenv("NGS_SAMPLE_SHEET_TEMPLATE", "NGS_sampleSheet_template.xlsx")
}

ngs_read_sample_sheet <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required to validate sample sheets.")
  }
  if (!file.exists(path)) stop("The uploaded sample sheet could not be found.")

  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) {
      stop(
        "The workbook could not be opened. Upload an unencrypted .xlsx file. Details: ",
        conditionMessage(e)
      )
    }
  )
  if (!(NGS_SAMPLE_SHEET_NAME %in% sheets)) {
    stop(
      "The workbook must contain a sheet named '",
      NGS_SAMPLE_SHEET_NAME,
      "'."
    )
  }

  raw <- tryCatch(
    suppressMessages(readxl::read_excel(
      path,
      sheet = NGS_SAMPLE_SHEET_NAME,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )),
    error = function(e) {
      stop("The project_metadata sheet could not be read: ", conditionMessage(e))
    }
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)

  if (nrow(raw) < 3 || ncol(raw) < length(NGS_SAMPLE_SHEET_HEADERS)) {
    stop(
      "The project_metadata sheet does not contain the expected header and instruction rows."
    )
  }

  if (ncol(raw) > length(NGS_SAMPLE_SHEET_HEADERS)) {
    extra <- raw[, (length(NGS_SAMPLE_SHEET_HEADERS) + 1):ncol(raw), drop = FALSE]
    if (any(nzchar(ngs_cell_text(unlist(extra, use.names = FALSE))))) {
      stop("The project_metadata sheet contains data outside columns A:H.")
    }
  }

  observed_headers <- ngs_cell_text(unlist(
    raw[2, seq_along(NGS_SAMPLE_SHEET_HEADERS), drop = FALSE],
    use.names = FALSE
  ))
  if (!identical(observed_headers, NGS_SAMPLE_SHEET_HEADERS)) {
    stop(
      "The project_metadata column names must be exactly: ",
      paste(NGS_SAMPLE_SHEET_HEADERS, collapse = ", "),
      "."
    )
  }

  if (nrow(raw) < 4) {
    data <- raw[0, seq_along(NGS_SAMPLE_SHEET_HEADERS), drop = FALSE]
    excel_rows <- integer(0)
  } else {
    data <- raw[4:nrow(raw), seq_along(NGS_SAMPLE_SHEET_HEADERS), drop = FALSE]
    names(data) <- NGS_SAMPLE_SHEET_HEADERS
    data[] <- lapply(data, ngs_cell_text)
    # SampleIndex/DataName contain facility formulas in unused template rows.
    # Only the user-entry section (A:E) determines whether a sample row exists.
    row_has_data <- apply(
      data[, NGS_SAMPLE_SHEET_USER_COLUMNS, drop = FALSE],
      1,
      function(row) any(nzchar(row))
    )
    excel_rows <- which(row_has_data) + 3L
    data <- data[row_has_data, , drop = FALSE]
  }
  names(data) <- NGS_SAMPLE_SHEET_HEADERS

  list(data = data, excel_rows = excel_rows, headers = observed_headers)
}

ngs_validate_sample_sheet_data <- function(data, excel_rows, expected_rows) {
  errors <- character(0)
  expected_rows <- if (is.null(expected_rows) || length(expected_rows) == 0) {
    NA_integer_
  } else {
    suppressWarnings(as.integer(expected_rows[[1]]))
  }
  if (is.na(expected_rows) || expected_rows < 1) {
    return(list(
      valid = FALSE,
      errors = "Number of Samples must be at least 1 before the sample sheet can be validated."
    ))
  }

  if (nrow(data) != expected_rows) {
    errors <- c(
      errors,
      paste0(
        "The sample sheet contains ", nrow(data),
        " sample row", if (nrow(data) == 1) "" else "s",
        ", but Number of Samples is ", expected_rows,
        ". Add or remove sample rows so the numbers match."
      )
    )
  }

  project_number_used <- which(nzchar(ngs_cell_text(data$ProjectNumber)))
  if (length(project_number_used) > 0) {
    errors <- c(
      errors,
      paste0(
        "ProjectNumber is reserved for the NGS/BCF facility. Leave it blank in Excel row",
        if (length(project_number_used) == 1) " " else "s ",
        paste(excel_rows[project_number_used], collapse = ", "),
        "."
      )
    )
  }

  for (column_name in NGS_SAMPLE_SHEET_REQUIRED_COLUMNS) {
    missing <- which(!nzchar(ngs_cell_text(data[[column_name]])))
    if (length(missing) > 0) {
      errors <- c(
        errors,
        paste0(
          column_name,
          " is required. Fill it in for Excel row",
          if (length(missing) == 1) " " else "s ",
          paste(excel_rows[missing], collapse = ", "),
          "."
        )
      )
    }
  }

  # The current template pre-generates these facility cells. Accept blank cells
  # or the unchanged generated values, but reject user-entered alternatives.
  for (i in seq_len(nrow(data))) {
    expected_index <- sprintf("Sample%02d", excel_rows[[i]] - 3L)
    sample_index <- ngs_cell_text(data$SampleIndex[[i]])
    sample_name <- ngs_cell_text(data$SampleName[[i]])
    data_name <- ngs_cell_text(data$DataName[[i]])
    allowed_data_names <- unique(c(
      paste0("_", expected_index, "_"),
      paste0("_", expected_index, "_", sample_name)
    ))

    if (nzchar(sample_index) && !identical(sample_index, expected_index)) {
      errors <- c(
        errors,
        paste0(
          "SampleIndex is reserved for the NGS/BCF facility. Restore the template value '",
          expected_index,
          "' or leave it blank in Excel row ", excel_rows[[i]], "."
        )
      )
    }
    if (nzchar(data_name) && !(data_name %in% allowed_data_names)) {
      errors <- c(
        errors,
        paste0(
          "DataName is reserved for the NGS/BCF facility. Restore its template formula or leave it blank in Excel row ",
          excel_rows[[i]], "."
        )
      )
    }
  }

  list(valid = length(errors) == 0, errors = unique(errors))
}

ngs_validate_sample_sheet <- function(path, original_name, expected_rows) {
  original_name <- ngs_scalar_text(original_name)
  extension <- tolower(tools::file_ext(original_name))
  if (!identical(extension, "xlsx")) {
    return(list(
      valid = FALSE,
      errors = "Upload the completed sample sheet as an .xlsx file.",
      data = NULL,
      excel_rows = integer(0)
    ))
  }

  parsed <- tryCatch(
    ngs_read_sample_sheet(path),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    return(list(
      valid = FALSE,
      errors = conditionMessage(parsed),
      data = NULL,
      excel_rows = integer(0)
    ))
  }

  checked <- ngs_validate_sample_sheet_data(
    parsed$data,
    parsed$excel_rows,
    expected_rows
  )
  list(
    valid = checked$valid,
    errors = checked$errors,
    data = parsed$data,
    excel_rows = parsed$excel_rows
  )
}

ngs_read_binary_text <- function(path) {
  size <- file.info(path)$size[[1]]
  if (is.na(size)) stop("Could not determine file size: ", path)
  rawToChar(readBin(path, what = "raw", n = size))
}

ngs_write_binary_text <- function(path, value) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(value), connection)
  invisible(path)
}

ngs_xml_escape <- function(value) {
  value <- as.character(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value <- gsub("'", "&apos;", value, fixed = TRUE)
  value
}

ngs_regex_escape <- function(value) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", value)
}

ngs_xml_tags <- function(xml, pattern) {
  positions <- gregexpr(pattern, xml, perl = TRUE)[[1]]
  if (identical(positions[[1]], -1L)) return(character(0))
  lengths <- attr(positions, "match.length")
  substring(xml, positions, positions + lengths - 1L)
}

ngs_replace_once <- function(text, old, new, description) {
  location <- regexpr(old, text, fixed = TRUE)[[1]]
  if (location < 1) stop("Could not update ", description, ".")
  paste0(
    substr(text, 1L, location - 1L),
    new,
    substr(text, location + nchar(old), nchar(text))
  )
}

ngs_set_inline_string_cell <- function(sheet_xml, cell_reference, value) {
  row_number <- sub("^[A-Z]+", "", cell_reference)
  row_pattern <- paste0(
    "(?s)<row\\b[^>]*\\br=\"",
    ngs_regex_escape(row_number),
    "\"[^>]*>.*?</row>"
  )
  row_tags <- ngs_xml_tags(sheet_xml, row_pattern)
  if (length(row_tags) != 1L) {
    stop("Could not find Excel row ", row_number, " in the sample sheet.")
  }
  row_tag <- row_tags[[1]]
  cell_pattern <- paste0(
    "(?s)<c\\b[^>]*\\br=\"",
    ngs_regex_escape(cell_reference),
    "\"[^>]*?(?:/>|>.*?</c>)"
  )
  cell_tags <- ngs_xml_tags(row_tag, cell_pattern)
  escaped_value <- ngs_xml_escape(value)

  if (length(cell_tags) == 1L) {
    cell_tag <- cell_tags[[1]]
    opening <- sub("(?s)^(<c\\b[^>]*)(?:/>|>.*)$", "\\1", cell_tag, perl = TRUE)
    opening <- sub("/\\s*$", "", opening, perl = TRUE)
    opening <- gsub('\\s+t="[^"]*"', "", opening, perl = TRUE)
    new_cell <- paste0(
      opening,
      ' t="inlineStr"><is><t>',
      escaped_value,
      "</t></is></c>"
    )
    new_row <- ngs_replace_once(row_tag, cell_tag, new_cell, cell_reference)
  } else if (length(cell_tags) == 0L) {
    new_cell <- paste0(
      '<c r="', cell_reference, '" t="inlineStr"><is><t>',
      escaped_value,
      "</t></is></c>"
    )
    next_column_pattern <- paste0(
      '<c\\b[^>]*\\br="[G-Z]+',
      ngs_regex_escape(row_number),
      '"'
    )
    next_position <- regexpr(next_column_pattern, row_tag, perl = TRUE)[[1]]
    if (next_position > 0L) {
      new_row <- paste0(
        substr(row_tag, 1L, next_position - 1L),
        new_cell,
        substr(row_tag, next_position, nchar(row_tag))
      )
    } else {
      new_row <- sub("</row>$", paste0(new_cell, "</row>"), row_tag)
    }
  } else {
    stop("The workbook contains duplicate cell ", cell_reference, ".")
  }

  ngs_replace_once(sheet_xml, row_tag, new_row, paste("Excel row", row_number))
}

ngs_set_formula_cached_value <- function(sheet_xml, cell_reference, value) {
  cell_pattern <- paste0(
    "(?s)<c\\b[^>]*\\br=\"",
    ngs_regex_escape(cell_reference),
    "\"[^>]*?>.*?</c>"
  )
  cell_tags <- ngs_xml_tags(sheet_xml, cell_pattern)
  if (length(cell_tags) == 0L) return(sheet_xml)
  if (length(cell_tags) != 1L || !grepl("<f(?:\\s|>)", cell_tags[[1]], perl = TRUE)) {
    stop("Could not update the cached formula result for ", cell_reference, ".")
  }

  cell_tag <- cell_tags[[1]]
  updated_cell <- if (grepl("(?s)<v>.*?</v>", cell_tag, perl = TRUE)) {
    sub(
      "(?s)<v>.*?</v>",
      paste0("<v>", ngs_xml_escape(value), "</v>"),
      cell_tag,
      perl = TRUE
    )
  } else {
    sub(
      "</c>$",
      paste0("<v>", ngs_xml_escape(value), "</v></c>"),
      cell_tag
    )
  }
  ngs_replace_once(sheet_xml, cell_tag, updated_cell, cell_reference)
}

ngs_assign_project_number <- function(
  source,
  project_code,
  excel_rows,
  sample_data = NULL
) {
  project_code <- ngs_scalar_text(project_code)
  if (!grepl("^P[0-9]+$", project_code)) {
    stop("Cannot finalize the sample sheet without a valid project code.")
  }
  excel_rows <- suppressWarnings(as.integer(excel_rows))
  if (length(excel_rows) == 0L || anyNA(excel_rows) || any(excel_rows < 4L)) {
    stop("Cannot finalize the sample sheet without valid sample-row numbers.")
  }
  if (!is.null(sample_data) && nrow(sample_data) != length(excel_rows)) {
    stop("Sample-sheet rows changed before project-number assignment.")
  }
  if (!file.exists(source)) stop("The uploaded sample sheet could not be found.")

  zip_tool <- Sys.which("zip")
  unzip_tool <- Sys.which("unzip")
  if (!nzchar(zip_tool) || !nzchar(unzip_tool)) {
    stop("The zip and unzip commands are required to finalize sample sheets.")
  }

  work_dir <- tempfile("ngs-sample-sheet-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  archive_listing <- tryCatch(
    utils::unzip(source, list = TRUE),
    error = function(e) stop("Could not inspect the uploaded workbook: ", conditionMessage(e))
  )
  archive_names <- gsub("\\\\", "/", archive_listing$Name)
  unsafe_archive_name <-
    grepl("^/", archive_names) |
    grepl("^[A-Za-z]:", archive_names) |
    grepl("(^|/)\\.\\.(/|$)", archive_names)
  if (any(unsafe_archive_name)) {
    stop("The uploaded workbook contains unsafe archive paths.")
  }
  if (sum(archive_listing$Length, na.rm = TRUE) > 100 * 1024^2) {
    stop("The uploaded workbook expands beyond the 100 MB safety limit.")
  }

  extracted <- tryCatch(
    utils::unzip(source, exdir = work_dir),
    error = function(e) stop("Could not unpack the uploaded workbook: ", conditionMessage(e))
  )
  if (length(extracted) == 0L) stop("The uploaded workbook is empty.")

  workbook_path <- file.path(work_dir, "xl", "workbook.xml")
  relationships_path <- file.path(work_dir, "xl", "_rels", "workbook.xml.rels")
  if (!file.exists(workbook_path) || !file.exists(relationships_path)) {
    stop("The uploaded file is not a valid Excel workbook.")
  }

  workbook_xml <- ngs_read_binary_text(workbook_path)
  sheet_tags <- ngs_xml_tags(workbook_xml, "<sheet\\b[^>]*/>")
  metadata_tags <- sheet_tags[
    grepl('name="project_metadata"', sheet_tags, fixed = TRUE)
  ]
  if (length(metadata_tags) != 1L) {
    stop("The workbook must contain exactly one project_metadata sheet.")
  }
  metadata_tag <- metadata_tags[[1]]
  if (any(grepl(
    paste0('name="', ngs_xml_escape(project_code), '"'),
    sheet_tags,
    fixed = TRUE
  ))) {
    stop("The workbook already contains a worksheet named ", project_code, ".")
  }
  relationship_id <- sub(
    '.*\\br:id="([^"]+)".*',
    "\\1",
    metadata_tag,
    perl = TRUE
  )
  if (identical(relationship_id, metadata_tag)) {
    stop("Could not resolve the project_metadata worksheet relationship.")
  }

  relationships_xml <- ngs_read_binary_text(relationships_path)
  relationship_tags <- ngs_xml_tags(relationships_xml, "<Relationship\\b[^>]*/>")
  target_tags <- relationship_tags[
    grepl(paste0('Id="', relationship_id, '"'), relationship_tags, fixed = TRUE)
  ]
  if (length(target_tags) != 1L) {
    stop("Could not resolve the project_metadata worksheet file.")
  }
  worksheet_target <- sub(
    '.*\\bTarget="([^"]+)".*',
    "\\1",
    target_tags[[1]],
    perl = TRUE
  )
  if (
    identical(worksheet_target, target_tags[[1]]) ||
      grepl("(^|/)\\.\\.(/|$)", worksheet_target)
  ) {
    stop("The project_metadata worksheet has an unsafe workbook path.")
  }
  worksheet_path <- file.path(work_dir, "xl", worksheet_target)
  if (!file.exists(worksheet_path)) {
    stop("The project_metadata worksheet file is missing.")
  }

  renamed_tag <- sub(
    'name="project_metadata"',
    paste0('name="', project_code, '"'),
    metadata_tag,
    fixed = TRUE
  )
  workbook_xml <- ngs_replace_once(
    workbook_xml,
    metadata_tag,
    renamed_tag,
    "worksheet name"
  )
  calc_tags <- ngs_xml_tags(workbook_xml, "<calcPr\\b[^>]*/>")
  if (length(calc_tags) == 1L) {
    calc_tag <- calc_tags[[1]]
    updated_calc_tag <- gsub(
      '\\s+(calcMode|fullCalcOnLoad|forceFullCalc)="[^"]*"',
      "",
      calc_tag,
      perl = TRUE
    )
    updated_calc_tag <- sub(
      "/>$",
      ' calcMode="auto" fullCalcOnLoad="1" forceFullCalc="1"/>',
      updated_calc_tag
    )
    workbook_xml <- ngs_replace_once(
      workbook_xml,
      calc_tag,
      updated_calc_tag,
      "workbook calculation settings"
    )
  }
  ngs_write_binary_text(workbook_path, workbook_xml)

  worksheet_xml <- ngs_read_binary_text(worksheet_path)
  for (i in seq_along(excel_rows)) {
    excel_row <- excel_rows[[i]]
    worksheet_xml <- ngs_set_inline_string_cell(
      worksheet_xml,
      paste0("F", excel_row),
      project_code
    )
    if (!is.null(sample_data)) {
      data_name <- paste(
        project_code,
        ngs_cell_text(sample_data$SampleIndex[[i]]),
        ngs_cell_text(sample_data$SampleName[[i]]),
        sep = "_"
      )
      worksheet_xml <- ngs_set_formula_cached_value(
        worksheet_xml,
        paste0("H", excel_row),
        data_name
      )
    }
  }
  ngs_write_binary_text(worksheet_path, worksheet_xml)

  output <- tempfile(
    paste0("NGS_sampleSheet_", project_code, "_"),
    fileext = ".xlsx"
  )
  archive_files <- list.files(
    work_dir,
    all.files = TRUE,
    no.. = TRUE,
    recursive = FALSE,
    include.dirs = TRUE
  )
  zip_command <- paste(
    "cd",
    shQuote(work_dir),
    "&&",
    shQuote(zip_tool),
    "-q -r -X",
    shQuote(output),
    paste(vapply(archive_files, shQuote, character(1)), collapse = " ")
  )
  zip_status <- suppressWarnings(system(
    zip_command,
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  ))
  if (!identical(as.integer(zip_status), 0L) || !file.exists(output)) {
    stop("Could not create the finalized sample sheet.")
  }

  integrity_status <- suppressWarnings(system2(
    unzip_tool,
    args = c("-tqq", shQuote(output)),
    stdout = FALSE,
    stderr = FALSE
  ))
  if (!identical(as.integer(integrity_status), 0L)) {
    unlink(output, force = TRUE)
    stop("The finalized sample sheet failed its ZIP integrity check.")
  }
  output
}

ngs_pool_mount_status <- function(root) {
  root <- ngs_scalar_text(root)
  if (!nzchar(root)) return(list(ok = FALSE, error = "No pool root is configured."))
  if (!dir.exists(root)) {
    return(list(ok = FALSE, error = paste0("Pool root does not exist: ", root)))
  }

  if (ngs_env_flag("NGS_REQUIRE_POOL_MOUNT", TRUE)) {
    mountpoint <- Sys.which("mountpoint")
    if (!nzchar(mountpoint)) {
      return(list(
        ok = FALSE,
        error = "The mountpoint command is unavailable, so the NFS mount cannot be verified."
      ))
    }
    mounted <- suppressWarnings(system2(
      mountpoint,
      args = c("-q", shQuote(root)),
      stdout = FALSE,
      stderr = FALSE
    ))
    if (!identical(as.integer(mounted), 0L)) {
      return(list(ok = FALSE, error = paste0("Pool root is not mounted: ", root)))
    }
  }

  if (file.access(root, 2) != 0) {
    return(list(ok = FALSE, error = paste0("Pool root is not writable: ", root)))
  }
  list(ok = TRUE, error = "")
}

ngs_copy_sample_sheet <- function(source, root, folder_name, stored_name) {
  folder <- file.path(root, folder_name)
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE, mode = "0770")
  }
  if (!dir.exists(folder)) stop("Could not create project folder: ", folder)
  Sys.chmod(folder, mode = "0770", use_umask = FALSE)

  destination <- file.path(folder, stored_name)
  copied <- file.copy(source, destination, overwrite = FALSE, copy.mode = FALSE)
  if (!isTRUE(copied) || !file.exists(destination)) {
    stop("Could not copy the sample sheet to: ", destination)
  }
  Sys.chmod(destination, mode = "0660", use_umask = FALSE)
  normalizePath(destination, mustWork = TRUE)
}

ngs_store_sample_sheet <- function(source, project_code, username) {
  project_code <- ngs_safe_path_part(project_code, "Punknown")
  username <- ngs_safe_path_part(username, "unknown-user")
  folder_name <- paste(project_code, username, sep = "_")
  stored_name <- paste0("NGS_sampleSheet_", folder_name, ".xlsx")
  pool_root <- Sys.getenv("NGS_UPLOAD_ROOT", "/fs/pool/pool-ngs-public")
  fallback_root <- Sys.getenv(
    "NGS_LOCAL_UPLOAD_FALLBACK",
    "/srv/ngs-app-data/uploads_pending_pool"
  )

  pool_status <- ngs_pool_mount_status(pool_root)
  pool_error <- pool_status$error
  if (isTRUE(pool_status$ok)) {
    pool_path <- tryCatch(
      ngs_copy_sample_sheet(source, pool_root, folder_name, stored_name),
      error = function(e) e
    )
    if (!inherits(pool_path, "error")) {
      return(list(
        status = "pool",
        path = pool_path,
        root = pool_root,
        folder_name = folder_name,
        stored_name = stored_name,
        pool_error = "",
        fallback_error = ""
      ))
    }
    pool_error <- conditionMessage(pool_path)
  }

  fallback_path <- tryCatch(
    ngs_copy_sample_sheet(source, fallback_root, folder_name, stored_name),
    error = function(e) e
  )
  if (!inherits(fallback_path, "error")) {
    return(list(
      status = "fallback",
      path = fallback_path,
      root = fallback_root,
      folder_name = folder_name,
      stored_name = stored_name,
      pool_error = pool_error,
      fallback_error = ""
    ))
  }

  list(
    status = "failed",
    path = NA_character_,
    root = NA_character_,
    folder_name = folder_name,
    stored_name = stored_name,
    pool_error = pool_error,
    fallback_error = conditionMessage(fallback_path)
  )
}

ngs_sample_sheet_storage_error <- function(storage) {
  errors <- character(0)
  if (nzchar(ngs_scalar_text(storage$pool_error))) {
    errors <- c(errors, paste0("Pool: ", storage$pool_error))
  }
  if (nzchar(ngs_scalar_text(storage$fallback_error))) {
    errors <- c(errors, paste0("Fallback: ", storage$fallback_error))
  }
  paste(errors, collapse = "\n")
}
