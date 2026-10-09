repo_prefix <- if (file.exists("sequencing-app/sample_sheet_helpers.R")) "." else "../.."
source(file.path(repo_prefix, "sequencing-app", "sample_sheet_helpers.R"))

sample_rows <- function() {
  data.frame(
    ProjectNumber = c("", ""),
    SampleName = c("sample-a", "sample-b"),
    SampleType = c("RNA", "DNA"),
    Volume = c("20", "10"),
    `Description_Condition (optional)` = c("treated", ""),
    `Concentration (optional)` = c("0.7", ""),
    SampleIndex = c("", ""),
    DataName = c("", ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("the committed workbook matches the current template contract", {
  template_path <- file.path(
    repo_prefix,
    "sequencing-app",
    "NGS_sampleSheet_template.xlsx"
  )
  parsed <- ngs_read_sample_sheet(
    template_path
  )
  expect_identical(parsed$headers, NGS_SAMPLE_SHEET_HEADERS)
  expect_equal(nrow(parsed$data), 0L)

  example <- suppressMessages(readxl::read_excel(
    template_path,
    sheet = "Example",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ))
  expect_identical(ngs_cell_text(example[[7]][4:7]), rep("", 4L))
  expect_identical(ngs_cell_text(example[[8]][4:7]), rep("", 4L))
})

test_that("valid user rows pass with optional fields empty", {
  checked <- ngs_validate_sample_sheet_data(sample_rows(), c(4L, 5L), 2L)
  expect_true(checked$valid)
  expect_length(checked$errors, 0L)
})

test_that("ProjectNumber must remain blank", {
  rows <- sample_rows()
  rows$ProjectNumber[[1]] <- "P1111"
  checked <- ngs_validate_sample_sheet_data(rows, c(4L, 5L), 2L)
  expect_false(checked$valid)
  expect_match(paste(checked$errors, collapse = " "), "ProjectNumber")
})

test_that("required user cells and sample count are checked", {
  rows <- sample_rows()
  rows$SampleType[[2]] <- ""
  checked <- ngs_validate_sample_sheet_data(rows, c(4L, 5L), 3L)
  expect_false(checked$valid)
  expect_match(paste(checked$errors, collapse = " "), "Number of Samples is 3")
  expect_match(paste(checked$errors, collapse = " "), "SampleType is required")
  expect_match(paste(checked$errors, collapse = " "), "Excel row 5")
})

test_that("facility-generated values must remain blank", {
  rows <- sample_rows()
  rows$SampleIndex[[1]] <- "custom-index"
  rows$DataName[[2]] <- "custom-name"
  checked <- ngs_validate_sample_sheet_data(rows, c(4L, 5L), 2L)
  expect_false(checked$valid)
  expect_match(paste(checked$errors, collapse = " "), "SampleIndex is reserved")
  expect_match(paste(checked$errors, collapse = " "), "DataName is reserved")
})

test_that("assigned project number is written to rows and worksheet name", {
  source_workbook <- file.path(
    repo_prefix,
    "sequencing-app",
    "NGS_sampleSheet_template.xlsx"
  )
  finalized <- ngs_assign_project_number(
    source_workbook,
    "P1111",
    4:7
  )
  on.exit(unlink(finalized, force = TRUE), add = TRUE)

  expect_true(file.exists(finalized))
  expect_identical(readxl::excel_sheets(finalized), c("Example", "P1111"))
  assigned <- suppressMessages(readxl::read_excel(
    finalized,
    sheet = "P1111",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ))
  expect_identical(assigned[[6]][4:7], rep("P1111", 4L))
  expect_identical(ngs_cell_text(assigned[[7]][4:7]), rep("", 4L))
  expect_identical(ngs_cell_text(assigned[[8]][4:7]), rep("", 4L))
  expect_identical(
    readxl::excel_sheets(source_workbook),
    c("Example", "project_metadata")
  )
})

test_that("storage uses pool, fallback, and failed statuses", {
  old_values <- Sys.getenv(c(
    "NGS_UPLOAD_ROOT",
    "NGS_LOCAL_UPLOAD_FALLBACK",
    "NGS_REQUIRE_POOL_MOUNT"
  ), unset = NA_character_)
  on.exit({
    for (name in names(old_values)) {
      if (is.na(old_values[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, setNames(list(old_values[[name]]), name))
      }
    }
  }, add = TRUE)

  temp_root <- tempfile("ngs-storage-test-")
  dir.create(temp_root)
  source_file <- file.path(temp_root, "source.xlsx")
  writeBin(charToRaw("sample-sheet"), source_file)
  pool <- file.path(temp_root, "pool")
  fallback <- file.path(temp_root, "fallback")
  dir.create(pool)
  dir.create(fallback)
  Sys.setenv(
    NGS_UPLOAD_ROOT = pool,
    NGS_LOCAL_UPLOAD_FALLBACK = fallback,
    NGS_REQUIRE_POOL_MOUNT = "0"
  )

  stored_pool <- ngs_store_sample_sheet(source_file, "P1111", "tester")
  expect_identical(stored_pool$status, "pool")
  expect_true(file.exists(stored_pool$path))

  Sys.setenv(NGS_UPLOAD_ROOT = file.path(temp_root, "missing-pool"))
  stored_fallback <- ngs_store_sample_sheet(source_file, "P1112", "tester")
  expect_identical(stored_fallback$status, "fallback")
  expect_true(file.exists(stored_fallback$path))
  expect_match(stored_fallback$pool_error, "does not exist")

  blocked_fallback <- file.path(temp_root, "not-a-directory")
  writeBin(charToRaw("blocked"), blocked_fallback)
  Sys.setenv(NGS_LOCAL_UPLOAD_FALLBACK = blocked_fallback)
  stored_failed <- ngs_store_sample_sheet(source_file, "P1113", "tester")
  expect_identical(stored_failed$status, "failed")
  expect_true(nzchar(stored_failed$pool_error))
  expect_true(nzchar(stored_failed$fallback_error))
})
