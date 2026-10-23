# -----------------------------------------------------------------------------
# END-USER LOADER -- "load the most up-to-date version of a file"
# -----------------------------------------------------------------------------

#' Folder on disk that holds a dataset
#' @keywords internal
ma_dataset_dir <- function(dataset_key, root = default_ma_plans_root()) {
  folder <- REGISTRY$folder[REGISTRY$dataset_key == dataset_key]
  if (length(folder) == 0) folder <- dataset_key
  file.path(root, folder)
}

#' Path of each file relative to its dataset folder. The year is read from
#' this (not just the file name) because annual datasets such as
#' `"MA_ratebook"` keep one `<year>/` subfolder per release.
#' @keywords internal
ma_rel_path <- function(files, dataset_key, root = default_ma_plans_root()) {
  as.character(fs::path_rel(files, start = ma_dataset_dir(dataset_key, root)))
}

#' List local files for a dataset
#'
#' @param dataset_key A key from `REGISTRY$dataset_key` (e.g. `"MA_plan"`),
#'   or a raw folder name.
#' @param year Optional year filter.
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @return A character vector of file paths, most recently modified first.
#' @export
list_ma_files <- function(dataset_key, year = NULL, root = default_ma_plans_root()) {
  dir_path <- ma_dataset_dir(dataset_key, root)
  lazy     <- is_lazy_dataset(dataset_key)
  if (lazy) {
    # only already-extracted years are "local files"; one folder per year
    dir_path <- file.path(dir_path, "extracted")
    if (!is.null(year)) dir_path <- file.path(dir_path, as.character(year))
    if (!fs::dir_exists(dir_path)) return(character(0))
  }
  files <- fs::dir_ls(dir_path, type = "file", regexp = "\\.(csv|xlsx|xls)$",
                      recurse = TRUE)
  files <- files[!stringr::str_detect(basename(files), "^~\\$")]
  files <- drop_archived(files)
  if (!is.null(year) && !lazy) {
    # match the year in the file name or in a <year>/ subfolder (annual datasets)
    rel   <- ma_rel_path(files, dataset_key, root)
    files <- files[stringr::str_detect(rel, as.character(year))]
  }
  info <- fs::file_info(files)
  files[order(info$modification_time, decreasing = TRUE)]
}

#' Load the most up-to-date local version of a dataset into R
#'
#' The single function end users need day-to-day. Loads a `.csv`, `.xls`,
#' or `.xlsx` file transparently.
#'
#' For zip-on-demand datasets (e.g. `"MA_landscape"`) the requested year is
#' unzipped from the stored zip the first time it is asked for, and
#' `year = NULL` means the latest year found inside the zip.
#'
#' @param dataset_key A key from `REGISTRY$dataset_key` (e.g. `"MA_plan"`).
#' @param year Specific year, or `NULL` (default) for the most recent year
#'   available locally.
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @param pattern Optional regex (case-insensitive) matched against file names,
#'   to choose among several files for the same year.
#' @param ... Passed on to `data.table::fread()` or `readxl::read_excel()`.
#' @return A data.frame/tibble/data.table of the loaded dataset.
#' @export
#' @examples
#' \dontrun{
#' plan_df   <- get_ma_data("MA_plan")            # most recent year available
#' ctrct_2020 <- get_ma_data("MA_ctrct", year = 2020)
#' }
get_ma_data <- function(dataset_key, year = NULL, root = default_ma_plans_root(),
                        pattern = NULL, ...) {
  if (is_lazy_dataset(dataset_key)) {
    # zip stays packed on disk; unpack just this year on first use
    if (is.null(year)) {
      zip_years <- ma_zip_years(dataset_key, root)
      if (length(zip_years) == 0) {
        stop(sprintf("No downloaded zip found for %s. Run update_ma_plan_data(datasets = \"%s\") first.",
                     dataset_key, dataset_key))
      }
      year <- max(zip_years)
    }
    ma_extract_year(dataset_key, year, root)
  } else if (is.null(year)) {
    all_files <- list_ma_files(dataset_key, root = root)
    if (length(all_files) == 0) stop("No local files found for ", dataset_key)
    years_found <- stringr::str_extract(ma_rel_path(all_files, dataset_key, root),
                                        "(19|20)\\d{2}")
    year <- max(as.integer(years_found), na.rm = TRUE)
  }
  
  candidates <- list_ma_files(dataset_key, year = year, root = root)
  if (!is.null(pattern)) {
    candidates <- candidates[stringr::str_detect(
      basename(candidates), stringr::regex(pattern, ignore_case = TRUE))]
  }
  if (length(candidates) == 0) {
    stop(sprintf("No local file found for %s / %s. Run update_ma_plan_data() first.",
                 dataset_key, year))
  }
  
  csvs <- candidates[stringr::str_detect(candidates, "\\.csv$")]
  pool   <- if (length(csvs) > 0) csvs else candidates
  target <- pool[1]
  if (length(pool) > 1) {
    message(sprintf("%d files for %s / %s; loading %s. Use `pattern =` to pick another: %s",
                    length(pool), dataset_key, year, basename(target),
                    paste(basename(pool[-1]), collapse = " | ")))
  }
  
  ext <- tolower(tools::file_ext(target))
  message(sprintf("Loading %s (%s, %s)", dataset_key, year, basename(target)))
  
  if (ext == "csv") {
    data.table::fread(target, ...)
  } else if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(target, ...)
  } else {
    stop("Unsupported file type: ", ext)
  }
}

#' Inventory of MA plan data currently on disk
#'
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @return A tibble with one row per dataset that has local files, listing
#'   the file count and years available.
#' @export
ma_data_inventory <- function(root = default_ma_plans_root()) {
  purrr::map_dfr(REGISTRY$dataset_key, function(dk) {
    if (is_lazy_dataset(dk)) {
      # zipped datasets: report years inside the zip, and which are unpacked
      zip_years <- tryCatch(ma_zip_years(dk, root), error = function(e) integer(0))
      if (length(zip_years) == 0) return(NULL)
      ex <- list_ma_files(dk, root = root)
      ex_years <- sort(unique(stringr::str_extract(ma_rel_path(ex, dk, root),
                                                   "(19|20)\\d{2}")))
      return(tibble::tibble(
        dataset_key = dk, n_files = length(ex),
        years = sprintf("%s (in zip; extracted: %s)", paste(zip_years, collapse = ", "),
                        if (length(ex_years)) paste(ex_years, collapse = ", ") else "none")))
    }
    files <- list_ma_files(dk, root = root)
    if (length(files) == 0) return(NULL)
    years <- sort(unique(stringr::str_extract(ma_rel_path(files, dk, root),
                                              "(19|20)\\d{2}")))
    tibble::tibble(dataset_key = dk, n_files = length(files),
                   years = paste(years, collapse = ", "))
  })
}