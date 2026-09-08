# -----------------------------------------------------------------------------
# END-USER LOADER -- "load the most up-to-date version of a file"
# -----------------------------------------------------------------------------

#' List local files for a dataset
#'
#' @param dataset_key A key from `REGISTRY$dataset_key` (e.g. `"MA_plan"`),
#'   or a raw folder name.
#' @param year Optional year filter.
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @return A character vector of file paths, most recently modified first.
#' @export
list_ma_files <- function(dataset_key, year = NULL, root = default_ma_plans_root()) {
  folder <- REGISTRY$folder[REGISTRY$dataset_key == dataset_key]
  if (length(folder) == 0) folder <- dataset_key
  dir_path <- file.path(root, folder)
  files <- fs::dir_ls(dir_path, type = "file", regexp = "\\.(csv|xlsx|xls)$",
                      recurse = TRUE)
  files <- files[!stringr::str_detect(basename(files), "^~\\$")]
  files <- drop_archived(files)
  if (!is.null(year)) {
    files <- files[stringr::str_detect(basename(files), as.character(year))]
  }
  info <- fs::file_info(files)
  files[order(info$modification_time, decreasing = TRUE)]
}

#' Load the most up-to-date local version of a dataset into R
#'
#' The single function end users need day-to-day. Loads a `.csv`, `.xls`,
#' or `.xlsx` file transparently.
#'
#' @param dataset_key A key from `REGISTRY$dataset_key` (e.g. `"MA_plan"`).
#' @param year Specific year, or `NULL` (default) for the most recent year
#'   available locally.
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @param ... Passed on to `data.table::fread()` or `readxl::read_excel()`.
#' @return A data.frame/tibble/data.table of the loaded dataset.
#' @export
#' @examples
#' \dontrun{
#' plan_df   <- get_ma_data("MA_plan")            # most recent year available
#' ctrct_2020 <- get_ma_data("MA_ctrct", year = 2020)
#' }
get_ma_data <- function(dataset_key, year = NULL, root = default_ma_plans_root(), ...) {
  if (is.null(year)) {
    all_files <- list_ma_files(dataset_key, root = root)
    if (length(all_files) == 0) stop("No local files found for ", dataset_key)
    years_found <- stringr::str_extract(basename(all_files), "(19|20)\\d{2}")
    year <- max(as.integer(years_found), na.rm = TRUE)
  }

  candidates <- list_ma_files(dataset_key, year = year, root = root)
  if (length(candidates) == 0) {
    stop(sprintf("No local file found for %s / %s. Run update_ma_plan_data() first.",
                 dataset_key, year))
  }

  csvs <- candidates[stringr::str_detect(candidates, "\\.csv$")]
  target <- if (length(csvs) > 0) csvs[1] else candidates[1]

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
    files <- list_ma_files(dk, root = root)
    if (length(files) == 0) return(NULL)
    years <- sort(unique(stringr::str_extract(basename(files), "(19|20)\\d{2}")))
    tibble::tibble(dataset_key = dk, n_files = length(files),
                   years = paste(years, collapse = ", "))
  })
}
