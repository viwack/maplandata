# -----------------------------------------------------------------------------
# LAZY-UNZIP DATASETS  (REGISTRY period_type == "static")
#
# A "static" dataset is one big zip on a single CMS page (no per-period index,
# no per-period landing pages), e.g. "MA_landscape" = CY2006-CY2025 Landscape
# Files. update_ma_plan_data() only DOWNLOADS the zip and leaves it packed in
# <root>/<folder>/. get_ma_data() then pulls out just the requested year into
# <root>/<folder>/extracted/<year>/ the first time that year is asked for;
# later calls reuse the extracted copy.
# -----------------------------------------------------------------------------

#' Is this dataset a download-now / unzip-on-demand ("static") dataset?
#' @keywords internal
is_lazy_dataset <- function(dataset_key) {
  identical(REGISTRY$period_type[REGISTRY$dataset_key == dataset_key][1], "static")
}

#' Zip file(s) stored in a lazy dataset's folder (not the extracted copies)
#' @keywords internal
ma_zip_paths <- function(dataset_key, root = default_ma_plans_root()) {
  d <- ma_dataset_dir(dataset_key, root)
  if (!fs::dir_exists(d)) return(character(0))
  as.character(fs::dir_ls(d, type = "file", regexp = "\\.zip$", recurse = FALSE))
}

#' File entries inside a zip, without extracting anything
#' @keywords internal
ma_zip_entries <- function(zip_path) {
  nm <- utils::unzip(zip_path, list = TRUE)$Name
  nm <- nm[!stringr::str_detect(nm, "/$")]                  # directories
  nm <- nm[!stringr::str_detect(nm, "^__MACOSX/")]
  nm[!stringr::str_detect(basename(nm), "^~\\$|^\\.")]      # temp / dot files
}

#' Work out which calendar/contract year a zip entry belongs to
#'
#' Looks for an explicit "CY2020" tag first (so a release date such as
#' "...20190930" in the name cannot be mistaken for the year), checking the
#' file name before its parent folders; falls back to a bare 4-digit year.
#' Range labels like "CY2006-CY2025" are ignored.
#' @keywords internal
ma_entry_year <- function(path) {
  comps    <- rev(strsplit(path, "/", fixed = TRUE)[[1]])    # file name first
  range_rx <- stringr::regex("(19|20)\\d{2}[ _]*-[ _]*(CY)?[ _]*(19|20)\\d{2}",
                             ignore_case = TRUE)
  comps    <- comps[!stringr::str_detect(comps, range_rx)]
  cy_rx    <- stringr::regex("CY[ _-]?((?:19|20)\\d{2})(?![0-9])", ignore_case = TRUE)
  bare_rx  <- "(?<![0-9])((?:19|20)\\d{2})(?![0-9])"
  for (rx in list(cy_rx, bare_rx)) {
    for (cp in comps) {
      m <- stringr::str_match(cp, rx)[, 2]
      if (!is.na(m)) return(as.integer(m))
    }
  }
  NA_integer_
}

#' Years available inside a lazy dataset's zip(s)
#' @keywords internal
ma_zip_years <- function(dataset_key, root = default_ma_plans_root()) {
  yrs <- integer(0)
  for (zp in ma_zip_paths(dataset_key, root)) {
    entries <- ma_zip_entries(zp)
    yrs <- c(yrs, vapply(entries, ma_entry_year, integer(1), USE.NAMES = FALSE))
  }
  sort(unique(yrs[!is.na(yrs)]))
}

#' Extract one year of a lazy dataset from its zip (no-op if already done)
#'
#' @param overwrite Re-extract even if `extracted/<year>/` already has files.
#' @return The extraction folder, invisibly.
#' @keywords internal
ma_extract_year <- function(dataset_key, year, root = default_ma_plans_root(),
                            overwrite = FALSE) {
  out_dir <- file.path(ma_dataset_dir(dataset_key, root), "extracted", as.character(year))
  already <- fs::dir_exists(out_dir) &&
    length(fs::dir_ls(out_dir, type = "file", recurse = TRUE)) > 0
  if (already && !overwrite) return(invisible(out_dir))
  
  zips <- ma_zip_paths(dataset_key, root)
  if (length(zips) == 0) {
    stop(sprintf("No downloaded zip found for %s. Run update_ma_plan_data(datasets = \"%s\") first.",
                 dataset_key, dataset_key))
  }
  
  n_extracted <- 0L
  all_entries <- character(0)
  for (zp in zips) {
    entries     <- ma_zip_entries(zp)
    all_entries <- c(all_entries, entries)
    entry_yrs   <- vapply(entries, ma_entry_year, integer(1), USE.NAMES = FALSE)
    hit         <- entries[!is.na(entry_yrs) & entry_yrs == as.integer(year)]
    if (length(hit) == 0) next
    fs::dir_create(out_dir)
    message(sprintf("[unzip] %s %s: extracting %d of %d entries from %s",
                    dataset_key, year, length(hit), length(entries), basename(zp)))
    utils::unzip(zp, files = hit, exdir = out_dir, overwrite = TRUE)
    n_extracted <- n_extracted + length(hit)
  }
  
  if (n_extracted == 0L) {
    avail <- ma_zip_years(dataset_key, root)
    stop(sprintf(
      "No entries for year %s found in the %s zip. Years detected: %s.\nFirst entries: %s",
      year, dataset_key,
      if (length(avail)) paste(avail, collapse = ", ") else "none",
      paste(utils::head(all_entries, 5), collapse = " | ")))
  }
  
  # If the year's files are themselves zips, open those too (up to 3 levels deep)
  for (depth in 1:3) {
    nested <- as.character(fs::dir_ls(out_dir, type = "file", regexp = "\\.zip$",
                                      recurse = TRUE))
    if (length(nested) == 0) break
    for (nz in nested) {
      utils::unzip(nz, exdir = dirname(nz), overwrite = TRUE)
      unlink(nz)
    }
  }
  invisible(out_dir)
}