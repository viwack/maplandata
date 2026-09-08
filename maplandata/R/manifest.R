# -----------------------------------------------------------------------------
# MANIFEST HELPERS
# -----------------------------------------------------------------------------

#' Drop files that live inside an "*_Archive" folder anywhere in their path.
#' @keywords internal
drop_archived <- function(files) {
  files[!stringr::str_detect(as.character(files), ARCHIVE_PATH_RE)]
}

#' @keywords internal
.empty_manifest <- function() {
  data.table::data.table(
    dataset_key   = character(), folder = character(), year = character(),
    month = character(), landing_url = character(), file_url = character(),
    local_path = character(), http_last_modified = character(),
    http_etag = character(), sha256 = character(), file_size = character(),
    pulled_at = character(), status = character()
  )
}

#' @keywords internal
read_manifest <- function(path) {
  if (fs::file_exists(path)) {
    data.table::fread(path, colClasses = "character")
  } else {
    .empty_manifest()
  }
}

#' @keywords internal
write_manifest <- function(manifest, path) {
  fs::dir_create(dirname(path))
  data.table::fwrite(manifest, path)
}

#' @keywords internal
upsert_manifest_row <- function(manifest, row) {
  norm <- function(v) {
    v <- v %||% NA
    if (length(v) == 0) return(NA_character_)
    as.character(unclass(v))[1]
  }
  row    <- lapply(row, norm)
  row_dt <- data.table::as.data.table(row)

  if (ncol(manifest) > 0) manifest <- manifest[, lapply(.SD, as.character)]

  key_match <- manifest$dataset_key == row$dataset_key &
    manifest$year     == row$year &
    manifest$month    == row$month &
    manifest$file_url == row$file_url
  if (length(key_match) > 0 && any(key_match, na.rm = TRUE)) {
    manifest <- manifest[!key_match, ]
  }
  data.table::rbindlist(list(manifest, row_dt), fill = TRUE, use.names = TRUE)
}

#' Backfill the manifest from files already on disk
#'
#' Records data files that were downloaded before the manifest existed, so
#' change-detection logic has a starting point. Rows are marked
#' `status = "preexisting"`. Safe to re-run: files already in the manifest
#' (matched by `local_path`) are skipped.
#'
#' @param root Folder to scan, default [default_ma_plans_root()].
#' @param month Month string assumed for historical files (the pipeline is
#'   January-only by default).
#' @return The updated manifest, invisibly.
#' @export
backfill_manifest <- function(root = default_ma_plans_root(), month = DEFAULT_MONTHS) {
  manifest <- read_manifest(file.path(root, "_manifest.csv"))
  known    <- manifest$local_path

  new_rows <- list()
  for (i in seq_len(nrow(REGISTRY))) {
    dataset_key <- REGISTRY$dataset_key[i]
    folder      <- REGISTRY$folder[i]
    dir_path    <- file.path(root, folder)
    if (!fs::dir_exists(dir_path)) next

    files <- fs::dir_ls(dir_path, type = "file", regexp = "\\.(csv|xlsx|xls)$",
                        recurse = TRUE)
    files <- files[!stringr::str_detect(basename(files), "^~\\$")]
    files <- drop_archived(files)
    for (f in files) {
      f <- as.character(f)
      if (f %in% known) next
      yr <- stringr::str_extract(basename(f), "(19|20)\\d{2}")
      if (is.na(yr)) next
      info <- fs::file_info(f)
      new_rows[[length(new_rows) + 1L]] <- list(
        dataset_key        = dataset_key,
        folder             = folder,
        year               = yr,
        month              = month,
        landing_url        = NA_character_,
        file_url           = NA_character_,
        local_path         = f,
        http_last_modified = NA_character_,
        http_etag          = NA_character_,
        sha256             = digest::digest(f, file = TRUE, algo = "sha256"),
        file_size          = as.character(unclass(info$size)),
        pulled_at          = as.character(info$modification_time),
        status             = "preexisting"
      )
    }
  }

  if (length(new_rows) == 0) {
    message("Backfill: nothing new to add; manifest already covers on-disk files.")
    return(invisible(manifest))
  }

  add_dt <- data.table::rbindlist(new_rows, use.names = TRUE, fill = TRUE)
  if (ncol(manifest) > 0) manifest <- manifest[, lapply(.SD, as.character)]
  manifest <- data.table::rbindlist(list(manifest, add_dt), use.names = TRUE, fill = TRUE)
  write_manifest(manifest, file.path(root, "_manifest.csv"))
  message(sprintf("Backfill: added %d file(s) across %d dataset folder(s).",
                  length(new_rows), length(unique(vapply(new_rows, `[[`, "", "folder")))))
  invisible(manifest)
}
