# -----------------------------------------------------------------------------
# DOWNLOAD + CHANGE DETECTION
# -----------------------------------------------------------------------------

#' HEAD a file URL to get change-detection metadata without downloading it
#' @keywords internal
get_remote_meta <- function(file_url) {
  resp <- tryCatch(httr::HEAD(file_url, httr::user_agent(CMS_UA), httr::timeout(30)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) >= 400) {
    return(list(last_modified = NA_character_, etag = NA_character_))
  }
  h <- httr::headers(resp)
  list(
    last_modified = h[["last-modified"]] %||% NA_character_,
    etag          = h[["etag"]] %||% NA_character_
  )
}

#' Download one file, unzip it if needed, and return the extracted (or raw)
#' local file path(s)
#' @keywords internal
download_and_extract <- function(file_url, dest_dir) {
  fs::dir_create(dest_dir)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(file_url)))
  resp <- httr::GET(file_url, httr::user_agent(CMS_UA), httr::timeout(60),
                    httr::write_disk(tmp, overwrite = TRUE))
  if (httr::status_code(resp) >= 400) {
    stop(sprintf("HTTP status was '%s Forbidden/Error'", httr::status_code(resp)))
  }

  if (tools::file_ext(tmp) == "zip") {
    extracted <- utils::unzip(tmp, exdir = dest_dir, overwrite = TRUE)
    unlink(tmp)
    extracted
  } else {
    dest <- file.path(dest_dir, basename(file_url))
    fs::file_copy(tmp, dest, overwrite = TRUE)
    unlink(tmp)
    dest
  }
}

#' Pull one dataset/year/month if it is new or changed
#' @keywords internal
pull_one <- function(dataset_key, folder, landing_url, year, month,
                     root, manifest, force = FALSE) {

  ym <- sprintf("%d-%s", year, month)
  dest_dir <- file.path(root, folder)

  if (is.na(landing_url)) {
    message(sprintf("[skip]  %s %s: no entry in period index for this period",
                    dataset_key, ym))
    return(manifest)
  }

  if (!force) {
    existing <- tryCatch(list_ma_files(dataset_key, year = year, root = root),
                         error = function(e) character(0))
    if (length(existing) > 0) {
      message(sprintf("[have]  %s %s: %d file(s) already on disk; skipping (force=TRUE to refresh)",
                      dataset_key, ym, length(existing)))
      return(manifest)
    }
  }

  file_links <- scrape_download_links(landing_url)
  if (length(file_links) == 0) {
    message(sprintf("[skip]  %s %s: no landing page / no download link found (%s)",
                    dataset_key, ym, landing_url))
    return(manifest)
  }

  for (file_url in file_links) {
    remote_meta <- get_remote_meta(file_url)

    prior <- manifest[manifest$dataset_key == dataset_key &
                        manifest$year == as.character(year) &
                        manifest$month == month &
                        manifest$file_url == file_url, ]

    unchanged <- nrow(prior) > 0 &&
      !force &&
      isTRUE(prior$http_last_modified[1] == remote_meta$last_modified) &&
      isTRUE(prior$http_etag[1] == remote_meta$etag) &&
      !is.na(remote_meta$last_modified)

    if (unchanged) {
      message(sprintf("[ok]    %s %s: already up to date (%s)",
                      dataset_key, ym, basename(file_url)))
      next
    }

    message(sprintf("[pull]  %s %s: %s", dataset_key, ym, file_url))
    extracted_paths <- tryCatch(
      download_and_extract(file_url, dest_dir),
      error = function(e) {
        message(sprintf("[error] %s %s: %s", dataset_key, ym, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(extracted_paths)) next
    extracted_paths <- extracted_paths[!grepl("^~\\$", basename(extracted_paths))]

    for (p in extracted_paths) {
      row <- list(
        dataset_key = dataset_key, folder = folder, year = year, month = month,
        landing_url = landing_url, file_url = file_url, local_path = p,
        http_last_modified = remote_meta$last_modified %||% NA_character_,
        http_etag = remote_meta$etag %||% NA_character_,
        sha256 = digest::digest(p, file = TRUE, algo = "sha256"),
        file_size = fs::file_size(p),
        pulled_at = as.character(Sys.time()),
        status = "ok"
      )
      manifest <- upsert_manifest_row(manifest, row)
    }
  }
  manifest
}
