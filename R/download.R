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
                     root, manifest, force = FALSE,
                     link_regex = NA_character_) {
  
  # month "00" marks an annual dataset (one release per year, no month).
  # Annual zips are extracted into <folder>/<year>/ so identically named
  # files inside different years' zips can never overwrite each other.
  annual   <- identical(month, "00")
  ym       <- if (annual) as.character(year) else sprintf("%d-%s", year, month)
  dest_dir <- if (annual) file.path(root, folder, year) else file.path(root, folder)
  
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
  
  file_links <- scrape_download_links(landing_url, link_regex = link_regex)
  if (length(file_links) == 0) {
    message(sprintf("[skip]  %s %s: no landing page / matching download link found (%s)",
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

#' Download one file as-is (no unzipping) into `dest_dir`
#' @keywords internal
download_file_only <- function(file_url, dest_dir, timeout = 900) {
  fs::dir_create(dest_dir)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(file_url)))
  on.exit(unlink(tmp), add = TRUE)
  resp <- httr::GET(file_url, httr::user_agent(CMS_UA), httr::timeout(timeout),
                    httr::write_disk(tmp, overwrite = TRUE))
  if (httr::status_code(resp) >= 400) {
    stop(sprintf("HTTP status %s", httr::status_code(resp)))
  }
  dest <- file.path(dest_dir, basename(file_url))
  fs::file_copy(tmp, dest, overwrite = TRUE)
  dest
}

#' Pull a "static" dataset: download its zip, but do NOT unzip it
#'
#' Unzipping happens later, per year, in [get_ma_data()] (see lazy.R).
#' Without `force`, an existing zip is left alone. With `force`, the server's
#' Last-Modified/ETag is compared with the manifest and the zip is only
#' re-downloaded if it changed; a refreshed zip clears `extracted/` so stale
#' years are re-extracted from the new zip on next use.
#' @keywords internal
pull_static <- function(dataset_key, folder, landing_url, root, manifest,
                        force = FALSE, link_regex = NA_character_) {
  dest_dir   <- file.path(root, folder)
  file_links <- scrape_download_links(landing_url, link_regex = link_regex)
  if (length(file_links) == 0) {
    message(sprintf("[skip]  %s: no matching download link found on %s",
                    dataset_key, landing_url))
    return(manifest)
  }
  
  for (file_url in file_links) {
    dest <- file.path(dest_dir, basename(file_url))
    
    if (!force && file.exists(dest)) {
      message(sprintf("[have]  %s: %s already downloaded; skipping (force=TRUE to re-check)",
                      dataset_key, basename(dest)))
      next
    }
    
    remote_meta <- get_remote_meta(file_url)
    # NB: build the row index OUTSIDE manifest[ ] -- inside a data.table's [ ],
    # a name like `dataset_key` resolves to the column, not this function's argument.
    prior_idx <- which(manifest$dataset_key == dataset_key &
                         manifest$year == "all" & manifest$month == "00" &
                         manifest$file_url == file_url)
    prior <- manifest[prior_idx, ]
    unchanged <- file.exists(dest) && nrow(prior) > 0 &&
      !is.na(remote_meta$last_modified) &&
      identical(as.character(prior$http_last_modified[1]), as.character(remote_meta$last_modified)) &&
      identical(as.character(prior$http_etag[1]), as.character(remote_meta$etag))
    if (unchanged) {
      message(sprintf("[ok]    %s: already up to date (%s)", dataset_key, basename(dest)))
      next
    }
    
    message(sprintf("[pull]  %s: %s (kept zipped)", dataset_key, file_url))
    ok <- tryCatch({ download_file_only(file_url, dest_dir); TRUE },
                   error = function(e) {
                     message(sprintf("[error] %s: %s", dataset_key, conditionMessage(e)))
                     FALSE
                   })
    if (!ok) next
    
    if (fs::dir_exists(file.path(dest_dir, "extracted"))) {
      unlink(file.path(dest_dir, "extracted"), recursive = TRUE)
      message(sprintf("[clear] %s: zip refreshed, cleared previously extracted years", dataset_key))
    }
    
    row <- list(
      dataset_key = dataset_key, folder = folder, year = "all", month = "00",
      landing_url = landing_url, file_url = file_url, local_path = dest,
      http_last_modified = remote_meta$last_modified %||% NA_character_,
      http_etag = remote_meta$etag %||% NA_character_,
      sha256 = digest::digest(dest, file = TRUE, algo = "sha256"),
      file_size = fs::file_size(dest),
      pulled_at = as.character(Sys.time()),
      status = "ok"
    )
    manifest <- upsert_manifest_row(manifest, row)
  }
  manifest
}
