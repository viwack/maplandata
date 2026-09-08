# -----------------------------------------------------------------------------
# SCRAPE THE PERIOD INDEX, THEN THE LANDING PAGE FOR ITS DOWNLOAD LINK(S)
# -----------------------------------------------------------------------------

#' Scrape a dataset's CMS "index" page into a period -> landing_url table
#' @keywords internal
get_period_index <- function(index_url) {
  empty <- data.table::data.table(period = character(0), landing_url = character(0))
  resp <- tryCatch(httr::GET(index_url, httr::user_agent(CMS_UA), httr::timeout(30)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) >= 400) {
    message(sprintf("[error] could not load period index: %s", index_url))
    return(empty)
  }
  page <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  rows <- rvest::html_elements(page, "table tr")
  if (length(rows) == 0) return(empty)

  entries <- lapply(rows, function(row) {
    href   <- stringr::str_trim(rvest::html_attr(rvest::html_element(row, "a"), "href"))
    period <- stringr::str_extract(rvest::html_text2(row), "(19|20)\\d{2}-\\d{2}")
    if (is.na(href) || is.na(period)) return(NULL)
    list(period = period, landing_url = href)
  })
  entries <- entries[!vapply(entries, is.null, logical(1))]
  if (length(entries) == 0) return(empty)

  idx <- data.table::rbindlist(entries)
  idx$landing_url <- ifelse(
    stringr::str_starts(idx$landing_url, "http"),
    idx$landing_url,
    paste0("https://www.cms.gov", idx$landing_url)
  )
  idx[!duplicated(idx$period), ]
}

#' Find actual downloadable file links on a CMS landing page
#' @keywords internal
scrape_download_links <- function(landing_url) {
  resp <- httr::GET(landing_url, httr::user_agent(CMS_UA), httr::timeout(30))
  if (httr::status_code(resp) >= 400) {
    return(character(0))
  }
  page <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  hrefs <- rvest::html_attr(rvest::html_elements(page, "a"), "href")
  hrefs <- hrefs[!is.na(hrefs)]
  hrefs <- stringr::str_trim(hrefs)
  file_links <- hrefs[stringr::str_detect(
    hrefs, "\\.(zip|csv|xlsx|xls)(\\?.*)?$"
  )]
  file_links <- file_links[!stringr::str_detect(
    file_links, "agent/broker|help-desk"
  )]
  file_links <- ifelse(
    stringr::str_starts(file_links, "http"),
    file_links,
    paste0("https://www.cms.gov", file_links)
  )
  unique(file_links)
}
