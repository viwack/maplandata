# -----------------------------------------------------------------------------
# SCRAPE THE PERIOD INDEX, THEN THE LANDING PAGE FOR ITS DOWNLOAD LINK(S)
# -----------------------------------------------------------------------------

#' Scrape a dataset's CMS "index" page into a period -> landing_url table
#'
#' @param period_type `"month"` (rows labelled `YYYY-MM`, period = `"YYYY-MM"`)
#'   or `"year"` (rows whose link text is a bare year, period = `"YYYY"`).
#' @keywords internal
get_period_index <- function(index_url, period_type = "month") {
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
    a      <- rvest::html_element(row, "a")
    href   <- stringr::str_trim(rvest::html_attr(a, "href"))
    period <- if (identical(period_type, "year")) {
      # link text is just the year, e.g. "2024" ("2004 Mar-Dec" etc. are skipped)
      stringr::str_extract(stringr::str_trim(rvest::html_text2(a)), "^(19|20)\\d{2}$")
    } else {
      stringr::str_extract(rvest::html_text2(row), "(19|20)\\d{2}-\\d{2}")
    }
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
#'
#' @param link_regex Optional (case-insensitive) regex. When not `NA`, only
#'   links whose href or visible link text matches it are returned.
#' @keywords internal
scrape_download_links <- function(landing_url, link_regex = NA_character_) {
  resp <- httr::GET(landing_url, httr::user_agent(CMS_UA), httr::timeout(30))
  if (httr::status_code(resp) >= 400) {
    return(character(0))
  }
  page <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  anchors <- rvest::html_elements(page, "a")
  hrefs   <- rvest::html_attr(anchors, "href")
  texts   <- rvest::html_text2(anchors)
  has_href <- !is.na(hrefs)
  hrefs <- stringr::str_trim(hrefs[has_href])
  texts <- texts[has_href]
  
  keep <- stringr::str_detect(hrefs, "\\.(zip|csv|xlsx|xls)(\\?.*)?$") &
    !stringr::str_detect(hrefs, "agent/broker|help-desk")
  if (!is.na(link_regex)) {
    rx <- stringr::regex(link_regex, ignore_case = TRUE)
    keep <- keep & (stringr::str_detect(hrefs, rx) |
                      stringr::str_detect(texts, rx))
  }
  file_links <- hrefs[keep]
  file_links <- ifelse(
    stringr::str_starts(file_links, "http"),
    file_links,
    paste0("https://www.cms.gov", file_links)
  )
  unique(file_links)
}

#' Build the per-year link filter for a "page" dataset
#'
#' Requires the file name (or link text) to START with the year, so e.g. the
#' "2027 Star Ratings Data Tables (Oct 8 2026)" zip is not picked up when
#' asking for 2026 just because "2026" appears later in its name.
#' @param base Optional regex for the kind of file wanted (`NA` = any).
#' @keywords internal
page_link_regex <- function(year, base = NA_character_) {
  rx <- sprintf("(?:^|/)%s[-_ ]", year)
  if (!is.na(base)) rx <- sprintf("%s.*(?:%s)", rx, base)
  rx
}