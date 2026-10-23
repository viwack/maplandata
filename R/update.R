# -----------------------------------------------------------------------------
# TOP-LEVEL ORCHESTRATOR -- knows when files update / re-run to catch up
# -----------------------------------------------------------------------------

#' Pull the latest CMS Medicare Advantage plan data
#'
#' Scrapes each dataset's CMS period index, walks years x months, and
#' downloads/unzips anything new or changed into `root`. Safe to re-run any
#' time (e.g. from a monthly cron job or by hand): by default a dataset/year
#' that already has files on disk is skipped outright, so only genuinely new
#' years are fetched. A manifest file (`_manifest.csv`) records what was
#' pulled and from where, and is written incrementally after each dataset.
#'
#' @param datasets Dataset key(s) to pull; `NULL` (default) = all automated
#'   datasets. See `REGISTRY$dataset_key` for the full list (e.g. `"MA_plan"`,
#'   `"MA_ctrct"`, `"SNP"`, ...).
#' @param years Years to check. Default `2015:current year`. Ignored for
#'   "static" datasets (e.g. `"MA_landscape"`), which are a single zip.
#' @param months Months to check. Default `"01"` (January only). Ignored for
#'   annual datasets (`period_type` "year" or "page" in `REGISTRY`, e.g.
#'   `"MA_ratebook"`, `"MA_star_ratings"`), which are pulled once per year in
#'   `years` (plus the following year when `years` reaches the current year).
#' @param root Root data folder. Default [default_ma_plans_root()].
#' @param force If `TRUE`, bypass the "already on disk" shortcut and
#'   re-check every dataset/year against the server; only files whose
#'   Last-Modified/ETag actually changed are re-downloaded.
#' @return The updated manifest, invisibly.
#' @export
#' @examples
#' \dontrun{
#' set_ma_plans_root("~/my-project/Data/MA_Plans")
#'
#' # Pull everything from 2015 to the current year, January only:
#' update_ma_plan_data()
#'
#' # Pull just contract + plan enrollment for specific years:
#' update_ma_plan_data(datasets = c("MA_ctrct", "MA_plan"), years = 2023:2025)
#'
#' # Just the 2024 MA Rate Book (county benchmarks):
#' update_ma_plan_data(datasets = "MA_ratebook", years = 2024)
#'
#' # Star Ratings / Display Measures for specific years:
#' update_ma_plan_data(datasets = "MA_star_ratings", years = 2024:2027)
#'
#' # Download the CY2006-CY2025 Landscape zip (stays zipped until needed):
#' update_ma_plan_data(datasets = "MA_landscape")
#'
#' # Re-check the server for re-releases of years you already have:
#' update_ma_plan_data(force = TRUE)
#' }
update_ma_plan_data <- function(datasets = NULL,
                                years = DEFAULT_START_YEAR:lubridate::year(Sys.Date()),
                                months = DEFAULT_MONTHS,
                                root = default_ma_plans_root(),
                                force = FALSE) {
  
  registry <- REGISTRY
  if (!is.null(datasets)) registry <- registry[registry$dataset_key %in% datasets, ]
  
  manifest_path <- file.path(root, "_manifest.csv")
  manifest <- read_manifest(manifest_path)
  
  for (i in seq_len(nrow(registry))) {
    
    # "static" = one zip on one page: download it, leave it packed (lazy.R)
    if (identical(registry$period_type[i], "static")) {
      manifest <- tryCatch(
        pull_static(
          dataset_key = registry$dataset_key[i],
          folder      = registry$folder[i],
          landing_url = registry$index_url[i],
          root = root, manifest = manifest, force = force,
          link_regex  = registry$link_regex[i]
        ),
        error = function(e) {
          message(sprintf("[error] %s: %s", registry$dataset_key[i], conditionMessage(e)))
          manifest
        }
      )
      write_manifest(manifest, manifest_path)
      next
    }
    
    ptype  <- registry$period_type[i]
    page   <- identical(ptype, "page")       # one page, files for many years
    annual <- ptype %in% c("year", "page")   # no month; stored in <folder>/<year>/
    
    if (page) {
      period_index <- NULL                   # the page itself is the landing page
    } else {
      period_index <- get_period_index(registry$index_url[i], period_type = ptype)
      message(sprintf("[index] %s: found %d period(s) in index",
                      registry$dataset_key[i], nrow(period_index)))
    }
    
    months_i <- if (annual) "00" else months
    
    # Annual releases run ahead of the calendar (e.g. the 2027 rate book and
    # 2027 Star Ratings are out in 2026), so when the requested years reach the
    # current year, also look one year ahead. Explicit older ranges are untouched.
    years_i <- years
    if (annual && max(years) >= lubridate::year(Sys.Date())) {
      years_i <- c(years, max(years) + 1L)
    }
    
    for (yr in years_i) {
      for (mo in months_i) {
        ym <- if (annual) as.character(yr) else sprintf("%d-%s", yr, mo)
        landing_url <- if (page) registry$index_url[i] else
          period_index$landing_url[match(ym, period_index$period)]
        link_rx <- if (page) page_link_regex(yr, registry$link_regex[i]) else
          registry$link_regex[i]
        
        manifest <- tryCatch(
          pull_one(
            dataset_key  = registry$dataset_key[i],
            folder       = registry$folder[i],
            landing_url  = landing_url,
            year = yr, month = mo,
            root = root, manifest = manifest, force = force,
            link_regex   = link_rx
          ),
          error = function(e) {
            message(sprintf("[error] %s %s: %s",
                            registry$dataset_key[i], ym, conditionMessage(e)))
            manifest
          }
        )
      }
    }
    write_manifest(manifest, manifest_path)
  }
  
  write_manifest(manifest, manifest_path)
  message(sprintf("\nDone. Manifest saved to %s", manifest_path))
  invisible(manifest)
}
