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
#' @param years Years to check. Default `2015:current year`.
#' @param months Months to check. Default `"01"` (January only).
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
    period_index <- get_period_index(registry$index_url[i])
    message(sprintf("[index] %s: found %d period(s) in index",
                    registry$dataset_key[i], nrow(period_index)))

    for (yr in years) {
      for (mo in months) {
        ym <- sprintf("%d-%s", yr, mo)
        landing_url <- period_index$landing_url[match(ym, period_index$period)]

        manifest <- tryCatch(
          pull_one(
            dataset_key  = registry$dataset_key[i],
            folder       = registry$folder[i],
            landing_url  = landing_url,
            year = yr, month = mo,
            root = root, manifest = manifest, force = force
          ),
          error = function(e) {
            message(sprintf("[error] %s %d-%s: %s",
                            registry$dataset_key[i], yr, mo, conditionMessage(e)))
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
