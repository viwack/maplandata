#' maplandata: Download and Load CMS Medicare Advantage Plan Data
#'
#' Pulls Medicare Advantage / Part D contract and enrollment data files
#' directly from cms.gov, detects when a source file has changed so re-runs
#' only re-pull what's new, and loads the latest local copy of any dataset
#' into R.
#'
#' Main functions:
#' \itemize{
#'   \item [update_ma_plan_data()] -- download/refresh data from CMS.
#'   \item [get_ma_data()] -- load the latest local copy of a dataset.
#'   \item [list_ma_files()] -- see what local files exist for a dataset.
#'   \item [ma_data_inventory()] -- summary of everything on disk.
#'   \item [backfill_manifest()] -- register pre-existing files on disk.
#'   \item [set_ma_plans_root()] -- point the package at your data folder.
#' }
#'
#' @keywords internal
"_PACKAGE"
