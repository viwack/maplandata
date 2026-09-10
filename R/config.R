# -----------------------------------------------------------------------------
# CONFIG
#
# The original standalone script derived MA_PLANS_ROOT from its own file
# location (Scripts/MA_plan_processing/../../Data/MA_Plans). Inside a package
# that no longer applies -- there is no "script location" -- so the data root
# is instead a package option with a sensible default, settable per-project
# with set_ma_plans_root().
# -----------------------------------------------------------------------------

#' Set the local folder where MA plan data is downloaded to / read from
#'
#' Every function in this package that touches disk takes a `root` argument
#' that defaults to this value, so you only need to set it once per session
#' (or once per project, e.g. in a `.Rprofile`).
#'
#' @param path Folder to use as the MA plan data root, e.g.
#'   `file.path(getwd(), "Data", "MA_Plans")`.
#' @return The path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' set_ma_plans_root("~/projects/my-analysis/Data/MA_Plans")
#' }
set_ma_plans_root <- function(path) {
  options(maplandata.root = path)
  invisible(path)
}

#' Current default MA plan data root
#' @keywords internal
default_ma_plans_root <- function() {
  getOption("maplandata.root", file.path(getwd(), "Data", "MA_Plans"))
}

DEFAULT_START_YEAR <- 2015            # (e) start in 2015
DEFAULT_MONTHS     <- "01"            # (e) typically only January

CMS_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Any file living inside a folder whose name ends in "_Archive" is a retired
# copy kept for reference only; see drop_archived() in manifest.R.
ARCHIVE_PATH_RE <- stringr::regex("_Archive[/\\\\]", ignore_case = TRUE)

# Registry of automated datasets -- see manifest.R / scrape.R for how it's used.
REGISTRY <- tibble::tribble(
  ~dataset_key,       ~folder,             ~label,                              ~index_url,
  "MA_ctrct",         "MA_ctrct",          "Monthly Report By Contract",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract",

  "MA_plan",          "MA_plan",           "Monthly Report By Plan",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-plan",

  "MA_plan_cty",      "MA_plan_cty",       "Monthly Enrollment by CPSC",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract/plan/state/county",

  "MA_st",            "MA_st",             "Monthly Report By State",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-state",

  "MA_st_cty_ctrct",  "MA_st_cty_ctrct",   "MA Enrollment by SCC (State/County/Contract)",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-ma-enrollment-state/county/contract",

  "PDP_st_cty_ctrct", "PDP_st_cty_ctrct",  "PDP Enrollment by SCC (State/County/Contract)",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-pdp-enrollment-state/county/contract",

  "SNP",              "SNP",               "SNP Comprehensive Report",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/special-needs-plan-snp-data",

  "MA_svc_area",      "MA_svc_area",       "MA Contract Service Area by State/County",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-contract-service-area-state/county"
)
