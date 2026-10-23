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
#
#   period_type : "month" -> the index page lists one landing page per YYYY-MM
#                            (pulled for every year x month requested).
#                 "year"  -> the index page lists one landing page per YYYY
#                            (pulled once per year; `months` is ignored and the
#                            manifest records month = "00").
#                 "page"  -> ONE page lists files for many years (no index of
#                            periods, no per-year landing pages). For each year
#                            requested, the links whose file name STARTS with
#                            that year (and match link_regex) are pulled into
#                            <folder>/<year>/. Treated like "year" otherwise.
#                 "static" -> a single page with a single big zip (no index of
#                            periods). The zip is downloaded but left packed;
#                            get_ma_data() extracts one year on demand (lazy.R).
#   link_regex  : NA -> take every downloadable file on the landing page.
#                 otherwise only links whose href OR link text matches this
#                 (case-insensitive) regex are pulled. Needed when a landing
#                 page carries several files and only one is wanted.
REGISTRY <- tibble::tribble(
  ~dataset_key,       ~folder,             ~label,                              ~index_url,  ~period_type, ~link_regex,
  "MA_ctrct",          "MA_ctrct",           "Monthly Report By Contract",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract",
  "month", NA_character_,
  
  "MA_plan",           "MA_plan",            "Monthly Report By Plan",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-plan",
  "month", NA_character_,
  
  "MA_plan_cty",       "MA_plan_cty",        "Monthly Enrollment by CPSC",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract/plan/state/county",
  "month", NA_character_,
  
  "MA_st",             "MA_st",              "Monthly Report By State",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-state",
  "month", NA_character_,
  
  "MA_st_cty_ctrct",   "MA_st_cty_ctrct",    "MA Enrollment by SCC (State/County/Contract)",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-ma-enrollment-state/county/contract",
  "month", NA_character_,
  
  "PDP_st_cty_ctrct",  "PDP_st_cty_ctrct",   "PDP Enrollment by SCC (State/County/Contract)",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-pdp-enrollment-state/county/contract",
  "month", NA_character_,
  
  "SNP",               "SNP",                "SNP Comprehensive Report",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/special-needs-plan-snp-data",
  "month", NA_character_,
  
  "MA_svc_area",       "MA_svc_area",        "MA Contract Service Area by State/County",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-contract-service-area-state/county",
  "month", NA_character_,
  
  "MA_st_cty_pen",     "MA_st_cty_pen",      "MA State/County Penetration",
  "https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-state/county-penetration",
  "month", NA_character_,
  
  # Annual ratebook (county benchmarks). The yearly landing page also holds the
  # announcement PDF, regional PPO/EGWP rates, rate calculation data and USPCC
  # zips -- link_regex keeps only the "<year> MA Rate Book (ZIP)" file.
  "MA_ratebook",      "MA_ratebook",       "MA Rate Book (County Benchmarks)",
  "https://www.cms.gov/medicare/payment/medicare-advantage-rates-statistics/ratebooks-supporting-data",
  "year", "rate[ _-]?book",
  
  # CY2006-CY2025 Landscape Files: one zip on the Prescription Drug Coverage
  # page (that page doubles as the "landing page"; there is no index). The page
  # also lists newer single-year CY2026/CY2027 landscape zips and congressional
  # district reports -- link_regex pins this to the 2006-2025 bundle only.
  "MA_landscape",     "MA_landscape",      "CY2006-CY2025 Landscape Files",
  "https://www.cms.gov/medicare/coverage/prescription-drug-coverage",
  "static", "cy2006-cy2025-landscape",
  
  # Part C & D Performance Data: one page holding Star Ratings Data Tables and
  # Display Measures zips for every year (plus PDFs, memos and one-off files
  # that link_regex leaves out). Years run ahead of the calendar: the 2027 Star
  # Ratings were released in Oct 2026.
  "MA_star_ratings",  "MA_star_ratings",   "Part C & D Performance Data (Star Ratings & Display Measures)",
  "https://www.cms.gov/medicare/health-drug-plans/part-c-d-performance-data",
  "page", "star[ _-]?ratings|display[ _-]?measures|plan[ _-]?ratings"
)
