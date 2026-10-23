# R package: "maplandata"

The purpose of this package is to download and load CMS Medicare Advantage / Part D contract & enrollment data
directly from cms.gov, with automatic change detection, so re-running the
pipeline only re-pulls what's new.

## Installation

```r
# install.packages("devtools")  # if you don't already have it
devtools::install_github("viwack/maplandata")
```

## Usage

```r
library(maplandata)

# Point the package at where you want data stored (once per project/session)
set_ma_plans_root("~/my-project/Data/MA_Plans")

# Pull everything from 2015 to the current year (January only for the monthly
# datasets), plus the annual datasets and the landscape zip
update_ma_plan_data()

# Pull just a couple of datasets/years
update_ma_plan_data(datasets = c("MA_ctrct", "MA_plan"), years = 2023:2025)

# See what's on disk
ma_data_inventory()

# Load the latest data for analysis
plan_df    <- get_ma_data("MA_plan")            # most recent year available
ctrct_2020 <- get_ma_data("MA_ctrct", year = 2020)
```

### Newer datasets

```r
# County penetration, January of each year 2015 - present
update_ma_plan_data(datasets = "MA_st_cty_pen")
pen_2024 <- get_ma_data("MA_st_cty_pen", year = 2024)

# MA Rate Book (county benchmarks) -- just 2024
update_ma_plan_data(datasets = "MA_ratebook", years = 2024)
rb_2024 <- get_ma_data("MA_ratebook", year = 2024)

# Star Ratings / Display Measures for specific years
update_ma_plan_data(datasets = "MA_star_ratings", years = 2024:2027)
stars_2025 <- get_ma_data("MA_star_ratings", year = 2025, pattern = "summary")

# CY2006-CY2025 Landscape files: the zip is downloaded but NOT unzipped...
update_ma_plan_data(datasets = "MA_landscape")
# ...each year is extracted from it only when you first ask for it
landscape_2020 <- get_ma_data("MA_landscape", year = 2020)
landscape_new  <- get_ma_data("MA_landscape")   # latest year inside the zip
```

When a year contains several files, `get_ma_data()` loads the first CSV (or the
first file if there are no CSVs), prints the names of the others, and lets you
choose with `pattern =` (a case-insensitive regex matched against file names).

## Datasets covered

| dataset_key        | Description                                                  | Pulled                                   |
|--------------------|--------------------------------------------------------------|------------------------------------------|
| `MA_ctrct`         | Monthly Report By Contract                                   | monthly (January by default)             |
| `MA_plan`          | Monthly Report By Plan                                       | monthly (January by default)             |
| `MA_plan_cty`      | Monthly Enrollment by CPSC                                   | monthly (January by default)             |
| `MA_st`            | Monthly Report By State                                      | monthly (January by default)             |
| `MA_st_cty_ctrct`  | MA Enrollment by State/County/Contract                       | monthly (January by default)             |
| `PDP_st_cty_ctrct` | PDP Enrollment by State/County/Contract                      | monthly (January by default)             |
| `SNP`              | SNP Comprehensive Report                                     | monthly (January by default)             |
| `MA_svc_area`      | MA Contract Service Area by State/County                     | monthly (January by default)             |
| `MA_st_cty_pen`    | MA State/County Penetration                                  | monthly (January by default)             |
| `MA_ratebook`      | MA Rate Book (county benchmarks) -- the "MA Rate Book (ZIP)" only | once per year                       |
| `MA_star_ratings`  | Part C & D Performance Data: Star Ratings Data Tables and Display Measures | once per year              |
| `MA_landscape`     | CY2006-CY2025 Landscape Files                                | one zip, unzipped on demand              |

## How the different dataset types behave

| Type      | Datasets                                   | Details |
|-----------|--------------------------------------------|---------|
| Monthly   | everything in the table above except the last three rows | Walks `years` x `months` (default `2015:current year`, `"01"`). Files are unzipped into `<root>/<dataset_key>/`. |
| Annual    | `MA_ratebook`, `MA_star_ratings`           | Pulled once per year in `years`; `months` is ignored. Files are unzipped into `<root>/<dataset_key>/<year>/`, so same-named files in different years never overwrite each other. The manifest records these with month `"00"`. When `years` reaches the current year, the following year is checked too, since these are released ahead of the calendar (e.g. the 2027 rate book and Star Ratings). |
| Zip on demand | `MA_landscape`                         | The zip is saved as-is in `<root>/MA_landscape/`. `get_ma_data()` extracts only the requested year into `<root>/MA_landscape/extracted/<year>/` the first time it is needed and reuses it afterwards. If the zip is refreshed on the server, `extracted/` is cleared so stale years are re-extracted. `years` is ignored. |

Notes:

- `MA_ratebook` pulls only the "MA Rate Book (ZIP)" from each year's page, not
  the announcement PDF, EGWP rates, rate calculation data or USPCC files.
- `MA_star_ratings` pulls the Data Tables / Display Measures zips whose file
  name starts with the year; PDFs, memos and one-off files on that page are
  skipped.
- `MA_landscape` is pinned to the CY2006-CY2025 bundle. The newer single-year
  CY2026 / CY2027 landscape zips on the same CMS page are not included.

## Change detection and re-running

- By default a dataset/year that already has files on disk is skipped, so only
  genuinely new years are fetched.
- `update_ma_plan_data(force = TRUE)` re-checks the server instead. For
  `MA_landscape` it compares the server's Last-Modified/ETag with the manifest
  and only re-downloads the zip if it changed.
- Everything pulled is recorded in `<root>/_manifest.csv` (source URL, server
  timestamp/ETag, SHA-256, size, when it was pulled).
- `backfill_manifest()` registers files that were already on disk before the
  manifest existed. It skips `MA_landscape`, whose `extracted/` folder is a
  cache rather than source data.

## Functions

| Function                | Purpose                                                        |
|-------------------------|----------------------------------------------------------------|
| `update_ma_plan_data()` | Download/refresh data from CMS                                 |
| `get_ma_data()`         | Load the latest (or a given year's) local copy of a dataset    |
| `list_ma_files()`       | See what local files exist for a dataset                       |
| `ma_data_inventory()`   | Summary of everything on disk (for `MA_landscape`: years in the zip and which are extracted) |
| `backfill_manifest()`   | Register pre-existing files on disk                            |
| `set_ma_plans_root()`   | Point the package at your data folder                          |

## License

MIT