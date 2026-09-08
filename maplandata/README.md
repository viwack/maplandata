# maplandata

Download and load CMS Medicare Advantage / Part D contract & enrollment data
directly from cms.gov, with automatic change detection so re-running the
pipeline only re-pulls what's new.

## Installation

```r
# install.packages("devtools")  # if you don't already have it
devtools::install_github("YOUR_GITHUB_USERNAME/maplandata")
```

## Usage

```r
library(maplandata)

# Point the package at where you want data stored (once per project/session)
set_ma_plans_root("~/my-project/Data/MA_Plans")

# Pull everything from 2015 to the current year, January only
update_ma_plan_data()

# Pull just a couple of datasets/years
update_ma_plan_data(datasets = c("MA_ctrct", "MA_plan"), years = 2023:2025)

# See what's on disk
ma_data_inventory()

# Load the latest data for analysis
plan_df    <- get_ma_data("MA_plan")            # most recent year available
ctrct_2020 <- get_ma_data("MA_ctrct", year = 2020)
```

## Datasets covered

| dataset_key       | Description                                   |
|-------------------|------------------------------------------------|
| `MA_ctrct`        | Monthly Report By Contract                     |
| `MA_plan`         | Monthly Report By Plan                         |
| `MA_plan_cty`     | Monthly Enrollment by CPSC                     |
| `MA_st`           | Monthly Report By State                        |
| `MA_st_cty_ctrct` | MA Enrollment by State/County/Contract         |
| `PDP_st_cty_ctrct`| PDP Enrollment by State/County/Contract        |
| `SNP`             | SNP Comprehensive Report                       |

`*serv_area` files are generated in-house and intentionally excluded.

## License

MIT
