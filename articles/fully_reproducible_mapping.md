# Processing Physical Activity to a Normalized Quantile

This report follows the structure of the original project page at
[johnmuschelli.com/upper_limb_gt3x_prosthesis](https://johnmuschelli.com/upper_limb_gt3x_prosthesis/)
and the source document at
[muschellij2/upper_limb_gt3x_prosthesis/index.Rmd](https://github.com/muschellij2/upper_limb_gt3x_prosthesis/blob/master/index.Rmd).
The goal is the same: show a fully reproducible pipeline for going from
raw GT3X+ data to a summary that can be mapped with participant
demographics.

The report is written so that every download is stored on disk under
`data/` instead of [`tempfile()`](https://rdrr.io/r/base/tempfile.html).
That keeps the workflow repeatable across knits and prevents the slow
network work from running over and over.

## Setup

We first load the packages used by the analysis and tell `reticulate`
which Python environment to manage. The Python `stepcount` module is
imported explicitly so the step-counting part of the pipeline is
reproducible too.

``` r

has_reticulate = requireNamespace("reticulate", quietly = TRUE)
if (has_reticulate) {
  Sys.setenv(
    RETICULATE_PYTHON = "managed",
    PATH = paste(path.expand("~/.local/bin"), Sys.getenv("PATH"), sep = ":"),
    # R_USER_CACHE_DIR = file.path("data", "r-cache"),
    UV_CACHE_DIR = file.path("data", "uv-cache")
    # UV_PYTHON_PREFERENCE = "only-system",
    # UV_OFFLINE = "1",
    # UV_PRERELEASE = "allow"
  )
  library(reticulate)
  reticulate::py_require("stepcount==3.11.0", python_version = "3.10")
  sc <- reticulate::import("stepcount")
  try({sc$stepcount})
  if (requireNamespace("stepcount", quietly = TRUE)) {
    stepcount::stepcount_check()
  }
}
#> Downloading uv...Done!
#> [1] TRUE

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> 
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(janitor)
#> 
#> Attaching package: 'janitor'
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
if (requireNamespace("readxl", quietly = TRUE)) {
  library(readxl)
}
library(activerse)
#> Attaching activerse packages
#> - actibase 0.5.0
#> - actiread 0.4.0
#> - actimetrics 0.2.0
#> - actisensorlog 0.1.0
#> - actiwalkability 0.0.1
#> - actiquantiles 0.0.1
```

The report keeps downloaded files in a small directory structure so that
each artifact only has to be fetched once.

``` r

raw_dir <- file.path("data", "raw")
derived_dir <- file.path("data", "derived")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

download_if_missing <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(dest)) {
    download.file(url, destfile = dest, quiet = FALSE, mode = "wb")
  }
  dest
}
```

## Goal

The data come from a publicly released study of upper-limb activity in
people with and without upper-limb prostheses. The original project
pairs raw GT3X+ files with a metadata spreadsheet, then uses those data
to build an analysis that can be checked end-to-end with open-source
tooling.

This report keeps the same logic, but it emphasizes why each step
exists:

1.  Download and cache the raw accelerometer file.
2.  Read the file with an open-source GT3X reader.
3.  Process the raw samples into minute-level and daily summaries.
4.  Rebuild the demographic table from the spreadsheet used in
    `demog_mapping.R`.
5.  Combine the daily activity summary with the demographics and
    generate the mapping display.

## Download Raw GT3X+

The sample GT3X+ file is downloaded once and kept on disk. That makes
the report self-contained after the first run and avoids repeated
network access.

``` r

gt3x_url <- "https://ndownloader.figshare.com/files/21855555"
gt3x_path <- download_if_missing(
  gt3x_url,
  file.path(raw_dir, "AI1_NEO1B41100255_2016-10-17.gt3x.gz")
)
gt3x_path
#> [1] "data/raw/AI1_NEO1B41100255_2016-10-17.gt3x.gz"
```

## Read the File

The raw file is read directly from the gzipped GT3X+ archive. No
temporary copy is needed because the reader can work with the file in
place.

``` r

data <- acti_read_gt3x(gt3x_path, cleanup = TRUE)
#> ℹ Filling zeros in data
#> ✔ Filled zeros in data
#> ℹ Timezone not applied to data
data
#> # A tibble: 18,143,952 × 4
#>    time                     X      Y      Z
#>    <dttm>               <dbl>  <dbl>  <dbl>
#>  1 2016-10-17 16:00:00 -0.569 -0.106 -0.68 
#>  2 2016-10-17 16:00:00 -0.584 -0.106 -0.695
#>  3 2016-10-17 16:00:00 -0.786 -0.106 -0.768
#>  4 2016-10-17 16:00:00 -0.695 -0.082 -0.812
#>  5 2016-10-17 16:00:00 -0.78  -0.018 -0.783
#>  6 2016-10-17 16:00:00 -0.883  0.003 -0.865
#>  7 2016-10-17 16:00:00 -0.894  0.067 -0.883
#>  8 2016-10-17 16:00:00 -0.938  0.155 -0.897
#>  9 2016-10-17 16:00:00 -1.05   0.217 -0.818
#> 10 2016-10-17 16:00:00 -1.14   0.282 -0.768
#> # ℹ 18,143,942 more rows
```

## Process the Accelerometry

The helper below mirrors the original script, but the comments explain
the purpose of each stage. Optional calibration is kept as an argument
so the same function can be reused if a later analysis needs it.

``` r

checker = function(data) {
  data = data %>%
    dplyr::mutate(
      date = as_date_safe(time),
      minute = lubridate::floor_date(time, unit = "minutes"),
      hourtime = hms::as_hms(time))
  
  data = data %>%
    dplyr::distinct(date, minute, hourtime, wear)
  
  # Check for duplicates - should not be there
  suppressMessages({
    dupes = janitor::get_dupes(data, date, minute)
  })
  if (nrow(dupes) > 0) {
    stop("There are duplicate rows of date and minute!")
  }
}
acti_run <- function(data, calibrate = FALSE,
                     run_stepcount = TRUE,
                     run_forest = FALSE) {
  if (calibrate) {
    data <- acti_calibrate(data)
  }
  
  message("Processing Data")
  # The reader already returns resampled data, so we can start processing.
  processed <- acti_process(data)
  
  if (run_stepcount) {
    if (requireNamespace("stepcount", quietly = TRUE) ||
        !stepcount::have_stepcount()) {
      run_stepcount = FALSE
      cli::cli_warn(paste0("`run_stepcount` = TRUE, but 
                    {.help [have_stepcount](stepcount::have_stepcount)} ", 
                    "came back `FALSE`"))
    }
  }
  if (run_stepcount) {
    message("Running Stepcount")
    # Step counts come from the Python-backed stepcount implementation.
    steps <- acti_calculate_stepcount(data) %>% 
      rename(steps_stepcount_ssl = steps) 
    
    processed <- processed %>%
      dplyr::full_join(steps)
    
    steps <- acti_calculate_stepcount(data, model_type = "rf") %>% 
      rename(steps_stepcount_rf = steps) %>% 
      select(-any_of("walking"))
    processed <- processed %>%
      dplyr::full_join(steps)
  }
  
  # Verisense steps  
  steps <- acti_calculate_verisense(data, resample_to_15hz = TRUE,
                                    method = "original") %>% 
    rename(steps_vs_original = steps)
  processed <- processed %>%
    dplyr::full_join(steps)
  
  # Verisense steps Revised
  steps <- acti_calculate_verisense(data, resample_to_15hz = TRUE,
                                    method = "revised") %>% 
    rename(steps_vs_revised = steps)
  processed <- processed %>%
    dplyr::full_join(steps)
  
  steps <- acti_calculate_sdt(data) %>% 
    rename(steps_sdt = steps)
  processed <- processed %>%
    dplyr::full_join(steps)
  
  if (run_forest) {
    if (!reticulate::py_module_available("forest")) {
      run_forest = FALSE
      cli::cli_warn(paste0("`run_forest` = TRUE, but 
                    {.help [have_forest](walking::have_forest)} ", 
                    "came back `FALSE`"))
    }
  }
  if (run_forest) {  
    steps <- acti_calculate_forest(data) %>% 
      rename(steps_forest = steps)
    processed <- processed %>%
      dplyr::full_join(steps)  
  }
  
  
  
  # Flag which days have enough valid wear time to count as included.
  inclusion <- create_day_inclusion(processed, min_required = 1368L)
  processed <- processed %>%
    mutate(date = as.Date(time)) %>%
    left_join(inclusion)
  
  # A small helper for daily totals that ignore missing values.
  na_sum <- function(x) sum(x, na.rm = TRUE)
  
  # Collapse the minute-level data to daily totals for the measures we care about.
  daily <- processed %>%
    acti_create_date() %>%
    group_by(date) %>%
    summarise(
      dplyr::across(
        c(any_of(c("counts", "counts_log10", "MIMS", "mims")),
          starts_with("steps")
        ),
        na_sum
      )
    )
  
  daily <- daily %>%
    full_join(inclusion)
  
  list(
    minute_level = processed,
    inclusion = inclusion,
    daily = daily
  )
}
```

``` r

out <- acti_run(data)
#> Processing Data
#> [1] "Creating Downsampled Data"
#> [1] "Filtering Data"
#> [1] "Trimming Data"
#> [1] "Getting data back to 10Hz for accumulation"
#> [1] "Summing epochs"
#> Joining with `by = join_by(timestamp)`
#> Warning: `run_stepcount` = TRUE, but have_stepcount (`?stepcount::have_stepcount()`)
#> came back `FALSE`
#> Finding Peak Locations
#> Thresholding Peak by VM Threshold
#> Thresholding Peak by Periodicity
#> Thresholding Peak by Similarity
#> Thresholding Peak by Continuity
#> Joining with `by = join_by(time)`
#> Finding Peak Locations
#> Thresholding Peak by VM Threshold
#> Thresholding Peak by Periodicity
#> Thresholding Peak by Similarity
#> Thresholding Peak by Continuity
#> Joining with `by = join_by(time)`
#> Registered S3 methods overwritten by 'signal': method from print.freqs gsignal
#> print.freqz gsignal print.grpdelay gsignal plot.grpdelay gsignal print.impz
#> gsignal print.specgram gsignal plot.specgram gsignal
#> sdt completed
#> Joining with `by = join_by(time)`
#> Joining with `by = join_by(date)`
#> Joining with `by = join_by(date)`
```

The minute-level output shows the raw time series after step counts,
wear-time classification, and the log-count transform have been added.

``` r

out$minute_level %>%
  select(time, counts, starts_with("steps"), counts_log10, date, is_included) %>%
  slice_head(n = 8) %>%
  knitr::kable()
```

| time | counts | steps_vs_original | steps_vs_revised | steps_sdt | counts_log10 | date | is_included |
|:---|---:|---:|---:|---:|---:|:---|:---|
| 2016-10-17 16:00:00 | 3939 | 0 | 1 | 25 | 3.595496 | 2016-10-17 | FALSE |
| 2016-10-17 16:01:00 | 3835 | 2 | 5 | 27 | 3.583879 | 2016-10-17 | FALSE |
| 2016-10-17 16:02:00 | 1954 | 1 | 4 | 15 | 3.291147 | 2016-10-17 | FALSE |
| 2016-10-17 16:03:00 | 1771 | 2 | 1 | 9 | 3.248464 | 2016-10-17 | FALSE |
| 2016-10-17 16:04:00 | 283 | 0 | 0 | 1 | 2.453318 | 2016-10-17 | FALSE |
| 2016-10-17 16:05:00 | 333 | 0 | 0 | 1 | 2.523747 | 2016-10-17 | FALSE |
| 2016-10-17 16:06:00 | 78 | 0 | 0 | 1 | 1.897627 | 2016-10-17 | FALSE |
| 2016-10-17 16:07:00 | 20 | 0 | 0 | 0 | 1.322219 | 2016-10-17 | FALSE |

The inclusion table records which days met the wear-time threshold. The
threshold is study-specific, but the key idea is that downstream
summaries should only use days with enough valid data.

``` r

out$inclusion %>%
  slice_head(n = 8) %>%
  knitr::kable()
```

| date | n_minutes_wear | n_minutes_observed | prop_minutes_wear | prop_minutes_wear_from_observed | is_included |
|:---|---:|---:|---:|---:|:---|
| 2016-10-17 | 382 | 1440 | 0.2652778 | 0.2652778 | FALSE |
| 2016-10-18 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-19 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-20 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-21 | 1300 | 1440 | 0.9027778 | 0.9027778 | FALSE |
| 2016-10-22 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-23 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-24 | 959 | 1440 | 0.6659722 | 0.6659722 | FALSE |

The daily table is the compact summary that gets paired with the
demographic data.

``` r

out$daily %>%
  slice_head(n = 8) %>%
  knitr::kable()
```

| date | counts | counts_log10 | steps_vs_original | steps_vs_revised | steps_sdt | n_minutes_wear | n_minutes_observed | prop_minutes_wear | prop_minutes_wear_from_observed | is_included |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| 2016-10-17 | 861018 | 1038.614 | 3013 | 3123 | 8807 | 382 | 1440 | 0.2652778 | 0.2652778 | FALSE |
| 2016-10-18 | 2752651 | 2996.724 | 9130 | 9412 | 28916 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-19 | 1932515 | 2554.070 | 8703 | 9251 | 20835 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-20 | 1654903 | 2131.413 | 5838 | 6279 | 16212 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-21 | 2261210 | 2814.163 | 7546 | 7627 | 22837 | 1300 | 1440 | 0.9027778 | 0.9027778 | FALSE |
| 2016-10-22 | 1640473 | 2321.432 | 7863 | 7162 | 21173 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-23 | 1742237 | 2408.970 | 3806 | 2831 | 17726 | 1440 | 1440 | 1.0000000 | 1.0000000 | TRUE |
| 2016-10-24 | 1027828 | 1481.536 | 5047 | 5104 | 11712 | 959 | 1440 | 0.6659722 | 0.6659722 | FALSE |

## Reproduce the Demographic Data

The next section reproduces the cleaned demographic table from
[`demog_mapping.R`](https://jhuwit.github.io/activerse/articles/demog_mapping.R).
It is wrapped in a collapsible block so the report stays readable, but
the code still runs when the document is rendered. The chunk is cached
and the spreadsheet is stored locally, so the expensive work only
happens once unless the inputs change.

### Show how the demographic table is rebuilt

``` r

if (requireNamespace("readxl", quietly = TRUE)) {
  meta_url <- "https://ndownloader.figshare.com/files/21881742"
  meta_path <- download_if_missing(meta_url, file.path(raw_dir, "Metadata.xlsx"))
  
  meta <- readxl::read_excel(meta_path)
  
  # Excel sometimes creates placeholder columns with names like "..1".
  bad_col <- grepl("^\\.\\.", colnames(meta))
  colnames(meta)[bad_col] <- NA
  
  # The first couple of rows contain extra header information, so we stitch the
  # real column names together before dropping those rows.
  potential_headers <- rbind(colnames(meta), meta[1:2, ])
  potential_headers <- apply(potential_headers, 2, function(x) {
    x <- paste(na.omit(x), collapse = "")
    x <- sub(" .csv", ".csv", x)
    x <- sub(" .wav", ".wav", x)
    x <- gsub(" ", "_", x)
    x
  })
  colnames(meta) <- potential_headers
  
  # The imported table is easiest to use after dropping the extra header rows and
  # normalizing the names to snake_case.
  meta <- meta[-c(1:2), ] %>%
    janitor::clean_names()
  
  # Standardize the participant identifier and clean obvious formatting artifacts.
  meta <- meta %>%
    rename(id = participant_identifier) %>%
    filter(!is.na(id), id != "Participant Identifier") %>%
    mutate(across(where(is.character), ~ gsub("ü", "yes", .x)))
  
  # Convert the fields that should behave like numbers.
  meta <- meta %>%
    mutate(
      age = readr::parse_number(as.character(age)),
      time_since_prescription_of_a_myoelectric_prosthesis_years =
        readr::parse_number(as.character(
          time_since_prescription_of_a_myoelectric_prosthesis_years
        ))
    )
  
  # "Congenital" means there is no time since limb loss, so we encode that as 0.
  meta <- meta %>%
    mutate(
      time_since_limb_loss_years = ifelse(
        time_since_limb_loss_years == "Congenital",
        0,
        time_since_limb_loss_years
      )
    )
  
  demog_lookup <- meta %>%
    filter(id == "AI1") %>%
    select(id, gender, age, right_sensor)
  
  demog_lookup
  
  # The mapping function only needs sex and age, so we convert the cleaned table
  # into the compact input expected by the quantile map.
  demog <- demog_lookup %>%
    transmute(sex = gender, age = age)
  
  demog
  
  saveRDS(demog_lookup, file.path(derived_dir, "demog_lookup_AI1.rds"))
  saveRDS(demog, file.path(derived_dir, "demog_AI1_for_map.rds"))
}
#> New names:
#> • `` -> `...5`
#> • `` -> `...11`
#> • `` -> `...12`
#> • `` -> `...13`
#> • `` -> `...14`
#> • `` -> `...15`
#> • `` -> `...16`
#> • `` -> `...17`
#> • `` -> `...18`
#> • `` -> `...19`
#> • `` -> `...20`
```

## Combine Activity and Demographics

Now we keep only included days, average the daily summaries, and attach
the demographic fields. This is the final data frame passed to the map
function.

``` r

map_data <- out$daily %>%
  filter(is_included) %>%
  summarise(
    dplyr::across(starts_with("steps"), mean),
    ac = mean(counts)
  ) %>%
  bind_cols(demog) %>%
  pivot_longer(
    cols = c(starts_with("steps"), ac),
    names_to = "measure",
    values_to = "value"
  )

map_data
#> # A tibble: 4 × 4
#>   sex     age measure              value
#>   <chr> <dbl> <chr>                <dbl>
#> 1 F        27 steps_vs_original    7068 
#> 2 F        27 steps_vs_revised     6987 
#> 3 F        27 steps_sdt           20972.
#> 4 F        27 ac                1944556.
```

The map itself is a compact visual summary of the mean steps and
accelerometer counts for the included days, positioned relative to the
demographic inputs.

``` r

actiquantiles::acti_map_nhanes(map_data)
#> # A tibble: 4 × 5
#>   sex     age measure              value acti_pa_quantile
#>   <chr> <dbl> <chr>                <dbl>            <dbl>
#> 1 F        27 steps_vs_original    7068            0.212 
#> 2 F        27 steps_vs_revised     6987            0.250 
#> 3 F        27 steps_sdt           20972.          NA     
#> 4 F        27 ac                1944556.           0.0977
```

## Why This Structure Helps

The report separates the analysis into small, named steps so each part
is easy to verify:

1.  The raw files are downloaded to persistent paths, so future knits do
    not spend time on repeat downloads.
2.  The GT3X+ reader works directly on the saved file, which keeps the
    raw data untouched.
3.  The processing function isolates the transformation logic, making it
    easier to test or reuse.
4.  The demographic reconstruction is hidden by default but fully
    documented and cached.
5.  The final mapping step uses the cleaned, joined data rather than a
    manually assembled example table.

That makes the document suitable both as a narrative report and as a
precise reproduction of the original analysis workflow.
