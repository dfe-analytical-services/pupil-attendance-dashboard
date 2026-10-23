# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
#
# Install packages -----------------------------------------------
# Run renv::restore()
# If it doesn't work first time, maybe try renv::activate() and then renv::restore()

message("Loading packages")
# Library calls ----------------------------------------------------------------------------------
library(rsconnect)
library(shinyGovstyle)
library(bslib)
library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(data.table)
library(ggplot2)
library(ggiraph)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(metathis)
library(styler)
library(bit64)
library(raster)
library(leaflet)
library(sf)
library(checkmate)
library(dfeshiny)
library(reactable)
library(shinytest2)
library(diffviewer)
library(dplyr)
library(eesyapi)
library(plotly)

# Few things to manage fonts
library(gfonts)
library(gdtools)
library(showtext)
gdtools::register_gfont("Noto Sans")
sysfonts::font_add_google("Noto Sans")
showtext::showtext_auto()
dfe_font <- "Noto Sans"
message("Selected ", dfe_font, " for plots")

# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
ees_api_env <- "prod"

# -------------------------------------------------------------------------
# Optional preview-token configuration
#
# For each dataset:
# - use the latest pre-release version when its preview token works;
# - otherwise fall back to the latest published version;
# - do not pass an invalid preview token into later API calls.
# -------------------------------------------------------------------------

read_optional_secret <- function(name) {
  value <- trimws(Sys.getenv(name, unset = ""))

  if (nzchar(value)) {
    value
  } else {
    NULL
  }
}

reasons_preview_token <- read_optional_secret(
  "REASONS_PREVIEW_TOKEN"
)

schools_preview_token <- read_optional_secret(
  "SCHOOLS_PREVIEW_TOKEN"
)


# -------------------------------------------------------------------------
# Dataset configuration
# -------------------------------------------------------------------------

if (ees_api_env == "prod") {
  api_verbose <- FALSE
  api_environment <- "prod"

  reasons_dataset_id <- "63629501-d3ca-c471-9780-ec4cb6fdf172"
  reasons_dataset_ref_version <- 1.0

  persistent_absence_dataset_id <-
    "55629501-e98b-0c75-adba-f95a0cfbb5e9"
  persistent_absence_ref_version <- 1.0

  schools_submitting_dataset_id <-
    "55629501-1945-0174-956c-594f21c90404"
  schools_submitting_ref_version <- 1.0
} else if (ees_api_env == "dev") {
  api_verbose <- FALSE
  api_environment <- "dev"

  reasons_dataset_id <- "ab619501-50cf-1b70-b276-76d72a3c141c"
  reasons_dataset_ref_version <- 1.1

  persistent_absence_dataset_id <-
    "3b299501-2ab1-de76-a9b5-a3ae26ac0bcd"
  persistent_absence_ref_version <- 3.0

  schools_submitting_dataset_id <-
    "ac619501-eb0a-ff71-b03c-5330fe30349a"
  schools_submitting_ref_version <- 1.1
} else if (ees_api_env == "test") {
  api_verbose <- TRUE
  api_environment <- "test"

  reasons_dataset_id <- "8e8c9301-55c5-3e71-abbb-73ac64420c4a"
  reasons_dataset_ref_version <- 2.0

  persistent_absence_dataset_id <- ""
  persistent_absence_ref_version <- 1.0

  schools_submitting_dataset_id <- ""
  schools_submitting_ref_version <- 1.0
} else {
  stop("Invalid environment given in ees_api_env variable.")
}


# -------------------------------------------------------------------------
# Select the latest usable dataset version
#
# The function returns:
# - version: selected API dataset version;
# - preview_token: original token when preview succeeds, otherwise NULL;
# - using_preview: TRUE when using pre-release data;
# - source: "pre-release" or "published".
# -------------------------------------------------------------------------

get_active_dataset <- function(
  dataset_id,
  preview_token = NULL,
  dataset_name = dataset_id
) {
  select_latest_version <- function(versions, published) {
    if (
      is.null(versions) ||
        !is.data.frame(versions) ||
        nrow(versions) == 0
    ) {
      return(NULL)
    }

    required_columns <- c("version", "release_date")

    if (!all(required_columns %in% names(versions))) {
      return(NULL)
    }

    versions <- versions |>
      dplyr::mutate(
        release_date_missing = is.na(release_date) |
          trimws(as.character(release_date)) == "",
        version_character = trimws(as.character(version)),
        version_numeric = suppressWarnings(
          as.numeric(version_character)
        )
      ) |>
      dplyr::filter(
        !is.na(version_character),
        nzchar(version_character)
      )

    if (published) {
      versions <- versions |>
        dplyr::filter(!release_date_missing)
    } else {
      versions <- versions |>
        dplyr::filter(release_date_missing)
    }

    if (nrow(versions) == 0) {
      return(NULL)
    }

    selected_version <- versions |>
      dplyr::arrange(
        dplyr::desc(version_numeric),
        dplyr::desc(version_character)
      ) |>
      dplyr::slice(1) |>
      dplyr::pull(version_character)

    if (
      length(selected_version) != 1 ||
        is.na(selected_version) ||
        !nzchar(selected_version)
    ) {
      return(NULL)
    }

    selected_version
  }

  # -----------------------------------------------------------------------
  # First try the preview token, where one has been supplied
  # -----------------------------------------------------------------------

  if (!is.null(preview_token)) {
    message(
      dataset_name,
      ": preview token found; checking for a pre-release version."
    )

    preview_versions <- tryCatch(
      eesyapi::get_dataset_versions(
        dataset_id,
        preview_token = preview_token,
        ees_environment = ees_api_env
      ),
      error = function(e) {
        warning(
          dataset_name,
          ": preview request failed. Falling back to published data. ",
          "API message: ",
          conditionMessage(e),
          call. = FALSE
        )

        NULL
      }
    )

    preview_version <- select_latest_version(
      versions = preview_versions,
      published = FALSE
    )

    if (!is.null(preview_version)) {
      message(
        dataset_name,
        ": using pre-release version ",
        preview_version,
        "."
      )

      return(
        list(
          version = preview_version,
          preview_token = preview_token,
          using_preview = TRUE,
          source = "pre-release"
        )
      )
    }

    message(
      dataset_name,
      ": no usable pre-release version found; ",
      "falling back to the latest published version."
    )
  } else {
    message(
      dataset_name,
      ": no preview token supplied; selecting the latest published version."
    )
  }

  # -----------------------------------------------------------------------
  # Retrieve published versions without passing the preview token
  # -----------------------------------------------------------------------

  published_versions <- tryCatch(
    eesyapi::get_dataset_versions(
      dataset_id,
      ees_environment = ees_api_env
    ),
    error = function(e) {
      stop(
        dataset_name,
        ": unable to retrieve published dataset versions. ",
        "API message: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  published_version <- select_latest_version(
    versions = published_versions,
    published = TRUE
  )

  if (is.null(published_version)) {
    stop(
      dataset_name,
      ": the API returned no usable published dataset version.",
      call. = FALSE
    )
  }

  message(
    dataset_name,
    ": using published version ",
    published_version,
    "."
  )

  list(
    version = published_version,
    preview_token = NULL,
    using_preview = FALSE,
    source = "published"
  )
}


# -------------------------------------------------------------------------
# Select the active Schools dataset
# -------------------------------------------------------------------------

schools_dataset <- get_active_dataset(
  dataset_id = schools_submitting_dataset_id,
  preview_token = schools_preview_token,
  dataset_name = "Schools"
)

schools_dataset_version <- schools_dataset$version

# This is automatically NULL when Schools falls back to published data.
schools_active_preview_token <- schools_dataset$preview_token


# -------------------------------------------------------------------------
# Select the active Reasons dataset
# -------------------------------------------------------------------------

reasons_dataset <- get_active_dataset(
  dataset_id = reasons_dataset_id,
  preview_token = reasons_preview_token,
  dataset_name = "Reasons"
)

reasons_dataset_version <- reasons_dataset$version

# This is automatically NULL when Reasons falls back to published data.
reasons_active_preview_token <- reasons_dataset$preview_token


# -------------------------------------------------------------------------
# Preview status
#
# TRUE if at least one dataset is successfully using preview data.
# -------------------------------------------------------------------------

preview_mode <-
  reasons_dataset$using_preview ||
    schools_dataset$using_preview

message("Preview data configured: ", preview_mode)

message(
  "Reasons preview token configured: ",
  !is.null(reasons_preview_token)
)

message(
  "Schools preview token configured: ",
  !is.null(schools_preview_token)
)

message(
  "Reasons using preview data: ",
  reasons_dataset$using_preview
)

message(
  "Schools using preview data: ",
  schools_dataset$using_preview
)

message(
  "Schools dataset version selected: ",
  schools_dataset_version,
  " (",
  schools_dataset$source,
  ")"
)

message(
  "Reasons dataset version selected: ",
  reasons_dataset_version,
  " (",
  reasons_dataset$source,
  ")"
)


# -------------------------------------------------------------------------
# Source supporting scripts
# -------------------------------------------------------------------------

message("Sourcing prerun utils")
source("R/prerun_utils.R")

message("Sourcing fetch data")
source("R/fetch_data.R")


# -------------------------------------------------------------------------
# Site global variables
# -------------------------------------------------------------------------

message("Setting site global variables")

site_title <- "Pupil attendance and absence in schools in England"

site_primary <-
  "https://department-for-education.shinyapps.io/pupil-attendance-in-schools"

site_c <- ""

google_analytics_key <- "DG7P4WLB0Y"

ees_pub_name <- "Pupil attendance in schools"
ees_pub_slug <- "pupil-attendance-in-schools"

team_email <- "schools.statistics@education.gov.uk"


# -------------------------------------------------------------------------
# Standard geography lookup
# -------------------------------------------------------------------------

region_la_lookup <- dfeR::wd_pcon_lad_la_rgn_ctry |>
  dplyr::filter(country_name == "England") |>
  dplyr::select(
    region_name,
    region_code,
    la_name,
    new_la_code
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(region_name)


# -------------------------------------------------------------------------
# Pull in SQID lookups
#
# The active preview tokens must be used here.
#
# If preview was successful:
# - the selected pre-release version and token are passed.
#
# If preview failed:
# - the selected published version is passed;
# - the active token is NULL.
# -------------------------------------------------------------------------

message("Pulling in SQID lookups from API")

reasons_sqids <- fetch_sqid_lookup(
  reasons_dataset_id,
  version = reasons_dataset_version,
  preview_token = reasons_active_preview_token,
  ees_environment = ees_api_env
)

persistent_absence_sqids <- fetch_sqid_lookup(
  persistent_absence_dataset_id,
  version = persistent_absence_ref_version,
  ees_environment = ees_api_env
)

schools_submitting_sqids <- fetch_sqid_lookup(
  schools_submitting_dataset_id,
  version = schools_dataset_version,
  preview_token = schools_active_preview_token,
  ees_environment = ees_api_env
)

# Data manipulation ----------------------------------------------------------------------------
# Read in data
# attendance_data_raw <- fread("data/Weekly_dummy_data.csv")

#### SECTION 1 - date filters ####
message("Setting dates")
date_stamp <- lubridate::stamp_date("20 March 2025")
start_date <- as.Date("2025-09-08")
end_date <- as.Date("2026-04-03")
# funeral_date <- as.Date("2022-09-19")
# strike_date_1 <- as.Date("2023-02-01")
# strike_date_2 <- as.Date("2023-03-15")
# strike_date_3 <- as.Date("2023-03-16")
# strike_date_4 <- as.Date("2023-04-27")
# strike_date_5 <- as.Date("2023-05-02")
# strike_date_6 <- as.Date("2023-07-05")
# strike_date_7 <- as.Date("2023-07-07")

# regional_strike_1 <- as.Date("2023-02-28")
# regional_strike_2 <- as.Date("2023-03-01")
# regional_strike_3 <- as.Date("2023-03-02")

autumn_start <- as.Date("2024-09-09")
autumn_end <- as.Date("2024-12-21")
# spring_start <- as.Date("2025-01-06")
# spring_end <- as.Date("2024-03-31")
# summer_start <- as.Date("2024-04-01")
# summer_end <- as.Date("2024-07-19")

most_recent_week_dates <- paste0(
  "Latest week - ",
  as.Date(end_date) - 4,
  " to ",
  as.Date(end_date)
)
ytd_dates <- paste0(
  "Year to date - ",
  as.Date(start_date),
  " to ",
  as.Date(end_date)
)

#### SECTION 2 - reading in csvs to run dashboard ####
attendance_data <- read.csv("data/attendance_data_dashboard.csv")
attendance_data$attendance_date <- as.Date(attendance_data$attendance_date)
attendance_data$week_commencing <- as.Date(attendance_data$week_commencing)

message(paste("Finished processing steps, ", Sys.time()))

EES_daily_data <- create_EES_daily_data(attendance_data)

#### SECTION 3 - Lookups ####
# Add geog lookup
geog_lookup <- attendance_data %>%
  dplyr::select(geographic_level, region_name, la_name) %>%
  unique() %>%
  arrange(region_name, la_name) %>%
  mutate(
    la_name = case_when(
      geographic_level == "Regional" ~ "All",
      geographic_level != "Regional" ~ la_name
    )
  )

school_type_lookup <- attendance_data %>%
  dplyr::select(geographic_level, school_type) %>%
  unique() %>%
  arrange(geographic_level, school_type)

# Combined local authority and region list
la_list <- geog_lookup %>%
  dplyr::select(region_name, la_name) %>%
  filter(region_name != "All") %>%
  filter(la_name != "NA") %>%
  distinct() %>%
  arrange(region_name, la_name) %>%
  group_by(region_name) %>%
  dplyr::select(region_name, la_name) %>%
  group_split(.keep = FALSE) %>%
  unlist(recursive = FALSE)

names(la_list) <- geog_lookup %>%
  dplyr::select(region_name) %>%
  filter(region_name != "All") %>%
  distinct() %>%
  pull(region_name) %>%
  sort()

# date filter lookups
most_recent_week_lookup <- attendance_data %>%
  group_by(geographic_level, region_name, la_name) %>%
  filter(time_period == max(time_period)) %>%
  filter(time_identifier == max(time_identifier)) %>%
  mutate(
    week_start = min(attendance_date),
    week_end = max(attendance_date)
  ) %>%
  dplyr::select(
    geographic_level,
    region_name,
    la_name,
    week_start,
    week_end
  ) %>%
  distinct()

year_lookup <- attendance_data %>%
  group_by(geographic_level, region_name, la_name) %>%
  mutate(
    year_start = min(attendance_date),
    year_end = max(attendance_date)
  ) %>%
  dplyr::select(
    geographic_level,
    region_name,
    la_name,
    year_start,
    year_end
  ) %>%
  distinct()

# Notes tables----------------------------------

notesTableHeadlines <- fread("data/Tech_guidance_headlines.csv")
notesTableReasons <- fread("data/Tech_guidance_reasons.csv")
notesTableLa <- fread("data/Tech_guidance_la.csv")

geog_levels <- geog_lookup %>%
  dplyr::select(geographic_level) %>%
  unique() %>%
  as.data.table()

regions <- geog_lookup %>%
  filter(geographic_level == "Regional") %>%
  arrange(region_name) %>%
  pull(region_name) %>%
  unique()
las <- geog_lookup %>%
  filter(geographic_level == "Local authority") %>%
  arrange(region_name, la_name) %>%
  pull(la_name) %>%
  unique()

#### SECTION 4 - Functions ####
# Expandable dropdown function----------------------------------
expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details",
    id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}


#### SECTION 5 - Map ####
## Reading in data ##########################################################

# Read in shapefile and transform coordinates (because map reasons...)
mapshape <- st_read("data/CTYUA_MAY_2023_UK_BUC.shp") %>%
  st_transform(crs = 4326) # %>%
#  mutate(CTYUA23CD = case_when(
#    CTYUA23NM == "Somerset" ~ "E10000027",
#    CTYUA23NM != "Somerset" ~ CTYUA23CD)
#  ) # TEMP addition working around Somerset LA code change

# Pull in the colours from another script
source("R/gov_colours.R")
