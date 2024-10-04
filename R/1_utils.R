#devtools::install_github("nationalparkservice/imd-fetchaquarius")
#devtools::install_github("wright13/imd-fetchaccess-package")
library(fetchaquarius)
library(fetchaccess)

pkg_globals <- new.env(parent = emptyenv())

# Load data from global package environment
get_data <- function(data.name) {
  data <- get(data.name, pkg_globals)

  return(data)
}

#' Read continuous data from Aquarius
#'
#' @return List of tibbles
#' @export
#'
readAquarius <- function() {

  timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")

  data <- list()
  well_data <- tibble::tibble()
  raw_data <- tibble::tibble()
  watertemp_data <- tibble::tibble()
  baro_data <- tibble::tibble()
# airtemp_data <- tibble::tibble() # can be added in later if data would be useful to import

  well <- c("SIEN_WEI_DEPO_Well-409",
            "SIEN_WEI_SEKI-02_Well-305",
            "SIEN_WEI_SEKI-02_Well-306",
            "SIEN_WEI_SEKI-01_Well-308",
            "SIEN_WEI_SEKI-04_Well-310",
            "SIEN_WEI_SEKI-06_Well-314",
            "SIEN_WEI_SEKI-06_Well-315",
            "SIEN_WEI_SEKI-03_Well-319",
            "SIEN_WEI_SEKI-03_Well-320",
            "SIEN_WEI_SEKI-05_Well-345",
            "SIEN_WEI_SEKI-05_Well-348",
            "SIEN_WEI_YOSE-01_Well-214",
            "SIEN_WEI_YOSE-03_Well-224",
            "SIEN_WEI_YOSE-04_Well-301",
            "SIEN_WEI_YOSE-04_Well-327",
            "SIEN_WEI_YOSE-06_Well-313",
            "SIEN_WEI_YOSE-07_Well-328",
            "SIEN_WEI_YOSE-11_Well-322",
            "SIEN_WEI_YOSE-11_Well-323")

  baro <- c("SIEN_WEI_DEPO_MesoWest-KMMH",
            "SIEN_WEI_SEKI-02_Well-306",
#           "SIEN_WEI_SEKI-01_Well-308", # add back in once data have been appended in Aquarius
            "SIEN_WEI_SEKI-06_Well-315",
            "SIEN_WEI_SEKI_CDEC-STL",
            "SIEN_WEI_YOSE-01_Well-214",
            "SIEN_WEI_YOSE_MERC_AHSC",
            "SIEN_WEI_YOSE_MERC_ILLI",
            "SIEN_WEI_YOSE_TUOL_BUGL")

  for (location in well) {
    import <- timeseries$getTimeSeriesData(paste0("Water Level.BaroCompensated@", location))
    location_data <- import$Points |>
      dplyr::select(Timestamp,
                    NumericValue1,
                    GradeName1,
                    ApprovalName1) |>
      dplyr::rename(WaterLevel_cm = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(grepl("DEPO", SiteCode) ~ "DEPO",
                                            grepl("SEKI", SiteCode) ~ "SEKI",
                                            grepl("YOSE", SiteCode) ~ "YOSE",
                                            TRUE ~ NA_character_)) |>
      dplyr::mutate(Well = as.integer(stringr::str_sub(SiteCode, start = -3)))

    well_data <- rbind(well_data, location_data)
  }

  well_data <- list(well_data)
  names(well_data) <- "TimeseriesWaterLevel"

  for (location in well) {
    import <- timeseries$getTimeSeriesData(paste0("DepthFromWaterPressure.Raw Levelogger@", location))
    location_data <- import$Points |>
      dplyr::select(Timestamp,
                    NumericValue1,
                    GradeName1,
                    ApprovalName1) |>
      dplyr::rename(RawLevel_cm = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(grepl("DEPO", SiteCode) ~ "DEPO",
                                            grepl("SEKI", SiteCode) ~ "SEKI",
                                            grepl("YOSE", SiteCode) ~ "YOSE",
                                            TRUE ~ NA_character_)) |>
      dplyr::mutate(Well = as.integer(stringr::str_sub(SiteCode, start = -3)))

    raw_data <- rbind(raw_data, location_data)
  }

  raw_data <- list(raw_data)
  names(raw_data) <- "TimeseriesRawLevel"

  for (location in well) {
    import <- timeseries$getTimeSeriesData(paste0("Groundwater Temp at Depth.Water Temp@", location))
    location_data <- import$Points |>
      dplyr::select(Timestamp,
                    NumericValue1,
                    GradeName1,
                    ApprovalName1) |>
      dplyr::rename(WaterTemp_C = NumericValue1, Grade = GradeName1, Approval = ApprovalName1, DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(grepl("DEPO", SiteCode) ~ "DEPO",
                                            grepl("SEKI", SiteCode) ~ "SEKI",
                                            grepl("YOSE", SiteCode) ~ "YOSE",
                                            TRUE ~ NA_character_)) |>
      dplyr::mutate(Well = as.integer(stringr::str_sub(SiteCode, start = -3)))

    watertemp_data <- rbind(watertemp_data, location_data)
  }

  watertemp_data <- list(watertemp_data)
  names(watertemp_data) <- "TimeseriesWaterTemperature"

  for (location in baro) {

    if(location %in% c("SIEN_WEI_YOSE_TUOL_BUGL")) {
      import <- timeseries$getTimeSeriesData(paste0("Absolute Pressure.Baro@", location))
    } else {
      import <- timeseries$getTimeSeriesData(paste0("Atmos Pres.Baro@", location))
    }

    location_data <- import$Points |>
      dplyr::select(Timestamp, NumericValue1, GradeName1, ApprovalName1) |>
      dplyr::rename(BaroPres_kPa = NumericValue1,
                    Grade = GradeName1,
                    Approval = ApprovalName1,
                    DateTime = Timestamp) |>
      dplyr::mutate(SiteCode = location) |>
      dplyr::mutate(Park = dplyr::case_when(grepl("DEPO", SiteCode) ~ "DEPO",
                                            grepl("SEKI", SiteCode) ~ "SEKI",
                                            grepl("YOSE", SiteCode) ~ "YOSE",
                                            TRUE ~ NA_character_)) |>
      dplyr::mutate(Site = dplyr::case_when(grepl("\\d", SiteCode) ~ stringr::str_sub(SiteCode, start = -3),
                                            grepl("CDEC", SiteCode) ~ stringr::str_sub(SiteCode, start = -3),
                                            grepl("MesoWest", SiteCode) ~ stringr::str_sub(SiteCode, start = -4),
                                            TRUE ~ stringr::str_sub(SiteCode, start = -4)))

    baro_data <- rbind(baro_data, location_data)
  }

  baro_data <- list(baro_data)
  names(baro_data) <- "TimeseriesBaroPressure"

  data <- c(well_data, raw_data, watertemp_data, baro_data)

  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate(DateTime = lubridate::ymd_hms(DateTime, tz = "America/Los_Angeles", quiet = TRUE))
  })

  return(data)

}

#' Read discrete data from Access database
#'
#' @return List of tibbles
#' @export
#'
readAccess <- function() {

#  return(data)
}

#' Load data into package environment
#'
#' @param data_path Path or URL to the data. Accepted inputs:
#' * `"aquarius"` (connect to the Aquarius database)
#' * `"database"` (connect to the Access database)
#' @param use_default_sql Use default SQL database? Ignored if `data_path != "database"`.
#' @param sql_drv Driver to use to connect to database. Ignored if `data_path != "database"`.
#'
#' @return Invisibly return a list containing all data
#' @export
#'
loadWetlandWells <- function(data_path = c("database", "aquarius"),
                             use_default_sql = TRUE,
                             sql_drv = odbc::odbc()) {

  # Figure out the format of the data
  is_aquarius <- ifelse(any(grepl("^aquarius$", data_path, ignore.case = TRUE) == TRUE), TRUE, FALSE)
  # is_access <- ifelse(any(grepl("^database$", data_path, ignore.case = TRUE) == TRUE), TRUE, FALSE)
  # if (!is_access) {
  #   # Standardize data path
  #   data_path <- normalizePath(data_path[1], mustWork = TRUE)
  # }

  data <- list()

  if(is_aquarius) { # Read from Aquarius
    aquarius <- readAquarius()
    data <- append(data, aquarius)
  }

  # if(is_access) {  # Read from Access database
  #   access <- readAccess()
  #   data <- append(data, access)
  # }

  # Tidy up the data
  data <- lapply(data, function(df) {
    df |>
      dplyr::mutate_if(is.character, utf8::utf8_encode) |>
      dplyr::mutate_if(is.character, trimws, whitespace = "[\\h\\v]") |>  # Trim leading and trailing whitespace
      dplyr::mutate_if(is.character, dplyr::na_if, "") |>  # Replace empty strings with NA
      dplyr::mutate_if(is.character, dplyr::na_if, "\\\\n") |>  # Replace newlines with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -9999) |>  # Replace -9999 or -999 with NA
      dplyr::mutate_if(is.numeric, dplyr::na_if, -999) |>
      dplyr::mutate_if(is.character, dplyr::na_if, "NA") |>  # Replace "NA" strings with NA
      dplyr::mutate_if(is.character, stringr::str_replace_all, pattern = "[\\v|\\n]+", replacement = ";  ")  # Replace newlines with semicolons - reading certain newlines into R can cause problems
  })

  # Actually load the data into an environment for the package to use
  tbl_names <- names(data)
  lapply(tbl_names, function(n) {assign(n, data[[n]], envir = pkg_globals)})

  invisible(data)
}

#' Read wetland well data from all sources and filter by name of data frame
#'
#' @param data.name Name of the analysis view containing the data. E.g., "TimeseriesWaterLevel", "Site". See details for full list of data name options.
#'
#' @return Tibble of filtered data
#' @export
#'
#' @details \code{data.name} options are: Site, Visit, TimeseriesWaterLevel, TimeseriesRawPressure, TimeseriesBaroPressure, TimeseriesWaterTemperature
#'
readAndFilterData <- function(data.name) {
  filtered_data <- get_data(data.name)

  return(filtered_data)
}
