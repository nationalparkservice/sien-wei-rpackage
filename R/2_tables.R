#' Generate table showing site information for wells and baro sites
#'
#' @param park DEPO, SEKI, YOSE
#' @param type Well, Baro
#'
#' @return Tibble
#' @export
#'
siteInformation  <- function(park, type) {

  data_import <- readAndFilterData(data.name = "Site")

  if(!missing(park)) {
    data_import <- data_import |>
      dplyr::filter(Park %in% park)
  }

  if(type == "Baro") {
    data_table <- data_import|>
      dplyr::filter(SiteType %in% type) |>
      dplyr::select(Identifier, Park, SiteShort, WetlandName, Latitude, Longitude, Easting, Northing, Elevation_m) |>
      dplyr::rename(SiteName = WetlandName)
  } else if (type == "Well") {
    data_table <- data_import|>
      dplyr::filter(SiteType %in% type) |>
      dplyr::select(Identifier, Park, WetlandNumber, WellNumber, WetlandName, Type, Latitude, Longitude, Easting, Northing, Elevation_m)
  } else {
    stop("Please enter a site type argument, either 'Well' or 'Baro'.")
  }

  return(data_table)
}

#' Generate table showing depth to water measurements collected during site visits at a well
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Tibble
#' @export
#'
discreteVisitData <- function(well) {

  data_import <- readAndFilterData(data.name = "Visit")
  site_import <- readAndFilterData(data.name = "Site")

  if(!missing(well)) {
    data_import <- data_import |>
      dplyr::mutate(SiteShort = dplyr::case_when(grepl("\\d", Identifier) ~ stringr::str_sub(Identifier, start = -3),
                                                 TRUE ~ NA_character_)) |>
      dplyr::filter(SiteShort %in% well)
  }

  site_import <- site_import |>
    dplyr::select(Identifier, WetlandNumber, WellNumber, WetlandName, Type)

  data_table <- data_import |>
    dplyr::left_join(site_import, by = "Identifier") |>
    dplyr::select(Identifier,
                  Park,
                  WetlandNumber,
                  WellNumber,
                  WetlandName,
                  Type,
                  VisitDate,
                  ArrivalTime,
                  Arrival_DepthToGroundFromMP_cm,
                  Arrival_DepthToWaterFromMP_cm,
                  Arrival_DepthToWaterFromGround_cm,
                  Arrival_CableLength_cm,
                  Departure_DepthToGroundFromMP_cm,
                  Departure_DepthToWaterFromMP_cm,
                  Departure_DepthToWaterFromGround_cm,
                  Departure_CableLength_cm,
                  WellConditionNotes) |>
    dplyr::mutate(Arrival_DepthToWaterFromGround_cm = Arrival_DepthToWaterFromMP_cm - Arrival_DepthToGroundFromMP_cm,
                  Departure_DepthToWaterFromGround_cm = Departure_DepthToWaterFromMP_cm - Departure_DepthToGroundFromMP_cm)

  return(data_table)
}

#' Generate table showing logger deployments at a well or baro site
#'
#' @param well Three-digit well code (e.g., 319)
#' @param site Three- or four-digit site code (e.g., KMMH, 315)
#'
#' @return Tibble
#' @export
#'
loggerDeployments <- function(well, site) {

  dp_import <- readAndFilterData(data.name = "Deployment")
  rc_import <- readAndFilterData(data.name = "Recovery")
  sens_import <- readAndFilterData(data.name = "Sensor")
  site_import <- readAndFilterData(data.name = "Site")

  site_import <- site_import |>
    dplyr::select(Identifier, WetlandNumber, WellNumber, WetlandName, SiteShort)

  sens_import <- sens_import |>
    dplyr::select(SerialNumber, Model, Type) |>
    unique()

  data_joined <- dp_import |>
    dplyr::left_join(site_import, by = "Identifier", relationship = "many-to-many") |>
    dplyr::left_join(sens_import, by = "SerialNumber", relationship = "many-to-one")

  if(!missing(well)) {
    data_table <- data_joined |>
      dplyr::filter(SiteShort %in% well,
                    Subsite == "Well"
                    ) |>
      dplyr::select(Identifier,
                    Park,
                    WellNumber,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DeploymentDate,
                    DeploymentTime,
                    DeploymentNotes) |>
      unique()
  } else if(!missing(site)) {
    data_table <- data_joined |>
      dplyr::filter(SiteShort %in% site,
                    Subsite == "Baro"
                    ) |>
      dplyr::select(Identifier,
                    Park,
                    SiteShort,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DeploymentDate,
                    DeploymentTime,
                    DeploymentNotes) |>
      unique()
  } else {
    data_table <- data_joined |>
      dplyr::select(Identifier,
                    Park,
                    SiteShort,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DeploymentDate,
                    DeploymentTime,
                    DeploymentNotes)
  }

  return(data_table)
}

#' Generate table showing logger downloads at a well or baro site
#'
#' @param well Three-digit well code (e.g., 319)
#' @param site Three- or four-digit site code (e.g., KMMH, 315)
#'
#' @return Tibble
#' @export
#'
loggerDownloads <- function(well, site) {

  rc_import <- readAndFilterData(data.name = "Recovery")
  sens_import <- readAndFilterData(data.name = "Sensor")
  site_import <- readAndFilterData(data.name = "Site")

  site_import <- site_import |>
    dplyr::select(Identifier, WetlandNumber, WellNumber, WetlandName, SiteShort)

  rc_import <- rc_import |>
    dplyr::select(Identifier, VisitDate, DownloadSuccesful) |>
    dplyr::rename(DownloadSuccessful = DownloadSuccesful)

  data_joined <- sens_import |>
    dplyr::rename(DeploymentSerialNumber = SerialNumber,
                  DeploymentModel = Model,
                  DeploymentType = Type) |>
    dplyr::rename(SerialNumber = SerialNumber.1,
                  Model = Model.1,
                  Type = Type.1) |>
    dplyr::mutate(DownloadDate = as.Date(DownloadDate, "%m/%d/%y")) |>
    dplyr::left_join(site_import, by = "Identifier", relationship = "many-to-many") |>
    dplyr::left_join(rc_import, by = c("Identifier", "VisitDate"), relationship = "many-to-many")

  if(!missing(well)) {
    data_table <- data_joined |>
      dplyr::filter(SiteShort %in% well) |>
      dplyr::select(Identifier,
                    Park,
                    WellNumber,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DownloadSuccessful,
                    DownloadDate,
                    DownloadTime,
                    DownloadNotes)
  } else if(!missing(site)) {
    data_import <- data_joined |>
      dplyr::filter(SiteShort %in% site) |>
      dplyr::select(Identifier,
                    Park,
                    SiteShort,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DownloadSuccessful,
                    DownloadDate,
                    DownloadTime,
                    DownloadNotes)
  } else {
    data_table <- data_joined |>
      dplyr::select(Identifier,
                    Park,
                    SiteShort,
                    WetlandName,
                    VisitDate,
                    SerialNumber,
                    Model,
                    Type,
                    DownloadSuccessful,
                    DownloadDate,
                    DownloadTime,
                    DownloadNotes)
  }

  return(data_table)
}

#' Generate table showing the count and percentage of days per water year where water is within 10 cm and 50 cm of the ground surface (for years where there are at least 300 days of data)
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Tibble
#' @export
#'
daysWithinSurface <- function(well) {

  data_import <- readAndFilterData(data.name = "TimeseriesWaterLevel") |>
    dplyr::filter(Well %in% well)

  data_summary <- data_import |>
    dplyr::filter(!(Grade %in% c("Dry"))) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    dplyr::group_by(Date) |>
    dplyr::summarize(MeanWaterLevel_cm = mean(WaterLevel_cm)) |>
    dplyr::ungroup() |>
    dplyr::mutate(WaterYear = ifelse(lubridate::month(Date) < 10,
                                     lubridate::year(Date),
                                     lubridate::year(Date) + 1)) |>
    dplyr::group_by(WaterYear) |>
    dplyr::summarize(DaysWithin10cm = sum(MeanWaterLevel_cm > -10),
                     DaysWithin50cm = sum(MeanWaterLevel_cm > -50),
                     n = dplyr::n()) |>
    dplyr::filter(n > 340) |>
    dplyr::mutate(DaysWithin10cm_Pct = DaysWithin10cm/n*100,
                  DaysWithin50cm_Pct = DaysWithin50cm/n*100) |>
    dplyr::select(-n)

  return(data_summary)
}
