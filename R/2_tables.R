#' Generate table showing site information for wells and baro sites
#'
#' @param park DEPO, SEKI, YOSE
#' @param type Well, Barometric
#'
#' @return Tibble
#' @export
#'
siteInformation  <- function(park, type) {

}

#' Generate table showing depth to water measurements collected during site visits at a well
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Tibble
#' @export
#'
discreteVisitData <- function(well) {

}

#' Generate table showing logger deployments at a well or baro site
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Tibble
#' @export
#'
loggerDeployments <- function(well, site) {

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
}
