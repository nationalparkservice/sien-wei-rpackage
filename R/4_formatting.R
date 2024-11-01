#' Standardize formatting of baro logger deployments table returned by reactable package
#'
#' @param site Three- or four-digit site code (e.g., KMMH, 315)
#'
#' @return Reactable HTML object
#' @export
#'
loggerBaroDeploymentsReactable <- function(site) {
  t <- loggerDeployments(site = site) |>
    dplyr::select(-c("Identifier", "Park", "WetlandName", "SiteShort", "Type")) |>
    dplyr::filter(!is.na(SerialNumber))

  t <- t |>
    reactable::reactable(
      sortable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        DeploymentNotes = reactable::colDef(width = 400)
      ),
      defaultColDef = reactable::colDef(
        header = function(value) snakecase::to_title_case(value))
    )
  return(t)
}

#' Standardize formatting of well logger deployments table returned by reactable package
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Reactable HTML object
#' @export
#'
loggerWellDeploymentsReactable <- function(well) {
  t <- loggerDeployments(well = well) |>
    dplyr::select(-c("Identifier", "Park", "WetlandName", "WellNumber", "Type", "DeploymentDate", "DeploymentTime")) |>
    dplyr::filter(!is.na(SerialNumber))

  t <- t |>
    reactable::reactable(
      sortable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        DeploymentNotes = reactable::colDef(width = 400)
      ),
      defaultColDef = reactable::colDef(
        header = function(value) snakecase::to_title_case(value))
    )
  return(t)
}

#' Standardize formatting of discrete well visit data table returned by reactable package
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Reactable HTML object
#' @export
#'
discreteVisitDataReactable <- function(well) {
  t <- discreteVisitData(well = well) |>
    dplyr::select(-c("Identifier",
                     "Park",
                     "WetlandName",
                     "WetlandNumber",
                     "WellNumber",
                     "Type",
                     "ArrivalTime",
                     "Arrival_DepthToGroundFromMP_cm",
                     "Arrival_DepthToWaterFromMP_cm",
                     "Departure_DepthToGroundFromMP_cm",
                     "Departure_DepthToWaterFromMP_cm",
                     "Arrival_CableLength_cm",
                     "Departure_CableLength_cm")) |>
    dplyr::rename(ArrivalWaterDepth_cm = Arrival_DepthToWaterFromGround_cm,
                  DepartureWaterDepth_cm = Departure_DepthToWaterFromGround_cm) |>
    dplyr::mutate(ArrivalWaterDepth_cm = round(ArrivalWaterDepth_cm, digits = 1),
                  DepartureWaterDepth_cm = round(DepartureWaterDepth_cm, digits = 1))

  t <- t |>
    reactable::reactable(
      sortable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        WellConditionNotes = reactable::colDef(width = 400)
      ),
      defaultColDef = reactable::colDef(
        header = function(value) snakecase::to_title_case(value)
      )
    )
  return(t)
}

#' Standardize formatting of well site information table returned by reactable package
#'
#' @param park
#'
#' @return Reactable HTML object
#' @export
#'
siteInformationWellReactable <-function(park) {
  t <- siteInformation(park = park, type = "Well") |>
    dplyr::select(-c("Identifier", "Park", "Easting", "Northing")) |>
    dplyr::relocate(WetlandName, .before = WetlandNumber) |>
    dplyr::mutate(Type = dplyr::case_when(Type == "WetMeadow" ~ "Wet Meadow",
                                          TRUE ~ Type)) |>
    dplyr::arrange(WetlandNumber, WellNumber)

  t <- t |>
    reactable::reactable(
      sortable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        Type = reactable::colDef(width = 120),
        Longitude = reactable::colDef(width = 120)
      ),
      defaultColDef = reactable::colDef(
        header = function(value) snakecase::to_title_case(value))
    )
  return(t)
}

#' Standardize formatting of baro site information table returned by reactable package
#'
#' @param park Four-digit park code (e.g., YOSE)
#'
#' @return Reactable HTML object
#' @export
#'
siteInformationBaroReactable <- function(park) {
  t <- siteInformation(park = park, type = "Baro") |>
    dplyr::select(-c("Identifier", "Park", "Easting", "Northing")) |>
    dplyr::relocate(SiteName, .before = SiteShort) |>
    dplyr::rename(SiteCode = SiteShort)

  t <- t |>
    reactable::reactable(
      sortable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      striped = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        Longitude = reactable::colDef(width = 120)
      ),
      defaultColDef = reactable::colDef(
        header = function(value) snakecase::to_title_case(value))
    )
  return(t)
}

fixLines <- function(plt) {
  lapply(1:length(plt$x$data),
         function(j) {
           if(plt$x$data[[j]]$mode == "lines") {
             plt$x$data[[j]]$showlegend <<- F
           }
         })
  plt
}
