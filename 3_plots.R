#' Generate well water level plot
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Ggplot object
#' @export
#'
continuousWellLogger <- function(well) {

  data_import <- list$TimeseriesWaterLevel |>
    dplyr::filter(Well == well)

  wl_data <- data_import |>
    dplyr::select(DateTime, WaterLevel_cm, Grade, Approval) |>
    dplyr::arrange(DateTime) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |>
    dplyr::mutate(DateTime = dplyr::case_when(is.na(DateTime) ~ as.POSIXct(Date),
                                            TRUE ~ DateTime)) |>
    dplyr::select(-Date) |>
    dplyr::mutate(Grade = as.factor(Grade))

  wl_plot <- ggplot2::ggplot(data = wl_data,
                             ggplot2::aes(x = DateTime,
                                          y = WaterLevel_cm)) +
    ggplot2::geom_line(ggplot2::aes(color = Grade,
                                    linewidth = Grade,
                                    group = 1)) +
    ggplot2::scale_color_manual(values = c("Dry" = "lightgray",
                                           "Unspecified" = "black")) +
    ggplot2::scale_linewidth_manual(values = c("Dry" = 0.5,
                                               "Unspecified" = 0.8)) +
    ggplot2::labs(x = "Date",
                  y = "Water Level (cm)") +
    ggplot2::theme(legend.position = "none")

  return(wl_plot)

}

#' Generate barometric pressure plot
#'
#' @param site Three- or four-digit site code (e.g., KMMH, 315)
#'
#' @return Ggplot object
#' @export
#'
continuousBaroData <- function(site) {

  data_import <- list$TimeseriesBaroPressure |>
    dplyr::filter(Site == site)

  baro_data <- data_import |>
    dplyr::select(DateTime, BaroPres_kPa, Grade, Approval) |>
    dplyr::arrange(DateTime) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |>
    dplyr::mutate(DateTime = dplyr::case_when(is.na(DateTime) ~ as.POSIXct(Date),
                                              TRUE ~ DateTime)) |>
    dplyr::select(-Date) |>
    dplyr::mutate(Grade = as.factor(Grade))

  baro_plot <- ggplot2::ggplot(data = baro_data,
                               ggplot2::aes(x = DateTime,
                                            y = BaroPres_kPa)) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_linewidth_manual(values = 0.8) +
    ggplot2::labs(x = "Date",
                  y = "Barometric Pressure (kPa)") +
    ggplot2::theme(legend.position = "none")

  return(baro_plot)

}
