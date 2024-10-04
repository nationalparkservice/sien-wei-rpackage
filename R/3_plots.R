#' Generate well water level plot
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Ggplot object
#' @export
#'
continuousWaterLevel <- function(well) {

  data_import <- readAndFilterData(data.name = "TimeseriesWaterLevel") |>
    dplyr::filter(Well %in% well)

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

#' Generate well raw water level plot (not baro compensated)
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Ggplot object
#' @export
#'
continuousRawLevel <- function(well) {

  data_import <- readAndFilterData(data.name = "TimeseriesRawLevel") |>
    dplyr::filter(Well %in% well)

  raw_data <- data_import |>
    dplyr::select(DateTime, RawLevel_cm, Grade, Approval) |>
    dplyr::arrange(DateTime) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |>
    dplyr::mutate(DateTime = dplyr::case_when(is.na(DateTime) ~ as.POSIXct(Date),
                                              TRUE ~ DateTime)) |>
    dplyr::select(-Date) |>
    dplyr::mutate(Grade = as.factor(Grade))

  raw_plot <- ggplot2::ggplot(data = raw_data,
                              ggplot2::aes(x = DateTime,
                                          y = RawLevel_cm)) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_linewidth_manual(values = 0.8) +
    ggplot2::labs(x = "Date",
                  y = "Raw Water Level (cm))") +
    ggplot2::theme(legend.position = "none")

  return(raw_plot)
}

#' Generate well water temperature plot
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Ggplot object
#' @export
#'
continuousWaterTemperature <- function(well) {

  data_import <- readAndFilterData(data.name = "TimeseriesWaterTemperature") |>
    dplyr::filter(Well %in% well)

  wt_data <- data_import |>
    dplyr::select(DateTime, WaterTemp_C, Grade, Approval) |>
    dplyr::arrange(DateTime) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |>
    dplyr::mutate(DateTime = dplyr::case_when(is.na(DateTime) ~ as.POSIXct(Date),
                                              TRUE ~ DateTime)) |>
    dplyr::select(-Date) |>
    dplyr::mutate(Grade = as.factor(Grade))

  wt_plot <- ggplot2::ggplot(data = wt_data,
                             ggplot2::aes(x = DateTime,
                                          y = WaterTemp_C)) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_linewidth_manual(values = 0.8) +
    ggplot2::labs(x = "Date",
                  y = "Water Temperature (C))") +
    ggplot2::theme(legend.position = "none")

  return(wt_plot)
}

#' Generate barometric pressure plot
#'
#' @param site Three- or four-digit site code (e.g., KMMH, 315)
#'
#' @return Ggplot object
#' @export
#'
continuousBaroPressure <- function(site) {

  data_import <- readAndFilterData(data.name = "TimeseriesBaroPressure") |>
    dplyr::filter(Site %in% site)

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

#' Generate bar plot of percentage of days per water year where water is within 10 cm and 50 cm of the ground surface (for years where there are at least 300 days of data)
#'
#' @param well Three-digit well code (e.g., 319)
#'
#' @return Ggplot object
#' @export
#'
plotDaysWithinSurface <- function(well) {

  data_summary <- daysWithinSurface(well = well)

  data_reshaped <- data_summary |>
    tidyr::pivot_longer(!WaterYear, names_to = "Category", values_to = "Value") |>
    dplyr::mutate(Type = dplyr::case_when(grepl("Pct", Category) ~ "Percent",
                                          TRUE ~ "Count")) |>
    dplyr::mutate(Category = dplyr::case_when(Category == "DaysWithin10cm_Pct" ~ "Within10cm",
                                              Category == "DaysWithin50cm_Pct" ~ "Within50cm",
                                              TRUE ~ Category)) |>
    dplyr::mutate(Category = as.factor(Category)) |>
    dplyr::arrange(rev(Category))

  plot <- ggplot2::ggplot(data_reshaped |> dplyr::filter(Type == "Percent"),
                          ggplot2::aes(x = WaterYear,
                                       y = Value,
                                       group = Category,
                                       fill = Category)) +
    ggplot2::geom_bar(stat = "identity",
                      position = "identity",
                      color = "white") +
    ggplot2::scale_fill_manual(values = c("Within10cm" = "navy",
                                          "Within50cm" = "dodgerblue")) +
    ggplot2::labs(x = "Water Year",
                  y = "Percentage of Days in the Year") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_continuous(breaks = min(data_reshaped$WaterYear):max(data_reshaped$WaterYear))

}
