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

  discrete <- discreteVisitData() |>
    dplyr::filter(WellNumber %in% well) |>
    dplyr::mutate(ArrivalTime = dplyr::case_when(is.na(ArrivalTime) ~ "12:00",
                                                 TRUE ~ ArrivalTime)) |>
    dplyr::mutate(DateTime = as.POSIXct(paste(VisitDate, ArrivalTime), format = "%Y-%m-%d %H:%M")) |>
    dplyr::rename(Well = WellNumber) |>
    dplyr::mutate(WaterLevel_Arv_cm = -Arrival_DepthToWaterFromGround_cm) |>
    dplyr::mutate(WaterLevel_Dep_cm = -Departure_DepthToWaterFromGround_cm) |>
    dplyr::select(DateTime, WaterLevel_Arv_cm, WaterLevel_Dep_cm, Identifier, Park, Well) |>
    tidyr::pivot_longer(cols = c("WaterLevel_Arv_cm", "WaterLevel_Dep_cm"), names_to = "MeasurementType", values_to = "WaterLevel_cm", values_drop_na = TRUE) |>
    dplyr::mutate(MeasurementType = dplyr::case_when(MeasurementType == "WaterLevel_Arv_cm" ~ "Arrival",
                                                     MeasurementType == "WaterLevel_Dep_cm" ~ "Departure",
                                                     TRUE ~ MeasurementType)) |>
    dplyr::mutate(Flag = as.factor("None")) |>
    dplyr::select(DateTime, WaterLevel_cm, MeasurementType, Flag, Identifier, Park, Well)

  wl_data <- data_import |>
    dplyr::select(DateTime, WaterLevel_cm, Grade, Approval) |>
    dplyr::arrange(DateTime) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "1 day")) |>
    dplyr::mutate(DateTime = dplyr::case_when(is.na(DateTime) ~ as.POSIXct(Date),
                                            TRUE ~ DateTime)) |>
    dplyr::select(-Date) |>
    dplyr::mutate(Grade = dplyr::case_when(Grade == "Unspecified" ~ "None",
                                           is.na(Grade) ~ "None",
                                           TRUE ~ Grade)) |>
    dplyr::rename(Flag = Grade) |>
    dplyr::mutate(Flag = as.factor(Flag))

  wl_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = wl_data,
                       ggplot2::aes(x = DateTime,
                                    y = WaterLevel_cm,
                                    color = Flag,
                                    linewidth = Flag,
                                    group = 1)) +
    ggplot2::scale_color_manual(values = c("Dry" = "lightgray",
                                           "Suspect" = "lightpink",
                                           "None" = "black")) +
    ggplot2::scale_linewidth_manual(values = c("Dry" = 0.8,
                                               "Suspect" = 0.8,
                                               "None" = 0.8)) +
    ggplot2::geom_point(data = discrete,
                        ggplot2::aes(x = DateTime,
                                     y = WaterLevel_cm,
                                     fill = MeasurementType),
                        color = "white",
                        shape = 23,
                        size = 4) +
    ggplot2::scale_fill_manual(values = c("Arrival" = "skyblue",
                                          "Departure" = "deepskyblue4")) +
    ggplot2::labs(x = "Date",
                  y = "Water Level (cm)",
                  fill = "Measurement Type",
                  color = "Logger Flag",
                  linewidth = "Logger Flag") +
    ggplot2::theme(legend.position = "bottom")

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
    dplyr::mutate(Grade = dplyr::case_when(Grade == "Unspecified" ~ "None",
                                           TRUE ~ Grade)) |>
    dplyr::rename(Flag = Grade) |>
    dplyr::mutate(Flag = as.factor(Flag))

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
    dplyr::mutate(Grade = dplyr::case_when(Grade == "Unspecified" ~ "None",
                                           TRUE ~ Grade)) |>
    dplyr::rename(Flag = Grade) |>
    dplyr::mutate(Flag = as.factor(Flag))

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
    dplyr::mutate(Grade = dplyr::case_when(Grade == "Unspecified" ~ "None",
                                           is.na(Grade) ~ "None",
                                           TRUE ~ Grade)) |>
    dplyr::rename(Flag = Grade) |>
    dplyr::mutate(Flag = as.factor(Flag))

  baro_plot <- ggplot2::ggplot(data = baro_data,
                               ggplot2::aes(x = DateTime,
                                            y = BaroPres_kPa)) +
    ggplot2::geom_line(ggplot2::aes(color = Flag,
                                    linewidth = Flag,
                                    group = 1)) +
    ggplot2::scale_color_manual(values = c("Suspect" = "lightpink",
                                           "None" = "black",
                                           na.translate = FALSE)) +
    ggplot2::scale_linewidth_manual(values = c("Suspect" = 0.8,
                                               "None" = 0.8)) +
    ggplot2::labs(x = "Date",
                  y = "Barometric Pressure (kPa)",
                  color = "Logger Flag",
                  linewidth = "Logger Flag") +
    ggplot2::theme(legend.position = "bottom")

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
                  y = "Percentage of Days in the Year",
                  title = paste0(unique(data_reshaped$Park, " Well ", well))) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_continuous(breaks = min(data_reshaped$WaterYear):max(data_reshaped$WaterYear))

  return(plot)
}

#' Generate map of well and baro sites
#'
#' @return Leaflet object
#' @export
#'
siteMap <- function() {

  site_import <- readAndFilterData(data.name = "Site")

  coords <- site_import |>
    dplyr::mutate(SiteName = dplyr::case_when(Type == "Barometric" ~ WetlandName,
                                              TRUE ~ NA_character_)) |>
    dplyr::mutate(Category = dplyr::case_when(Type == "Barometric" ~ "Baro",
                                              Type %in% c("Fen", "WetMeadow") ~ "Well",
                                              TRUE ~ NA_character_)) |>
    dplyr::select(Identifier,
                  Park,
                  Panel,
                  WetlandNumber,
                  WellNumber,
                  WetlandName,
                  SiteShort,
                  SiteName,
                  Category,
                  Type,
                  Latitude,
                  Longitude,
                  Easting,
                  Northing,
                  Elevation_m) |>
    dplyr::mutate(CategoryRadius = dplyr::case_when(Category == "Well" ~ 5,
                                                    Category == "Baro" ~ 3,
                                                    TRUE ~ 3))

  coords$Category <- factor(coords$Category, levels = c("Well", "Baro"))
  coords$Type <- factor(coords$Type, levels = c("Fen", "WetMeadow", "Barometric"))

  coords <- coords |> dplyr::arrange(desc(Category))

  pal <- leaflet::colorFactor(palette = c("royalblue1", "firebrick"),
                              domain = coords$Category)

  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )

  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  # width <- 700
  # height <- 700

  sitemap <- leaflet::leaflet(coords |> dplyr::filter(Category %in% c("Well", "Baro")) # ,
                              # width = width # ,
                              # height = height
  ) |>
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) |>
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) |>
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) |>
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) |>
    leaflet::addScaleBar('bottomright') |>
    leaflet::addCircleMarkers(lng = ~Longitude,
                              lat = ~Latitude,
                              popup = paste ("Identifier: ", coords$Identifier, "<br>",
                                             "Site Name: ", coords$WetlandName, "<br>",
                                             "Site Code: ", coords$SiteShort, "<br>",
                                             "Panel: ", coords$Panel, "<br>",
                                             #"Latitude (UTM): ", coords$Northing, "<br>",
                                             #"Longitude (UTM): ", coords$Easting, "<br>",
                                             "Latitude (Decimal Degrees): ", coords$Latitude, "<br>",
                                             "Longitude (Decimal Degrees): ", coords$Longitude),
                              radius = ~CategoryRadius,
                              stroke = TRUE,
                              color = "black",
                              weight = 2,
                              fillOpacity = 0.8,
                              fillColor = ~pal(Category),
                              group = ~Category) |>
    leaflet::addLegend(pal = pal,
                       values = ~Category,
                       title = "Site Type",
                       opacity = 1,
                       position = "bottomleft") |>
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                              overlayGroups = ~Category,
                              options=leaflet::layersControlOptions(collapsed = FALSE))

  return(sitemap)
}
