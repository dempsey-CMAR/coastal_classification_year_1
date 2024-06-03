# November 23, 2023

# Adds circle markers to leaflet plot at LATITUDE and LONGITUDE, with a STATION label.

# dat: data frame with columns LATITUDE, LONGITUDE, and STATION.
# map: a map widget object created from leaflet().
# fill_col: colour to fill the circle marker.
# group: group that the layer belongs to (for interactive layer).
# rad: radius of circle markers

add_circle_markers <- function(map, dat, fill_col, group, rad = 5) {

  addCircleMarkers(
    map = map,
    data = dat,
    lng = ~longitude, lat = ~latitude, label = ~station,
    weight = 1,
    color = "black",
    fillOpacity = 0.75,

    fillColor = fill_col,
    radius = rad,
    group = group
  )

}


# Adds circle markers to leaflet plot at LATITUDE and LONGITUDE,
# with a STATION label. Coloured and grouped by n_year.

# dat: data frame with columns LATITUDE, LONGITUDE, and STATION.
# pal: colour pal fill the circle markers.
# rad: radius of circle markers

map_stations_by_years_data <- function(
    dat,
    pal,
    legend_title = "",
    legend_lab,
    rad = 5) {

  leaflet(dat, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 0), fill_col = pal[1], group = "0"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 1), fill_col = pal[2], group = "1"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 2), fill_col = pal[3], group = "2"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 3), fill_col = pal[4], group = "3"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 4), fill_col = pal[5], group = "4"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year == 5), fill_col = pal[6], group = "5"
    ) %>%
    add_circle_markers(
      dat = filter(dat, n_year >= 6), fill_col = pal[7], group = "6+"
    ) %>%
    addLegend(
      title = legend_title,
      labels = legend_lab,
      colors = pal,
      position = "bottomright", opacity = 0.75
    ) %>%
    addLayersControl(
      baseGroups = "# Years",
      overlayGroups = c("0", "1", "2", "3", "4", "5", "6+"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomleft"
    )
}
