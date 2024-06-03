# June 3, 2024
# This script imports CMP data and exports the QC'd temperature data for
# Coastal Classification Analysis.

# Data sent to the NS Open Data Portal March 2024
# Automated and Human-in-Loop QC applied
# Suspect/Of Interested and Not Evaluated flags were reviewed and included

# 2023_cmp_temperature_data.rds
## Temperature observations
## Filtered out freshwater stations
## Filtered out depths greater than 15 m

# heat_stress_events_18deg_24hrs_not_filtered.rds
## Duration of heat stress events by county, station, and depth
## heat stress threshold = 18 degree C
## heat stress for 24 hours after observation >= 18 deg C

## preliminary only - data series not standardized

library(dplyr)
library(ggplot2)
library(here)
library(leaflet)
library(lubridate)
library(qaqcmar)
library(sensorstrings)
library(tgc)
library(viridis)

source(here("functions/assign_standard_depths.R"))
source(here("functions/map_stations_by_years_data.R"))

# map params --------------------------------------------------------------

theme_set(theme_light())

# import all CMP data -----------------------------------------------------

dat_all <- ss_import_data(input_path = here("data-raw"))

# filter for temperature observations of interest ------------------------------------------------

dat <- dat_all %>%
  filter(
    !is.na(temperature_degree_c),
    !(station %in% c("Piper Lake", "Hourglass Lake", "Sissiboo")),
    sensor_depth_at_low_tide_m <= 18,
    qc_flag_temperature_degree_c != 4
  ) %>%
  select(
    -string_configuration,
    -contains("dissolved_oxygen"),
    -contains("salinity"),
    -contains("sensor_depth_measured"),
    -contains("flag")
  ) %>%
  filter(temperature_degree_c > -10) #  observation at Taylors Rock should be flagged 4

# some strings have more than one sensor at a given depth
# remove the data from one of these sensors so that heat stress events
## are not double-counted

# sensors to remove:
dup_sensors <- dat %>%
  group_by(county, station, deployment_range, sensor_depth_at_low_tide_m) %>%
  mutate(n_sensor = length(unique(sensor_serial_number))) %>%
  ungroup() %>%
  filter(n_sensor > 1) %>%
  distinct(
    county, station, deployment_range,
    sensor_depth_at_low_tide_m, sensor_type, sensor_serial_number
  ) %>%
  # when aquameasure sensor is present, keep that; otherwise, hobo
  filter(
    sensor_type != "aquameasure" &
      !(sensor_type == "hobo" & sensor_serial_number %in%
          c(20834024, 20834033, 2029147))
  )


dat <- dat %>%
  # remove data from additional sensors
  anti_join(
    dup_sensors,
    by = join_by(county, station, deployment_range,
                 sensor_type, sensor_serial_number,
                 sensor_depth_at_low_tide_m)
  ) %>% 
  # nudge depths where required
  assign_standard_depths() %>% 
  select(-sensor_depth_at_low_tide_m) %>% 
  rename(sensor_depth_at_low_tide_m = standard_depth_m)

gc()

# 2 m ---------------------------------------------------------------------

dat_2 <- dat %>% 
  filter(sensor_depth_at_low_tide_m >= 1 & 
           sensor_depth_at_low_tide_m <= 3) %>% 
  distinct(county, station, sensor_depth_at_low_tide_m, latitude, longitude) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal_2 <- colorFactor(
  palette =  get_depth_col_palette(
    length(unique(dat_2$sensor_depth_at_low_tide_m))
  ),
  domain = unique(dat_2$sensor_depth_at_low_tide_m)
)

leaflet(dat_2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # add_circle_markers(
  #   dat = filter(dat_2, sensor_depth_at_low_tide_m == 0.4), 
  #   fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "0.4"
  # ) %>% 
  # add_circle_markers(
  #   dat = filter(dat_2, sensor_depth_at_low_tide_m == 0.5), 
  #   fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "0.5"
  # ) %>% 
  # add_circle_markers(
  #   dat = filter(dat_2, sensor_depth_at_low_tide_m == 0.8), 
  #   fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "0.8"
# ) %>% 
add_circle_markers(
  dat = filter(dat_2, sensor_depth_at_low_tide_m == 1), 
  fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "1.0"
) %>% 
  add_circle_markers(
    dat = filter(dat_2, sensor_depth_at_low_tide_m == 1.3), 
    fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "1.3"
  ) %>% 
  # add_circle_markers(
  #   dat = filter(dat_2, sensor_depth_at_low_tide_m == 1.5), 
  #   fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "1.5"
  # ) %>% 
  # add_circle_markers(
  #   dat = filter(dat_2, sensor_depth_at_low_tide_m == 1.6), 
  #   fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "1.6"
  # ) %>% 
  add_circle_markers(
    dat = filter(dat_2, sensor_depth_at_low_tide_m == 2), 
    fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "2.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_2, sensor_depth_at_low_tide_m == 3), 
    fill_col = ~depth_pal_2(sensor_depth_at_low_tide_m), group = "3.0"
  ) %>% 
  addLegend(
    "bottomright", pal = depth_pal_2, 
    values = unique(dat_2$sensor_depth_at_low_tide_m),
    title = "Sensor Depth (m)",
    opacity = 0.75
  ) %>% 
  addLayersControl(
    baseGroups = "Sensor Depth (m)",
    overlayGroups = c("1.0", "1.3", "2.0", "3.0"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  )


# 5 m ---------------------------------------------------------------------

dat_5 <- dat %>% 
  filter(sensor_depth_at_low_tide_m >= 4 & 
           sensor_depth_at_low_tide_m <= 6) %>% 
  distinct(county, station, sensor_depth_at_low_tide_m, latitude, longitude) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal_5 <- colorFactor(
  palette =  get_depth_col_palette(
    length(unique(dat_5$sensor_depth_at_low_tide_m))
  ),
  domain = unique(dat_5$sensor_depth_at_low_tide_m)
)

leaflet(dat_5) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 4), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "4.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 4.5), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "4.5"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 4.87), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "4.87"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 5), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "5.0"
  ) %>%
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 5.5), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "5.5"
  ) %>%
  add_circle_markers(
    dat = filter(dat_5, sensor_depth_at_low_tide_m == 6), 
    fill_col = ~depth_pal_5(sensor_depth_at_low_tide_m), group = "6.0"
  ) %>%
  addLegend(
    "bottomright", pal = depth_pal_5, 
    values = unique(dat_5$sensor_depth_at_low_tide_m),
    title = "Sensor Depth (m)",
    opacity = 0.75
  ) %>% 
  addLayersControl(
    baseGroups = "Sensor Depth (m)",
    overlayGroups = c("4.0", "4.5", "4.87", "5.0", "5.5", "6.0"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  )

# 10 m ---------------------------------------------------------------------

dat_10 <- dat %>% 
  filter(sensor_depth_at_low_tide_m >= 8 & 
           sensor_depth_at_low_tide_m <= 12) %>% 
  distinct(county, station, sensor_depth_at_low_tide_m, latitude, longitude) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal_10 <- colorFactor(
  palette =  get_depth_col_palette(
    length(unique(dat_10$sensor_depth_at_low_tide_m))
  ),
  domain = unique(dat_10$sensor_depth_at_low_tide_m)
)

leaflet(dat_10) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 8), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "8.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 9), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "9.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 9.82), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "9.82"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 10), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "10.0"
  ) %>%
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 10.5), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "10.5"
  ) %>%
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 11), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "11.0"
  ) %>%
  add_circle_markers(
    dat = filter(dat_10, sensor_depth_at_low_tide_m == 12), 
    fill_col = ~depth_pal_10(sensor_depth_at_low_tide_m), group = "12.0"
  ) %>%
  addLegend(
    "bottomright", pal = depth_pal_10, 
    values = unique(dat_10$sensor_depth_at_low_tide_m),
    title = "Sensor Depth (m)",
    opacity = 0.75
  ) %>% 
  addLayersControl(
    baseGroups = "Sensor Depth (m)",
    overlayGroups = c("8.0", "9.0", "9.82", "10.0", "10.5", "11.0", "12.0"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  )

# 15 m ---------------------------------------------------------------------

dat_15 <- dat %>% 
  filter(sensor_depth_at_low_tide_m >= 13 & 
           sensor_depth_at_low_tide_m <= 18) %>% 
  distinct(county, station, sensor_depth_at_low_tide_m, latitude, longitude) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal_15 <- colorFactor(
  palette =  get_depth_col_palette(
    length(unique(dat_15$sensor_depth_at_low_tide_m))
  ),
  domain = unique(dat_15$sensor_depth_at_low_tide_m)
)

leaflet(dat_15) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 13), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "13.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 14), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "14.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 15), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "15.0"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 16), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "16.0"
  ) %>%
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 17.5), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "17.5"
  ) %>%
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 17.5), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "17.5"
  ) %>%
  add_circle_markers(
    dat = filter(dat_15, sensor_depth_at_low_tide_m == 18), 
    fill_col = ~depth_pal_15(sensor_depth_at_low_tide_m), group = "18.0"
  ) %>%
  addLegend(
    "bottomright", pal = depth_pal_15, 
    values = unique(dat_15$sensor_depth_at_low_tide_m),
    title = "Sensor Depth (m)",
    opacity = 0.75
  ) %>% 
  addLayersControl(
    baseGroups = "Sensor Depth (m)",
    overlayGroups = c("13.0", "14.0", "15.0", "16.0", "17.5", "18.0"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  )














#export
saveRDS(dat, here("data/2023_cmp_temperature.rds"))


# heat stress events - to explore -----------------------------------------

dat <- readRDS(here("data/2023_cmp_temperature.rds"))

dat_heat_stress <- dat %>%
  select(-contains("flag")) %>%
  rename(
    TIMESTAMP = timestamp_utc,
    DEPTH = sensor_depth_at_low_tide_m,
    VALUE = temperature_degree_c
  )

# preliminary heat stress event = obs > 18 degrees + 24 hours
heat_stress_24 <- dat_heat_stress %>%
  identify_heat_stress_events(county, station) %>%
  mutate(sensor_depth_at_low_tide_m = round(DEPTH)) %>%
  ss_convert_depth_to_ordered_factor() %>%
  mutate(
    year_utc = factor(year(stress_start)),
    month_utc = month(stress_start),
    event_duration_days = difftime(stress_end, stress_start, units = "days"),
    event_duration_days = round(unclass(event_duration_days), digits = 2)
  )

saveRDS(
  heat_stress_24,
  file = here("data/heat_stress_events_18deg_24hrs_not_filtered.rds")
)


