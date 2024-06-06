# June 4, 2024
# This script imports CMP data and exports data measured at
## standard depths (2, 5, 10, and 15 m).

# Some depths were modified to fill in spatial or temporal gaps

# Data sent to the NS Open Data Portal March 2024
# Automated and Human-in-Loop QC applied
# Suspect/Of Interested and Not Evaluated flags were reviewed and included


# 2023_cmp_temperature_data.rds
## Temperature observations
## Filtered out freshwater stations
## Standard depths

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
library(raster)
library(sensorstrings)
library(tgc)
library(viridis)

source(here("functions/map_stations_by_years_data.R"))
source(here("functions/assign_standard_depths.R"))
source(here("functions/merge_stations.R"))

get_depth_col_palette <- colorRampPalette(viridis(8, option = "D", direction = -1))


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
  dplyr::select(
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
  # standard depths
  assign_standard_depths() %>% 
  rename(
    original_sensor_depth_m = sensor_depth_at_low_tide_m,
    sensor_depth_at_low_tide_m = standard_depth_m
    ) %>% 
  filter(sensor_depth_at_low_tide_m %in% c(2, 5, 10, 15)) %>% 
  # rename
  merge_stations()

gc()

# Map ---------------------------------------------------------------------

dat_map <- dat %>% 
  distinct(county, station, sensor_depth_at_low_tide_m, latitude, longitude) %>% 
  ss_convert_depth_to_ordered_factor()

depth_pal <- colorFactor(
  palette =  get_depth_col_palette(
    length(unique(dat_map$sensor_depth_at_low_tide_m))
  ),
  domain = unique(dat_map$sensor_depth_at_low_tide_m)
)

leaflet(dat_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    dat = grid, weight = 1, color = "black",
    fillColor = NA, fillOpacity = 0,
    group = "Sample Grid"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_map, sensor_depth_at_low_tide_m == 2), 
    fill_col = ~depth_pal(sensor_depth_at_low_tide_m), group = "2"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_map, sensor_depth_at_low_tide_m == 5), 
    fill_col = ~depth_pal(sensor_depth_at_low_tide_m), group = "5"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_map, sensor_depth_at_low_tide_m == 10), 
    fill_col = ~depth_pal(sensor_depth_at_low_tide_m), group = "10"
  ) %>% 
  add_circle_markers(
    dat = filter(dat_map, sensor_depth_at_low_tide_m == 15), 
    fill_col = ~depth_pal(sensor_depth_at_low_tide_m), group = "15"
  ) %>% 
  addLegend(
    "topright", pal = depth_pal, 
    values = unique(dat_map$sensor_depth_at_low_tide_m),
    title = "Sensor Depth (m)",
    opacity = 0.75
  ) %>% 
  addLayersControl(
    baseGroups = "Sensor Depth (m)",
    overlayGroups = c("2", "5", "10", "15"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  ) %>% 
  addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = FALSE)
  )
  

# export ------------------------------------------------------------------

saveRDS(dat, here("data/2023_cmp_temperature_standard_depths.rds"))

# heat stress -------------------------------------------------------------

dat <- readRDS(here("data/2023_cmp_temperature_standard_depths.rds"))

dat_heat_stress <- dat %>%
  rename(
    TIMESTAMP = timestamp_utc,
    DEPTH = sensor_depth_at_low_tide_m,
    VALUE = temperature_degree_c
  )

# all of the data series in dat_heat_stress - not all have heat stress
data_series_out <- dat_heat_stress %>%
  distinct(county, station, DEPTH) %>%
  rename(sensor_depth_at_low_tide_m = DEPTH) 

# preliminary heat stress event = obs > 18 degrees + 24 hours
heat_stress_24 <- dat_heat_stress %>%
  identify_heat_stress_events(county, station) %>%
  rename(sensor_depth_at_low_tide_m = DEPTH) %>%
  mutate(
    year_utc = factor(year(stress_start)),
    month_utc = month(stress_start),
    event_duration_days = difftime(stress_end, stress_start, units = "days"),
    event_duration_days = round(unclass(event_duration_days), digits = 2)
  )


# add in data series with no heat stress
heat_stress_24_out <- heat_stress_24 %>%
  full_join(
    data_series_out,
    by = join_by(county, station, sensor_depth_at_low_tide_m)
  ) %>% 
  mutate(
    event_duration_days = if_else(
      is.na(event_id) & is.na(event_duration_days), 0, event_duration_days)
  ) %>%
  dplyr::select(county, station, year_utc, month_utc, sensor_depth_at_low_tide_m,
                event_id, stress_start, stress_end, event_duration_days) %>%
  arrange(county, station, year_utc, sensor_depth_at_low_tide_m) %>% 
  ss_convert_depth_to_ordered_factor() 

saveRDS(
  heat_stress_24_out,
  file = here("data/heat_stress_events_18deg_24hrs_not_filtered_standard_depths.rds")
)


