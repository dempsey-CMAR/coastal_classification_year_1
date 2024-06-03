# May 7, 2024

# Round or modify sensor_depth_at_low_tide_m to create continuous data series
# for analysis

standardize_depths <- function(dat) {

  dat %>%
    mutate(
      sensor_depth_at_low_tide_m = case_when(
        station %in% c("Spry Harbour", "5007") ~ round(sensor_depth_at_low_tide_m),

        station == "Captains Pond" &
          sensor_depth_at_low_tide_m == 1 ~ 1.3,
        station == "Captains Pond" & year_utc == 2022 &
          sensor_depth_at_low_tide_m == 0.5 ~ 0.4,
        station == "Captains Pond" & year_utc == 2022 &
          sensor_depth_at_low_tide_m == 1.5 ~ 1.3,

        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 9 ~ 10,

        station == "Monks Head" & sensor_depth_at_low_tide_m == 1 ~ 0.8,
        station == "Monks Head" & sensor_depth_at_low_tide_m == 1.5 ~ 1.6,

        station == "Sober Island" & sensor_depth_at_low_tide_m == 0.4 ~ 0.5,

        # station == "The Basin" & year_utc == 2020 &
        #   sensor_depth_at_low_tide_m == 2 ~ 1,

        TRUE ~ sensor_depth_at_low_tide_m
      )
    )

}
