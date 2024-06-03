# May 9, 2024

# some stations overlap with land with costras resoltion of 500 m
# nudge the coordinates so they will contribute to the interpolation

nudge_station_coordinates <- function(dat) {

  dat %>%
    mutate(
      latitude = case_when(
        station == "Canso Lock" ~ 45.642,
        # Little Bras d'Or Channel is ~ 100 m wide at original station.
        # moved 1 km to outside of channel
        station == "Little Bras d'Or Channel" ~ 46.316,
        # Whitehead Harbour is ~ 400 m wide at original station moved 1.3 km to harbour mouth
        station == "1199" ~ 45.266,
        # moved Burnt Island 300 m to North side of Island (into the channel)
        station == "Burnt Island" ~  45.260,
        # The Channel is only 160 m at the original location. Moved 880 m to wider section
        station == "The Channel" ~  45.245,
        # moved 550 m into a wider section of channel
        station == "Denys Basin W" ~ 45.897,
        # moved 2.5 km tp wider section of channel
        station == "Little Narrows N" ~ 46.015,
        # moved into inlet
        station == "St. Peters Canal N" ~ 45.658,
        station == "St. Peters Canal S" ~ 45.6507,
        # coordinates are on the island
        station == "Snake Island" ~ 44.5466,

        TRUE ~ latitude
      ),

      longitude = case_when(
        station == "Canso Lock" ~ -61.4117,
        station == "Little Bras d'Or Channel" ~ -60.284,
        station == "1199" ~ -61.142,
        station == "Burnt Island" ~ -61.152,
        station == "The Channel" ~ -61.156,
        station == "Denys Basin W" ~ -61.026,
        station == "Little Narrows N" ~ -60.955,
        station == "St. Peters Canal N" ~ -60.856,
        station == "St. Peters Canal S" ~ -60.879,
        station == "Snake Island" ~ -64.17354,

        TRUE ~ longitude
      )
    )

}
