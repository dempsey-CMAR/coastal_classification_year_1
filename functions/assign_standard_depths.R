# May 29, 2024

# Heat stress is calculated and interpolated by depth
# Some stations do not have sensors at standard depths (2 , 5, 10 m), but have
# sensors +/- 1 to 2 m from the standard depths

# Here, depths from some stations are modified to match standard depths, particularly
# for counties with limited data

assign_standard_depths <- function(dat) {

  dat %>%
    mutate(
     # sensor_depth_at_low_tide_m = as.numeric(as.character(sensor_depth_at_low_tide_m)),
      standard_depth_m = case_when(
        # Annapolis
        station == "Lobster Ledge" & sensor_depth_at_low_tide_m == 4 ~ 5,

        # Antigonish
        station == "Captains Pond" & sensor_depth_at_low_tide_m == 1.3 ~ 2,
        station == "Captains Pond" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Monks Head" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Monks Head" & sensor_depth_at_low_tide_m == 1.6 ~ 2,
        station == "Southside" & sensor_depth_at_low_tide_m == 1.5 ~ 2,

        # Cape Breton
        station == "St. Andrews Channel E" & sensor_depth_at_low_tide_m == 6 ~ 5,
        station == "Little Bras d'Or Channel" & sensor_depth_at_low_tide_m == 8 ~ 10,

        # Halifax
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 4.87 ~ 5,
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 9.82 ~ 10,
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 16 ~ 15,
        
        # Inverness
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 1 ~ 2,
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 4.5 ~ 5,
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 8 ~ 10,
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 3 ~ 2,
        
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 10.5 ~ 10,
       # station == "Deep Basin" & sensor_depth_at_low_tide_m == 18 ~ 15,
        station == "Denys Basin E" & sensor_depth_at_low_tide_m == 11 ~ 10,
      #  station == "0814x East" & sensor_depth_at_low_tide_m == 17.5 ~ 15,
       # station == "0814x West" & sensor_depth_at_low_tide_m == 17.5 ~ 15,
       # station == "Aberdeen" & sensor_depth_at_low_tide_m == 18 ~ 15,

        # Pictou
        station == "Olding Island" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        # Richmond
        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "St. Peters Canal S" & sensor_depth_at_low_tide_m == 12 ~ 10,

        # Shelburne
        station == "Camerons Cove" & sensor_depth_at_low_tide_m == 13 ~ 15,

        # Victoria
        station == "Little Narrows N" & sensor_depth_at_low_tide_m == 11 ~ 10,
        station == "Nyanza Bay E" & sensor_depth_at_low_tide_m == 8 ~ 10,
        station == "Nyanza Bay W" & sensor_depth_at_low_tide_m == 8 ~ 10,
       # station == "Great Bras d'Or Channel NE-E" & sensor_depth_at_low_tide_m == 14 ~ 15,
       # station == "Little Narrows S" & sensor_depth_at_low_tide_m == 13 ~ 15,

        # Yarmouth
        station == "Ram Island" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        TRUE ~ sensor_depth_at_low_tide_m
      )
    )
}
