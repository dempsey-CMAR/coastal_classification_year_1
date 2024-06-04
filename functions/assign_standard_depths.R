# May 29, 2024

# Some stations do not have sensors at standard depths (2 , 5, 10 m), but have
# sensors +/- 1 to 2 m from the standard depths

# Here, depths from some stations are modified to match standard depths, particularly
# for counties with limited data

# Depths were modified on a case by case basis to fill spatial and temporal data
# gaps

assign_standard_depths <- function(dat) {
  
  dat %>%
    mutate(
      standard_depth_m = case_when(
        # 2m ----------------------------------------------------------------------
        # Antigonish
        station == "Captains Pond" & sensor_depth_at_low_tide_m == 1.3 ~ 2,
        station == "Captains Pond" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Monks Head" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        station == "Monks Head" & sensor_depth_at_low_tide_m == 1.6 ~ 2,
        station == "Southside" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        # could add Melmerby Beach - 1 m here but trim last day of obs (flag = 3)
        
        # Digby
        station == "5008" & sensor_depth_at_low_tide_m == 1 ~ 2,
        station == "5008" & sensor_depth_at_low_tide_m == 3 ~ 2,
        
        # Guysborough
        station == "Northeast Branch" & sensor_depth_at_low_tide_m == 1 ~ 2,
        station == "Northwest Branch" & sensor_depth_at_low_tide_m == 1 ~ 2,
        
        # Inverness
        station == "Aberdeen" & sensor_depth_at_low_tide_m == 3 ~ 2,
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 1 ~ 2,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 3 ~ 2,
        
        # Pictou
        station == "Waterside" & sensor_depth_at_low_tide_m == 1 ~ 2,
        
        # Richmond
        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 1.5 ~ 2,
        
        # 5 m --------------------------------------------------------------------
        
        # Annapolis
        station == "Lobster Ledge" & sensor_depth_at_low_tide_m == 4 ~ 5,
        
        # Cape Breton
        station == "St. Andrews Channel E" & sensor_depth_at_low_tide_m == 6 ~ 5,
        
        # Guysborough
        station == "Eddy Cove" & sensor_depth_at_low_tide_m == 6 ~ 5,
        
        # Halifax
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 4.87 ~ 5,
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 6 ~ 5,
        
        # Inverness
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 4.5 ~ 5,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 5.5 ~ 5,
       
        
        # 10 m --------------------------------------------------------------------
        
        # Guysborough
        station == "Northeast Branch" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "West Ferry Ramp" & sensor_depth_at_low_tide_m == 9 ~ 10,
        
        # Halifax
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 9.82 ~ 10,
        
        # Inverness
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 8 ~ 10,
        station == "Canso Lock" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 10.5 ~ 10,
        station == "Denys Basin E" & sensor_depth_at_low_tide_m == 11 ~ 10,
        station == "Little Bras d'Or Channel" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        # Lunenburg
        station == "Saddle Island" & sensor_depth_at_low_tide_m == 12 ~ 10,
        
        # Pictou
        station == "Olding Island" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        # Richmond
        station == "Kavanagh Point" & sensor_depth_at_low_tide_m == 9 ~ 10,
        station == "St. Peters Canal S" & sensor_depth_at_low_tide_m == 12 ~ 10,
        
        # Victoria
        station == "Little Narrows N" & sensor_depth_at_low_tide_m == 11 ~ 10,
        station == "Nyanza Bay E" & sensor_depth_at_low_tide_m == 8 ~ 10,
        station == "Nyanza Bay W" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        # Yarmouth
        station == "Ram Island" & sensor_depth_at_low_tide_m == 8 ~ 10,
        
        # 15 m --------------------------------------------------------------------
        station == "Spry Harbour" & sensor_depth_at_low_tide_m == 16 ~ 15,
        
        station == "Deep Basin" & sensor_depth_at_low_tide_m == 18 ~ 15,
        station == "0814x East" & sensor_depth_at_low_tide_m == 17.5 ~ 15,
        station == "0814x West" & sensor_depth_at_low_tide_m == 17.5 ~ 15,
        station == "Aberdeen" & sensor_depth_at_low_tide_m == 18 ~ 15,
        station == "Camerons Cove" & sensor_depth_at_low_tide_m == 13 ~ 15,
        station == "Ingomar" & sensor_depth_at_low_tide_m == 16 ~ 15,
        station == "Great Bras d'Or Channel NE-E" & sensor_depth_at_low_tide_m == 14 ~ 15,
        station == "Little Narrows S" & sensor_depth_at_low_tide_m == 13 ~ 15,
        
        TRUE ~ sensor_depth_at_low_tide_m
      )
    )
}
