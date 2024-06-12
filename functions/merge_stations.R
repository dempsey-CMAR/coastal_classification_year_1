# June 4, 2024


merge_stations <- function(dat) {
  
  dat %>% 
    mutate(
      station = case_when(
        station == "Centreville" ~ "Centreville 1",
        station == "Church Point" ~ "Church Point 2",
        
        station == "Dorts Cove" ~ "Dorts Cove 1",
        station == "Madeline Point" ~ "Madeline Point 1",
        station == "Moose Point" ~ "Moose Point 1",
        station == "Rook Island" ~ "Rook Island 1",
        station == "Seal Cove" ~ "Seal Cove 1",
        station == "Tickle Island" ~ "Tickle Island 1",
        
        TRUE ~ station
      )
    )
}




