# Calculate the contribution of each station to the weighted average of each cell

# Creates a raster layer for each training station
# Option to plot each raster

# rstack_dist: raster stack with 1 raster layer for each station.
## Each layer holds the accumulated cost ("distance") from the station to every cell

# rstack_dist_sum: raster holding the sum of the distances from each station to each cell

# dist_power: distance power

# training: training data. Rows must be in the same order as rstack_dist (i.e,
## row 1 corresponds to the first raster layer)

# figures: logical argument indicating whether to plot the contribution layer for each station

# paramlist: the name of the column in training that is being weighted

# exports a raster layer for each training station with the contribution to the
# weighted average in each cell
# option to generate a figure for each layer


ipdw_calculate_station_contributions <- function(
   rstack_dist, rstack_dist_sum,
   dist_power, training, figures = FALSE,
   paramlist = "likelihood") {

  na_stations <- NULL

  # for each layer
  for (i in 1:dim(rstack_dist)[3]) {

    # calculate weight from station to each cell
    # this is \frac{\frac{1}{d_i^p}} {\sum_{i=1}^n \frac{1}{d_i^p}}
    ras_weight <- rstack_dist[[i]] ^ dist_power / rstack_dist_sum

    param_value <- data.frame(training[i, paramlist])
    param_value <- as.vector(unlist(param_value[1]))

    # get the contribution of station i to every cell
    ras_contribution <- ras_weight * param_value


    if(all(is.na(unique(ras_contribution@data@values)))) {

      na_stations <- c(na_stations, training[i,]$STATION)
    }

    if(isTRUE(figures)) {
      plot(ras_contribution, main = paste0("station_likelihood = ", param_value))
      points(training[i,]$east, training[i,]$north, col = "red", pch = 19)
      # points(training[-i,]$east, training[-i,]$north)
    }

    # rf <- raster::writeRaster(
    #   ras_contribution,
    #   filename = file.path(
    #     tempdir(),
    #     paste(paramlist, "A5ras", training[i,]$STATION, ".grd", sep = "")
    #   ),
    #   overwrite = TRUE
    # )
  }

 na_stations

}
