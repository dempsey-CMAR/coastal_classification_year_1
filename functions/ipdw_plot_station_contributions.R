# Assign station locations to a grid cell

# Creates a grid of a user-specified size
# Then spatially joins the grid to the station locations

# sf: a spatial data frame of sample locations. Must be in projected coordinates (not lat/long)
# costras: cost raster generated from ipdw::costrasterGen()
# dist_factor: factor controlling the size of the grid cells. Grid size is the mean distance between
## neighbors * dist_factor. Default is dist_factor = 2.
# figures: logical argument. If TRUE, will generate figures of sample_window, point_pattern, and
## costras with the grid overlaid

# returns sf with additional columns: layer and value
# value is the cell number, i.e., each station is now associated with one grid cell
# (but each grid cell can have more than one station)


ipdw_calculate_station_contributions <- function(
   rstack, rstack_sum,
   dist_power,training, figures = FALSE,
   paramlist = "likelihood") {

  # for each layer
  for (i in 1:dim(rstack)[3]) {

    # calculate weight from station to each cell
    # this is \frac{\frac{1}{d_i^p}} {\sum_{i=1}^n \frac{1}{d_i^p}}
    ras_weight <- rstack[[i]] ^ dist_power / rstack_sum

    param_value <- data.frame(training[i, paramlist])
    param_value <- as.vector(unlist(param_value[1]))

    # get the contribution of station i to every cell
    ras_contribution <- ras_weight * param_value

    if(isTRUE(figures)) {
      plot(ras_contribution, main = paste0("station_likelihood = ", param_value))
      points(training[i,]$east, training[i,]$north, col = "red", pch = 19)
      # points(training[-i,]$east, training[-i,]$north)
    }

    rf <- raster::writeRaster(
      ras_contribution,
      filename = file.path(
        tempdir(),
        paste(paramlist, "A5ras", training[i,]$STATION, ".grd", sep = "")
      ),
      overwrite = TRUE
    )
  }



  list(

  )


}
