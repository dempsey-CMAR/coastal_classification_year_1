# Assign station locations to a grid cell

# Creates a grid of a user-specified size
# Then spatially joins the grid to the station locations

# sf: a spatial data frame of sample locations. Must be in projected coordinates (not lat/long)
# costras: cost raster generated from ipdw::costrasterGen()
# base_grid: base grid size in units of the map. Will be multiplied by dist_factor 
## for final grid size. Default is the mean distance between nearest neighbors.
# dist_factor: factor controlling the size of the grid cells. Grid size is the mean distance between
## neighbors * dist_factor. Default is dist_factor = 2.
# figures: logical argument. If TRUE, will generate figures of sample_window, point_pattern, and
## costras with the grid overlaid

# returns sf with additional columns: layer and cell_id
# cell_id is the cell number, i.e., each station is now associated with one grid cell
# (but each grid cell can have more than one station)


ipdw_generate_sample_grid <- function(
    sf, costras,
    base_grid = NULL,
    dist_factor = 2) {

  # create box around the sample points
  sample_window <- spatstat.geom::owin(
    range(c(st_bbox(sf)["xmin"], st_bbox(sf)["xmax"])),
    range(c(st_bbox(sf)["ymin"], st_bbox(sf)["ymax"]))
  )
  
  if (is.null(base_grid)) {
    # create point pattern object
    point_pattern <- spatstat.geom::ppp(
      st_coordinates(sf)[, 1], st_coordinates(sf)[, 2], window = sample_window
    )
    # mean distance between neighbors
    base_grid <- mean(nndist(point_pattern))
  }
  
  # build grid
  gridsize <- base_grid * dist_factor
  grainscale_factor <- gridsize / res(costras)[1]

  # create grid raster
  gridras <- aggregate(costras, fact = grainscale_factor)
  gridpol <- rasterToPolygons(gridras)
  gridpol$cell_id  <- row.names(gridpol)

  # spatial join (assign each station to a grid cell)
  sf_grid <- sf::st_join(sf, st_as_sf(gridpol))

  # if(isTRUE(figures)) {
  # 
  # #  par(mfrow = c(2, 2))
  # 
  #   plot(sample_window)
  #   plot(point_pattern)
  # 
  #   plot(costras, main = "sample_grid")
  #   lines(gridpol, col = "grey")
  #  # points(sf)
  # }

  list(
    base_grid = base_grid,
    grid = gridpol,
    sf_grid = sf_grid
  )
}
