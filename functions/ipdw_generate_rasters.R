# May 14, 2024


# ipdw_generate_ns_costras ------------------------------------------------

# this function returns a cost raster at the specified resolution
# if the .tif file already exists in the specified path, this is returned
# if no .tif file exists, one is generated and returned

# resolution < 1000 m can be slow to generate

# packages
# canadianmaps, dplyr, here, raster

# arguments:
## path = path to the existing cost raster/where the new cost raster will be exported
## costras res = resoltion of the cost raster in m
## costras_sf = shapefile used to generate the cost raster
## dat = data points used to specify extent of cost raster (sf object)
### projstr = projection of the cost raster ( projection(ns))

ipdw_generate_ns_costras <- function(
    path = NULL,
    costras_res = 500,
    costras_sf = NULL,
    dat = NULL,
    projstr = NULL
) {

  if(is.null(path)) path <- here("output/costras")

  ras_files <- list.files(path, pattern = ".tif")

  file_name <- paste0("costras_", costras_res, "m.tif")

  if(file_name %in% ras_files) {
    costras <- raster(paste0(path, "/", file_name))
  } else {

    costras <- ipdw::costrasterGen(
      dat, costras_sf,
      extent = "polys", projstr = projstr,
      resolution = costras_res
    )

    raster::writeRaster(
      costras,
      filename = paste0(path, "/", file_name),
      overwrite = TRUE
    )
  }
  costras
}


# rstack_dist -------------------------------------------------------------

# need to export as .grd file so that the layer names will be preserved when
## re-imported
# layer names are lost when exported as a .tif file

# arguments:
## path = path to the existing distance raster/where the new raster will be exported
## costras res = resolution of the cost raster in m
## range_factor = factor used to dictate the farthest cell(s) influenced by
## station likelihood value
## mean_neigh_dist_m = mean distance between stations (returned by ipdw_generate_sample_grid)
## costras = cost raster
## training = data points used for interpolation (sf object)

ipdw_generate_ns_distances <- function(
    path = NULL,
    min_number_years,
    costras_res,
    range_factor,
    dist_power,
    grid_factor,
    training_seed,
    mean_neigh_dist_m,
    training = NULL,
    costras = NULL,
    standard_depth = NULL
) {

  if(is.null(path)) path <- here("output/rstack_dist")

  ras_files <- list.files(path, pattern = "grd")

  if(is.null(standard_depth)) {
    file_name <- paste0(
      "rstack_dist_",
      min_number_years, "_",
      costras_res, "_",
      range_factor, "_",
      dist_power, "_",
      grid_factor, "_",
      training_seed,
      ".grd"
    )
  } else {
    file_name <- paste0(
      "rstack_dist_",
      min_number_years, "_",
      costras_res, "_",
      range_factor, "_",
      dist_power, "_",
      grid_factor, "_",
      training_seed, "_",
      standard_depth,
      ".grd"
    )
  }


  if(file_name %in% ras_files) {
    rstack_dist <- raster::stack(paste0(path, "/", file_name))
  } else {

    rstack_dist <- ipdw::pathdistGen(
      training, costras,
      range = range_factor * mean_neigh_dist_m,
      progressbar = FALSE
    )

    # add the user-specified range to rstack
    range <- slot(rstack_dist, "range")

    raster::writeRaster(
      rstack_dist,
      filename = paste0(path, "/", file_name),
      overwrite = TRUE,
      bylayer = FALSE
    )

  }
  rstack_dist
}
