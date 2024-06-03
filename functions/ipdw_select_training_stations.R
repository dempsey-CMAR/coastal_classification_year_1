# Select 1 training data station per grid cell

# sf: spatial object, where each coordinate is assigned to a group (e.g., grid cell)
# through column << cell_id >>. ie., output from ipdw_generate_sample_grid

# seed: numeric value used to set seed. Used to randomly select which coordinate (station)
## to include in the training data when there is more than one coordinate
# per group (i.e., more than one station per cell). Default uses seed = 23634


# returns a list with 2 elements:
## training: spatial data frame with 1 station per grid cell, to use for training dataset.
## dups: spatial dataframe of cells with more than one station. Use to evaluate
## grid size

ipdw_select_training_stations <- function(sf, seed = NULL) {

  if(is.null(seed)) seed <- 23634

  # unique cells
  grid_vals <- unique(sf$cell_id)

  training <- list(NULL)
  cell_dups <- list(NULL)

  for (i in seq_along(grid_vals)) {

    # select rows with stations in cell i
    cell_i <- subset(sf, sf$cell_id == grid_vals[i])

    # record if more than one station in the cell
    if(nrow(cell_i) > 1) cell_dups[[i]] <- cell_i

    # randomly sample to choose row to keep for test data
    set.seed(seed + i)
    row_keep <- gdata::resample(seq_len(nrow(cell_i)), 1)

    training[[i]] <- cell_i[row_keep, ]
  }

  # unlist and bind rows together
  # list_rbind loses spatial info, so add back in
  training <- list_rbind(training) %>% st_as_sf(crs = st_crs(sf))
  cell_dups <- list_rbind(cell_dups) %>% st_as_sf(crs = st_crs(sf))

  list(training = training, cell_dups = cell_dups)
}
