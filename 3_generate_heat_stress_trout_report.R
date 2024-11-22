# October 1, 2024

# This script generates an html file with the results of idpw for the
## specified parameters
# The parameters and associated goodness of fit metrics are exported
## to output/interpolation_gof.csv

library(here)
library(quarto)

# user defined parameters -------------------------------------------------

# sensor depth to use
standard_depth = 5

# minimum number of years of data to include station
min_number_years = 2

# resolution of the cost raster
# coarse resolution will run faster, but if too coarse, some stations will
# be on land and will not be considered in the interpolation
# reasonable values are 500 - 1000 m, although some stations are still
# on land at this scale
# not enough memory to run at 100 m resolution
costras_res = 500

# factor used to dictate the farthest cell(s) influenced by station likelihood value
# range will be mean_neigh_dist_m * range_factor
# for small values (~ 10), there will be gaps between stations
# larger values will provide smoother results, but stations will impacts areas far away
range_factor = 50

# exponent on the distance used to calculate weights
# larger values mean only closer stations are included in the weighted average
# typical values are 1 - 2
dist_power = 1

# factor used to create the grid that the training data is selected from
# grid size will be grid_factor * base_grid
# reasonable values are 2 - 10
grid_factor = 1.5

# base size of the grid that the training data will be selected from (in m)
# reasonable values are xxxx
base_grid = 10000

# random value used to set the seed for selecting training stations from each grid cell
# the default value used in ipdw_select_training_stations is 23634
training_seed = 55616

# logical argument indicating whether an offset was applied to the likelihood values
like_offset = FALSE


# export html file --------------------------------------------------------

quarto::quarto_render(
  here("3_heat_stress_trout_report_template.qmd"),
  execute_params = list(
    standard_depth = standard_depth,
    min_number_years = min_number_years,
    costras_res = costras_res,
    range_factor = range_factor,
    dist_power = dist_power,
    grid_factor = grid_factor,
    base_grid = base_grid,
    training_seed = training_seed,
    like_offset = like_offset
  ),
  output_file = paste0(
    paste(
      "heat_stress_trout_report",
      standard_depth,
      min_number_years,
      costras_res,
      range_factor,
      dist_power,
      grid_factor,
      base_grid,
      training_seed,
      like_offset,
      sep = "_"
    ), ".html")
)




