library(mapview)
library(sf)
library(mapview)

VPU = "01"

#Download Reference Refactored Fabric for VPU
g01 = get_reference_fabric(VPU = VPU, dir = "data") |>
  # Apply default aggregation
    aggregate_network_to_distribution(
      outfile        = glue::glue("data/ngen_{VPU}.gpkg"),
      routelink_path = get_routelink_path(),
      overwrite      = FALSE
    ) |>
  # Add cfe and noahowp parameters to gpkg
    aggregate_cfe_noahowp(dir = '/Volumes/Transcend/nwmCONUS-v216/', add_to_gpkg = TRUE) |>
  # Write ngen file set w/ shapefiles
    write_ngen_dir(export_shapefiles = TRUE)



g01 = glue::glue("data/ngen_{VPU}.gpkg")

## Example 1: Subset based on ID:
set = subset_network(g01, 'wb-16354')

mapview::mapview(set)

## Example 2: Subset based on ID and find mainstem:
set = subset_network(g01, 'wb-16354', mainstem = TRUE)

mapview::mapview(set)

## Example 3: Subset based on location:
pt = data.frame(x = 2141136, y = 2824888) |>
  st_as_sf(coords = c("x", "y"), crs = 5070)

set = subset_network(g01, find_origin(gpkg, pt))

mapview::mapview(set) + pt




