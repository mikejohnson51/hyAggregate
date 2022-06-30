#origin = 'wb-16354'
#gpkg = '/Users/mjohnson/github/hyAggregate/data/ngen_01.gpkg'


#' Find ID from location
#' @param gpkg path to a hydrofabric
#' @param pt a spatial point (sf)
#' @return a waterbody ID (character)
#' @export
#' @importFrom sf read_sf st_transform

find_origin = function(gpkg, pt){
  tmp = read_sf(gpkg,  'aggregate_catchment')[st_transform(pt, 5070),]
  gsub("cat-", "wb-", tmp$id)
}

#' Subset the upstream protion of a network
#' @param gpkg path to a hydrofabric
#' @param origin the ID to begin navigatipn
#' @return mainstem should only the mainstem flowpath be retruned (default = FALSE)
#' @export
#' @importFrom nhdplusTools get_sorted
#' @importFrom sf read_sf
#' @importFrom dplyr filter

subset_network = function(gpkg, origin, mainstem = FALSE){

  trace = get_sorted(read_sf(gpkg, 'flowpath_edge_list'),
                     split = TRUE,
                     outlets = origin)

  flowpaths = filter(read_sf(gpkg,  'aggregate_flowpath'),  id %in% trace$id)
  divides   = filter(read_sf(gpkg,  'aggregate_catchment'), id %in% flowpaths$realized_catchment)

  if(mainstem){
    tmp = filter(flowpaths, id == origin)
    flowpaths = filter(flowpaths, main_id == tmp$main_id)
  }

  return(list(flowpaths = flowpaths, divides = divides))
}

# pt = data.frame(x = 2141136, y = 2824888) |>
#   st_as_sf(coords = c("x", "y"), crs = 5070)
#
# set = subset_network(gpkg, find_origin(gpkg, pt))
# mapview::mapview(set)  +pt

