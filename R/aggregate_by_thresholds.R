#' Aggregate Refactored Hydrofabric by Thresholds
#' @description A complete network aggregating workflow has been packaged
#' into this function. Builds a set of normalized catchment-flowpaths from
#' input flowline and catchment layer that meet specified area and length thresholds.
#' @param fl a flowline sf object
#' @param catchments a catchment sf object
#' @param gpkg A path to a geopackage with refactored output
#' @param fl_name The name of the refactored flowline layer in `gpkg`
#' @param cat_name The name of the refactored catchment layer in `gpkg`
#' @param ideal_size The ideal size of output hydrofabric catchments
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @param term_cut cutoff integer to define terminal IDs
#' @param condition How should headwaters be collapsed? Those where the area AND length are less then
#' the prescribed thresholds, or where the area OR length are less then the thresholds
#' (options are "and" or "or").
#' @details This is a convenient wrapper function that implements four phases
#' of the network aggregation workflow: merge along mainstems, correcting parallel flow, collapsing headwaters.
#' @seealso
#' The following three functions are used in the `aggregate_by_thresholds` workflow.
#' \enumerate{
#'   \item \code{\link{merge_along_mainstem}}
#'   \item \code{\link{collapse_headwaters}}
#'   \item \code{\link{realign_topology}}
#' }
#' @return list object
#' @export
#' @importFrom dplyr `%>%`
#'

# agg = hyAggregate::aggregate_by_thresholds(gpkg = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-refactor/01a/rf_s10000_m1000_c1000.gpkg')

aggregate_by_thresholds = function(fl = NULL,
                                   catchments = NULL,
                                   gpkg = NULL,
                                   fl_name  = "refactored_flowpaths",
                                   cat_name = "refactored_catchments",
                                   ideal_size    = 10,
                                   min_area_sqkm = 3,
                                   min_length_km = 1,
                                   term_cut = 100000000,
                                   condition = "or"){


  nl = if(is.null(gpkg)){
    if(is.null(fl) | is.null(catchments)){
      stop("Must provide a gpkg OR both fl and cat argument")
    }
    check_network_validity(fl = fl, cat = catchments, term_cut = term_cut)
  } else {
    build_network_list(gpkg, fl_name = fl_name, cat_name = cat_name, term_cut = term_cut)
  }

  nl1 = merge_along_mainstem(network_list = nl, ideal_size, min_area_sqkm, min_length_km, term_cut = term_cut)

  nl2 = collapse_headwaters(network_list = nl1, min_area_sqkm, min_length_km, condition, term_cut = term_cut)

  nl3 = realign_topology(network_list = nl2, term_cut = term_cut)
}


#' Build a network list object
#' @description  A network_list is a list of catchment and flowpaths `sf` objects that are validated.
#' Validations include that all required names are present; that all toIDs are actual
#' present IDs and that the flowline network is a DAG (Directed acyclic graph).
#' @param gpkg A path to a geopackage with refactored output
#' @param fl_name The name of the refactored flowline layer in `gpkg`
#' @param cat_name The name of the refactored catchment layer in `gpkg`
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing flowline and catchment `sf` objects
#' @export
#' @importFrom sf read_sf

build_network_list = function(gpkg = NULL, fl_name = "refactored_flowpaths", cat_name = "refactored_catchments", term_cut = 100000000){
  check_network_validity(fl = read_sf(gpkg, fl_name), cat = read_sf(gpkg, cat_name), term_cut = term_cut)
}
