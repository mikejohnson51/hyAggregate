#' Aggregate along network mainstems
#' @description Given a set of ideal catchment sizes, plus the
#' minimum allowable catchment size and segment length, aggregate the network along mainstems.
#' @param network_list a list containing flowline and catchment `sf` objects
#' @param ideal_size The ideal size of output hydrofabric catchments
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing aggregated and validated flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr filter group_by arrange mutate ungroup select cur_group_id select
#' @importFrom sf st_drop_geometry

merge_along_mainstem  <- function(network_list, ideal_size = 10, min_area_sqkm = 3, min_length_km = 1, term_cut =  100000000) {

  n = check_network_validity(fl = network_list$flowpaths,
                             cat = network_list$catchments,
                             term_cut)

  cat = n$catchments
  fl  = n$flowpaths

  if(!'areasqkm' %in% names(fl)){
    rm_area_sqkm = TRUE
    fl$areasqkm = 999
  } else {
    rm_area_sqkm = FALSE
  }

  if(!is.null(cat)){

  index_table = fl %>%
    st_drop_geometry() %>%
    filter(.data$ID %in% cat$ID) %>%
    group_by(.data$LevelPathID) %>%
    arrange(-.data$Hydroseq) %>%
    mutate(ind = cs_group(.data$areasqkm, ideal_size)) %>%
    ungroup()   %>%
    group_by(.data$LevelPathID, .data$ind) %>%
    mutate(set = cur_group_id()) %>%
    ungroup() %>%
    select(-.data$ind)

    agg2 = aggregate_network_by_index(fl, cat, index_table, "set")

  } else {
    agg2 = list(flowpaths = fl, cat)
  }

  index_table_2 = agg2$flowpaths %>%
    st_drop_geometry() %>%
    group_by(.data$LevelPathID) %>%
    arrange(-.data$Hydroseq) %>%
    mutate(ind = agg_length_area(.data$lengthkm, .data$areasqkm, min_length_km, min_area_sqkm)) %>%
    ungroup()   %>%
    group_by(.data$LevelPathID, .data$ind) %>%
    mutate(set = cur_group_id()) %>%
    ungroup() %>%
    select(-.data$ind)

  if(rm_area_sqkm){
    agg2$flowpaths$areasqkm = NULL
  }

  aggregate_network_by_index(fl = agg2$flowpaths, cat = agg2$catchments, index_table = index_table_2, "set")
}
