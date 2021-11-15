#' Collapse headwaters
#' @description Headwaters are those segments in which there are no inflows (!ID %in% toID).
#' This function removes and normalizes the network list to collapse headwater locations that violate
#' prescribed minimum length and area thresholds.
#' @param network_list  a list containing flowline and catchment `sf` objects
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @param term_cut cutoff integer to define terminal IDs
#' @param condition How should headwaters be collapsed? Those where the area AND length are less then
#' the prescribed thresholds, or where the area OR length are less then the thresholds
#' (options are "and" or "or").
#' @return list of sf objects
#' @export
#' @importFrom sf st_is_empty st_drop_geometry st_intersects
#' @importFrom dplyr filter select group_by mutate ungroup select left_join slice_min summarize anti_join
#' @importFrom tidyr unnest_longer
#' @importFrom nhdplusTools get_node

collapse_headwaters   <- function(network_list, min_area_sqkm = 3,  min_length_km = 1, condition = "or", term_cut = 100000000) {

  n   = check_network_validity(fl = network_list$flowpaths, cat = network_list$catchments)

  cat = n$catchments

  fl  = drop_artifical_splits(n$flowpaths)

  candidates =  filter(fl, !.data$ID %in% .data$toID)


  if (condition == "and") {
    candidates = candidates %>%
      filter(.data$lengthkm < min_length_km & .data$areasqkm < min_area_sqkm)  %>%
      select(.data$ID, .data$toID)
  } else {
    candidates = candidates %>%
      filter(.data$lengthkm < min_length_km | .data$areasqkm < min_area_sqkm)  %>%
      select(.data$ID, .data$toID)
  }


  if(nrow(candidates) == 0){ return(network_list) }

    end_nodes = fl %>%
      select(.data$ID, .data$toID) %>%
      nhdplusTools::rename_geometry("geometry") %>%
      mutate(geometry = nhdplusTools::get_node(., "end")$geometry)

    dmap = st_intersects(end_nodes, fl)

  # Here we are finding all of the  which flowlines the endpoint touch and seeing if
  # the toID is included. If it is not, then the flowline is disconnected from the
  # graph network

    disconnected = data.frame(
      ID       = rep(end_nodes$ID, times = lengths(dmap)),
      touches  = fl$ID[unlist(dmap)],
      graph    = rep(end_nodes$toID, times = lengths(dmap))) %>%
      filter(.data$ID != .data$touches) %>%
      group_by(.data$ID) %>%
      summarize(connected_to_toID = sum(.data$touches == .data$graph)) %>%
      ungroup()

  #######

    # Here we find which flowlines each candidate line endpoint touches
    emap = st_intersects(nhdplusTools::get_node(candidates, "end"), fl)

    dfe = data.frame(
      ID       = rep(candidates$ID, times = lengths(emap)),
      touches  = fl$ID[unlist(emap)]) %>%
      filter(!.data$ID == .data$touches)
    # Here we find which flowlines each candidate line start point touches
    smap = st_intersects(nhdplusTools::get_node(candidates, "start"), fl)

    dfs = data.frame(
      ID       = rep(candidates$ID, times = lengths(smap)),
      touches  = fl$ID[unlist(smap)]) %>%
      filter(!.data$ID == .data$touches)

    # Combine them! This is a mapping file of all start/end point
    # intersections
    ends = bind_rows(dfe, dfs)

    # Now, to identify inter-segment flows, we compute all flowlines that touch each candidate
    all_map = st_intersects(candidates, fl)

    dfa = data.frame(
      ID       = rep(candidates$ID, times = lengths(all_map)),
      touches  = fl$ID[unlist(all_map)]) %>%
      filter(!.data$ID == .data$touches)

    ### Anti-joining the end(s) map with the full map finds those segments with
    ### Mid reach inflows
    ### Joining that to the disconnected map, tells us which flowline have inflows that are disconnected!
    ### lastly, we only keep those in which the disconnected inflow, is NOT also a candidtate to be aggregated

    special_friends = anti_join(dfa, ends, by = c("ID", "touches")) %>%
      left_join(disconnected, by = c("touches" = 'ID')) %>%
      filter(!.data$touches %in% candidates$ID)

    candidates = filter(candidates, !.data$ID %in% special_friends$ID)

  #####

  if(nrow(candidates) == 0){ return(network_list) }

    nfp = filter(fl, !.data$ID %in% candidates$ID)

    emap = st_intersects(nhdplusTools::get_node(candidates, "end"), fl)
    smap = st_intersects(nhdplusTools::get_node(candidates, "start"), fl)

    dfe = data.frame(
      ID       = rep(candidates$ID, times = lengths(emap)),
      touches  = fl$ID[unlist(emap)]) %>%
      filter(!.data$ID == .data$touches)

    dfs = data.frame(
      ID       = rep(candidates$ID, times = lengths(smap)),
      touches  = fl$ID[unlist(smap)]) %>%
      filter(!.data$ID == .data$touches)

    int = bind_rows(dfs, dfe) %>%
      left_join(st_drop_geometry(select(fl, .data$ID, .data$toID)), by = "ID")

    keep = filter(int, .data$touches == .data$toID)

    fix = filter(int, !.data$ID %in% keep$ID) %>%
      group_by(.data$ID) %>%
      summarise(connected = sum(.data$touches == .data$toID | .data$toID == 0),
                correction = unique(.data$touches)) %>%
      left_join(select(st_drop_geometry(fl), correction = .data$ID, .data$Hydroseq), by = "correction") %>%
      group_by(.data$ID) %>%
      filter(.data$connected < 1) %>%
      slice_min(.data$Hydroseq) %>%
      select(-.data$connected, -.data$Hydroseq) %>%
      ungroup()

    d = left_join(candidates, fix, by = "ID") %>%
      st_drop_geometry()

    d$toID = ifelse(is.na(d$correction), d$toID, d$correction)
    d$correction = NULL

    d = left_join(d, select(d, .data$ID, newID = .data$toID), by = c("toID" = "ID"))

    d$toID = ifelse(is.na(d$newID), d$toID, d$newID)
    d$newID = NULL

    index_map =  d %>%
      group_by(.data$toID) %>%
      mutate(oldIDs = list(c(c(.data$ID), c(.data$toID)))) %>%
      ungroup() %>%
      select(newID = .data$toID, .data$oldIDs) %>%
      tidyr::unnest_longer(col = .data$oldIDs) %>%
      filter(!duplicated(.))

    tmp = index_map %>%
      filter(.data$newID != .data$oldIDs) %>%
      filter(.data$oldIDs %in%  .data$newID) %>%
      select(update = .data$newID, newID = .data$oldIDs)

    index_map = left_join(index_map, tmp, by = "newID")
    index_map$newID = ifelse(is.na(index_map$update), index_map$newID, index_map$update)
    index_map$update = NULL

    im =  filter(index_map, !duplicated(index_map))

    ccc = left_join(im, cat, by = c("oldIDs" = "ID")) %>%
      st_as_sf() %>%
      filter(!st_is_empty(.)) %>%
      hyRefactor::union_polygons_geos('newID') %>%
      hyRefactor::clean_geometry('newID', keep = NULL) %>%
      select(ID = .data$newID) %>%
      bind_rows(filter(cat, !.data$ID %in% im$oldIDs)) %>%
      mutate(areasqkm = hyRefactor::add_areasqkm(.))

    check_network_validity(nfp, ccc, term_cut = term_cut)

}

