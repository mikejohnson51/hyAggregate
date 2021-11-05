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
#' @importFrom dplyr filter select group_by mutate ungroup select left_join slice_min summarise
#' @importFrom tidyr unnest_longer
#' @importFrom nhdplusTools get_node

collapse_headwaters   <- function(network_list, min_area_sqkm = 3,  min_length_km = 1, condition = "or", term_cut = 100000000) {

  n   = check_network_validity(fl = network_list$flowpaths, cat = network_list$catchments)

  cat = n$catchments
  fl  = drop_artifical_splits(n$flowpaths)

  candidates =  filter(fl, !.data$ID %in% .data$toID)

  if (condition == "and") {
    candidates = candidates %>%
      filter(.data$lengthkm < min_area_sqkm & .data$areasqkm < min_area_sqkm)  %>%
      select(.data$ID, .data$toID)
  } else {
    candidates = candidates %>%
      filter(.data$lengthkm < min_area_sqkm | .data$areasqkm < min_area_sqkm)  %>%
      select(.data$ID, .data$toID)
  }

  #####

  if(nrow(candidates) > 0){

    nfp = filter(fl, !.data$ID %in% candidates$ID)

    emap = st_intersects(nhdplusTools::get_node(candidates, "end"), fl)
    smap = st_intersects(nhdplusTools::get_node(candidates, "start"), fl)

    dfe = data.frame(
      ID       = rep(candidates$ID, times = lengths(emap)),
      touches  = fl$ID[unlist(emap)],
      pos = "e" ) %>%
      filter(!.data$ID == .data$touches)

    dfs = data.frame(
      ID       = rep(candidates$ID, times = lengths(smap)),
      touches  = fl$ID[unlist(smap)],
      pos = "s" ) %>%
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
      bind_rows(filter(cat, !.data$ID %in% im$oldIDs))

 ## TODO: look more closely at the example in the README and ID=125
    find_and_remove_detached(fl  = nfp, cats = ccc, term_cut)

  } else {
    find_and_remove_detached(fl, cat, term_cut)
  }
}


#' On a rare occasion, the combination of HY_Refactor water
#' flying can result in a flowpath that (A) does not have any inflows, (B) violates lengths/area assumptions,
#' (C) is NOT a headwater. In these cases a segement within the network can get removed leaving isolated/detached flowpaths.
#' This functions is a first, but probably not ideal, stab at cleaning these up.
#' @param fl a flowline sf object
#' @param cats a catchment sf object
#' @param term_cut cutoff integer to define terminal IDs
#' @return a validated network list
#' @export
#' @importFrom  sf st_intersects
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom hyRefactor union_polygons_geos

find_and_remove_detached = function(fl, cats, term_cut){

  fl$test = lengths(st_intersects(fl))

  fix  = filter(fl, .data$test < 2)

  if(nrow(fix) > 0){
    fl = filter(fl, !.data$ID %in% fix$ID)

    fix_cats = filter(cats, .data$ID %in% fix$ID) %>%
      mutate(ID = fix$toID) %>%
      bind_rows(filter(cats, !.data$ID %in% fix$ID)) %>%
      hyRefactor::union_polygons_geos("ID")

    check_network_validity(select(fl, -.data$test), fix_cats, term_cut)
  } else {
    check_network_validity(select(fl, -.data$test), cats, term_cut)
  }
}
