#' Realign topology to Hy Feature standard
#' @param network_list a list containing flowline and catchment `sf` objects
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing aggregated and validated flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr select filter bind_rows mutate left_join distinct
#' @importFrom nhdplusTools rename_geometry get_node
#' @importFrom sf st_intersects st_drop_geometry

realign_topology = function(network_list, term_cut = 100000000){

  tmp = network_list$flowpaths %>%
    drop_artifical_splits() %>%
    select(-.data$set, -.data$n)

  term_fl      <- filter(tmp, .data$toID == 0)
  term_fl$toID <- (nrow(tmp) + term_cut + 1:nrow(term_fl))

  tmp     = bind_rows(term_fl,  filter(tmp, !.data$ID %in% term_fl$ID))

  ends = tmp  %>%
    nhdplusTools::rename_geometry('geometry') %>%
    mutate(geometry = nhdplusTools::get_node(., "end")$geometry)

  starts_ends = bind_rows(get_node(tmp, "start"),
                          get_node(tmp, "end"))

  emap     = st_intersects(ends, starts_ends)
  tmp$type =  ifelse(lengths(emap) > 1, "nex", "jun")
  tmp$type = ifelse(tmp$toID > term_cut, "term", tmp$type)

  ends = tmp  %>%
    nhdplusTools::rename_geometry('geometry') %>%
    mutate(geometry = nhdplusTools::get_node(., "end")$geometry)

  tmap = st_intersects(ends, tmp)

  data.frame(
    ID       = rep(ends$ID, times = lengths(tmap)),
    touches  = tmp$ID[unlist(tmap)],
    touches_toID = tmp$toID[unlist(tmap)],
    type = rep(ends$type, times = lengths(tmap))) %>%
    filter(.data$ID != .data$touches) %>%
    left_join(st_drop_geometry(select(tmp, .data$ID, .data$toID)), by = "ID") %>%
    mutate(real_toID = ifelse(.data$type == "nex", .data$toID, .data$touches_toID)) %>%
    select(.data$ID, toID = .data$real_toID) %>%
    distinct() %>%
    left_join(select(tmp, -.data$toID), by = "ID") %>%
    bind_rows(filter(tmp, .data$toID > term_cut)) %>%
    st_as_sf() %>%
    select(-.data$type) %>%
    check_network_validity(network_list$catchments, term_cut)

  # tmp %>%
  #   left_join(st_drop_geometry(select(tmp, toID = .data$ID, ds_ID = .data$toID)), by = 'toID') %>%
  #   mutate(toID = ifelse(.data$type == "jun", .data$ds_ID, .data$toID))  %>%
  #   select(-.data$ds_ID) %>%
  #   check_network_validity(network_list$catchments, term_cut)
}

