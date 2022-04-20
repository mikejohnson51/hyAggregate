#' Realign topology to Hy Feature standard
#' @param network_list a list containing flowline and catchment `sf` objects
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing aggregated and validated flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr select filter bind_rows mutate left_join slice_min group_by right_join rename
#' @importFrom nhdplusTools rename_geometry get_node
#' @importFrom sf st_intersects st_drop_geometry st_as_sf
#' @importFrom nhdplusTools rename_geometry get_node

realign_topology = function(network_list, term_cut = 100000000){

  if(nrow(network_list$flowpaths) <= 2){ return(network_list)}

  tmp      <- drop_artifical_splits(network_list$flowpaths)
  term_fl  <- filter(tmp, .data$toID == 0)

  if(nrow(term_fl) > 0){
    term_fl$toID <- (nrow(tmp) + term_cut + 1:nrow(term_fl))
  }

  tmp = bind_rows(term_fl,  filter(tmp, !.data$ID %in% term_fl$ID))

  # ID every line as a inflow-nex or inflow-junction

  nexIDs = get_nexus_locations(fp = tmp)

  ends = tmp  %>%
    nhdplusTools::rename_geometry('geometry') %>%
    mutate(geometry = nhdplusTools::get_node(., "end")$geometry)

  tmp$type = ifelse(lengths(st_intersects(ends, nexIDs))> 0, "into_nex", "into_jun")


  #####
  imap = st_intersects(ends, tmp)

  df = data.frame(
    ID            = rep(ends$ID, times = lengths(imap)),
    graph_toID    = rep(ends$toID, times = lengths(imap)),
    spatial_toID  = tmp$ID[unlist(imap)]) %>%
    mutate(parralel = .data$graph_toID != .data$spatial_toID) %>%
    group_by(.data$ID) %>%
    dplyr::slice_min(.data$parralel) %>%
    ungroup() %>%
    left_join(select(., ds_nx = .data$graph_toID, graph_toID = .data$ID), by = "graph_toID") %>%
    left_join(st_drop_geometry(select(tmp, .data$ID, .data$type)), by = "ID") %>%
    mutate(real_toID = ifelse(.data$type == "into_jun" & !.data$parralel, .data$ds_nx, .data$graph_toID)) %>%
    select(.data$ID, .data$real_toID) %>%
    filter(!duplicated(.)) %>%
    right_join(select(tmp, -.data$toID, -.data$type), by = "ID") %>%
    rename(toID = .data$real_toID) %>%
    st_as_sf() %>%
    check_network_validity(network_list$catchments, term_cut)

  df

  # tmp <- drop_artifical_splits(network_list$flowpaths)
  #
  # term_fl  <- filter(tmp, .data$toID == 0)
  #
  # if(nrow(term_fl) > 0){
  #   term_fl$toID <- (nrow(tmp) + term_cut + 1:nrow(term_fl))
  # }
  #
  # tmp = bind_rows(term_fl,  filter(tmp, !.data$ID %in% term_fl$ID))
  #
  # ends = tmp  %>%
  #   nhdplusTools::rename_geometry('geometry') %>%
  #   mutate(geometry = nhdplusTools::get_node(., "end")$geometry)
  #
  # starts_ends = bind_rows(get_node(tmp, "start"),
  #                         get_node(tmp, "end"))
  #
  # emap     = st_intersects(ends, starts_ends)
  # tmp$type = ifelse(lengths(emap) > 1, "nex", "jun")
  # tmp$type = ifelse(tmp$toID > term_cut, "term", tmp$type)
  #
  # ends = tmp  %>%
  #   nhdplusTools::rename_geometry('geometry') %>%
  #   mutate(geometry = nhdplusTools::get_node(., "end")$geometry)
  #
  # tmap = st_intersects(ends, tmp)
  #
  #
  # df = data.frame(
  #   ID       = rep(ends$ID, times = lengths(tmap)),
  #   touches  = tmp$ID[unlist(tmap)],
  #   touches_toID = tmp$toID[unlist(tmap)],
  #   type = rep(ends$type, times = lengths(tmap))
  # ) %>%
  #   filter(.data$ID != .data$touches) %>%
  #   left_join(st_drop_geometry(select(tmp, .data$ID, .data$toID)), by = "ID") %>%
  #   mutate(real_toID = ifelse(.data$type == "nex", .data$toID, .data$touches_toID)) %>%
  #   select(.data$ID, toID = .data$real_toID) %>%
  #   distinct() %>%
  #   left_join(select(tmp, -.data$toID), by = "ID") %>%
  #   bind_rows(filter(tmp, .data$toID > term_cut)) %>%
  #   st_as_sf() %>%
  #   select(-.data$type) %>%
  #   check_network_validity(network_list$catchments, term_cut)
  #
  # df
}

