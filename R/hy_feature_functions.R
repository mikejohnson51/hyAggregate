#' Get catchment edge list
#' @description get a edge list for catchments
#' @param flowpaths  sf data.frame containing hyRefactor or hyAggregate output.
#' @param nexus_prefix  character prefix for nexus IDs
#' @param terminal_nexus_prefix character prefix for terminal nexus IDs
#' @param catchment_prefix character prefix for catchment IDs
#' @param cutoff terminal IDs begin above a defiend threshold
#' @return data.frame
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select mutate left_join bind_rows `%>%`

get_catchment_edges_terms = function(flowpaths,
                                     nexus_prefix = "nex-",
                                     terminal_nexus_prefix = "tnex-",
                                     catchment_prefix = "cat-",
                                     cutoff = 100000000) {

  fline = select(st_drop_geometry(flowpaths), id, toid)

  fline = flush_prefix(fline, "id")
  fline = flush_prefix(fline, "toid")

  obj1 = fline %>%
    select(id = .data$id, toid = .data$toid) %>%
    mutate(id = paste0(catchment_prefix, .data$id),
           toid = paste0(
             ifelse(.data$toid > cutoff, terminal_nexus_prefix, nexus_prefix),
             .data$toid
           ))

  obj2 =  data.frame(id = unique(fline$toid)) %>%
    left_join(mutate(select(fline, id), toid = id), by = "id") %>%
    mutate(toid = ifelse(is.na(.data$toid), 0, .data$toid)) %>%
    mutate(id =  paste0(
      ifelse(.data$id > cutoff, terminal_nexus_prefix, nexus_prefix),
      .data$id
    ),
    toid = paste0(catchment_prefix, .data$toid))

  bind_rows(obj1, obj2)
}


assign_nex_ids = function(fline, term_cut = 1e9) {

  term_node = filter(fline, toid == 0 | is.na(toid)) %>%
    mutate(toid = term_cut + 1:n())

  no_term = filter(fline, !id %in% term_node$id)

  bind_rows(term_node, no_term) %>%
    rename_geometry("geometry")

}



#' @title get nexuses
#' @param fline sf data.frame NHDPlus Flowlines or hyRefactor output.
#' @param nexus_prefix character prefix for nexus IDs
#' @param terminal_nexus_prefix character prefix for ternimal nexus IDs
#' @importFrom sf st_coordinates st_as_sf st_crs st_cast
#' @importFrom dplyr group_by filter ungroup select n row_number rename %>%
#' @export

get_nexus = function(fp, term_cut = 1e9, nexus_prefix = "nex-", terminal_nexus_prefix = "tnx-") {

  fp = st_cast(fp, "MULTILINESTRING")
  fp = flush_prefix(fp, "id")
  fp = flush_prefix(fp, "toid")

  if(!"hydroseq" %in% names(fp)){
    fp = add_hydroseq(fp)
  }

  #fp = identify_terminals(fp, term_cut = term_cut)

  term_node = filter(fp, toid > term_cut |
                       toid == 0 | is.na(toid)) %>%
    rename_geometry("geometry") %>%
    mutate(geometry = get_node(., "end")$geometry) %>%
    select(id = toid)

  nex = fp %>%
    left_join(st_drop_geometry(select(
      fp, toid = id, ds_toID = toid
    )), by = c("toid")) %>%
    filter(id %in% unique(toid)) %>%
    rename_geometry("geometry") %>%
    mutate(geometry = get_node(., "start")$geometry) %>%
    select(id, toid)

  imap = st_intersects(nex, fp)

  df = data.frame(id       = rep(nex$id, times = lengths(imap)),
                  touches  = fp$id[unlist(imap)]) %>%
    mutate(cond = ifelse(id == touches, "move", "aaa")) %>%
    group_by(id) %>%
    arrange(cond) %>%
    slice_head(n = 1) %>%
    ungroup()

  to_move = filter(fp, id %in% filter(df, cond == "move")$id) %>%
    select(id) %>%
    rename_geometry("geometry")

  to_keep = filter(nex, id %in% filter(df, cond != "move")$id) %>%
    select(id) %>%
    rename_geometry("geometry")

  fp_ends = bind_rows(
    select(fp, id, hydroseq, toid) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "end")$geometry, pos = "end"),
    select(fp, id, hydroseq, toid) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "start")$geometry, pos = "start")
  )

  imap = st_intersects(to_move, fp_ends)

  df = data.frame(
    id       = rep(to_move$id, times = lengths(imap)),
    touches  = fp_ends$id[unlist(imap)],
    pos      = fp_ends$pos[unlist(imap)],
    hs       = fp_ends$hydroseq[unlist(imap)]
  ) %>%
    left_join(rename(fp_ends, touches = id), by = c('touches', 'pos')) %>%
    group_by(id) %>%
    filter(id == toid) %>%
    dplyr::slice_max(hydroseq) %>%
    ungroup() %>%
    st_as_sf() %>%
    select(id) %>%
    bind_rows(to_keep) %>%
    bind_rows(term_node)

  df$id = ifelse(df$id >= term_cut, paste0(terminal_nexus_prefix, df$id), paste0(nexus_prefix, df$id))

  df
}



#' Get waterbody edge list
#' @description get a edge list for waterbodies
#' @param flowpaths  sf data.frame containing hyRefactor or hyAggregate output.
#' @param wb_prefix  character prefix for waterbody IDs
#' @param terminal_wb_prefix character prefix for terminal waterbody IDs
#' @param cutoff terminal IDs begin above a defined threshold
#' @return data.frame
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select mutate

get_waterbody_edges_terms = function(flowpaths,
                                     wb_prefix = "wb-",
                                     terminal_wb_prefix = "twb-",
                                     cutoff = 100000000) {

  fline = select(st_drop_geometry(flowpaths), id, toid)

  fline = flush_prefix(fline, "id")
  fline = flush_prefix(fline, "toid")

  fline %>% select(.data$id, .data$toid) %>%
    mutate(id = paste0(
      ifelse(.data$id > cutoff, terminal_wb_prefix, wb_prefix),
      .data$id
    ),
    toid = paste0(
      ifelse(.data$toid > cutoff, terminal_wb_prefix, wb_prefix),
      .data$toid
    ))
}


#' Get Catchment Data
#' @description get a edge list for waterbodies
#' @param catchment  sf data.frame containing hyRefactor or hyAggregate output.
#' @param wb_prefix  character prefix for waterbody IDs
#' @param terminal_wb_prefix character prefix for terminal waterbody IDs
#' @param cutoff terminal IDs begin above a defined threshold
#' @return data.frame
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select mutate

get_catchment_data = function(catchment,
                              catchment_edge_list,
                              catchment_prefix = "cat-") {
  catchment %>%
    mutate(id = paste0(catchment_prefix, .data$id)) %>%
    left_join(catchment_edge_list,  by = "id")
}


get_flowpath_data = function(fline,
                             catchment_edge_list,
                             waterbody_prefix = "wb-",
                             catchment_prefix = "cat-") {

  if ("main_id" %in% names(fline)) {
    fline = rename(fline, levelpathid = main_id)
  }

  if (!"slope" %in% names(fline)) {
    fline = add_slope(flowpaths = fline)
  }


  select(
      fline,
      id = .data$id,
      lengthkm = .data$lengthkm,
      slope_percent = .data$slope,
      main_id = .data$levelpathid,
      member_comid = .data$member_comid,
      tot_drainage_areasqkm = tot_drainage_areasqkm,
      order = order
    ) %>%
    mutate(id = paste0(waterbody_prefix, .data$id)) %>%
    mutate(realized_catchment = gsub(waterbody_prefix, catchment_prefix, id)) %>%
    left_join(catchment_edge_list, by = c("realized_catchment" = "id"))

}
