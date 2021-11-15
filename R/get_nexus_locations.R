#' Get Nexus Locations
#'
#' @param fp a sf flowpath object
#' @param term_cut cutoff integer to define terminal IDs
#' @return POINT sf object
#' @export
#' @importFrom dplyr left_join select mutate filter select bind_rows group_by ungroup arrange rename slice_head
#' @importFrom nhdplusTools get_node rename_geometry
#' @importFrom sf st_intersects st_drop_geometry st_as_sf

get_nexus_locations = function(fp, term_cut =  100000000){

  term_node = filter(fp, .data$toID > term_cut | .data$toID == 0) %>%
    rename_geometry("geometry") %>%
    mutate(geometry = get_node(., "end")$geometry) %>%
    slice_min(.data$Hydroseq) %>%
    select(ID = .data$toID)


  if(nrow(fp) <= 1){
    nex = term_node
  } else {
    nex = fp %>%
      left_join(st_drop_geometry(select(., toID = .data$ID, ds_toID = .data$toID)), by = c("toID")) %>%
      filter(.data$ID %in% unique(.data$toID)) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "start")$geometry) %>%
      select(.data$ID, .data$toID)
  }

  imap = st_intersects(nex, fp)

  df = data.frame(
    ID       = rep(nex$ID, times = lengths(imap)),
    touches  = fp$ID[unlist(imap)]) %>%
    mutate(cond = ifelse(.data$ID == .data$touches, "move","aaa")) %>%
    group_by(.data$ID) %>%
    arrange(.data$cond) %>%
    slice_head(n = 1) %>%
    ungroup()

  to_move = filter(fp, .data$ID %in% filter(df, .data$cond == "move")$ID) %>%
    select(.data$ID) %>%
    rename_geometry("geometry")

  to_keep = filter(nex, .data$ID %in% filter(df, .data$cond != "move")$ID) %>%
    select(.data$ID) %>%
    rename_geometry("geometry")

  fp_ends = bind_rows(
    select(fp, .data$ID, .data$Hydroseq, .data$toID) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "end")$geometry, pos = "end"),
    select(fp, .data$ID, .data$Hydroseq, .data$toID) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "start")$geometry, pos = "start")
  )

  imap = st_intersects(to_move, fp_ends)

  df = data.frame(
    ID       = rep(to_move$ID, times = lengths(imap)),
    touches  = fp_ends$ID[unlist(imap)],
    pos      = fp_ends$pos[unlist(imap)],
    hs       = fp_ends$Hydroseq[unlist(imap)]) %>%
    left_join(rename(fp_ends, touches = .data$ID), by = c('touches', 'pos')) %>%
    group_by(.data$ID) %>%
    filter(.data$ID == .data$toID) %>%
    dplyr::slice_max(.data$Hydroseq) %>%
    ungroup() %>%
    st_as_sf() %>%
    select(.data$ID) %>%
    bind_rows(to_keep) %>%
    bind_rows(term_node) %>%
    filter(!duplicated(.))

  df
}

