#' Flush existing ID prefixes
#' Given a data object and column, remove a prefix and adjoining "-"
#' @param input input data object
#' @param col column to remove prefix from
#' @return data object with updated column
#' @export
flush_prefix = function(input, col) {
  for (i in col) {
    input[[i]] = as.numeric(gsub(".*-", "", input[[i]]))
  }
  input
}

#' Add hydrosequence
#' @param flowpaths sf object (LINESTRING)
#' @return sf object
#' @export
#' @importFrom  nhdplusTools get_sorted rename_geometry
add_hydroseq = function(flowpaths) {
  flowpaths$terminalID = NULL
  flowpaths$terminalid = NULL
  flowpaths$toid = ifelse(is.na(flowpaths$toid), 0, flowpaths$toid)
  flowpaths = get_sorted(select(flowpaths, id, toid, everything()), split = TRUE)
  flowpaths['hydroseq'] = 1:nrow(flowpaths)
  rename_geometry(flowpaths, "geometry")
}
#' Add Measures to Flowlines and Catchments
#' @param flowpaths sf object (LINESTRING)
#' @param cat sf object (POLYGON)
#' @return list
#' @export
#' @importFrom dplyr select left_join
#' @importFrom sf st_drop_geometry
#' @importFrom nhdplusTools rename_geometry

add_measures = function(flowpaths, cat) {
  flowpaths$lengthkm  = add_lengthkm(flowpaths)
  cat$areasqkm = add_areasqkm(cat)
  flowpaths$areasqkm = NULL
  flowpaths = left_join(flowpaths,
                 select(st_drop_geometry(cat), id, areasqkm),
                 by = "id")
  list(flowpaths  = rename_geometry(flowpaths, "geometry"),
       catchments = rename_geometry(cat, "geometry"))
}


#' @title Flowpaths to linestrings
#' @description Takes an input list of flowpaths object and converts
#' all possible features to LINESTRING
#' @param fl a LINESTRING/MULTILINESTRING `sf` flowlines object
#' @return a LINESTRING `sf` flowlines object
#' @export
#' @importFrom sf st_geometry_type st_geometry st_line_merge
#' @importFrom dplyr bind_rows

flowpaths_to_linestrings = function(fl){
  bool = (st_geometry_type(sf::st_geometry(fl)) == "MULTILINESTRING")
  multis = fl[bool, ]
  if(nrow(multis) > 0){
    sf::st_geometry(multis) = st_line_merge(sf::st_geometry(multis))
  }
  singles = fl[!bool, ]

  bind_rows(multis, singles)
}

#' Compute length in kilometers
#' @param x LINESTRING sf object
#' @return numeric vector
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_length

add_lengthkm = function (x) { drop_units(units::set_units(st_length(x), "km")) }

#' Compute area in square kilometers
#' @param x POLYGON sf object
#' @return numeric vector
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_area

add_areasqkm = function (x) { drop_units(set_units(st_area(x), "km2")) }

#' Aggregate Sets by Index Table
#' @param flowpaths LINESTRING flowpaths
#' @param cat POLYGON catchments
#' @param index_table index table to aggregate with
#' @return a list of catchments and flowpaths that have been validated
#' @export
#' @importFrom dplyr group_by mutate slice_max ungroup select left_join everything filter bind_rows rename `%>%`
#' @importFrom sf st_as_sf
#' @importFrom nhdplusTools rename_geometry get_sorted
#'
aggregate_sets = function(flowpaths, cat, index_table) {

  set_topo = index_table %>%
    group_by(set) %>%
    mutate(member_comid  = paste(.data$member_comid, collapse = ",")) %>%
    slice_max(hydroseq) %>%
    ungroup() %>%
    select(set, id = toid, levelpathid, member_comid) %>%
    left_join(select(index_table, toset = set, id), by = "id") %>%
    select(-id)

  single_flowpaths = filter(index_table, n == 1) %>%
    left_join(flowpaths, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    rename_geometry("geometry")

  flowpaths_out  = filter(index_table, n > 1) %>%
    left_join(flowpaths, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    geos_union_linestring_hyaggregate('set') %>%
    rename_geometry("geometry") %>%
    bind_rows(single_flowpaths) %>%
    select(set) %>%
    left_join(set_topo, by = "set") %>%
    rename(ID = set, toID = toset) %>%
    flowpaths_to_linestrings()
  ####

  single_catchments = filter(index_table, n == 1) %>%
    left_join(cat, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    rename_geometry("geometry")

  catchments_out  = filter(index_table, n != 1) %>%
    left_join(cat, by = "id") %>%
    st_as_sf() %>%
    #TODO: this is dealing with disjoint levelpath catchments that are "merged". Creates a MULTIPOLYGON from
    # Multiple input polygons when needed
    geos_union_polygon_hyaggregate('set') %>%
    rename_geometry("geometry") %>%
    bind_rows(single_catchments) %>%
    select(set) %>%
    left_join(set_topo, by = "set") %>%
    rename(ID = set, toID = toset) %>%
    filter(!sf::st_is_empty(.))

  catchments_out$toID = ifelse(is.na(catchments_out$toID), 0, catchments_out$toID)
  names(flowpaths_out) = tolower(names(flowpaths_out))
  names(catchments_out) = tolower(names(catchments_out))
  flowpaths_out = add_hydroseq(flowpaths_out)
  nl = add_measures(flowpaths_out, catchments_out)
  check_network_validity(flowpaths = nl$flowpaths, cat = nl$catchments)
}

##' @title Fast LINESTRING union
#' @description Wayyyy faster then either data.table, or sf based line merging
#' @param lines lines to merge
#' @param ID ID to merge over
#' @return LINESTRING sf object
#' @export
#' @importFrom terra aggregate vect
#' @importFrom dplyr select
#' @importFrom sf st_as_sf

geos_union_linestring_hyaggregate = function (lines, ID)  {
  aggregate(vect(lines), by = eval(ID)) %>%
    st_as_sf() %>%
    select(!!ID) %>%
    flowpaths_to_linestrings()
}

#' @title Fast POLYGON Union
#' @description This is significantly faster then sf::st_union or summarize
#' @param poly sf POLYGON object
#' @param ID the column name over which to union geometries
#' @return sf object
#' @export
#' @importFrom terra aggregate vect
#' @importFrom dplyr select
#' @importFrom sf st_as_sf st_collection_extract st_geometry_type st_make_valid

geos_union_polygon_hyaggregate = function(poly, ID) {
  poly = aggregate(vect(poly), by = eval(ID)) %>%
    st_as_sf() %>%
    select(!!ID) %>%
    st_make_valid()

  if (any(grepl("COLLECTION",  st_geometry_type(poly)))) {
    poly = st_collection_extract(poly, "POLYGON")
  }
  return(poly)
}

#' Enforces area and length grouping
#' @description This function takes a vector of area's and length's and returns a
#' grouping vector that enforces the grouping of lengths and areas less then defined thresholds
#' @param l a vector of lengths
#' @param a a vector of areas
#' @param lthres a minimum length that must be achieved
#' @param athres a minimum length that must be achieved
#' @return a vector of length(a) containing grouping indexes
#' @export

agg_length_area   <- function(l, a, lthres, athres) {

  ids = 1:length(l)

  if(length(ids) != 1){

    if(!is.null(lthres)){
      for (i in 1:(length(l)-1)) {
        if (l[i] < lthres) {
          ids[(i+1):length(l)] = ids[(i+1):length(l)] - 1
          l[i+1] = l[i] + l[i+1]
          l[i]   = l[i+1]
          a[i+1] = a[i] + a[i+1]
          a[i] =   a[i+1]
        }
      }
    }

    if(!is.null(athres)){
      for (i in 1:(length(a)-1)) {
        if (a[i] < athres) {
          ids[(i+1):length(a)] = ids[(i+1):length(a)] - 1
          a[i+1] = a[i] + a[i+1]
          a[i] =   a[i+1]
        }
      }
    }

    if(is.null(athres)){ athres = 0 }
    if(is.null(lthres)){ lthres = 0 }

    if(a[length(a)] < athres | l[length(l)] < lthres){
      ids[length(ids)] = pmax(1, ids[length(ids)] - 1)
    }
  }

  return (ids)
}

#' Cumulative sum area grouping
#' @description This function takes a vector of area's and returns a
#' grouping vector that aims to combine then towards an ideal aggregate size (threshold)
#' @param a a vector of areas
#' @param athres a threshold, or target, cumulative size
#' @return a vector of length(a) containing grouping indexes
#' @export

cs_group                   <- function(a, athres) {
  cumsum <- 0
  group  <- 1
  result <- numeric()
  for (i in 1:length(a)) {
    cumsum <- cumsum + a[i]
    if (cumsum > athres) {
      group <- group + 1
      cumsum <- a[i]
    }
    result = c(result, group)
  }
  return (result)
}

#' Check Network Validity
#' **INTERNAL** function that validates a flowpath and catchment network
#' @param flowpaths a LINESTRING `sf` flowpaths object
#' @param cat a POLYGON `sf` catchments object
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr mutate select left_join
#' @importFrom sf st_drop_geometry

check_network_validity     <- function(flowpaths, cat, term_cut = 100000000, check = TRUE){

  flowpaths  = flowpaths[!duplicated(flowpaths),]
  cat = cat[!duplicated(cat),]

  names(flowpaths) = tolower(names(flowpaths))
  names(cat) = tolower(names(cat))

  flowpaths$toid    = ifelse(is.na(flowpaths$toid), 0, flowpaths$toid)
  DAG        = network_is_dag(flowpaths)
  CONNECTION = sum(!(flowpaths$toid %in% flowpaths$id | flowpaths$toid > term_cut | flowpaths$toid == 0)) == 0

  if(!check){ return(list(flowpaths = fl, catchments = cat))}

  if(all(DAG,  CONNECTION)){

    flowpaths  = mutate(flowpaths, lengthkm = add_lengthkm(flowpaths))

    if(!is.null(cat)){
      cat =  mutate(cat, areasqkm = add_areasqkm(cat))  %>%
        select(.data$id, .data$areasqkm)

      if('areasqkm' %in% names(flowpaths)){
        flowpaths = flowpaths %>%
          select(-.data$areasqkm) %>%
          left_join(st_drop_geometry(cat), by = "id")
      } else {
        flowpaths =  left_join(flowpaths, st_drop_geometry(cat), by = "id")
      }
    }

    return(list(flowpaths = flowpaths, catchments = cat))

  } else {
    if(!DAG){ stop("Network is not a graph.")}
    if(!CONNECTION){stop("All toIDs are not present in network")}

    return(NULL)
  }
}

#' Check if network is DAG
#' Checks in a `sf` flowline network is a DAG (Directed acyclic graph).
#' @param fl a LINESTRING `sf` flowlines object
#' @param ID the name of the ID column in `fl`
#' @param toID the name of the toID column in `fl`
#' @return boolean
#' @export
#' @importFrom igraph graph_from_data_frame is.dag
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select

network_is_dag = function(fl, ID = "id", toID = "toid"){
  st_drop_geometry(select(fl, !!ID, !!toID)) %>%
    graph_from_data_frame(directed = TRUE) %>%
    is.dag()
}

#' Get Nexus Locations
#'
#' @param fp a sf flowpath object
#' @param term_cut cutoff integer to define terminal IDs
#' @return POINT sf object
#' @export
#' @importFrom dplyr left_join select mutate filter select bind_rows group_by ungroup arrange rename slice_head slice_min
#' @importFrom nhdplusTools get_node rename_geometry
#' @importFrom sf st_intersects st_drop_geometry st_as_sf

get_nexus_locations = function(fp, term_cut =  100000000){

  fp$toid = ifelse(is.na(fp$toid), 0, fp$toid)

  term_node = filter(fp, .data$toid > term_cut | .data$toid == 0) %>%
    rename_geometry("geometry") %>%
    mutate(geometry = get_node(., "end")$geometry) %>%
    dplyr::slice_min(.data$hydroseq) %>%
    select(id = .data$toid)


  if(nrow(fp) <= 2){
    nex = term_node
  } else {
    nex = fp %>%
      left_join(st_drop_geometry(select(., toid = .data$id, ds_toID = .data$toid)), by = c("toid")) %>%
      filter(.data$id %in% unique(.data$toid)) %>%
      rename_geometry("geometry") %>%
      rmapshaper::ms_explode() %>%
      mutate(geometry = get_node(., "start")$geometry) %>%
      select(.data$id, .data$toid)
  }

  imap = st_intersects(nex, fp)

  df = data.frame(
    id       = rep(nex$id, times = lengths(imap)),
    touches  = fp$id[unlist(imap)]) %>%
    mutate(cond = ifelse(.data$id == .data$touches, "move","aaa")) %>%
    group_by(.data$id) %>%
    arrange(.data$cond) %>%
    slice_head(n = 1) %>%
    ungroup()

  to_move = filter(fp, .data$id %in% filter(df, .data$cond == "move")$id) %>%
    select(.data$id) %>%
    rename_geometry("geometry")

  to_keep = filter(nex, .data$id %in% filter(df, .data$cond != "move")$id) %>%
    select(.data$id) %>%
    rename_geometry("geometry")

  fp_ends = bind_rows(
    select(fp, .data$id, .data$hydroseq, .data$toid) %>%
      rename_geometry("geometry") %>%
      rmapshaper::ms_explode() %>%
      mutate(geometry = get_node(., "end")$geometry, pos = "end"),
    select(fp, .data$id, .data$hydroseq, .data$toid) %>%
      rename_geometry("geometry") %>%
      mutate(geometry = get_node(., "start")$geometry, pos = "start")
  )

  imap = st_intersects(to_move, fp_ends)

  df = data.frame(
    id       = rep(to_move$id, times = lengths(imap)),
    touches  = fp_ends$id[unlist(imap)],
    pos      = fp_ends$pos[unlist(imap)],
    hs       = fp_ends$hydroseq[unlist(imap)]) %>%
    left_join(rename(fp_ends, touches = .data$id), by = c('touches', 'pos')) %>%
    group_by(.data$id) %>%
    filter(.data$id == .data$toid) %>%
    dplyr::slice_max(.data$hydroseq) %>%
    ungroup() %>%
    st_as_sf() %>%
    select(.data$id) %>%
    bind_rows(to_keep) %>%
    bind_rows(term_node) %>%
    filter(!duplicated(.))

  df
}


