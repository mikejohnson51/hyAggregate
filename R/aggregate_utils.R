#' Drop Artifical Splits
#' @description Removes Orphan flowpaths that do not connect to the network
#' and that are represented as a MULTILINESTRING object
#' @param fl a LINESTRING `sf` flowlines object
#' @return a LINESTRING `sf` flowlines object
#' @export
#' @importFrom sf st_geometry_type st_cast st_filter
#' @importFrom dplyr filter bind_rows


drop_artifical_splits = function(fl){
  fl2  = flowpaths_to_linestrings(fl)
  gaps = filter(fl2, st_geometry_type(fl2) == "MULTILINESTRING")
  ls   = filter(fl2, st_geometry_type(fl2) == "LINESTRING")

  if(nrow(gaps) > 0){
    new_geom = lapply(1:nrow(gaps), function(x){
      tmp = suppressWarnings({ st_cast(gaps[x,], "LINESTRING") })
      st_filter(tmp, filter(fl2, .data$ID != tmp$ID[1]))
    })

    return(bind_rows(ls, new_geom))

  } else {
    return(fl2)
  }
}

#' Flowpaths to linestrings
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


#' Compute km length
#' Short hand for safely computing length in km and returning as numeric vector.
#' @param x sf object
#' @return numeric vector
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_length

add_lengthkm = function(x){ as.numeric(units::set_units(st_length(x), "km")) }

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
      ids[length(ids)] = ids[length(ids)] - 1
    }
  }

  return (ids)
}

#' Aggregate Network by Index
#' @description When provided with an index table, return a set of aggregated
#' and  normalized catchment-flowpaths
#' @param fl a LINESTRING `sf` flowlines object
#' @param cat a POLYGON `sf` catchments object
#' @param index_table a table of network metadata including
#' c(grouping_id, 'ID',  'toID', 'Hydroseq', 'member_COMID', 'LevelPathID')
#' @param grouping_id the name of the index table containing the grouping variable
#' @return a list containing aggregated and validated flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr group_by mutate select ungroup filter left_join right_join slice_min summarise rename bind_rows n
#' @importFrom sf st_as_sf st_line_merge
#' @importFrom nhdplusTools rename_geometry
#' @importFrom hyRefactor union_polygons_geos clean_geometry add_areasqkm

aggregate_network_by_index <- function(fl, cat, index_table, grouping_id = "set"){

  required_names = c(grouping_id, 'ID',  'toID', 'Hydroseq', 'member_COMID', 'LevelPathID')

  missing_names  = required_names[(!required_names %in% names(index_table))]

  if(length(missing_names) > 0){
    stop("Missing attribtues (", missing_names, ") in index table")
  }

  index_table = index_table %>%
    group_by(.data[[grouping_id]]) %>%
    mutate(n = n()) %>%
    select(.data$ID, n, !!grouping_id, .data$toID, .data$Hydroseq, .data$member_COMID, .data$LevelPathID) %>%
    ungroup()

  not_to_merge = filter(index_table, .data$n == 1)

  to_merge = filter(index_table, n > 1) %>%
    group_by(.data[[grouping_id]]) %>%
    mutate(ID       = .data$ID,
           newID    = .data$ID[which.max(.data$Hydroseq)],
           toID     = .data$toID[which.min(.data$Hydroseq)],
           member_COMID  = paste(.data$member_COMID, collapse = ",")) %>%
    ungroup()


  if(!is.null(fl)){
    not_to_merge_fp = left_join(not_to_merge, select(fl, .data$ID), by = "ID")  %>%
      st_as_sf() %>%
      nhdplusTools::rename_geometry("geometry")

  } else {
    not_to_merge_fp = NULL
  }


  if(!is.null(cat)) {
    not_to_merge_cat = left_join(not_to_merge, select(cat, .data$ID), by = "ID")  %>%
      st_as_sf() %>%
      nhdplusTools::rename_geometry("geometry")
  } else {
    not_to_merge_cat = NULL
  }



  if(nrow(to_merge) == 0){

    check_network_validity(fl = not_to_merge_fp, cat = not_to_merge_cat)

  } else {

    stopifnot( nrow(index_table) == (nrow(to_merge) + nrow(not_to_merge)))

    retained = to_merge %>%
      group_by(.data$newID, .data$toID, .data$member_COMID, .data$LevelPathID) %>%
      slice_min(.data$Hydroseq) %>%
      ungroup()

    if(!is.null(fl)){
      fin = right_join(select(fl, .data$ID), to_merge, by = "ID") %>%
        group_by(.data$newID) %>%
        summarise() %>%
        flowpaths_to_linestrings() %>%
        nhdplusTools::rename_geometry("geometry") %>%
        #hyRefactor::union_linestrings_geos("newID") %>%
        left_join(retained, by = "newID") %>%
        select(-.data$ID) %>%
        rename(ID = .data$newID) %>%
        bind_rows(not_to_merge_fp)

      fp = filter(fin, !.data$toID %in% .data$ID) %>%
        filter(!st_is_empty(.)) %>%
        left_join(select(to_merge, .data$ID, .data$newID), by = c("toID" = "ID")) %>%
        select(-.data$toID) %>%
        rename(toID = .data$newID) %>%
        bind_rows(filter(fin, .data$toID %in%.data$ID)) %>%
        select(-.data$n, -.data$set)

      } else {
      fp = NULL
    }


    if(!is.null(cat)){

      ncat = right_join(cat, to_merge, by = 'ID') %>%
        filter(!st_is_empty(.)) %>%
        hyRefactor::union_polygons_geos("newID")  %>%
        hyRefactor::clean_geometry("newID", keep = NULL) %>%
        left_join(retained, by = "newID") %>%
        select(ID = .data$newID) %>%
        mutate(areasqkm = add_areasqkm(.)) %>%
        bind_rows(not_to_merge_cat)
    } else {
      ncat = NULL
    }

    check_network_validity(fl = fp, cat = ncat)
  }
}
