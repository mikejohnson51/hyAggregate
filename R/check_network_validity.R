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

network_is_dag = function(fl, ID = "ID", toID = "toID"){
  st_drop_geometry(select(fl, !!ID, !!toID)) %>%
    igraph::graph_from_data_frame(directed = TRUE) %>%
    igraph::is.dag()
}

#' Check Network Validity
#' **INTERNAL** function that validates a flowline and catchment network
#' @param fl a LINESTRING `sf` flowlines object
#' @param cat a POLYGON `sf` catchments object
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing flowline and catchment `sf` objects
#' @export
#' @importFrom hyRefactor add_areasqkm
#' @importFrom dplyr mutate select left_join
#' @importFrom sf st_drop_geometry

check_network_validity     <- function(fl, cat, term_cut = 100000000){

  fl  = fl[!duplicated(fl),]
  cat = cat[!duplicated(cat),]

  fl$toID    = ifelse(is.na(fl$toID), 0, fl$toID)
  DAG        = network_is_dag(fl)
  CONNECTION = sum(!(fl$toID %in% fl$ID | fl$toID > term_cut | fl$toID == 0)) == 0

  if(all(DAG,  CONNECTION)){

    fl  = mutate(fl, lengthkm = add_lengthkm(fl))

    if(!is.null(cat)){
      cat =  mutate(cat, areasqkm = hyRefactor::add_areasqkm(cat)) %>%
        select(.data$ID, .data$areasqkm)

      if('areasqkm' %in% names(fl)){
        fl = fl %>%
          select(-.data$areasqkm) %>%
          left_join(st_drop_geometry(cat), by = "ID")
      } else {
        fl = fl %>%
          left_join(st_drop_geometry(cat), by = "ID")
      }
    }

    return(list(flowpaths = fl, catchments = cat))

  } else {
    if(!DAG){ stop("Network is not a graph.")}
    if(!CONNECTION){stop("All toIDs are not present in network")}

    return(NULL)
  }
}
