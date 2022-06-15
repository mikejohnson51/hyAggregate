#' Collapse Network
#' @description Headwaters are those segments in which there are no inflows (!ID %in% toID).
#' This function removes and normalizes the network list to collapse headwater locations that violate
#' prescribed minimum length and area thresholds.
#' @param network_list  a list containing flowline and catchment `sf` objects
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @return list of sf objects
#' @export
#' @importFrom sf st_buffer st_intersects st_drop_geometry
#' @importFrom dplyr group_by mutate filter arrange slice_head ungroup select left_join bind_rows
#' @importFrom rmapshaper ms_explode
#' @importFrom tidyr pivot_longer
#' @importFrom nhdplusTools get_node

collapse_network_inward =  function(network_list,
                                    min_area_sqkm,
                                    min_length_km,
                                    verbose = TRUE) {
  bad = network_list$flowpaths |>
    group_by(levelpathid) |>
    mutate(n = n()) |>
    filter(areasqkm < min_area_sqkm | lengthkm < min_length_km) |>
    flowpaths_to_linestrings() |>
    ms_explode()

  bad_outlets = bad %>%
    mutate(geom = get_node(bad, "end")$geometry) |>
    st_buffer(1)

  emap = st_intersects(bad_outlets, network_list$flowpaths)

  tmp = data.frame(
    id       = rep(bad_outlets$id, times = lengths(emap)),
    touches  = network_list$flowpaths$id[unlist(emap)],
    toid     = rep(bad_outlets$toid, times = lengths(emap))
  ) |>
    filter(!.data$id == .data$touches) |>
    group_by(id)  |>
    mutate(match = touches == toid) |>
    arrange(-match) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(match = NULL) |>
    mutate(set = 1:n()) |>
    select(id, DS_fl = touches) |>
    filter(!is.na(DS_fl))

  ggg = left_join(bad, tmp, by = "id") |>
    mutate(headwater = !(id %in% DS_fl |
                           id %in% network_list$flowpaths$toid)) |>
    filter(!is.na(DS_fl))

  set_topo = filter(ggg, headwater) |>
    st_drop_geometry() |>
    select(id, DS_fl) |>
    mutate(set = 1:n())

  tmp_fl = network_list$flowpaths |>
    filter(!id %in% set_topo$id)

  master = set_topo |>
    mutate(to_lose = id) |>
    mutate(new_id = DS_fl) |>
    pivot_longer(-c(new_id, set)) |>
    filter(name != "id") |>
    select(set, new_id, id = value)


  retained = filter(network_list$catchments, !id %in% master$id) |>
    rename_geometry("geometry")

  lumped = filter(network_list$catchments, id %in% master$id) |>
    left_join(master, by = "id") |>
    geos_union_polygon_hyaggregate("new_id") |>
    rename_geometry("geometry") |>
    rename(id = new_id) |>
    bind_rows(retained)


  tmp_fl = add_hydroseq(tmp_fl)
  nl     = add_measures(tmp_fl, lumped)

  nl = check_network_validity(nl$flowpaths, nl$catchments)
  if (verbose) {
    logger::log_info("Collapsing inward.")
  }
  nl
}

#' @title Aggregate Network
#' @param gf A path to a refactored geofarbric file (see US reference fabric)
#' @param flowpaths an sf object
#' @param catchments an sf object
#' @param ideal_size_sqkm The ideal size of catchments (default = 10 sqkm)
#' @param min_length_km The minimum allowable length of flowpaths (default = 1 km)
#' @param min_area_sqkm The minimum allowable size of catchments (default = 3 sqkm)
#' @param outfile of not NULL, where to write the output files
#' @param verbose print status updates. Default = TRUE
#' @param overwrite overwrite existing gf file. Default is FALSE
#' @param nexus_topology should a hy-features cat-->nex topology be enforced? default = TRUE
#' @return if outfile = TRUE, a file path, else a list object
#' @export
#' @importFrom sf st_transform read_sf st_set_crs write_sf st_layers
#' @importFrom dplyr left_join filter
#' @importFrom nhdplusTools get_sorted calculate_total_drainage_area get_streamorder
#' @importFrom logger log_success log_info

aggregate_network_to_distribution = function(gf = NULL,
                             flowpaths  = NULL,
                             catchments = NULL,
                             ideal_size_sqkm = 10,
                             min_length_km = 1,
                             min_area_sqkm  = 3,
                             outfile = NULL,
                             verbose = TRUE,
                             overwrite = FALSE,
                             nexus_topology = TRUE,
                             routelink_path = NULL) {

  if (!is.null(gf)) {

    if(file.exists(outfile) & overwrite){
      unlink(outfile)
    } else if(file.exists(outfile)){
      warning(outfile, " already exists and overwrite is FALSE", call. = FALSE)
      return(outfile)
    }

    # Make sure needed layers exist
    if (!all(c("reconciled", "divides") %in% sf::st_layers(gf)$name)) {
      stop("Make sure you are using a refactored gf product!")
    }

    # Read Data into R
    flowpaths  <- st_transform(read_sf(gf, "reconciled"), 5070)
    catchments <- st_set_crs(read_sf(gf, "divides"), 5070)

    # Remove non-associated flowpaths and catchments
      #fl <- filter(fl, ID %in% catchments$ID)
      #catchments <- filter(catchments, ID %in% fl$ID)
    # Create a network list and check in DAG and connected
    network_list <- check_network_validity(flowpaths = flowpaths, cat = catchments)
  } else {
    # Create a network list and check if DAG and connected
    network_list <- check_network_validity(flowpaths = flowpaths, cat = catchments)
  }

  # Set all names to lower case!
  names(network_list$flowpaths)  = tolower(names(network_list$flowpaths))
  names(network_list$catchments) = tolower(names(network_list$catchments))

  # Add a hydrosequence to the flowpaths
  network_list$flowpaths = add_hydroseq(flowpaths = network_list$flowpaths)

  # Add area and length measures to the network list
  network_list = add_measures(network_list$flowpaths, network_list$catchments)

  # Perform first aggregation step!
  network_list = aggregate_along_mainstems(network_list,
                                           ideal_size_sqkm,
                                           min_area_sqkm,
                                           min_length_km,
                                           verbose = verbose)

  if (verbose) {
    log_success("Aggregated Along mainstem(s).")
  }

  network_list = collapse_network_inward(network_list, min_area_sqkm, min_length_km, verbose = verbose)
  network_list = collapse_network_inward(network_list, min_area_sqkm, min_length_km, verbose = verbose)

  if (verbose) {
    log_success("Collapsing headwaters inward complete")
  }

  # Create a network list and check if DAG and connected
  network_list = check_network_validity(network_list$flowpaths, network_list$catchments)

  if (verbose) {
    log_success("Network is valid.")
  }

  network_list$flowpaths$tot_drainage_areasqkm = calculate_total_drainage_area(st_drop_geometry(
    select(
      network_list$flowpaths,
      ID = id,
      toID = toid,
      area = areasqkm
    )
  ))

  if (verbose) {
    log_success("Total Upstream Drainage Computed and Added.")
  }

  network_list$flowpaths$order = get_streamorder(st_drop_geometry(
    select(
      network_list$flowpaths,
      ID = id,
      toID = toid
    )
  ))

  if (verbose) {
    log_success("Stream Order Computed and Added.")
  }

  if (nexus_topology) {

    if (verbose) {
      log_info("Applying hy_feature topology...")
    }

    network_list$flowpaths =  assign_nex_ids(network_list$flowpaths) |>
      flowpaths_to_linestrings()

    network_list$catchment_edge_list <- get_catchment_edges_terms(network_list$flowpaths)

    network_list$flowpath_edge_list  <- get_catchment_edges_terms(network_list$flowpaths, catchment_prefix = 'wb-')

    #network_list$waterbody_edge_list <- get_waterbody_edges_terms(network_list$flowpaths)

    network_list$nex =  left_join(
      get_nexus(fp = network_list$flowpaths),
      network_list$catchment_edge_list,
      by = "id"
    )

    network_list$catchments = get_catchment_data(network_list$catchments, network_list$catchment_edge_list)

    network_list$flowpaths  = get_flowpath_data(network_list$flowpaths,   network_list$catchment_edge_list)

    if(!is.null(routelink_path)){
      if (verbose) {
        log_info("Creating Routeing Table based on: {routelink_path}")
      }

      network_list$waterbody_attributes =  length_average_routelink(flowpaths = network_list$flowpaths,
                                                                    rl_path = get_routelink_path())

      if (verbose) {
        log_success("Done!")
      }
    }
  }


  if (is.null(outfile)) {
    return(network_list)
  } else {
    if (verbose) {
      log_info("Writing data to: {outfile}")
    }

    write_sf(network_list$flowpaths,  outfile, "aggregate_flowpaths")
    write_sf(network_list$catchments, outfile, "aggregate_catchments")

    if (!is.null(network_list$nex)) {
      write_sf(network_list$nex, outfile, "nexus")
    }

    # if (!is.null(network_list$catchment_edge_list)) {
    #   write_sf(network_list$catchment_edge_list, outfile, "catchment_edge_list")
    # }

    if (!is.null(network_list$flowpath_edge_list)) {
      write_sf(network_list$flowpath_edge_list, outfile, "flowpath_edge_list")
    }

    # if (!is.null(network_list$waterbody_edge_list)) {
    #   write_sf(network_list$waterbody_edge_list, outfile, "waterbody_edge_list")
    # }

    if (!is.null(network_list$waterbody_attributes)) {
      write_sf(network_list$waterbody_attributes, outfile, "flowpath_attributes")
    }

    if (verbose) {
      log_success("Done!")
    }

    return(outfile)
  }
}


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
#' @importFrom dplyr filter group_by arrange mutate ungroup select
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr %>% cur_group_id n
#' @importFrom logger log_info

aggregate_along_mainstems = function(network_list,
                                     ideal_size_sqkm,
                                     min_area_sqkm,
                                     min_length_km,
                                     verbose = TRUE) {

  index_table = network_list$flowpaths %>%
    st_drop_geometry() %>%
    filter(.data$id %in% network_list$catchments$id) %>%
    group_by(.data$levelpathid) %>%
    arrange(-.data$hydroseq) %>%
    mutate(ind = cs_group(.data$areasqkm, ideal_size_sqkm)) %>%
    ungroup()   %>%
    group_by(.data$levelpathid, .data$ind) %>%
    mutate(set = cur_group_id(),
           n = n()) %>%
    ungroup() %>%
    select(set, id, toid, levelpathid, hydroseq, n, member_comid)

  v = aggregate_sets(
    flowpaths  = filter(network_list$flowpaths,  id %in% index_table$id),
    cat = filter(network_list$catchments, id %in% index_table$id),
    index_table
  )

  if (verbose) {
    log_info("Merged to idealized catchment size of {ideal_size_sqkm} sqkm")
  }

  ##### aggregate to forced size along mainstems

  index_table = v$flowpaths %>%
    st_drop_geometry() %>%
    filter(.data$id %in% v$catchments$id) %>%
    group_by(.data$levelpathid) %>%
    arrange(-.data$hydroseq) %>%
    mutate(
      ind = agg_length_area(
        l = .data$lengthkm,
        a = .data$areasqkm,
        lthres = min_length_km,
        athres = min_area_sqkm
      )
    ) |>
    ungroup()   %>%
    group_by(.data$levelpathid, .data$ind) %>%
    mutate(set = cur_group_id(),
           n = n()) %>%
    ungroup() %>%
    select(set, id, toid, levelpathid, hydroseq, n, member_comid)

  v2 = aggregate_sets(flowpaths = v$flowpaths,
                      cat = v$catchments,
                      index_table)

  if (verbose) {
    log_info("Forced aggregation of catchments less then {min_area_sqkm} sqkm and flowpaths shorter then {min_length_km} km.")
  }

  v2
}
