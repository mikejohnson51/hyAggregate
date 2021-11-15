#' Refactoring Wrapper
#' @description A wrapper around hyRefactor refactor_nhdplus and
#' reconcile_catchment_divides that optionally adds routing parameters to the
#' flowpath objects.
#' @param flowpaths Reference flowline features
#' @param catchments Reference catchment features
#' @param events 	data.frame containing events
#' @param avoid integer vector of COMIDs to be excluded from collapse modifications.
#' @param split_flines_meters numeric the maximum length flowpath desired in the output.
#' @param collapse_flines_meters numeric the minimum length of inter-confluence flowpath desired in the output.
#' @param collapse_flines_main_meters numeric the minimum length of between-confluence flowpaths.
#' @param cores  integer numer of cores to use for parallel execution
#' @param keep   Defines the proportion of points to retain in geometry simplification (0-1; default .9). See ms_simplify.
#' @param facfdr path to directory with flow direction and flow accumulation rasters. If NULL (default) then catchments are NOT reconciled.
#' @param routing path to National Water Model RouteLink file. If NULL (default) then routing parameters are NOT added to the refactroed flowlines.
#' @param outfile path to geopackage to write refactored_flowlines, and if facfdr != NULL, refactored catchments.
#' @return data to the specified gpkg
#' @export
#' @importFrom dplyr filter select rename
#' @importFrom hyRefactor refactor_nhdplus add_lengthmap reconcile_catchment_divides
#' @importFrom sf read_sf st_transform st_drop_geometry write_sf st_crs st_precision
#' @importFrom raster raster res
#' @importFrom nhdplusTools get_streamorder get_vaa

refactor_wrapper = function(flowpaths,
                            catchments,
                            events = NULL,
                            avoid = NULL,
                            split_flines_meters = 10000,
                            collapse_flines_meters = 1000,
                            collapse_flines_main_meters = 1000,
                            cores = 1,
                            facfdr = NULL,
                            routing = NULL,
                            keep = .9,
                            outfile){

  tf <- tempfile(pattern = "refactored", fileext = ".gpkg")
  tr <- tempfile(pattern = "reconciled", fileext = ".gpkg")

  if(!is.null(events)){
    events = filter(events, .data$COMID %in% flowpaths$COMID)
  }

  if(!is.null(events)){
    avoid = avoid[avoid %in% flowpaths$COMID]
  }

  refactor_nhdplus(nhdplus_flines              = flowpaths,
                   split_flines_meters         = split_flines_meters,
                   split_flines_cores          = 1,
                   collapse_flines_meters      = collapse_flines_meters,
                   collapse_flines_main_meters = collapse_flines_main_meters,
                   out_refactored = tf,
                   out_reconciled = tr,
                   three_pass          = TRUE,
                   purge_non_dendritic = FALSE,
                   events = events,
                   exclude_cats = avoid,
                   warn = FALSE)

  rec = st_transform(read_sf(tr), 5070)

  if(!is.null(routing)){

    rec$order = nhdplusTools::get_streamorder(st_drop_geometry(select(rec, .data$ID, .data$toID)),
                                              status = FALSE)

    rec = rec %>%
      rename(length_km = .data$lengthkm) %>%
      length_average_routlink(
        rl_vars = c(
          "link", "Qi", "MusK", "MusX", "n", "So",  "ChSlp",
          "BtmWdth", "time", "Kchan", "nCC", "TopWdthCC", "TopWdth"),
        rl_path  = routing)

    write_sf(st_transform(rec, 5070), outfile, "refactored_flowpaths", overwrite = TRUE)

  } else {

    write_sf(st_transform(rec, 5070), outfile, "refactored_flowpaths", overwrite = TRUE)

  }


  if(!is.null(facfdr)){

    rpus = unique(flowpaths$RPUID)
    rpus = rpus[!is.na(rpus)]

    fdrfac_files = list.files(facfdr, pattern = rpus, full.names = TRUE)
    fdr = raster::raster(grep("_fdr", fdrfac_files, value = TRUE))
    fac = raster::raster(grep("_fac", fdrfac_files, value = TRUE))
    catchments <- st_transform(catchments, st_crs(fdr))
    sf::st_precision(catchments) <- raster::res(fdr)[1]

    if("featureid" %in% names(catchments)){
      catchments = rename(catchments, FEATUREID = .data$featureid)
    }

    reconciled <- st_transform(read_sf(tr), st_crs(fdr))
    refactored <- st_transform(read_sf(tf),  st_crs(fdr))

    divides    <- reconcile_catchment_divides(catchment = catchments,
                                              fline_ref = refactored,
                                              fline_rec = reconciled,
                                              fdr       = fdr,
                                              fac       = fac,
                                              para      = cores,
                                              cache     = NULL,
                                              fix_catchments = TRUE)

    write_sf(st_transform(divides, 5070), outfile, "refactored_catchments", overwrite = TRUE)
  }

  unlink(list(tr, tf))

  list(fps  = read_sf(outfile, "refactored_flowpaths"),
       cats = read_sf(outfile, "refactored_catchments"))
}


#' Assign Length Weighted Flowpath Attributes from Routelink file
#' @description This uses the hyRefactor::add_lengthmap to determine the weighted
#' length of comtributing NHD COMIDs to any refactored/aggregated flowpath.
#' These are then used to assign length averaged values from NWM RouteLink file.
#' @param flowpaths flowpaths with a`member_COMID` attribute
#' @param rl_vars RouteLink variable to append
#' @param rl_path Path to RouteLink
#' @return flowpaths with length weighted RouteLink Attributes
#' @export
#' @importFrom sf st_drop_geometry st_length
#' @importFrom dplyr select mutate rename right_join group_by summarise across everything left_join bind_cols
#' @importFrom tidyr unnest
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom stats weighted.mean
#' @importFrom nhdplusTools get_vaa
#' @importFrom hyRefactor add_lengthmap

length_average_routlink = function(flowpaths, rl_vars, rl_path){

  flowpaths = flowpaths %>%
    hyRefactor::add_lengthmap(length_table = nhdplusTools::get_vaa("lengthkm"))

  if(!"Length" %in% rl_vars){ rl_vars = c("Length", rl_vars) }

  net_map  <- dplyr::select(st_drop_geometry(flowpaths), .data$ID, .data$lengthMap) %>%
    mutate(comid = strsplit(.data$lengthMap, ",")) %>%
    tidyr::unnest(cols = .data$comid) %>%
    mutate(full_comids = floor(as.numeric(.data$comid)),
           w = 10 * (as.numeric(.data$comid) - .data$full_comids),
           comid = NULL)

  nc = RNetCDF::open.nc(rl_path)

  df = data.frame(do.call(cbind, lapply(rl_vars, function(x) RNetCDF::var.get.nc(nc, x))))

  names(df) = rl_vars

  df = df %>%
    rename(comid = .data$link) %>%
    right_join(net_map, by = c('comid' = 'full_comids')) %>%
    select(-.data$lengthMap) %>%
    mutate(w = .data$w * .data$Length) %>%
    group_by(.data$ID) %>%
    summarise(across(everything(), ~ round(
      weighted.mean(x = .,
                    w = .data$w,
                    na.rm = TRUE), 3))) %>%
    dplyr::select(-.data$comid, -.data$Length, -.data$w)

  df2 = suppressMessages({
    lapply(c("link", "gages", 'NHDWaterbodyComID'), function(x) x = RNetCDF::var.get.nc(nc, x)) %>%
      bind_cols()
  })

  names(df2) = c("link", "gages", 'NHDWaterbodyComID')

  df2 = df2 %>%
    rename(comid = .data$link) %>%
    right_join(net_map, by = c('comid' = 'full_comids')) %>%
    mutate(gages = trimws(.data$gages),
           gages = ifelse(.data$gages == "", NA, .data$gages),
           NHDWaterbodyComID = ifelse(.data$NHDWaterbodyComID == -9999, NA, .data$NHDWaterbodyComID)
    ) %>%
    group_by(.data$ID) %>%
    summarise(gages = paste(.data$gages[!is.na(.data$gages)], collapse = ","),
              NHDWaterbodyComID = paste(unique(.data$NHDWaterbodyComID[!is.na(.data$NHDWaterbodyComID)]), collapse = ",")) %>%
    left_join(df) %>%
    mutate(gages = ifelse(.data$gages == "", NA, .data$gages),
           NHDWaterbodyComID = ifelse(.data$NHDWaterbodyComID == "", NA, .data$NHDWaterbodyComID))

  left_join(flowpaths, df2, by = "ID") %>%
    mutate(Length_m = sf::st_length(.))
}
