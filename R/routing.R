#' Check for a package
#' @param pkg package name

check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required for this functionality, but is not installed.")
}


#' Find Latest Version of NWM on NCEP
#' @return character
#' @export

latest_nwm_version = function(){
  ncep = 'https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/'
  ver = grep("nwm", readLines(ncep), value = TRUE)
  gsub('^.*href=\"\\s*|\\s*/.*$', '', ver)
}


#' hyAggregate data direcory
#' @param dir if not supplies will default to `get("ngen_dat_dir", envir = hyAggregate_env)`
#' @return character
#' @export

hyAggregate_data_dir  = function (dir = NULL){
  if (is.null(dir)) {
    return(get("ngen_dat_dir", envir = hyAggregate_env))
  }
  else {
    assign("ngen_dat_dir", dir, envir = hyAggregate_env)
    return(invisible(get("ngen_dat_dir", envir = hyAggregate_env)))
  }
}


#' Get Routelink Path
#' @param dir if not supplies will default to `get("ngen_dat_dir", envir = hyAggregate_env)`
#' @param build if TRUE, and the file does not exist, should it be built?
#' @return character
#' @export

get_routelink_path = function(dir = hyAggregate_data_dir(), build = TRUE){

  check_pkg("RNetCDF")

  ver = latest_nwm_version()

  local_netcdf = file.path(dir, paste0("RouteLink_", gsub("\\.", "_", ver), ".nc"))
  local_fst    = gsub(".nc", ".fst", local_netcdf)

  if(!file.exists(local_fst)){
    if(build){

      rl = paste0('https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/', ver, '/parm/domain/RouteLink_CONUS.nc')

      logger::log_info("Downloading: \n{rl} \n to \n{local_netcdf}")

      dir.create(dir, recursive = TRUE, showWarnings = FALSE)

      httr::GET(rl, httr::write_disk(local_netcdf, overwrite = TRUE), httr::progress())

      logger::log_info("Extracting NetCDF variables.")

      rl_vars = c("link", "from", "to", "alt", "order", "Qi", "MusK", "MusX", "Length", "n", "So", "ChSlp", "BtmWdth", "time", "Kchan", "nCC", "TopWdthCC", "TopWdth")

      nc = open.nc(local_netcdf)
      on.exit(close.nc(nc))

      df = data.frame(do.call(cbind, lapply(rl_vars, function(x) var.get.nc(nc, x))))
      names(df) = rl_vars

      df2 = data.frame(do.call(cbind, lapply(c("link", "NHDWaterbodyComID", "gages"), function(x) var.get.nc(nc, x))))
      names(df2) = c("link", "NHDWaterbodyComID", "gages")

      df  = df2 |>
        mutate(link = as.numeric(link)) |>
        right_join(df, by = "link")  |>
        rename(comid = link) |>
        mutate(gages = trimws(.data$gages),
               gages = ifelse(.data$gages == "", NA, .data$gages),
                NHDWaterbodyComID = ifelse(NHDWaterbodyComID == -9999, NA, NHDWaterbodyComID),
               NHDWaterbodyComID = as.numeric(.data$NHDWaterbodyComID))

      logger::log_info("Converting NetCDF to fst")
      fst::write.fst(df,  local_fst)
      unlink(local_netcdf)
    } else {
      stop("File does not exisit, set `build = TRUE`", call. = FALSE)
    }
  }
    return(local_fst)
}

get_routelink_names = function(build = FALSE){
  path <- get_routelink_path()
  fst::metadata_fst(path)[["columnNames"]]
}


#' Routlink Attribute Subset
#' @param atts character The variable names you would like, always includes comid
#' @param path	character path where the file should be saved. Default is a persistent system data as retrieved by hyAggregate_data_dir. Also see: get_routlink_path
#' @param build if TRUE, and the file does not exist, should it be built?
#' @return character
#' @export


get_routelink = function(atts = NULL, path = get_routelink_path(), build = TRUE){

  available_names = get_routelink_names()

  if (is.null(atts)) {
    atts <- available_names
  } else {
    bad_atts = atts[!atts %in% available_names]
    atts = atts[atts %in% available_names]
    if (length(bad_atts) > 0) {
      message(paste(bad_atts, collapse = ", "), " not in routelink data. Ignoring...")
    }
  }
    return(fst::read_fst(path, c("comid", atts[atts != "comid"])))
}


#' Add Length Mapping from VAA
#' @param flowpaths an sf object
#' @return data.frame
#' @export
#' @importFrom dplyr select mutate filter left_join right_join arrange group_by summarize
#' @importFrom sf st_drop_geometry st_as_sf st_cast st_length

add_length_map = function (flowpaths, length_table) {

  tmp = select(st_drop_geometry(flowpaths), .data$id, comid = .data$member_comid) %>%
    mutate(comid = strsplit(.data$comid, ","))

  unnested = data.frame(id = rep(tmp$id, times = lengths(tmp$comid)),
                        comid = as.character(unlist(tmp$comid)))

  unnested2 = filter(unnested, grepl("\\.", comid)) %>%
    mutate(baseCOMID = floor(as.numeric(comid)))

  lengthm_fp = length_table %>%
    select(baseCOMID = .data$comid, .data$lengthkm) %>%
    mutate(lengthm_fp = .data$lengthkm * 1000, lengthkm = NULL)

  lengthm_id = flowpaths %>%
    mutate(lengthm_id = as.numeric(st_length(.))) %>%
    select(.data$id, .data$lengthm_id) %>%
    st_drop_geometry()

  left_join(unnested2, lengthm_fp, "baseCOMID") |>
    left_join(lengthm_id, by = "id") %>%
    mutate(perLength = round(.data$lengthm_id / .data$lengthm_fp, 3) / 10) %>%
    select(.data$id, .data$comid, .data$perLength) %>%
    right_join(unnested, by = c("id", "comid")) %>%
    mutate(perLength = ifelse(is.na(.data$perLength), 1, as.character(.data$perLength))) %>%
    arrange(.data$id) %>%
    mutate(new = paste0(floor(as.numeric(.data$comid)), ".", gsub("0\\.", "", .data$perLength))) %>%
    group_by(.data$id) %>%
    summarize(lengthMap = paste(.data$new, collapse = ",")) %>%
    right_join(flowpaths, by = "id") %>%
    st_as_sf() |>
    st_cast("MULTILINESTRING")
}

#' Add Slope to Flowpaths
#' @param flowpaths sf object (LINESTRING)
#' @return sf object
#' @export
#' @importFrom nhdplusTools get_vaa
#' @importFrom dplyr select mutate right_join group_by summarize across everything
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr unnest
#' @importFrom stats weighted.mean

add_slope = function(flowpaths) {

  .data <- NULL

  flowpaths =  add_length_map(flowpaths, length_table = get_vaa(c("lengthkm")))

  net_map  <-
    dplyr::select(st_drop_geometry(flowpaths), .data$id, .data$lengthMap) %>%
    mutate(comid = strsplit(.data$lengthMap, ",")) %>%
    tidyr::unnest(cols = .data$comid) %>%
    mutate(
      full_comids = floor(as.numeric(.data$comid)),
      w = 10 * (as.numeric(.data$comid) - .data$full_comids),
      comid = NULL
    )

  df = nhdplusTools::get_vaa(c('lengthkm', "slope")) %>%
    right_join(net_map, by = c('comid' = 'full_comids')) %>%
    select(-.data$lengthMap) %>%
    mutate(w = .data$w * .data$lengthkm) %>%
    group_by(.data$id) %>%
    summarize(across(everything(), ~ round(
      weighted.mean(x = .,
                    w = .data$w,
                    na.rm = TRUE), 8
    ))) %>%
    dplyr::select(-.data$comid, -.data$lengthkm, -.data$w)

  left_join(flowpaths, df, by = "id")
}


#' Length Average Routelink Variables
#' @param flowpaths sf LINESTRING
#' @param rl_vars routelink variables
#' @param rl_path routelink path (see get_routelink_path())
#' @return data.frame
#' @export
#' @importFrom nhdplusTools get_vaa
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select mutate rename right_join rename right_join across everything summarize %>% bind_cols
#' @importFrom stats weighted.mean
#' @importFrom tidyr unnest
#' @importFrom RNetCDF open.nc close.nc var.get.nc

length_average_routelink = function (flowpaths,
                                     rl_vars = c("comid", "Qi", "MusK", "MusX", "n", "So", "ChSlp", "BtmWdth",
                                                 "time", "Kchan", "nCC", "TopWdthCC", "TopWdth"),
                                     rl_path = get_routelink_path()) {


  flowpaths =  add_length_map(flowpaths, length_table = get_vaa("lengthkm"))

  length_m_mapping = flowpaths |>
    mutate(length_m = as.numeric(st_length(flowpaths))) |>
    select(id, length_m) |>
    st_drop_geometry()

  if (!"Length" %in% rl_vars) { rl_vars = c("Length", rl_vars) }

  net_map <- select(st_drop_geometry(flowpaths), .data$id, .data$lengthMap) %>%
    mutate(comid = strsplit(.data$lengthMap, ",")) %>%
    tidyr::unnest(cols = .data$comid) %>%
    mutate(full_comids = floor(as.numeric(.data$comid)),
           w = 10 * (as.numeric(comid) - full_comids),
           comid = NULL)


  df = get_routelink(rl_vars, path = rl_path) %>%
    mutate(comid = as.numeric(comid)) %>%
    right_join(net_map, by = c(comid = "full_comids")) %>%
    select(-.data$lengthMap) %>%
    mutate(w = .data$w * .data$Length) %>%
    group_by(.data$id) %>%
    summarize(across(everything(), ~ round(
      weighted.mean(x = .,
                    w = .data$w, na.rm = TRUE), 8))) %>%
    select(-.data$comid, -.data$Length, -.data$w)

  df2 = get_routelink(c("comid", "gages", "NHDWaterbodyComID"), path = rl_path) %>%
    right_join(net_map, by = c('comid' = "full_comids")) %>%
    group_by(.data$id) %>%
    summarize(gages = paste(.data$gages[!is.na(.data$gages)], collapse = ","),
              NHDWaterbodyComID = paste(unique(.data$NHDWaterbodyComID[!is.na(.data$NHDWaterbodyComID)]), collapse = ",")) %>%
    left_join(df, by = "id") %>%
    mutate(gages = ifelse(.data$gages == "", NA, .data$gages),
           NHDWaterbodyComID = ifelse(.data$NHDWaterbodyComID == "", NA, .data$NHDWaterbodyComID)) |>
    mutate(gages = as.character(.data$gages),
           NHDWaterbodyComID = as.character(.data$NHDWaterbodyComID))

  left_join(df2, length_m_mapping, by = "id")
}
