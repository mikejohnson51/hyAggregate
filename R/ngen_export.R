#' Write GeoJSON
#' @param x sf object
#' @param file file path
#' @export
#' @importFrom sf write_sf st_make_valid st_transform
#'
write_geojson <- function(x, file) {
  names(x) <- tolower(names(x))
  unlink(file)
  write_sf(st_make_valid(st_transform(x, 4326)), file,
           layer_options = c("ID_FIELD=id", "ID_TYPE=String"))
}


#' @title Write NextGen files from GPKG
#' @param gpkg path to geopackage
#' @param catchment_name name of catchment layer
#' @param flowpath_name name of flowpath layer
#' @return NULL
#' @export
#' @importFrom logger log_info log_success
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom jsonlite write_json
#' @importFrom dplyr select mutate left_join group_by arrange ungroup
#' @importFrom nhdplusTools get_vaa
#' @importFrom tidyr unnest_longer

write_ngen_dir = function(gpkg,
                          catchment_name = "aggregate_divides",
                          flowpath_name  = "aggregate_flowpaths",
                          export_shapefiles = FALSE){

  dir = strsplit(gpkg, "\\.")[[1]][1]
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  log_info("Writing to: {dir}")

  write_geojson(
    read_sf(gpkg, catchment_name),
    file.path(dir, "catchment_data.geojson")
  )

  log_success("Completed: ", file.path(dir, "catchment_data.geojson"))

  write_geojson(
    read_sf(gpkg, "nexus"),
    file.path(dir, "nexus_data.geojson")
  )

  log_success("Completed: ", file.path(dir, "nexus_data.geojson"))

  write_json(
    read_sf(gpkg, "flowpath_edge_list"),
    file.path(dir, "flowpath_edge_list.json"),
    pretty = TRUE
  )

  log_success("Completed: ", file.path(dir, "flowpath_edge_list.json"))

  wb_feilds = tryCatch({
    read_sf(gpkg, "flowpath_attributes") },
    error = function(e) {
      read_sf(gpkg, "flowpath_params")
  })

  wb_feilds2 <- split(select(wb_feilds, -id), seq(nrow(wb_feilds)))

  names(wb_feilds2) = wb_feilds$id

  write_json(
    wb_feilds2,
    file.path(dir, "flowpath_params.json"),
    pretty = TRUE
  )

  log_success("Completed: ", file.path(dir, "flowpath_params.json"))

  network_order <-  get_vaa("hydroseq")

  nwis_sites =  select(wb_feilds, id, gages)

  nhd_crosswalk <- st_drop_geometry(read_sf(gpkg, flowpath_name)) %>%
    select(id, member_comid, main_id) %>%
    mutate(comid = strsplit(member_comid, ",")) %>%
    unnest_longer(col = c("comid")) %>%
    mutate(id = id,
           comid = as.numeric(comid)) %>%
    select(id, comid, main_id) %>%
    left_join(network_order, by = "comid") %>%
    group_by(id) %>%
    arrange(hydroseq) %>%
    mutate(outlet_comid = dplyr::first(comid)) %>%
    left_join(nwis_sites, by = "id") %>%
    ungroup()

  nhd_crosswalk_list <- lapply(unique(nhd_crosswalk$id),
                               function(x, df) {
                                 df_sub <- df[df$id == x, ]
                                 out <- list(member_comids = df_sub$comid)
                                 if (any(!is.na(df_sub$gages))) {
                                   out$gages <- unique(df_sub$gages[!is.na(df_sub$gages)])
                                 }
                                 out$outlet_comid <- unique(df_sub$outlet_comid)
                                 out$main_id = unique(df_sub$main_id)
                                 out
                               }, df = nhd_crosswalk)


  names(nhd_crosswalk_list) <- unique(nhd_crosswalk$id)

  write_json(nhd_crosswalk_list,
                       file.path(dir, "crosswalk-mapping.json"),
                       pretty = TRUE,
                       auto_unbox = TRUE)

  log_success("Completed: ", file.path(dir, "crosswalk-mapping.json"))

  if(export_shapefiles){ write_shapefile_dir(gpkg, dir = dir) }

  return(dir)
}


#' @title Write NextGen geopackage as directory of shapefiles
#' @param gpkg path to geopackage
#' @param dir directory path to create a 'shp' folder for output
#' @return NULL
#' @export
#' @importFrom logger log_info
#' @importFrom sf gdal_utils

#' @importFrom tidyr unnest_longer

write_shapefile_dir = function(gpkg, dir){

  outpath = file.path(dir, "shps")
  dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
  log_info("Writing shapefiles to: {outpath}")


  sf::gdal_utils(
    util = "vectortranslate",
    source = gpkg,
    destination = outpath, # output format must be specified for GDAL < 2.3
    options = c("-f", "ESRI Shapefile", "-overwrite")
)
}


