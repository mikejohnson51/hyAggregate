#' Extract upstream network from reference fabric
#' @param network enhd_nhdplusatts data.frame
#' @param reference_fabric_dir path to refernece network directory
#' @param comid Origin of network trace (COMID)
#' @return list containing catchment and flowpath networks
#' @export
#' @importFrom dplyr left_join filter pull tbl collect bind_rows
#' @importFrom nhdplusTools get_UT get_vaa
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom sf st_as_sf

# network          = arrow::read_parquet("/Volumes/Transcend/ngen/climate/enhd_nhdplusatts.parquet")
# reference_fabric_dir = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-reference')


get_UT_geoms = function(network,
                        reference_fabric_dir,
                        comid){

  reference_fabric = list.files(reference_fabric_dir, full.names = TRUE)

  master = left_join(data.frame(comid =  get_UT(network, comid)), get_vaa("rpuid"), by = "comid")

  RPUS = unique(master$rpuid)
  RPUS = RPUS[!is.na(RPUS)]

  cats = list()
  fps  = list()

  for(i in 1:length(RPUS)){

    fin = grep(RPUS[i],reference_fabric, value = TRUE)

    db <- dbConnect(RSQLite::SQLite(), fin)

    mas = filter(master, .data$rpuid == RPUS[i]) %>%
      pull(.data$comid)

    fps[[i]] = tbl(db, "flowpaths") %>%
      dplyr::filter(.data$COMID %in% mas) %>%
      collect() %>%
      st_as_sf(crs = 4326)

    cats[[i]] =  tbl(db, "nhd_catchment") %>%
      dplyr::filter(.data$featureid %in% mas) %>%
      collect() %>%
      st_as_sf(crs = 4326)

    dbDisconnect(db)

  }

  list(fps = bind_rows(fps), cats = bind_rows(cats))

}
