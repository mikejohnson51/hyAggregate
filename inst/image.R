ref = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-reference/ngen_reference_01a.gpkg'
st_layers(ref)
ref_fl = read_sf(ref, 'flowpaths')
ref_cat = read_sf(ref, 'nhd_catchment')
rfc = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-refactor/01a/rf_s10000_m1000_c1000.gpkg'
st_layers(rfc)
rfc_fl = read_sf(rfc, 'refactored_flowpaths')
rfc_cat = read_sf(rfc, 'refactored_catchments')
agg = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-release/01a/2021-10-28/spatial/hydrofabric.gpkg'
st_layers(agg)
agg_fl = read_sf(agg, 'flowpaths')
agg_cat = read_sf(agg, 'catchments')

catchment_edge_list = hygeo::get_catchment_edges(agg_fl)
hygeo::get_nexus(ref_fl)
hygeo::get_nexus(rfc_fl)

sum(rfc_cat$areasqkm < 3) / nrow(rfc_cat)
library(ggplot2)

ref_ln = ggplot(data = ref_fl, aes(x = LENGTHKM)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 1, lwd = 1) +
  labs(x = "Length (KM)", title = paste0("Reference fabric (", nrow(ref_fl), ")"))

rfc_ln = ggplot(data = rfc_fl, aes(x = LENGTHKM)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 1, lwd = 1) +
  labs(x = "Length (KM)", title = paste0("Refactored fabric (", nrow(rfc_fl), ")"))

agg_ln = ggplot(data = agg_fl, aes(x = length_km)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 1, lwd = 1) +
  labs(x = "Length (KM)", title = paste0("Aggregated fabric (", nrow(agg_fl), ")"))

#####

ref_ar = ggplot(data = ref_cat, aes(x = areqsqkm)) +
  geom_histogram(binwidth=1, fill="#404080", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 3, lwd = 1) +
  geom_vline(xintercept = 10, lwd = 1) +
  geom_vline(xintercept = 20, lwd = 1) +
  xlim(0,30) +
  labs(x = "Area (KM2)", title = paste0("Reference fabric (", nrow(ref_cat), ")"))

rfc_ar = ggplot(data = rfc_cat, aes(x = areasqkm)) +
  geom_histogram(binwidth=1, fill="#404080", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 3, lwd = 1) +
  geom_vline(xintercept = 10, lwd = 1) +
  geom_vline(xintercept = 20, lwd = 1) +
  xlim(0,30) +
  labs(x = "Area (KM2)", title = paste0("Refactored fabric (", nrow(rfc_cat), ")"))

agg_ar = ggplot(data = agg_cat, aes(x = area_sqkm)) +
  geom_histogram(binwidth=1, fill="#404080", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept = 3, lwd = 1) +
  geom_vline(xintercept = 10, lwd = 1) +
  geom_vline(xintercept = 20, lwd = 1) +
  xlim(0,30) +
  labs(x = "Area (KM2)", title = paste0("Aggregated fabric (", nrow(agg_cat), ")"))

library(patchwork)

p_combined = ref_ln + rfc_ln + agg_ln + ref_ar + rfc_ar + agg_ar

ggsave(p_combined,
       filename = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-release/01a/2021-10-28/summary_img.png')

p_combined_scaled = p_combined &
  ylim(0,20000)


ggsave(p_combined_scaled,
       filename = '/Volumes/Transcend/ngen/CONUS-hydrofabric/ngen-release/01a/2021-10-28/summary_img_scaled.png')

