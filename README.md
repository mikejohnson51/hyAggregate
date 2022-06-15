
<!-- README.md is generated from README.Rmd. Please edit that file -->

.blackbox { padding: 1em; background: black; color: white; border: 2px
solid orange; border-radius: 10px; } .center { text-align: center; }

# hyAggregate

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-15/82-red?style=flat)](#)
[![R CMD
Check](https://github.com/mikejohnson51/hyAggregate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/hyAggregate/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `hyAggregate` is to aggregate refactored (see
[hyRefactor](https://github.com/dblodgett-usgs/hyRefactor)) hydrologic
networks to meet user defined length and area minimum thresholds

## Installation

You can install the released version of `hyAggregate` from Github with:

``` r
remotes::install_github("mikejohnson51/hyAggregate")
```

## Motivation

`hyAggregate` is part of a larger family of hydrofabric development
projects/packages aiming to support federal (USGS/NOAA) water modeling
efforts. `hyAggregate` relies on data products generated as part of the
[Geospatial Fabric for National Hydrologic Modeling, version
2.0](https://www.sciencebase.gov/catalog/item/60be0e53d34e86b93891012b)
project. The general outline of how this project is conceptualized can
be seen in the roadmap below:

``` r
knitr::include_graphics("img/roadmap.png")
```

<img src="img/roadmap.png" width="100%" /> In the first row of Figure 1,
there are thre (3) baseline products. 1. An updated *network attributes*
table that provides attributes for the network features in the data
model of NHDPlus2, but with substantial network improvements based on
contributions for the USGS, NOAA OWP, NCAR and others. 2. A set of
*reference catchment* geometries in which geometric topology errors and
artifacts in the NHDPlus CatchmentSP layer are corrected 3. A set of
*reference flowpath* geometries were the headwater flowpaths have been
replaced with the NHDBurn lines

The CONUS reference files for these baseline datasets can be downloaded
here respectively (attributes, catchments, flowpaths)

In the second row of Figure 2, the baseline products are **refactored**
based on a minimum flowpath criterion

<div id="hello" class="blackbox center">

**NOTICE!** ::

Thank you for noticing this **new notice**! Your noticing it has been
noted, and *will be reported to the authorities*!

</div>

## Getting Baseline Data

the `hyAggregate::get_reference_fabric()` utility will download the most
current geofabric for

``` r
file = get_reference_fabric(VPU = "01", dir = "./data")

sf::st_layers(file)
#> Driver: GPKG 
#> Available layers:
#>       layer_name     geometry_type features fields
#> 1     final_POIS             Point     3185     11
#> 2   nhd_flowline       Line String    65600     36
#> 3  nhd_catchment           Polygon    65968      6
#> 4     reconciled Multi Line String    40269      7
#> 5        divides           Polygon    40191      4
#> 6 mapped_outlets             Point     3400      3
#> 7       agg_cats           Polygon     3400      3
#> 8      agg_fline       Line String     3400      3
#> 9   lookup_table                NA    63892      6
```

For this VPU - the ‘nhd_flowline’ and ‘nhd_catchment’ layers are those
associated with the baseline data. The `reconciled` and `divides` layers
are the output of the refactoring process (row 2 in figure 1). The
remaining layers are those that are central to the `USGS gfv2.0`
modeling task.

This repo (`hyAggregate`) houses the workflow(s) for generating the
needed output for the `NOAA NextGen` modeling task, that starts from the
`reconciled` and `divides` layers.

### Read a File

``` r
## Read Options
# pacman::p_load(sf, DBI, RSQLite, dplyr) 
# 
# ### 1. GDAL/geopackage
# st_layers(g01)
# fps1 = read_sf(g01, "catchment_edge_list")
# head(fps1)
# 
# ### 2. SQLite/Database
# db <- dbConnect(SQLite(), g01)
# dbListTables(db)
# fps2 = tbl(db, "waterbody_params") |> 
#   select(-fid) |> 
#   collect()
# 
# head(fps2)
# 
# dbDisconnect(db)
```
