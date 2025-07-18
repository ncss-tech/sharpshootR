# sharpshootR 2.4 (2025-07-15)
 * fixed bug in `formatPLSS()`, affecting township and range values >99
 * rebuilt example data `table5.2`, now without NA in the adjacency matrix (igraph compatibility)
 * rebuilt example data `OSDexamples`


# sharpshootR 2.3.3 (2025-04-16)
 * breaking change: `plotProfileDendrogram()` moved to aqp
 * breaking change: `aggregateColorPlot()` moved to aqp
 * breaking change: removed deprecated functions `geomPosMountainProbability()`, `geomPosHillProbability()`, `surfaceShapeProbability()`, `hillslopeProbability()`
 * `plotSoilRelationGraph()` no longer uses `set.seed(1010101)` for deterministic output
   - be sure it set a seed just before calling for reproducible output
 * update SPC fields from data derived from NASIS (via examples and soilDB data)
 * CRAN release

# sharpshootR 2.3.2 (2024-10-22)
 * rebuild `OSDexamples` with modern `SoilProfileCollection` object
 * CRAN release

# sharpshootR 2.3.1 (2024-05-29)
 * moving vegan to SUGGESTS
 * CRAN release

# sharpshootR 2.3 (2024-04-08)
 * CRAN release
 * `prepare_SSURGO_hydro_data()` now returns more soil property data
 * major updates to `monthlyWB()`:
   - mass balance check, reporting, and messages when not closed
   - new arguments which enable / modify the distribution of PPT and PET within k-bins / month 
 * bug fix in `plotWB()`
 * minor updates to `prepare_SSURGO_hydro_data()`

# sharpshootR 2.2 (2024-08-24)
 * CRAN release
 * added a few more soil hydraulic properties to `prepare_SSURGO_hydro_data()`
 * `plotGeomorphCrossSection()` now respects `aqp::plotSPC()` arguments via `options(.aqp.plotSPC.args = list(...))`
 * removing some hard-coded arguments passed through to `aqp::plotSPC()`
 * `SoilTaxonomyDendrogram()` gains argument to adjust taxon label font

# sharpshootR 2.1 (2023-03-21)
 * CRAN release
 * all functions depending on `sp` classes or methods have been converted to `sf`
 * water balance helper functions converted from `sp`/`rgeos` to `sf`
 * breaking changes in `plotTransect()`, coordinate are now provided as an additional argument / `sf` object
 * breaking changes in `LL2PLSS()`, all return data now packed into `sf` object vs. `list` (sharpshootR <= 1.12)
 * `generateLineHash()` now uses `sf` class LINESTRING features
 * `huePositionPlot()` gains `origin` argument for calculation of dE00 from arbitrary colors specified in CIELAB or Munsell
 * `huePositionPlot()` now passes arguments to `contour()` via `...`
 * breaking changes in `joinAdjacency()`, now expects a `data.frame` (or compatible) vs. `SpatialLinesDataFrame`
 * `polygonAdjacency()` now compatible with `sf` objects
 * `sampleRasterStackByMU()` ignores color table and any categories from source file to ensure raw values are extracted (fixes parity with prior {raster}-based behavior) for categorical data
 
# sharpshootR 1.12 (2022-12-09)
 * CRAN release
 * adding SoilTaxonomy package to suggests, to allow for better encoding of taxa levels by `SoilTaxonomyDendrogram()`
 * `SoilTaxonomyDendrogram()` gains argument `KST.order` to adjust encoding / ordering criteria and `level` argument to specify the taxonomic levels to use
 * `SoilTaxonomyDendrogram()` gains argument `cluster.method` to select `"divisive"` (default) or `"agglomerative"` clustering methods. Additional arguments to `cluster::diana()` and `cluster::agnes()` may be specified with a list supplied as `cluster.args` argument
 * Spatial/raster sampling functions `sampleRasterStackByMU()`, `samplingStability()`, `sample.by.poly()` and `MoranI_By_Raster()` now use terra internally
 * `aspect.plot()` now returns the Rayleigh Uniformity statistic and corresponding p-value as an attribute named `"uniformity"`
 * bug fix in `monthlyWB_summary()` to address warning / `Inf` when 0 dry days
 * `plotWB()` aesthetic fixes and additional arguments, including manual specification of y-axis limits
 * `aggregateColorPlot()` gets a new argument for setting label orientation
 * geomorphic `viz*` functions now use an iterative approach to re-arrange dendrogram according to "hydologic ordering"
 * hydrologic ordering functions now exported: `hydOrder()` and `iterateHydOrder()`
 * new function `plotGeomorphCrossSection()` presents a `SoilProfileCollection` aligned to a geomorphic summary as cross-section
 * new function `reconcileOSDGeomorp()` convenience function for reconciling SPC and geomorphic summary tables as returned by `soilDB::fetchOSD()`
 * `plotProfileDendrogram()` gains argument for re-ordering dendrogram as close to original profile IDs as possible
 * `dueling.dendrograms()` has been deprecated, please use `dendextend::tanglegram()` for similar tasks

# sharpshootR 1.9.1 (2022-02-16)
 * bugfix in `plotWB()` related to figure y-scale range
 * `plotWB_lines()` gets `legend.cex` and aesthetic adjustments to legend position
 
# sharpshootR 1.9 (2022-01-03)
 * CRAN release
 * water balance functions now depend on hydromad >= 0.9-27
 * new function `vizSurfaceShape()`
 * all `viz` functions return clustering object

# sharpshootR 1.8.3 (2021-11-02)
 * `SoilTaxonomyDendrogram()` no longer resetting `par` as it prevents further annotation of the resulting figure
 * bug fix in `CDEC_StationInfo` related to parsing station comments
 * `vizHillslopePosition()` now returns clustering object

# sharpshootR 1.8.2 (2021-09-07)
 * hydromad issue #188 closed, latest binaries include fix in bucket.c
 * `colorMixtureVenn()` now using `method = 'exact'`
 * safer examples for CRAN/Solaris checks
 * new example data for testing "OSDexamples"
 * `vizAnnualClimte`, `vizHillslopePosition`, and related will now work with a single soil series 

# sharpshootR 1.8.1 (2021-05-03)
 * using local (modified) copy of leaky-bucket until resolved:
   - https://github.com/ncss-tech/sharpshootR/issues/40
   - https://github.com/josephguillaume/hydromad/issues/188
 * bugfixes in `plotWB()`
 * new function `plotWB_lines()` c/o J.M. Skovlin
 * `monthlyWB` now includes `AWC` used to init model as an attribute in return value
 * `prepare_SSURGO_hydro_data()` now returns drainage class

# sharpshootR 1.8 (2021-04-22)
 * added `simpleWB`, `dailyWB`, and `dailyWB_SSURGO`
