# sharpshootR 1.11 (2022-09-26)
 * adding SoilTaxonomy package to suggests, to allow for better encoding of taxa levels by `SoilTaxonomyDendrogram`
 * `SoilTaxonomyDendrogram()` gains argument `KST.order` to adjust encoding / ordering criteria
 * bug fix in `monthlyWB_summary()` to address warning / `inf` when 0 dry days
 * `plotWB()` aesthetic fixes and additional arguments, including manual specification of y-axis limits
 * `aggregateColorPlot()` gets a new argument for setting label orientation
 * geomorphic `viz*` functions now use an iterative approach to re-arrange dendrogram according to "hydologic ordering"
 * hydrologic ordering functions now exported: `hydOrder()` and `iterateHydOrder()`
 * new function `plotGeomorphCrossSection()` presents a `SoilProfileCollection` aligned to a geomorphic summary as cross-section
 * new function `reconcileOSDGeomorp()` convenience function for reconciling SPC and geomorphic summary tables as returned by `soilDB::soilDB()`

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
