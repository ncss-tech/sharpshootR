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
