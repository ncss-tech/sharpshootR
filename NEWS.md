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
