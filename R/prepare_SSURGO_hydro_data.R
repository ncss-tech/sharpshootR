
#' @title Get and prepare basic soil hydraulic parameters from SSURGO via SDA
#'
#' @param cokeys vector of component keys (cokey) in current SSURGO snapshot
#' @param max.depth target depth of aggregation, corrected later by real soil depth as reported by `slab()`
#'
#' @author D.E. Beaudette
#'
#' @return a `list` containing:
#'    * `SPC`: `SoilProfileCollection`
#'    * `agg`: aggregate representation of hydraulic parameters
#'    
#' @export
#'
#' 
#' 
prepare_SSURGO_hydro_data <- function(cokeys, max.depth) {
  
  
  in.statement <- format_SQL_in_statement(cokeys)
  
  # assemble SDA query
  sql <- sprintf("
                 SELECT chorizon.cokey, compname, drainagecl, hzname, hzdept_r AS hz_top, hzdepb_r AS hz_bottom,
                 (hzdepb_r - hzdept_r) AS thick,
                 wsatiated_r / 100.0 AS sat,
                 wthirdbar_r / 100.0 AS fc,
                 wfifteenbar_r / 100.0 as pwp,
                 awc_r as awc,
                 -- catch strange cases where soil_fraction is NULL
                 COALESCE(soil_fraction, 1) as soil_fraction
                 FROM component 
                 JOIN chorizon ON component.cokey = chorizon.cokey
                 -- soil_fraction = 100 pct - (rock fragment volume pct)
                 LEFT JOIN (
                 SELECT chkey, (100.0 - sum(COALESCE(fragvol_r, 0))) / 100.0 as soil_fraction
                 FROM chfrags
                 GROUP BY chkey
                 ) AS frags ON chorizon.chkey = frags.chkey
                 WHERE chorizon.cokey IN %s
                 AND wsatiated_r IS NOT NULL
                 ORDER BY compname, hzdept_r;
                 ", in.statement 
  )
  
  # get via SDA
  s <- suppressMessages(SDA_query(sql))
  
  ## TODO account for no / missing data
  
  ## TODO: use cokey vs. compname in case of multiple phases
  
  # init SPC for slab()
  s$cokey <- as.character(s$cokey)
  depths(s) <- cokey ~ hz_top + hz_bottom
  site(s) <- ~ compname + drainagecl
  
  ## TODO: weighted mean is simpler to explain (physical mixture)
  # weighted median values
  agg.soil.data <- slab(s, compname ~ sat + fc + pwp + awc + soil_fraction, slab.structure = c(0, max.depth))
  
  # retain medians for now
  # using reshape2
  agg.soil.data.median <- dcast(agg.soil.data, compname ~ variable, value.var = 'p.q50')
  
  
  # get contributing_fraction for corrected depth calculation
  real.depths <- unique(agg.soil.data[, c('compname', 'contributing_fraction')])
  
  # join corrected depths with aggregate data
  agg.soil.data.median <- merge(agg.soil.data.median, real.depths, by='compname', all.x=TRUE, sort=FALSE)
  
  ## Note: contributing_fraction is typically a good correction factor for depth, 
  ##       but not when there are missing data
  
  # corrected depth = max.depth * contributing_fraction (from slab) * soil_fraction (rock frag adjust)
  agg.soil.data.median$corrected_depth <- with(agg.soil.data.median,
                                               soil_fraction * contributing_fraction * max.depth
  )
  
  ## TODO return lower / upper limits
  
  # package and return
  res <- list(
    SPC = s, 
    agg = agg.soil.data.median
  )
  
  return(res)
}


