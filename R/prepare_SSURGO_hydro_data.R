
#' @title Get and prepare basic soil hydraulic parameters from SSURGO via SDA
#' 
#' @details Weighted mean soil hydraulic parameters are returned over the interval of `0-max.depth`, calculated by `aqp::slab()`.
#'
#' @param cokeys vector of component keys (cokey) in current SSURGO snapshot
#' @param max.depth target depth of aggregation (cm), corrected later by real soil depth as reported by `slab()`
#'
#' @author D.E. Beaudette
#'
#' @return a `list` containing:
#'    * `SPC`: `SoilProfileCollection`
#'    * `agg`: aggregate representation of hydraulic parameters, by cokey
#' 
#' The following soil hydraulic properties are included:
#'  
#'   |variable      |description                                                   |
#'   |:-------------|:-------------------------------------------------------------|
#'   |cokey         |component key                                                 |
#'   |hzname        |horizon name                                                  |
#'   |hz_top        |horizon top depth (cm)                                        |
#'   |hz_bottom     |horizon bottom depth (cm)                                     |
#'   |thick         |horizon thickness (cm)                                        |
#'   |sat           |VWC at saturation (cm/cm)                                     |
#'   |fc            |VWC at field capacity defined by 1/3rd bar tension (cm/cm)    |
#'   |fc_tenthbar   |VWC at field capacity defined by 1/3rd bar tension (cm/cm)    |
#'   |pwp           |VWC at permanent wilting point or 15 bar tension (cm/cm)      |
#'   |awc           |total sand content (<2mm fraction, mass %)                    |
#'   |sand          |total silt content (<2mm fraction, mass %)                    |
#'   |silt          |total clay content (<2mm fraction, mass %)                    |
#'   |clay          |total sand content (<2mm fraction, mass %)                    |
#'   |dbthirdbar    |bulk density at 1/3 bar tension (g/cm^3)                      |
#'   |ksat          |Ksat (um/second)                                              |
#'   |soil_fraction |volume fraction of soil (1 - coarse fragment volume fraction) |
#' 
#'     
#' @export
#'
#' 
#' 
prepare_SSURGO_hydro_data <- function(cokeys, max.depth) {
  
  ## TODO return lower / upper limits
  
  in.statement <- format_SQL_in_statement(cokeys)
  
  # assemble SDA query
  sql <- sprintf("
                 SELECT chorizon.cokey, compname, comppct_r, majcompflag, drainagecl, hzname, hzdept_r AS hz_top, hzdepb_r AS hz_bottom,
                 (hzdepb_r - hzdept_r) AS thick,
                 wsatiated_r / 100.0 AS sat,
                 wthirdbar_r / 100.0 AS fc,
                 wtenthbar_r / 100.0 AS fc_tenthbar,
                 wfifteenbar_r / 100.0 as pwp,
                 awc_r as awc,
                 sandtotal_r as sand,
                 silttotal_r as silt,
                 claytotal_r as clay,
                 dbthirdbar_r as dbthirdbar,
                 ksat_r as ksat,
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
  
  ## account for no / missing data
  if(is.null(s)) {
    stop('no data returned', call. = FALSE)
  }
  
  
  # init SPC for slab()
  s$cokey <- as.character(s$cokey)
  depths(s) <- cokey ~ hz_top + hz_bottom
  site(s) <- ~ compname + comppct_r + majcompflag + drainagecl
  hzdesgnname(s) <- 'hzname'
  
  # weighted mean
  agg.soil.data <- slab(
    s, 
    fm = cokey ~ sat + fc + pwp + awc + soil_fraction, 
    slab.structure = c(0, max.depth), 
    slab.fun = mean, 
    na.rm = TRUE
  )
  
  # retaining only wt.mean
  # using reshape2
  agg.soil.data.wide <- dcast(agg.soil.data, cokey ~ variable, value.var = 'value')
  
  # get contributing_fraction for corrected depth calculation
  real.depths <- unique(agg.soil.data[, c('cokey', 'contributing_fraction')])
  
  # join component names
  real.depths <- merge(real.depths, site(s)[, c('cokey', 'compname')], by = 'cokey', all.x = TRUE, sort = FALSE)
  
  # join corrected depths with aggregate data
  agg.soil.data.wide <- merge(agg.soil.data.wide, real.depths, by='cokey', all.x = TRUE, sort = FALSE)
  
  ## Note: contributing_fraction is typically a good correction factor for depth, 
  ##       but not when there are missing data
  
  # corrected depth = max.depth * contributing_fraction (from slab) * soil_fraction (rock frag adjust)
  agg.soil.data.wide$corrected_depth <- with(agg.soil.data.wide,
                                             soil_fraction * contributing_fraction * max.depth
  )
  
  # package and return
  res <- list(
    SPC = s, 
    agg = agg.soil.data.wide
  )
  
  return(res)
}


