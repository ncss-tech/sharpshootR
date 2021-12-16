
## TODO: remove all of these in the next major release


##
## Note: these queries are not very efficient because of the use of WHERE UPPER(compname) IN ", in.statement, "
## this doesn't properly utilize existing indexes
##

## related issue
# https://github.com/ncss-tech/sharpshootR/issues/12

# s: vector of soil series names
# replaceNA: convert missing categories into 0 probabilities
geomPosMountainProbability <- function(s, replaceNA=TRUE) {
  
  .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
  
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
    
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, geomposmntn as q_param, CAST(count(geomposmntn) AS numeric) AS q_param_n
             FROM legend
             INNER JOIN mapunit mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey 
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
             WHERE
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND geomposmntn IS NOT NULL
             GROUP BY UPPER(compname), geomposmntn
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM legend
             INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey 
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND geomposmntn IS NOT NULL
             GROUP BY UPPER(compname)
             ) AS b
             ON a.compname = b.compname
             ORDER BY compname, p DESC;", sep='')
  
  # perform query
  x <- SDA_query(q)
  
  # re-level
  x$q_param <- factor(x$q_param, levels=c('Mountaintop', 'Mountainflank', 'Upper third of mountainflank', 'Center third of mountainflank', 'Lower third of mountainflank', 'Mountainbase'))
  
  # convert from long-wide format
  y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
  
  # optionally convert NA to 0
  if(replaceNA) {
    for(i in 1:nrow(y)) {
      idx <- which(is.na(y[i, ]))
      y[i, ][idx] <- 0
    }
  }
  
  return(y)
}


# s: vector of soil series names
# replaceNA: convert missing categories into 0 probabilities
geomPosHillProbability <- function(s, replaceNA=TRUE) {
  
  .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
  
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
  
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, geomposhill as q_param, CAST(count(geomposhill) AS numeric) AS q_param_n
             FROM legend
             INNER JOIN mapunit mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey 
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND geomposhill IS NOT NULL
             GROUP BY UPPER(compname), geomposhill
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM legend
             INNER JOIN mapunit mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphgc on cogeomordesc.cogeomdkey = cosurfmorphgc.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND geomposhill IS NOT NULL
             GROUP BY UPPER(compname)
             ) AS b
             ON a.compname = b.compname
             ORDER BY compname, p DESC;", sep='')
  
  # perform query
  x <- SDA_query(q)
  
  # re-level
  x$q_param <- factor(x$q_param, levels=c('Interfluve', 'Crest', 'Head Slope', 'Nose Slope', 'Side Slope', 'Base Slope'))
  
  # convert from long-wide format
  y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
  
  # optionally convert NA to 0
  if(replaceNA) {
    for(i in 1:nrow(y)) {
      idx <- which(is.na(y[i, ]))
      y[i, ][idx] <- 0
    }
  }
  
  return(y)
}



# s: vector of soil series names
# replaceNA: convert missing categories into 0 probabilities
surfaceShapeProbability <- function(s, replaceNA=TRUE) {
  
  .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
  
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
  
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, shapeacross + '/' + shapedown as q_param, CAST(count(shapeacross + '/' + shapedown) AS numeric) AS q_param_n
             FROM legend
             INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey 
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND shapeacross IS NOT NULL
             AND shapedown IS NOT NULL
             GROUP BY UPPER(compname), shapeacross + '/' + shapedown
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM legend
             INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND shapeacross IS NOT NULL
             AND shapedown IS NOT NULL
             GROUP BY UPPER(compname)
             ) AS b
             ON a.compname = b.compname
             ORDER BY compname, p DESC;", sep='')
  
  # perform query
  x <- SDA_query(q)
  
  # re-level
  x$q_param <- factor(x$q_param, levels=c('Convex/Convex', 'Linear/Convex', 'Convex/Linear', 'Concave/Convex', 'Linear/Linear', 'Concave/Linear', 'Convex/Concave', 'Linear/Concave', 'Concave/Concave'))
  
  # convert from long-wide format
  y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
  
  # optionally convert NA to 0
  if(replaceNA) {
    for(i in 1:nrow(y)) {
      idx <- which(is.na(y[i, ]))
      y[i, ][idx] <- 0
    }
  }
  
  return(y)
}



# 's' is a vector of soil series names
hillslopeProbability <- function(s, replaceNA=TRUE) {	
  
  .Deprecated(msg = 'This function is now deprecated, consider using soilDB::fetchSDA() or soilDB::fetchOSD(..., extended = TRUE)')
  
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
  
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, hillslopeprof as q_param, CAST(count(hillslopeprof) AS numeric) AS q_param_n
             FROM legend
             INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey 
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND hillslopeprof IS NOT NULL
             GROUP BY UPPER(compname), hillslopeprof
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM legend
             INNER JOIN mapunit AS mu ON mu.lkey = legend.lkey
             INNER JOIN component AS co ON mu.mukey = co.mukey
             LEFT JOIN cogeomordesc ON co.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
             WHERE 
             legend.areasymbol != 'US'
             AND UPPER(compname) IN ", in.statement, "
             AND hillslopeprof IS NOT NULL
             GROUP BY UPPER(compname)
             ) AS b
             ON a.compname = b.compname
             ORDER BY compname, p DESC;", sep='')
  
  # perform query
  x <- SDA_query(q)
  
  # re-level hillslope positions
  x$q_param <- factor(x$q_param, levels=c('Toeslope', 'Footslope', 'Backslope', 'Shoulder', 'Summit'))
  
  # convert from long-wide format
  y <- dcast(x, compname ~ q_param, value.var='p', drop=FALSE)
  
  # optionally convert NA to 0
  if(replaceNA) {
    for(i in 1:nrow(y)) {
      idx <- which(is.na(y[i, ]))
      y[i, ][idx] <- 0
    }
  }
  return(y)
}

# for backwards compatibility:
hillslope.probability <- hillslopeProbability

