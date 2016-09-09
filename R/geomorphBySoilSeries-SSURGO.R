

# s: vector of soil series names
# replaceNA: convert missing categories into 0 probabilities
surfaceShapeProbability <- function(s, replaceNA=TRUE) {
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
  
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, shapeacross + '/' + shapedown as q_param, CAST(count(shapeacross + '/' + shapedown) AS numeric) AS q_param_n
             FROM component 
             LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
             WHERE UPPER(compname) IN ", in.statement, "
             AND shapeacross IS NOT NULL
             AND shapedown IS NOT NULL
             GROUP BY UPPER(compname), shapeacross + '/' + shapedown
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM component
             LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphss on cogeomordesc.cogeomdkey = cosurfmorphss.cogeomdkey
             WHERE UPPER(compname) IN ", in.statement, "
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
  # format IN statement, convert to upper case for comp name normalization
  in.statement <- format_SQL_in_statement(toupper(s))
  
  # format query
  q <- paste("
             SELECT a.compname, q_param, q_param_n, total, round(q_param_n / total, 2) AS p
             FROM
             (
             SELECT UPPER(compname) AS compname, hillslopeprof as q_param, CAST(count(hillslopeprof) AS numeric) AS q_param_n
             FROM component 
             LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
             WHERE UPPER(compname) IN ", in.statement, "
             AND geomftname = 'Landform'
             AND hillslopeprof IS NOT NULL
             GROUP BY UPPER(compname), hillslopeprof
             ) AS a
             JOIN
             (
             SELECT UPPER(compname) AS compname, CAST(count(compname) AS numeric) AS total
             FROM component
             LEFT JOIN cogeomordesc ON component.cokey = cogeomordesc.cokey
             LEFT JOIN cosurfmorphhpp on cogeomordesc.cogeomdkey = cosurfmorphhpp.cogeomdkey
             WHERE UPPER(compname) IN ", in.statement, "
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

