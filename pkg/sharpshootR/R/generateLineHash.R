# generate a MD5 hash using a line's start / end coordinates 
# after rounding to a fixed precision
generateLineHash <- function(x, precision=-1) {
  the.lines <- slot(x, 'lines')
  # sanity check:
  # (there should only be 1 / Line object)
  lines.per.Line <- sapply(the.lines, function(i) length(i@Lines))
  if(any(lines.per.Line > 1))
    stop('more than 1 line per segment, why?')
  
  # unzip lines into single list
  the.coords <- lapply(the.lines, function(i) slot(slot(i, 'Lines')[[1]], 'coords'))
  
  # generate digest from start / end vertices, rounded to defined precision
  res <- sapply(the.coords, function(i) {
    n <- nrow(i)
    start.coord <- i[1, ]
    end.coord <- i[n, ]
    hash <- digest(c(round(start.coord, precision), round(end.coord, precision)))
    return(hash)
  })
  
  return(res)
}
