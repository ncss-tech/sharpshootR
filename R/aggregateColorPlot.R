

# x: results from aggregateColor()
aggregateColorPlot <- function(x, label.font=1, label.cex=0.65, buffer.pct=0.02, print.n.hz=FALSE, ...) {
 
  # extract just the scaled data from the results of aggregateColor()
  s.scaled <- x$scaled.data
  
  # get max re-scaled summation for xlim
  max.plot <- max(sapply(s.scaled, function(i) sum(i$weight)))
  
  # setup plot
  plot(1,1, type='n', xlim=c(0, max.plot), ylim=c(length(names(s.scaled))+0.5, 0.5), axes=FALSE, ylab='', xlab='', ...)
  box()
  # iterate over horizons
  for(i in seq_along(names(s.scaled))) {
    s.i <- s.scaled[[i]]
    n.colors <- nrow(s.i)
    if(n.colors > 0) {
      # get an index to the last weight
      last.weight <- length(s.i$weight)
      # compute cumulative left / right rectangle boundaries
      x.left <- cumsum(c(0, s.i$weight[-last.weight]))
      x.right <- c(x.left[-1], x.left[last.weight] + s.i$weight[last.weight])
      
      # plot rectanges from vectorized coordinates / colors
      rect(xleft = x.left, ybottom = i-0.5, xright = x.right, ytop = i+0.5, col = s.i$soil_color)
      
      # compute center point for color labels
      centers <- (x.right + x.left) / 2
      
      # create label
      if(print.n.hz)
        color.labels <- paste0(s.i$munsell, '\n', '(', s.i$n.hz, ')')
      else
        color.labels <- s.i$munsell
      
      # determine if there is enough room to label colors: weight of current color - some % of max weight (1)
      print.label <- which(abs(strheight(color.labels, cex=label.cex, font=label.font)) < (s.i$weight - buffer.pct) )
      
      # print labels
      if(length(print.label) > 0)
        text(x=centers[print.label], y=i, labels=color.labels[print.label], col='white', font=label.font, cex=label.cex, srt=90)
    }
  }
  
  axis(2, at = seq_along(names(s.scaled)), labels = names(s.scaled), las=2)
  
}
