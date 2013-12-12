# TODO: better generalization:
#         - generate box-whisker plots on re-scaled axis with custom scales
dissimilarity.sketch <- function(x, n.groups=2, v=c('clay', 'total_frags_pct')) {
  require(cluster)
  require(MASS)
  require(ape)
  require(reshape2)
  require(RColorBrewer)
  
  # nice colors
  cols <- brewer.pal(n=8, name='Set1')
  
  # generate an index to non-R|Cr|Cd horizons
  filter.idx <- grep('R|Cr|Cd', x$hzname, invert=TRUE)
  
  # establish max depth of eval, using presence of clay
  max.depth <- max(x, 'clay') + 10
  
  # dissimilarity matrix
  d <- profile_compare(x, vars=v, k=0, max_d=max.depth, rescale.result=TRUE, filter=filter.idx)
  
  # reduce to 2D: fudge 0-distances by adding a little bit
  s <- isoMDS(d+0.001, trace=FALSE)
  # generate plotting order as Euclidean distance to origin
  # this is similar to the similarity to the multivariate mean -- "modal concept"
  new.order <- order(sqrt(rowSums(sweep(s$points, MARGIN=2, STATS=c(0, 0), FUN='-')^2)))

  # convert distance matrix into dendrogram
  h <- as.hclust(diana(d))
  p <- as.phylo(h)
  
  # cut into groups
  g <- cutree(h, n.groups)
  # merge into site data
  site(x) <- data.frame(pedon_id=names(g), group=g, stringsAsFactors=FALSE)
  
  # convert SPC into data.frame
  df <- as(x, 'data.frame')
  
  # return pedon ID + hzname for those values:
  # based on "outliers" as defined by standard box-whisker plot interpretation
  values.to.check <- sapply(v, simplify=FALSE, FUN=function(v.i) {
    # stolen from boxplot.stats
    x.i <- df[[v.i]]
    stats <- fivenum(x.i, na.rm=TRUE)
    iqr <- diff(stats[c(2, 4)])
    outlier.idx <- which(x.i < (stats[2] - 1.5 * iqr) | x.i > (stats[4] + 1.5 * iqr))
    res <- paste(df$pedon_id[outlier.idx], '-', df$hzname[outlier.idx], ':', x.i[outlier.idx], sep='')
    if(length(outlier.idx) > 0) return(res) else return(NA)
  })
  
  
  # subset variable + group for box-whisker plot
  m <- melt(df, id.vars='group', measure.vars=c(v))
  m$group <- factor(m$group)
  m$variable <- factor(m$variable)
  
  # setup layout
  layout(matrix(c(1,2,3,3), byrow=TRUE, nrow=2), heights=c(1, 1.5))
  
  # dendrogram
  par(mar=c(0.125,0.25,0.25,0.25))
  plot(p, label.offset=0.01, x.lim=c(0,0.75), font=1, cex=0.75)
  tiplabels(pch=15, col=cols[g])
  
  # box-whisker plot + jittered points
  par(mar=c(0.125,0.125,0.25,0.25))
  boxplot(value ~ variable, data=m, horizontal=TRUE, las=1, cex.axis=0.75, xlab='Percent', boxwex=0.5, border=grey(0.5), axes=FALSE)
  text(x=tapply(m$value, m$variable, median, na.rm=TRUE), y=(1:length(v))+0.35, levels(m$variable), cex=1, font=2, pos=4)
  points(m$value, jitter(as.numeric(m$variable)), col=cols[m$group], pch=0, cex=1)
  axis(side=1, line=-1.25, cex.axis=0.75, padj=-1)
  
  # lower panel: profiles
  par(mar=c(0.25,0.25,0.25,2))
  plot(x, name='hzname', id.style='side', alt.label='taxonname', cex.depth.axis=1.125, cex.id=0.8, cex.names=0.65, plot.order=new.order, max.depth=max.depth, axis.line.offset=-3.5)
  segments(x0=0.5, y0=c(50, 75, 100, 150), x1=length(x) + 1.5, y1=c(50, 75, 100, 150), lty=3, col=rgb(0, 0, 0, alpha=0.25))
  points(x=1:length(x), y=rep(-2.5, times=length(x)), pch=15, col=cols[x$group[new.order]], cex=1.5)
    
  # return list of values that may need review:
  return(values.to.check)
}
